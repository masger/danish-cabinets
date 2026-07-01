library(data.table)
library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(broom)

# Helper: Parse dates
parse_dt <- function(d_str) {
    if (is.null(d_str)) return(as.POSIXct(NA))
    d_str <- trimws(as.character(d_str))
    d_str[d_str %in% c("Incumbent", "Present", "")] <- NA
    parse_date_time(d_str, orders = c("d B Y", "Y-m-d", "d m Y", "B d, Y", "Y", "Y-m"))
}

# 1. Load International Minister Data
csv_files <- list.files("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/danish-cabinets/data/international", pattern = "_enriched\\.csv$", full.names = TRUE)
intl_min <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)

intl_min[, start_dt := parse_dt(start_date)]
intl_min[, end_dt := parse_dt(end_date)]
intl_min[, dob_dt := parse_dt(dob)]
intl_min[, dod_dt := parse_dt(dod)]

# 2. Load WhoGov for Government Dates
whogov <- fread("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/danish-cabinets/data/WhoGov_crosssectional_V3.1.csv")
target_countries <- c("Denmark", "France", "Germany", "Greece", "Italy", "Netherlands", "Norway", "Spain", "Sweden", "United Kingdom")
gov_dates <- whogov[country_name %in% target_countries & !is.na(govern_start_date), 
                    .(country_name, govern_name, govern_start_date, govern_end_date)]
gov_dates[, govern_start_date := parse_dt(govern_start_date)]
gov_dates[, govern_end_date := parse_dt(govern_end_date)]
gov_dates <- unique(gov_dates, by = c("country_name", "govern_name", "govern_start_date"))
setorder(gov_dates, country_name, govern_start_date)

# 3. Match Ministers to Governments and Determine "Fired"
intl_min[, country_name := country]
intl_min[country == "UK", country_name := "United Kingdom"]
intl_min <- intl_min[!is.na(start_dt)]

# Vectorized Matching
find_gov_end <- function(c_name, s_dt) {
    c_govs <- gov_dates[country_name == c_name]
    if (nrow(c_govs) == 0) return(as.POSIXct(NA))
    match_idx <- which(c_govs$govern_start_date <= s_dt & c_govs$govern_end_date >= s_dt)
    if (length(match_idx) > 0) return(c_govs$govern_end_date[max(match_idx)])
    return(as.POSIXct(NA))
}

intl_min[, gov_end_dt := as.POSIXct(NA)]
for(c in target_countries) {
    c_govs <- gov_dates[country_name == c]
    if(nrow(c_govs) == 0) next
    for(i in 1:nrow(c_govs)) {
        intl_min[country_name == c & start_dt >= c_govs$govern_start_date[i] & start_dt <= c_govs$govern_end_date[i], 
                 gov_end_dt := c_govs$govern_end_date[i]]
    }
}

# Define "Early termination" (Fired)
intl_min[, fired := 0]
intl_min[!is.na(gov_end_dt) & !is.na(end_dt), fired := ifelse(as.numeric(difftime(gov_end_dt, end_dt, units="days")) > 30, 1, 0)]

# Identify "Died in Office"
intl_min[, dod_y := year(dod_dt)]
intl_min[, end_y := year(end_dt)]
intl_min[, died_in_office := 0]
intl_min[(!is.na(dod_dt) & !is.na(end_dt) & abs(as.numeric(difftime(dod_dt, end_dt, units="days"))) <= 30) | 
         (!is.na(dod_y) & !is.na(end_y) & dod_y == end_y), died_in_office := 1]

# Career time
intl_min[, career_time := as.numeric(difftime(end_dt, start_dt, units = "days")) / 365.25]
intl_min[!is.na(career_time) & career_time <= 0, career_time := 0.01]

# Biological Stats
intl_unique <- unique(intl_min[!is.na(dob_dt)], by = "name")
intl_unique[, dead := ifelse(!is.na(dod_dt), 1, 0)]
intl_unique[, end_obs_dt := dod_dt]
intl_unique[is.na(end_obs_dt), end_obs_dt := Sys.Date()]
intl_unique[, lifespan := as.numeric(difftime(end_obs_dt, dob_dt, units = "days")) / 365.25]
intl_unique <- intl_unique[lifespan >= 35]

# 4. Generate Table 1
table1_rep <- intl_min[!is.na(career_time), .(
    N_Appt = .N,
    N_Fired = sum(fired, na.rm=T),
    N_Died_In_Office = sum(died_in_office, na.rm=T)
), by = country_name]
table1_rep[, Pct_Fired := round(100 * N_Fired/N_Appt, 1)]

table1_bio <- intl_unique[, .(
    N_Indiv = .N,
    N_Dead_Total = sum(dead),
    Median_Age = round(median(lifespan), 1)
), by = country_name]

final_table1 <- merge(table1_rep, table1_bio, by = "country_name", all = TRUE)
setorder(final_table1, -Pct_Fired)

cat("\n--- TABLE 1: CROSS-COUNTRY COMPARISON ---\n")
print(final_table1)

# 5. Survival Analyses
km_career <- survfit(Surv(career_time, fired) ~ country_name, data = intl_min[!is.na(career_time) & career_time < 15])
km_bio <- survfit(Surv(lifespan, dead) ~ country_name, data = intl_unique)

saveRDS(list(
    table1 = final_table1,
    km_career = km_career,
    km_bio = km_bio
), "c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/danish-cabinets/reports/intl_analysis_results.rds")

cat("\nAnalysis Complete.\n")
