# loop_results_v2.R
library(data.table)
library(survival)
library(lubridate)
library(knitr)

# Helper for BMJ formatting
bmj_pct <- function(n, N) {
  pct <- round(100 * n/N, 1)
  sprintf("%.1f%% (%d/%d)", pct, as.integer(n), as.integer(N))
}

bmj_p <- function(p) {
  sapply(p, function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
    if (x < 0.001) return("<0.001")
    sprintf("%.3f", x)
  })
}

parse_dt <- function(d_str) {
    if (is.null(d_str)) return(as.POSIXct(NA))
    d_str <- trimws(d_str)
    d_str[d_str %in% c("Incumbent", "Present", "")] <- NA
    parse_date_time(d_str, orders = c("d B Y", "Y-m-d", "d m Y", "B d, Y", "Y"))
}

target_countries <- c("Denmark", "France", "Germany", "Greece", "Italy", "Netherlands", "Norway", "Spain", "Sweden", "United Kingdom")
csv_files <- list.files("data/international", pattern = "_enriched\\.csv$", full.names = TRUE)
intl_min <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)
intl_min[, start_dt := parse_dt(start_date)]
intl_min[, end_dt := parse_dt(end_date)]
intl_min[, dob_dt := parse_dt(dob)]
intl_min[, dod_dt := parse_dt(dod)]
intl_min[, country_name := country]
intl_min[country == "UK", country_name := "United Kingdom"]
intl_min <- intl_min[!is.na(start_dt)]

# Match gov dates
whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")
gov_dates <- whogov[country_name %in% target_countries & !is.na(govern_start_date), 
                    .(country_name, govern_name, govern_start_date, govern_end_date)]
gov_dates[, govern_start_date := parse_dt(govern_start_date)]
gov_dates[, govern_end_date := parse_dt(govern_end_date)]
gov_dates <- unique(gov_dates, by = c("country_name", "govern_name", "govern_start_date"))

intl_min[, gov_end_dt := as.POSIXct(NA)]
for(c in target_countries) {
    c_govs <- gov_dates[country_name == c]
    if(nrow(c_govs) == 0) next
    for(i in 1:nrow(c_govs)) {
        intl_min[country_name == c & start_dt >= c_govs$govern_start_date[i] & start_dt <= c_govs$govern_end_date[i], 
                 gov_end_dt := c_govs$govern_end_date[i]]
    }
}

# Career & Death flags
intl_min[, fired := 0]
intl_min[!is.na(gov_end_dt) & !is.na(end_dt), fired := ifelse(as.numeric(difftime(gov_end_dt, end_dt, units="days")) > 30, 1, 0)]
intl_min[, career_time := as.numeric(difftime(end_dt, start_dt, units = "days")) / 365.25]
intl_min[!is.na(career_time) & career_time <= 0, career_time := 0.01]

intl_min[, dod_y := year(dod_dt)]
intl_min[, end_y := year(end_dt)]
intl_min[, died_in_office := 0]
intl_min[(!is.na(dod_dt) & !is.na(end_dt) & abs(as.numeric(difftime(dod_dt, end_dt, units="days"))) <= 30) | 
         (!is.na(dod_y) & !is.na(end_y) & dod_y == end_y), died_in_office := 1]

intl_unique <- unique(intl_min[!is.na(dob_dt)], by = "name")
intl_unique[, dead := ifelse(!is.na(dod_dt), 1, 0)]
intl_unique[, end_obs_dt := dod_dt]
intl_unique[is.na(end_obs_dt), end_obs_dt := Sys.Date()]
intl_unique[, lifespan := as.numeric(difftime(end_obs_dt, dob_dt, units = "days")) / 365.25]
intl_unique <- intl_unique[lifespan >= 35]

# --- 1. Baseline Table (Refined) ---
baseline <- intl_min[, .(
    Appointments = .N,
    `Unique Ministers` = length(unique(name)),
    `Study Period` = sprintf("%d-%d", year(min(start_dt, na.rm=T)), year(max(start_dt, na.rm=T)))
), by = .(Country = country_name)]
setorder(baseline, Country)

# --- 2. Portfolio Hazard League Table ---
# Calculate HR for Transport vs Others within each country
league <- data.table(Country = target_countries, HR = as.numeric(NA), P = as.numeric(NA))
for(c in target_countries) {
    c_dat <- intl_min[country_name == c & !is.na(career_time)]
    c_dat[, is_transport := grepl("transport|trafik", tolower(portfolio))]
    if(sum(c_dat$is_transport) > 3 & sum(c_dat$fired) > 5) {
        fit <- coxph(Surv(career_time, fired) ~ is_transport, data = c_dat)
        league[Country == c, HR := exp(coef(fit))]
        league[Country == c, P := summary(fit)$coefficients[1, 5]]
    }
}
league[, `Hazard Ratio (Transport)` := sprintf("%.2f", HR)]
league[is.na(HR), `Hazard Ratio (Transport)` := "Insufficient data"]
league[, `*P* value` := bmj_p(P)]
setorder(league, -HR, na.last = TRUE)

# --- 3. Fatal Attraction: Death in Office vs Career Termination ---
fatal <- intl_min[, .(
    Appointments = .N,
    `Fired (N)` = sum(fired),
    `Died in Office (N)` = sum(died_in_office)
), by = .(Country = country_name)]
fatal[, `Ratio (Fired/Died)` := round(`Fired (N)` / pmax(`Died in Office (N)`, 0.1), 1)]
setorder(fatal, -`Fired (N)`)

# --- Output MD ---
cat("# Iteration 2 Results\n\n", file="reports/expanded_results.md")
cat("## Table 1. Baseline characteristics of European cabinet ministers\n", file="reports/expanded_results.md", append=T)
cat(kable(baseline, format="markdown"), file="reports/expanded_results.md", append=T)
cat("\n\n## Table 2. The Portfolio Hazard League Table: Transport vs Other Portfolios\n", file="reports/expanded_results.md", append=T)
cat("This table assesses the 'curse' of the Transport portfolio within each nation. A Hazard Ratio > 1 indicates that Transport Ministers face higher political mortality than their colleagues.\n\n", file="reports/expanded_results.md", append=T)
cat(kable(league[, .(Country, `Hazard Ratio (Transport)`, `*P* value`)], format="markdown"), file="reports/expanded_results.md", append=T)
cat("\n\n## Table 3. Fatal Attraction: Political vs Biological 'Death' in Office\n", file="reports/expanded_results.md", append=T)
cat(kable(fatal[, .(Country, Appointments, `Fired (N)`, `Died in Office (N)`, `Ratio (Fired/Died)`)], format="markdown"), file="reports/expanded_results.md", append=T)
