library(data.table)
library(lubridate)
library(survival)

# Load data
csv_files <- list.files("data/international", pattern = "_(Finance|Health|Defense|Interior|Transport)_enriched\\.csv$", full.names = TRUE)
dat <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)

parse_dt <- function(d_str) {
    d_str <- trimws(d_str)
    d_str[d_str %in% c("Incumbent", "Present", "")] <- NA
    parse_date_time(d_str, orders = c("d B Y", "Y-m-d", "d m Y", "B d, Y", "Y"))
}

dat[, start_dt := parse_dt(start_date)]
dat[, end_dt := parse_dt(end_date)]
dat[is.na(end_dt) & (end_date %in% c("Incumbent", "Present")), end_dt := Sys.time()]
dat[, dob_dt := parse_dt(dob)]
dat[, dod_dt := parse_dt(dod)]

# 1. How many died on the job?
# We define this strictly: either their Date of Death matches the end date year/month
# Or DOD is within 30 days of them leaving the role.
dat[, end_y := as.numeric(format(end_dt, "%Y"))]
dat[, dod_y := as.numeric(format(dod_dt, "%Y"))]
dat[, died_in_office := 0]
dat[(!is.na(dod_dt) & !is.na(end_dt) & abs(as.numeric(difftime(dod_dt, end_dt, units = "days"))) <= 30) |
    (!is.na(dod_y) & !is.na(end_y) & dod_y == end_y), died_in_office := 1]

# 2. Total lifespan calculations
# Since many ministers are still alive today, we use survival analysis to estimate mean lifespan.
# Event: 1 = dead, 0 = alive as of today (censored)
dat_unique <- unique(dat[!is.na(dob_dt), .(name, country, dob_dt, dod_dt)])
dat_unique[, event_death := ifelse(!is.na(dod_dt), 1, 0)]

# If dead, age at death. If alive, age today.
dat_unique[, end_obs_dt := dod_dt]
dat_unique[is.na(end_obs_dt), end_obs_dt := Sys.Date()]
dat_unique[, lifespan_years := as.numeric(difftime(end_obs_dt, dob_dt, units = "days")) / 365.25]
dat_unique <- dat_unique[lifespan_years >= 30] # Filter out bad Wiki parsing (e.g. 0 year olds)

# Outputs
cat("=== DEATHS IN OFFICE ===\n")
dead_in_office <- dat[died_in_office == 1, .(name, country, portfolio, start_dt, end_dt, dod_dt)]
print(dead_in_office)
cat("\nTotal Ministers who died in office: ", nrow(dead_in_office), "\n")

cat("\n=== AVERAGE LIFESPAN BY COUNTRY (Kaplan-Meier Median) ===\n")
# We use survfit to properly account for the ministers who are still alive (right-censored)
km_fit <- survfit(Surv(lifespan_years, event_death) ~ country, data = dat_unique)

# Extract Median Survival from summary table
res_table <- summary(km_fit)$table
country_names <- gsub("country=", "", rownames(res_table))
med_df <- data.table(
    Country = country_names,
    MinistersAnalyzed = res_table[, "records"],
    DeceasedSoFar = res_table[, "events"],
    MedianLifespanYrs = round(res_table[, "median"], 1)
)
print(med_df[order(-MedianLifespanYrs)])

cat("\n=== COX PROPORTIONAL HAZARDS (Mortality Risk relative to Denmark) ===\n")
dat_unique[, country_f := relevel(as.factor(country), ref = "Denmark")]
cox_life <- coxph(Surv(lifespan_years, event_death) ~ country_f, data = dat_unique)
print(summary(cox_life)$coefficients)
