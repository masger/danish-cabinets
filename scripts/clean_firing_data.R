library(data.table)
library(lubridate)

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
dat[, dod_dt := parse_dt(dod)]

dat[, tenure_days := as.numeric(difftime(end_dt, start_dt, units = "days"))]
dat[, tenure_years := tenure_days / 365.25]
dat[is.na(tenure_years) | tenure_years <= 0, tenure_years := 0.1]

# Step 1: Identify "Mass Extinction Events" (Elections / Cabinet Collapses)
# If multiple ministers in the same country end their term within 14 days of each other,
# it is a change of government, not an individual firing.
dat[, end_year_month := format(end_dt, "%Y-%m")]
extinctions <- dat[!is.na(end_dt), .N, by = .(country, end_year_month)][N >= 2]
dat[, is_mass_extinction := ifelse(!is.na(end_dt) & paste(country, end_year_month) %in% paste(extinctions$country, extinctions$end_year_month), 1, 0)]

# Step 2: Define the TRUE Events
# Event 0 = Censored (Still incumbent, or safely reached a Mass Extinction / Election)
# Event 1 = TRULY Fired (Left office prematurely without a mass extinction event)
# Event 2 = Died in Office
dat[, true_event := 0]

# Fired = Left office, NOT during a mass extinction, and didn't die
dat[is_mass_extinction == 0 & end_date != "Incumbent" & end_date != "Present" & !is.na(end_dt), true_event := 1]

# Died = dod is within 30 days of end_dt, OR dod year is the exactly the end_year
dat[, end_y := as.numeric(format(end_dt, "%Y"))]
dat[, dod_y := as.numeric(format(dod_dt, "%Y"))]
dat[(!is.na(dod_dt) & abs(as.numeric(difftime(dod_dt, end_dt, units = "days"))) <= 30) | (!is.na(dod_y) & dod_y == end_y), true_event := 2]

cat("=== THE TRUE EMPIRICAL FATES OVER 50 YEARS ===\n")
cat("Total Ministers Analyzed: ", nrow(dat), "\n\n")

fate_summary <- dat[, .(
    Total = .N,
    `Survived to Election (%)` = round(100 * sum(true_event == 0) / .N, 1),
    `Truly Fired (%)` = round(100 * sum(true_event == 1) / .N, 1),
    `Died in Office (%)` = round(100 * sum(true_event == 2) / .N, 1)
), by = portfolio][order(-`Truly Fired (%)`)]

print(fate_summary)

cat("\n=== SURVIVED PAST THE PENSION POINT (1.5 YEARS) ===\n")
# The Danish Pension Threshold Analysis
dat[, reached_pension := ifelse(tenure_years >= 1.5, 1, 0)]
pension_summary <- dat[, .(
    Total = .N,
    `Reached 1.5 Yr Pension (%)` = round(100 * mean(reached_pension), 1)
), by = portfolio][order(`Reached 1.5 Yr Pension (%)`)]
print(pension_summary)

cat("\n=== THE DEFENSE MEAT GRINDER: FIRING OVER TIME ===\n")
dat[, portfolio_f := relevel(as.factor(portfolio), ref = "Finance")]
library(survival)
cox_fire <- coxph(Surv(tenure_years, true_event == 1) ~ portfolio_f, data = dat)
print(summary(cox_fire)$coefficients)
