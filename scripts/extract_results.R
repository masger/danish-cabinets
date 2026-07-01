library(data.table)
library(survival)

# Load data
csv_files <- list.files("data/international", pattern = "_(Finance|Health|Defense|Interior|Transport)_enriched\\.csv$", full.names = TRUE)
dat <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)

# Clean dates
extract_year <- function(date_str) {
    as.numeric(substr(date_str, 1, 4))
}
dat[, start_y := extract_year(start_date)]
dat[, end_y := ifelse(end_date == "Incumbent" | end_date == "Present" | is.na(end_date), 2026, extract_year(end_date))]
dat[, dod_y := extract_year(dod)]

# Calculate duration
dat[, base_t := as.numeric(end_y) - as.numeric(start_y)]
dat[is.na(base_t) | base_t < 0, base_t := 0.5]

# Define events
dat[, event := 0]
dat[end_date != "Incumbent" & end_date != "Present" & !is.na(end_date), event := 1] # Fired/Exited
dat[!is.na(dod_y) & dod_y == end_y, event := 2] # Died in office year

dat[, final_time := pmax(base_t, 0.1)]
dat[, portfolio_f := relevel(factor(portfolio), ref = "Finance")]

cat("TOTAL MINISTERS:", nrow(dat), "\n")
cat("\n--- SUMMARY BY PORTFOLIO ---\n")
print(dat[, .(N = .N, Fired = sum(event == 1), Died = sum(event == 2)), by = portfolio])

cat("\n--- COX MODEL: FIRING (Event 1) ---\n")
cox_fire <- coxph(Surv(final_time, event == 1) ~ portfolio_f, data = dat)
print(summary(cox_fire)$coefficients)

cat("\n--- COX MODEL: DEATH (Event 2) ---\n")
dod_count <- sum(dat$event == 2)
cat("Total deaths in office:", dod_count, "\n")
if (dod_count > 0) {
    cox_death <- coxph(Surv(final_time, event == 2) ~ portfolio_f, data = dat)
    print(summary(cox_death)$coefficients)
}
