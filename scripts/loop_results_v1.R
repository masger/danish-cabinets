# loop_results_v1.R
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
    if (is.na(x)) return("-")
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

# Career Survival
intl_min[, fired := 0]
intl_min[!is.na(gov_end_dt) & !is.na(end_dt), fired := ifelse(as.numeric(difftime(gov_end_dt, end_dt, units="days")) > 30, 1, 0)]
intl_min[, career_time := as.numeric(difftime(end_dt, start_dt, units = "days")) / 365.25]
intl_min[!is.na(career_time) & career_time <= 0, career_time := 0.01]

# Biological Survival
intl_unique <- unique(intl_min[!is.na(dob_dt)], by = "name")
intl_unique[, dead := ifelse(!is.na(dod_dt), 1, 0)]
intl_unique[, end_obs_dt := dod_dt]
intl_unique[is.na(end_obs_dt), end_obs_dt := Sys.Date()]
intl_unique[, lifespan := as.numeric(difftime(end_obs_dt, dob_dt, units = "days")) / 365.25]
intl_unique <- intl_unique[lifespan >= 35]

# --- 1. Baseline Table ---
baseline <- intl_min[, .(
    Appointments = .N,
    Unique_Ministers = length(unique(name)),
    Era_Start = year(min(start_dt, na.rm=T)),
    Era_End = year(max(start_dt, na.rm=T))
), by = country_name]
setorder(baseline, country_name)

# --- 2. Career Survival Table ---
# HR vs Denmark
km_career <- survfit(Surv(career_time, fired) ~ country_name, data = intl_min[!is.na(career_time)])
cox_career <- coxph(Surv(career_time, fired) ~ relevel(factor(country_name), ref="Denmark"), data = intl_min[!is.na(career_time)])
summary_career <- summary(cox_career)$coefficients

career_tbl <- intl_min[!is.na(career_time), .(
    N = .N,
    Fired_N = sum(fired),
    Fired_Pct = bmj_pct(sum(fired), .N)
), by = country_name]

# Add HRs
hr_vals <- data.frame(country_name = target_countries, HR = 1, CI_L = 1, CI_H = 1, P = NA)
for(c in target_countries) {
    if(c == "Denmark") next
    term <- paste0("relevel(factor(country_name), ref = \"Denmark\")", c)
    if(term %in% rownames(summary_career)) {
        hr_vals[hr_vals$country_name == c, "HR"] <- exp(summary_career[term, 1])
        hr_vals[hr_vals$country_name == c, "CI_L"] <- exp(summary_career[term, 1] - 1.96*summary_career[term, 3])
        hr_vals[hr_vals$country_name == c, "CI_H"] <- exp(summary_career[term, 1] + 1.96*summary_career[term, 3])
        hr_vals[hr_vals$country_name == c, "P"] <- summary_career[term, 5]
    }
}
career_tbl <- merge(career_tbl, hr_vals, by="country_name")
career_tbl[, `HR (95% CI)` := ifelse(country_name == "Denmark", "1.00 (Reference)", 
                                     sprintf("%.2f (%.2f to %.2f)", HR, CI_L, CI_H))]
career_tbl[, `*P* value` := ifelse(is.na(P), "-", bmj_p(P))]

# --- 3. Biological Survival Table ---
cox_bio <- coxph(Surv(lifespan, dead) ~ relevel(factor(country_name), ref="Denmark"), data = intl_unique)
summary_bio <- summary(cox_bio)$coefficients

bio_tbl <- intl_unique[, .(
    N = .N,
    Dead_N = sum(dead),
    Dead_Pct = bmj_pct(sum(dead), .N),
    Median_Age = round(median(lifespan), 1)
), by = country_name]

hr_bio_vals <- data.frame(country_name = target_countries, HR = 1, CI_L = 1, CI_H = 1, P = NA)
for(c in target_countries) {
    if(c == "Denmark") next
    term <- paste0("relevel(factor(country_name), ref = \"Denmark\")", c)
    if(term %in% rownames(summary_bio)) {
        hr_bio_vals[hr_bio_vals$country_name == c, "HR"] <- exp(summary_bio[term, 1])
        hr_bio_vals[hr_bio_vals$country_name == c, "CI_L"] <- exp(summary_bio[term, 1] - 1.96*summary_bio[term, 3])
        hr_bio_vals[hr_bio_vals$country_name == c, "CI_H"] <- exp(summary_bio[term, 1] + 1.96*summary_bio[term, 3])
        hr_bio_vals[hr_bio_vals$country_name == c, "P"] <- summary_bio[term, 5]
    }
}
bio_tbl <- merge(bio_tbl, hr_bio_vals, by="country_name")
bio_tbl[, `HR (95% CI)` := ifelse(country_name == "Denmark", "1.00 (Reference)", 
                                  sprintf("%.2f (%.2f to %.2f)", HR, CI_L, CI_H))]
bio_tbl[, `*P* value` := ifelse(is.na(P), "-", bmj_p(P))]

# --- Output MD ---
cat("# Iteration 1 Results\n\n", file="reports/expanded_results.md")
cat("## Table 1. Baseline characteristics of European cabinet ministers\n", file="reports/expanded_results.md", append=T)
cat(kable(baseline, format="markdown"), file="reports/expanded_results.md", append=T)
cat("\n\n## Table 2. Political career survival: Hazard of early termination (fired) by country\n", file="reports/expanded_results.md", append=T)
cat(kable(career_tbl[, .(Country=country_name, N, Fired=Fired_Pct, `HR (95% CI)`, `*P* value`)], format="markdown"), file="reports/expanded_results.md", append=T)
cat("\n\n## Table 3. Biological mortality: Hazard of death by country\n", file="reports/expanded_results.md", append=T)
cat(kable(bio_tbl[, .(Country=country_name, N, Dead=Dead_Pct, `Median Age`=Median_Age, `HR (95% CI)`, `*P* value`)], format="markdown"), file="reports/expanded_results.md", append=T)
