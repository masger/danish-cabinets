library(data.table)
library(survival)
library(lubridate)
library(stringr)

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

dat[, tenure_days := as.numeric(difftime(end_dt, start_dt, units = "days"))]
dat[, tenure_years := tenure_days / 365.25]
dat[is.na(tenure_years) | tenure_years <= 0, tenure_years := 0.1]
dat[, age_start := as.numeric(difftime(start_dt, dob_dt, units = "days")) / 365.25]

dat[, end_month := month(end_dt)]
dat[, start_month := month(start_dt)]
dat[, start_weekday := wday(start_dt)]
dat[, event_fire := ifelse(tenure_years < 3.5, 1, 0)]

results <- list()

add_res <- function(name, p_val) {
    results[[name]] <<- p_val
}

# H1: Name Length
dat[, name_len := nchar(name)]
c1 <- cor.test(dat$name_len, dat$tenure_years)
add_res("H1: Name Length vs Tenure", c1$p.value)

# H2: Zodiac (Capricorn)
dat[, zodiac_capricorn := ifelse(!is.na(dob_dt) & month(dob_dt) %in% c(12, 1), 1, 0)]
c2 <- coxph(Surv(tenure_years, event_fire) ~ zodiac_capricorn, data = dat)
add_res("H2: Zodiac Capricorn", summary(c2)$coefficients[, 5])

# H4: Serial Masochist
dat[, num_terms := .N, by = name]
dat[, serial_masochist := ifelse(num_terms > 1, 1, 0)]
c4 <- coxph(Surv(tenure_years, event_fire) ~ serial_masochist, data = dat)
add_res("H4: Multi-term Ministers", summary(c4)$coefficients[, 5])

# H5: Alphabet (K or F)
dat[, last_initial := str_sub(word(name, -1), 1, 1)]
dat[, is_KF := ifelse(last_initial %in% c("K", "F"), 1, 0)]
c5 <- coxph(Surv(tenure_years, event_fire) ~ is_KF, data = dat)
add_res("H5: Last initial K or F", summary(c5)$coefficients[, 5])

# H7: Youth Overconfidence (<40)
dat[, is_young := ifelse(age_start < 40, 1, 0)]
c7 <- coxph(Surv(tenure_years, event_fire) ~ is_young, data = dat)
add_res("H7: Age < 40", summary(c7)$coefficients[, 5])

# H8: Old Guard (>65)
dat[, is_elder := ifelse(age_start > 65, 1, 0)]
c8 <- coxph(Surv(tenure_years, event_fire) ~ is_elder, data = dat)
add_res("H8: Age > 65", summary(c8)$coefficients[, 5])

# H9: Mediterranean Chaos (Greece/Italy)
dat[, is_med := ifelse(country %in% c("Greece", "Italy"), 1, 0)]
c9 <- coxph(Surv(tenure_years, event_fire) ~ is_med, data = dat)
add_res("H9: Greece/Italy", summary(c9)$coefficients[, 5])

# H10: Midlife crisis
dat[, midlife_crisis := ifelse(floor(age_start) %in% 49:51, 1, 0)]
c10 <- coxph(Surv(tenure_years, event_fire) ~ midlife_crisis, data = dat)
add_res("H10: Age 49-51", summary(c10)$coefficients[, 5])

# H11: November
dat[, is_nov := ifelse(end_month == 11, 1, 0)]
c11 <- coxph(Surv(tenure_years, event_fire) ~ is_nov, data = dat)
add_res("H11: Fired in November", summary(c11)$coefficients[, 5])

# H12: Alliteration
dat[, first_init := str_sub(name, 1, 1)]
dat[, alliteration := ifelse(first_init == last_initial, 1, 0)]
c12 <- coxph(Surv(tenure_years, event_fire) ~ alliteration, data = dat)
add_res("H12: Alliterative Name", summary(c12)$coefficients[, 5])

# H13: Weekend appt
dat[, weekend_start := ifelse(start_weekday %in% c(1, 7), 1, 0)]
c13 <- coxph(Surv(tenure_years, event_fire) ~ weekend_start, data = dat)
add_res("H13: Weekend Appointment", summary(c13)$coefficients[, 5])

# H14: UK Transport
dat[, uk_transport := ifelse(country == "UK" & portfolio == "Transport", 1, 0)]
c14 <- coxph(Surv(tenure_years, event_fire) ~ uk_transport, data = dat)
add_res("H14: UK Transport Minister", summary(c14)$coefficients[, 5])

# H15: Leap Year
dat[, leap_year_start := ifelse(year(start_dt) %% 4 == 0, 1, 0)]
c15 <- coxph(Surv(tenure_years, event_fire) ~ leap_year_start, data = dat)
add_res("H15: Appointed in Leap Year", summary(c15)$coefficients[, 5])

# H18: Female-coded names in Defense
dat[, ends_in_a := ifelse(str_sub(name, -1) == "a", 1, 0)]
c18 <- coxph(Surv(tenure_years, event_fire) ~ ends_in_a, data = dat[portfolio == "Defense"])
add_res("H18: Name ends in 'a' (Defense)", summary(c18)$coefficients[, 5])

# H19: Short last name
dat[, short_last := ifelse(nchar(word(name, -1)) <= 3, 1, 0)]
c19 <- coxph(Surv(tenure_years, event_fire) ~ short_last, data = dat)
add_res("H19: 3-Letter Last Name", summary(c19)$coefficients[, 5])

cat("--- SIGNIFICANT JOKER HYPOTHESES (p < 0.05) ---\n")
for (n in names(results)) {
    p <- results[[n]]
    if (!is.na(p) && p < 0.05) {
        cat(sprintf("[SIGNIFICANT] %s: p = %.4f\n", n, p))
    }
}

cat("\n--- ALL OTHER JOKER HYPOTHESES (p >= 0.05) ---\n")
for (n in names(results)) {
    p <- results[[n]]
    if (is.na(p) || p >= 0.05) {
        cat(sprintf("[Not Sig] %s: p = %.4f\n", n, p))
    }
}
