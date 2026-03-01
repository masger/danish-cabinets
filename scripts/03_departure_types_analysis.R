# =============================================================================
# ANALYSIS OF MINISTERIAL DEPARTURE TYPES
# Distinguishing: Reshuffle vs Election exit vs "Fired" mid-term
# =============================================================================

library(data.table)
library(lubridate)
library(survival)
library(ggplot2)

cat("\n")
cat("================================================================\n")
cat("  MINISTERIAL DEPARTURE TYPES ANALYSIS\n")
cat("================================================================\n\n")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]
Encoding(data$ministerpost) <- "latin1"

# Calculate government dates
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]

# Person-level dates within each government
data[, p_start := min(start, na.rm = TRUE), by = .(regering, navn)]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]

# Time in position
data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]
data[is.na(time) | time <= 0, time := 0.01]

# Ministry type extraction
extract_ministry <- function(x) {
  x <- tryCatch(tolower(x), error = function(e) x)
  if (is.na(x) || x == "") return("Other")
  if (grepl("statsminister|premierminister|konsejlspr", x)) return("Statsminister")
  if (grepl("skatteminister", x)) return("Skatteminister")
  if (grepl("finansminister", x)) return("Finansminister")
  if (grepl("udenrigsminister", x)) return("Udenrigsminister")
  if (grepl("justitsminister", x)) return("Justitsminister")
  if (grepl("forsvarsminister|krigsminister", x)) return("Forsvarsminister")
  if (grepl("sundhedsminister", x)) return("Sundhedsminister")
  if (grepl("undervisningsminister|uddannelses|kultusminister", x)) return("Undervisningsminister")
  if (grepl("indenrigsminister", x)) return("Indenrigsminister")
  if (grepl("socialminister", x)) return("Socialminister")
  if (grepl("erhvervsminister|handelsminister", x)) return("Erhvervsminister")
  if (grepl("arbejdsminister|besk.ftigelsesminister", x)) return("Arbejdsminister")
  if (grepl("milj", x)) return("Miljoeminister")
  if (grepl("kulturminister", x)) return("Kulturminister")
  if (grepl("transport|trafik", x)) return("Transportminister")
  if (grepl("landbrug|f.devare", x)) return("Foedevareminister")
  if (grepl("kirke", x)) return("Kirkeminister")
  return("Other")
}

data[, ministry_type := sapply(ministerpost, extract_ministry)]

# Filter to post-1945
dat <- data[decimal_date(start) > 1945 & !is.na(time) & time > 0]

cat("Data loaded:", nrow(dat), "ministerial appointments\n")
cat("Unique ministers:", uniqueN(dat$navn), "\n")
cat("Unique governments:", uniqueN(dat$regering), "\n\n")

# =============================================================================
# 2. CLASSIFY DEPARTURE TYPES
# =============================================================================

cat("================================================================\n")
cat("  CLASSIFYING DEPARTURE TYPES\n")
cat("================================================================\n\n")

# For each minister-position, determine what happened when they left:
# 1. Did the government fall at the same time? (election/government change)
# 2. Did the minister get another position in the same government? (reshuffle)
# 3. Did the minister leave the cabinet before the government fell? (fired/resigned)

# First, create a person-government level dataset
person_govt <- dat[, .(
  first_start = min(start, na.rm = TRUE),
  last_stop = max(stop, na.rm = TRUE),
  n_positions = .N,
  positions = paste(unique(ministry_type), collapse = " -> "),
  total_time = sum(time, na.rm = TRUE)
), by = .(navn, regering, r_start, r_stop)]

# Determine departure type
person_govt[, govt_duration := as.numeric(difftime(r_stop, r_start, units = "days"))]
person_govt[, person_duration := as.numeric(difftime(last_stop, first_start, units = "days"))]

# Calculate how close the departure was to government end
person_govt[, days_before_govt_end := as.numeric(difftime(r_stop, last_stop, units = "days"))]

# Classification:
# - "Government fell" = left within 7 days of government end
# - "Fired/Resigned" = left more than 30 days before government end
# - "Near election" = left 8-30 days before government end (ambiguous)

person_govt[, departure_type := fcase(
  days_before_govt_end <= 7, "Government fell (election)",
  days_before_govt_end > 30, "Left mid-term (fired/resigned)",
  default = "Near government end (ambiguous)"
)]

# Summary of departure types
cat("DEPARTURE TYPES (person-government level):\n\n")
departure_summary <- person_govt[, .N, by = departure_type][order(-N)]
departure_summary[, pct := round(100 * N / sum(N), 1)]
print(departure_summary)

# =============================================================================
# 3. TRACK INDIVIDUAL CAREERS ACROSS GOVERNMENTS
# =============================================================================

cat("\n\n================================================================\n")
cat("  TRACKING CAREERS ACROSS GOVERNMENTS\n")
cat("================================================================\n\n")

# For each person, track their complete ministerial career
setorder(person_govt, navn, first_start)

# Did they return in a later government?
person_govt[, next_govt := shift(regering, type = "lead"), by = navn]
person_govt[, next_start := shift(first_start, type = "lead"), by = navn]

# Gap between leaving and returning (if they returned)
person_govt[, gap_to_next := as.numeric(difftime(next_start, last_stop, units = "days")) / 365.25]

# Career outcome after leaving this government
person_govt[, career_after := fcase(
  is.na(next_govt), "Left politics (final exit)",
  gap_to_next < 0.5, "Returned quickly (< 6 months)",
  gap_to_next < 2, "Returned within 2 years",
  gap_to_next < 5, "Returned within 5 years",
  default = "Returned after 5+ years"
)]

cat("CAREER OUTCOMES AFTER LEAVING GOVERNMENT:\n\n")
career_summary <- person_govt[, .N, by = career_after][order(-N)]
career_summary[, pct := round(100 * N / sum(N), 1)]
print(career_summary)

# =============================================================================
# 4. COMBINE: DEPARTURE TYPE + CAREER OUTCOME
# =============================================================================

cat("\n\n================================================================\n")
cat("  COMBINED ANALYSIS: How did they leave AND what happened after?\n")
cat("================================================================\n\n")

combined <- person_govt[, .N, by = .(departure_type, career_after)]
combined_wide <- dcast(combined, departure_type ~ career_after, value.var = "N", fill = 0)
print(combined_wide)

# =============================================================================
# 5. ANALYSIS BY PORTFOLIO: WHO GETS "FIRED"?
# =============================================================================

cat("\n\n================================================================\n")
cat("  WHICH PORTFOLIOS HAVE HIGHEST 'FIRED' RATE?\n")
cat("================================================================\n\n")

# Merge departure type back to position level
dat_with_departure <- merge(
  dat,
  person_govt[, .(navn, regering, departure_type, career_after)],
  by = c("navn", "regering"),
  all.x = TRUE
)

# Calculate "fired" rate by portfolio
portfolio_fired <- dat_with_departure[, .(
  N = .N,
  N_fired = sum(departure_type == "Left mid-term (fired/resigned)", na.rm = TRUE),
  N_election = sum(departure_type == "Government fell (election)", na.rm = TRUE),
  Fired_rate = round(mean(departure_type == "Left mid-term (fired/resigned)", na.rm = TRUE), 3)
), by = ministry_type][N >= 10][order(-Fired_rate)]

cat("PORTFOLIOS BY 'FIRED' RATE (left mid-term):\n\n")
print(portfolio_fired)

cat("\n*** KEY FINDING ***\n")
cat("Most likely to be 'fired':", portfolio_fired[1, ministry_type],
    "- Rate:", portfolio_fired[1, Fired_rate], "\n")
cat("Least likely to be 'fired':", portfolio_fired[.N, ministry_type],
    "- Rate:", portfolio_fired[.N, Fired_rate], "\n")

# =============================================================================
# 6. SURVIVAL ANALYSIS: TIME TO "FIRED" (excluding election exits)
# =============================================================================

cat("\n\n================================================================\n")
cat("  SURVIVAL ANALYSIS: Time to 'fired' (true early exit)\n")
cat("================================================================\n\n")

# Create event indicator: 1 = fired mid-term, 0 = censored (election or still serving)
dat_with_departure[, fired := as.numeric(departure_type == "Left mid-term (fired/resigned)")]

# Only include first position per person-government (avoid double counting)
dat_first <- dat_with_departure[, .SD[1], by = .(navn, regering)]

cat("Analysis dataset:", nrow(dat_first), "person-government observations\n")
cat("'Fired' events:", sum(dat_first$fired, na.rm = TRUE), "\n\n")

# Add fired indicator to person_govt
person_govt[, fired := as.numeric(departure_type == "Left mid-term (fired/resigned)")]

# Kaplan-Meier for fired
km_fired <- survfit(Surv(total_time, fired) ~ 1, data = person_govt)
cat("Median time to 'fired' (if it happens):", round(median(person_govt[departure_type == "Left mid-term (fired/resigned)", total_time]), 2), "years\n")
cat("Median time for election exits:", round(median(person_govt[departure_type == "Government fell (election)", total_time]), 2), "years\n")

# Add ministry type to person_govt (use first position's ministry)
first_ministry <- dat[, .(ministry_type = ministry_type[which.min(start)]), by = .(navn, regering)]
person_govt <- merge(person_govt, first_ministry, by = c("navn", "regering"), all.x = TRUE)

# Cox model by portfolio (for fired events only)
top_ports <- portfolio_fired[N >= 20, ministry_type]
dat_cox <- person_govt[ministry_type %in% top_ports]
dat_cox[, ministry_factor := relevel(factor(ministry_type), ref = "Finansminister")]

if (sum(dat_cox$fired, na.rm = TRUE) >= 10) {
  cox_fired <- coxph(Surv(total_time, fired) ~ ministry_factor, data = dat_cox)

  cat("\nCOX MODEL: Hazard of being 'fired' by portfolio (ref = Finance):\n\n")

  cox_results <- data.table(
    Portfolio = names(coef(cox_fired)),
    HR = round(exp(coef(cox_fired)), 2),
    p = round(summary(cox_fired)$coefficients[,5], 3)
  )
  cox_results[, Portfolio := gsub("ministry_factor", "", Portfolio)]
  cox_results <- cox_results[order(-HR)]
  print(cox_results)

  # Significant findings
  sig_fired <- cox_results[p < 0.05]
  if (nrow(sig_fired) > 0) {
    cat("\n*** SIGNIFICANTLY ELEVATED FIRING HAZARD ***\n")
    print(sig_fired[HR > 1])
  }
}

# =============================================================================
# 7. RESHUFFLE ANALYSIS: WHO GETS MOVED AROUND?
# =============================================================================

cat("\n\n================================================================\n")
cat("  RESHUFFLE ANALYSIS: Multiple positions in same government\n")
cat("================================================================\n\n")

reshuffled <- person_govt[n_positions > 1]
cat("Ministers with multiple positions in same government:", nrow(reshuffled), "\n\n")

if (nrow(reshuffled) > 0) {
  cat("Examples of reshuffled ministers:\n")
  print(head(reshuffled[, .(navn, regering, n_positions, positions)], 20))

  # Which starting portfolios lead to reshuffles?
  first_positions <- dat_with_departure[, .SD[which.min(start)], by = .(navn, regering)]
  first_positions[, was_reshuffled := person_govt[.SD, n_positions > 1, on = .(navn, regering)]]

  cat("\n\nSTARTING PORTFOLIOS THAT LEAD TO RESHUFFLES:\n")
  reshuffle_by_start <- first_positions[, .(
    N = .N,
    N_reshuffled = sum(was_reshuffled, na.rm = TRUE),
    Reshuffle_rate = round(mean(was_reshuffled, na.rm = TRUE), 3)
  ), by = ministry_type][N >= 10][order(-Reshuffle_rate)]
  print(reshuffle_by_start)
}

# =============================================================================
# 8. FINAL SUMMARY
# =============================================================================

cat("\n\n================================================================\n")
cat("  FINAL SUMMARY: The Complete Picture\n")
cat("================================================================\n\n")

cat("WHAT HAPPENS TO DANISH MINISTERS?\n\n")

total <- nrow(person_govt)
cat("Total minister-government observations:", total, "\n\n")

cat("1. DEPARTURE TYPE:\n")
for (i in 1:nrow(departure_summary)) {
  cat("   -", departure_summary[i, departure_type], ":",
      departure_summary[i, N], "(", departure_summary[i, pct], "%)\n")
}

cat("\n2. CAREER AFTER:\n")
for (i in 1:nrow(career_summary)) {
  cat("   -", career_summary[i, career_after], ":",
      career_summary[i, N], "(", career_summary[i, pct], "%)\n")
}

cat("\n3. KEY INSIGHT:\n")
fired_pct <- round(100 * sum(person_govt$departure_type == "Left mid-term (fired/resigned)") / total, 1)
cat("   Only", fired_pct, "% of ministers are truly 'fired' mid-term.\n")
cat("   Most exits (", round(100 - fired_pct, 1), "%) are due to elections/government changes.\n")

# =============================================================================
# 9. SAVE RESULTS
# =============================================================================

saveRDS(list(
  person_govt = person_govt,
  portfolio_fired = portfolio_fired,
  departure_summary = departure_summary,
  career_summary = career_summary
), "output/departure_types_analysis.rds")

cat("\n\nResults saved to output/departure_types_analysis.rds\n")
cat("================================================================\n")
