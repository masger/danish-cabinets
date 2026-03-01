# =============================================================================
# EXPLORATORY DATA ANALYSIS: Danish Ministerial Survival
# Run this FIRST to discover the actual story in the data
# =============================================================================

library(data.table)
library(lubridate)
library(stringr)
library(survival)
library(survminer)
library(ggplot2)

cat("\n")
cat("================================================================\n")
cat("  EXPLORATORY ANALYSIS: What's the REAL story in the data?\n")
cat("================================================================\n\n")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]
Encoding(data$ministerpost) <- "latin1"

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

# Calculate dates and events
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]

data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]
data[is.na(time) & !is.na(p_stop), time := as.numeric(difftime(p_stop, start, units = "days")) / 365.25]
data[is.na(time) & !is.na(r_stop), time := as.numeric(difftime(r_stop, start, units = "days")) / 365.25]
data[!is.na(time) & time <= 0, time := 0.01]

data[, early := as.numeric(p_stop < r_stop)]
data[is.na(early) | is.infinite(p_stop) | is.infinite(r_stop), early := 0]

# Analysis data (post-1945)
dat <- data[decimal_date(start) > 1945 & !is.na(time) & time > 0]

# Political bloc
dat[, bloc := fifelse(grepl("Socialdemokratiet", `Parti(er)`, ignore.case = TRUE), "Red", "Blue")]

cat("Data loaded:", nrow(dat), "ministerial appointments post-1945\n")
cat("Events (early terminations):", sum(dat$early), "\n")
cat("Unique ministers:", uniqueN(dat$navn), "\n\n")

# =============================================================================
# 2. WHICH PORTFOLIOS ARE MOST DANGEROUS?
# =============================================================================

cat("================================================================\n")
cat("  QUESTION 1: Which portfolios have HIGHEST termination rates?\n")
cat("================================================================\n\n")

portfolio_stats <- dat[, .(
  N = .N,
  Events = sum(early),
  PersonYears = round(sum(time), 1),
  Rate = round(sum(early) / sum(time), 3),
  MedianTenure = round(median(time), 2)
), by = ministry_type][order(-Rate)]

# Only show portfolios with enough data
portfolio_stats_main <- portfolio_stats[N >= 10]

cat("TERMINATION RATES BY PORTFOLIO (n >= 10):\n")
cat("(Higher rate = more dangerous)\n\n")
print(portfolio_stats_main)

cat("\n\n*** KEY FINDING ***\n")
cat("MOST DANGEROUS portfolio:", portfolio_stats_main[1, ministry_type],
    "- Rate:", portfolio_stats_main[1, Rate], "/person-year\n")
cat("SAFEST portfolio:", portfolio_stats_main[.N, ministry_type],
    "- Rate:", portfolio_stats_main[.N, Rate], "/person-year\n")

# =============================================================================
# 3. SURVIVAL ANALYSIS BY PORTFOLIO
# =============================================================================

cat("\n\n================================================================\n")
cat("  QUESTION 2: Survival curves by portfolio\n")
cat("================================================================\n\n")

# Select top portfolios
top_ports <- portfolio_stats_main[N >= 20, ministry_type]
dat_top <- dat[ministry_type %in% top_ports]

km_portfolio <- survfit(Surv(time, early) ~ ministry_type, data = dat_top)

cat("Log-rank test across portfolios:\n")
print(survdiff(Surv(time, early) ~ ministry_type, data = dat_top))

# =============================================================================
# 4. COX REGRESSION - FIND THE REAL HAZARDS
# =============================================================================

cat("\n\n================================================================\n")
cat("  QUESTION 3: Which factors predict early termination?\n")
cat("================================================================\n\n")

dat_top[, ministry_factor := relevel(factor(ministry_type), ref = "Finansminister")]

cox_model <- coxph(Surv(time, early) ~ ministry_factor, data = dat_top)

cat("COX MODEL RESULTS (reference = Finansminister):\n\n")

# Extract and sort by HR
cox_results <- data.table(
  Portfolio = names(coef(cox_model)),
  HR = round(exp(coef(cox_model)), 2),
  CI_low = round(exp(confint(cox_model))[,1], 2),
  CI_high = round(exp(confint(cox_model))[,2], 2),
  p = round(summary(cox_model)$coefficients[,5], 3)
)
cox_results[, Portfolio := gsub("ministry_factor", "", Portfolio)]
cox_results <- cox_results[order(-HR)]

print(cox_results)

cat("\n*** INTERPRETATION ***\n")
cat("HR > 1 = MORE dangerous than Finance Minister\n")
cat("HR < 1 = SAFER than Finance Minister\n")

# Identify significant findings
sig_dangerous <- cox_results[HR > 1 & p < 0.05]
sig_safe <- cox_results[HR < 1 & p < 0.05]

if (nrow(sig_dangerous) > 0) {
  cat("\nSIGNIFICANTLY MORE DANGEROUS than Finance:\n")
  print(sig_dangerous)
}

if (nrow(sig_safe) > 0) {
  cat("\nSIGNIFICANTLY SAFER than Finance:\n")
  print(sig_safe)
}

# =============================================================================
# 5. RED VS BLUE - DOES POLITICS MATTER?
# =============================================================================

cat("\n\n================================================================\n")
cat("  QUESTION 4: Red vs Blue governments - any difference?\n")
cat("================================================================\n\n")

bloc_stats <- dat[, .(
  N = .N,
  Events = sum(early),
  Rate = round(sum(early) / sum(time), 3)
), by = bloc]

print(bloc_stats)

cox_bloc <- coxph(Surv(time, early) ~ bloc, data = dat)
cat("\nCox model: Blue vs Red\n")
cat("HR:", round(exp(coef(cox_bloc)), 2), "\n")
cat("p-value:", round(summary(cox_bloc)$coefficients[5], 3), "\n")

# =============================================================================
# 6. TEMPORAL TRENDS
# =============================================================================

cat("\n\n================================================================\n")
cat("  QUESTION 5: Has ministerial survival changed over time?\n")
cat("================================================================\n\n")

dat[, decade := floor(year(start) / 10) * 10]

decade_stats <- dat[, .(
  N = .N,
  Events = sum(early),
  Rate = round(sum(early) / sum(time), 3),
  MedianTenure = round(median(time), 2)
), by = decade][order(decade)]

print(decade_stats)

# =============================================================================
# 7. WHO ARE THE SURVIVORS AND CASUALTIES?
# =============================================================================

cat("\n\n================================================================\n")
cat("  QUESTION 6: Notable survivors and casualties\n")
cat("================================================================\n\n")

# Longest serving ministers
longest <- dat[, .(total_time = sum(time)), by = navn][order(-total_time)][1:10]
cat("LONGEST SERVING MINISTERS (total years in office):\n")
print(longest)

# Most appointments
most_appts <- dat[, .N, by = navn][order(-N)][1:10]
cat("\nMOST APPOINTMENTS:\n")
print(most_appts)

# =============================================================================
# 8. SUMMARY: WHAT'S THE REAL STORY?
# =============================================================================

cat("\n\n================================================================\n")
cat("  SUMMARY: THE REAL STORY IN THE DATA\n")
cat("================================================================\n\n")

cat("1. SKATTEMINISTER MYTH BUSTED:\n")
skatte_rate <- portfolio_stats[ministry_type == "Skatteminister", Rate]
avg_rate <- dat[, sum(early) / sum(time)]
cat("   Tax Minister rate:", skatte_rate, "/py vs overall:", round(avg_rate, 3), "/py\n")
if (skatte_rate < avg_rate) {
  cat("   -> Tax Ministers are actually SAFER than average!\n")
} else {
  cat("   -> Tax Ministers are more dangerous than average\n")
}

cat("\n2. THE REAL DANGER ZONES:\n")
cat("   Most dangerous portfolios:\n")
print(portfolio_stats_main[1:3, .(ministry_type, Rate, N)])

cat("\n3. POTENTIAL STORIES:\n")
cat("   - 'The curse of X minister' (find the actually dangerous one)\n")
cat("   - 'Survival of the fittest: which portfolios are safe havens?'\n")
cat("   - 'Red vs Blue: does political color predict ministerial mortality?'\n")
cat("   - 'The changing hazards of public office: 1945-2022'\n")

# =============================================================================
# 9. SAVE EXPLORATORY RESULTS
# =============================================================================

saveRDS(list(
  portfolio_stats = portfolio_stats,
  cox_results = cox_results,
  bloc_stats = bloc_stats,
  decade_stats = decade_stats
), "output/exploratory_results.rds")

cat("\n\nExploratory results saved to output/exploratory_results.rds\n")
cat("================================================================\n")
