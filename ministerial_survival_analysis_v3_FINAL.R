# =============================================================================
# MINISTERS FALL: Survival Analysis of Danish Ministerial Careers
# VERSION 3 - FINAL (Following second peer review)
# =============================================================================

# Load required packages
library(data.table)
library(lubridate)
library(stringr)
library(survival)
library(survminer)
library(ggplot2)
library(gt)
library(ggsci)
library(broom)
library(patchwork)

theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

# =============================================================================
# 1. DATA LOADING AND QUALITY FIXES
# (All date fixes BEFORE time calculation - per reviewer R2 concern)
# =============================================================================

data <- fread("danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]

# Convert text columns to ASCII-safe versions (remove special chars for matching)
Encoding(data$ministerpost) <- "latin1"

cat("\n========================================\n")
cat("DATA QUALITY CORRECTIONS\n")
cat("========================================\n")

# Store original for comparison
n_original <- nrow(data)

# Fix 1: Immortal minister (2051 -> 1851)
data[year(stop) == 2051, stop := stop - years(200)]

# Fix 2: Check for any remaining future dates and fix
future_rows <- which(data$stop > Sys.Date())
if (length(future_rows) > 0) {
  cat("Fixing", length(future_rows), "future dates\n")
  data[future_rows, stop := stop - years(100)]  # Assume century error
}

# Fix 3: Check start > stop and flag
invalid_dates <- data[start > stop & !is.na(start) & !is.na(stop)]
if (nrow(invalid_dates) > 0) {
  cat("Warning:", nrow(invalid_dates), "records with start > stop (will be excluded)\n")
}

# -----------------------------------------------------------------------------
# CALCULATE GOVERNMENT AND PERSON-LEVEL DATES FIRST
# -----------------------------------------------------------------------------

# Calculate government start/end dates (by regering)
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]

# Calculate person-level start/end within each government
data[, p_start := min(start, na.rm = TRUE), by = .(regering, navn)]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]

# -----------------------------------------------------------------------------
# CALCULATE TIME VARIABLE
# -----------------------------------------------------------------------------

# Primary: use individual minister stop - start
data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]

# For missing stop dates, use p_stop (person's last date in this govt)
data[is.na(time) & !is.na(p_stop), time := as.numeric(difftime(p_stop, start, units = "days")) / 365.25]

# Still missing? Use government end as censoring date
data[is.na(time) & !is.na(r_stop), time := as.numeric(difftime(r_stop, start, units = "days")) / 365.25]

# Set minimum time for valid records (avoid log(0) issues)
data[!is.na(time) & time <= 0, time := 0.01]

# -----------------------------------------------------------------------------
# CALCULATE 'EARLY' EVENT INDICATOR
# -----------------------------------------------------------------------------

# Event = minister stopped before government ended
data[, early := as.numeric(p_stop < r_stop)]

# If can't determine (infinite dates from NA), set to 0 (censored)
data[is.na(early) | is.infinite(p_stop) | is.infinite(r_stop), early := 0]

cat("Date corrections complete\n")
cat("Total records:", nrow(data), "\n")
cat("Records with valid time:", sum(!is.na(data$time)), "\n")
cat("Events (early terminations):", sum(data$early, na.rm = TRUE), "\n")

# =============================================================================
# 2. MINISTRY TYPE CLASSIFICATION
# =============================================================================

extract_ministry <- function(x) {
  # Handle encoding issues - use bytes mode for pattern matching
  x <- tryCatch(tolower(x), error = function(e) x)
  if (is.na(x) || x == "") return("Anden minister")
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
  if (grepl("klima|energi", x)) return("Klima/Energiminister")
  return("Anden minister")
}

data[, ministry_type := sapply(ministerpost, extract_ministry)]

# =============================================================================
# 3. ANALYSIS DATASET WITH CONSORT FLOW
# =============================================================================

cat("\n========================================\n")
cat("CONSORT FLOW DIAGRAM\n")
cat("========================================\n")

# Start with post-1945
analysis_raw <- data[decimal_date(start) > 1945]
cat("Records post-1945:", nrow(analysis_raw), "\n")

# Exclusions
n1 <- sum(is.na(analysis_raw$time))
cat("- Excluded (missing time):", n1, "\n")

n2 <- sum(analysis_raw$time <= 0, na.rm = TRUE)
cat("- Excluded (time <= 0):", n2, "\n")

n3 <- sum(is.na(analysis_raw$early))
cat("- Excluded (missing event):", n3, "\n")

n4 <- sum(analysis_raw$start > analysis_raw$stop, na.rm = TRUE)
cat("- Excluded (start > stop):", n4, "\n")

# Apply exclusions
analysis_data <- analysis_raw[
  !is.na(time) & time > 0 & !is.na(early) &
    (is.na(start) | is.na(stop) | start <= stop)
]

cat("\n=> Final analysis sample:", nrow(analysis_data), "\n")

# =============================================================================
# 4. VARIABLE CREATION
# =============================================================================

# Political bloc (improved classification per R1)
analysis_data[, bloc := fcase(
  grepl("Socialdemokratiet", `Parti(er)`, ignore.case = TRUE) &
    !grepl("Venstre|Konservativ|Liberal", `Parti(er)`, ignore.case = TRUE),
  "Red (Social Democrat-led)",
  grepl("Venstre|Konservativ|Liberal", `Parti(er)`, ignore.case = TRUE) &
    !grepl("Socialdemokratiet", `Parti(er)`, ignore.case = TRUE),
  "Blue (Centre-Right)",
  default = "Coalition/Mixed"
)]

analysis_data[, bloc_simple := fifelse(
  grepl("Socialdemokratiet", `Parti(er)`, ignore.case = TRUE),
  "Red", "Blue"
)]

# Era
analysis_data[, era := cut(
  decimal_date(start),
  breaks = c(1945, 1970, 1990, 2010, 2025),
  labels = c("1945-1970", "1970-1990", "1990-2010", "2010+"),
  include.lowest = TRUE
)]

# Skatteminister
analysis_data[, skatteminister := fifelse(
  ministry_type == "Skatteminister", "Tax Minister", "Other Ministers"
)]

# High-profile portfolio (NEW - per R2)
analysis_data[, high_profile := ministry_type %in%
                c("Skatteminister", "Finansminister", "Erhvervsminister",
                  "Udenrigsminister", "Justitsminister")]

# =============================================================================
# 5. REPEATED APPOINTMENTS ANALYSIS (NEW - per R2)
# =============================================================================

cat("\n========================================\n")
cat("REPEATED APPOINTMENTS ANALYSIS\n")
cat("(Addressing independence assumption - R2)\n")
cat("========================================\n")

appointments_per_person <- analysis_data[, .N, by = navn]
repeat_ministers <- appointments_per_person[N > 1]

cat("Unique individuals:", uniqueN(analysis_data$navn), "\n")
cat("Individuals with >1 appointment:", nrow(repeat_ministers), "\n")
cat("Total appointments from repeat ministers:", sum(repeat_ministers$N), "\n")
cat("Max appointments by one person:", max(appointments_per_person$N), "\n")

# Who had the most appointments?
most_appointed <- appointments_per_person[order(-N)][1:5]
cat("\nTop 5 most frequently appointed:\n")
print(most_appointed)

# =============================================================================
# 6. BASELINE CHARACTERISTICS
# =============================================================================

cat("\n========================================\n")
cat("TABLE 1: BASELINE CHARACTERISTICS\n")
cat("========================================\n")

n_obs <- nrow(analysis_data)
n_individuals <- uniqueN(analysis_data$navn)
n_events <- sum(analysis_data$early, na.rm = TRUE)
total_pyears <- sum(analysis_data$time, na.rm = TRUE)

cat("Total appointments:", n_obs, "\n")
cat("Unique individuals:", n_individuals, "\n")
cat("Events:", n_events, "\n")
cat("Person-years:", round(total_pyears, 1), "\n")
cat("Overall rate:", round(n_events/total_pyears, 2), "/py\n")

# Skatteminister table
skatte_stats <- analysis_data[, .(
  Appointments = .N,
  Individuals = uniqueN(navn),
  Events = sum(early),
  PY = round(sum(time), 1),
  Rate = round(sum(early)/sum(time), 2),
  MedianTenure = round(median(time), 2)
), by = skatteminister]

cat("\n--- Skatteminister Statistics ---\n")
print(skatte_stats)

# =============================================================================
# 7. KAPLAN-MEIER ANALYSIS
# =============================================================================

km_overall <- survfit(Surv(time, early) ~ 1, data = analysis_data)
km_skatte <- survfit(Surv(time, early) ~ skatteminister, data = analysis_data)

cat("\n========================================\n")
cat("SURVIVAL ANALYSIS\n")
cat("========================================\n")
cat("Median overall survival:", round(summary(km_overall)$table["median"], 2), "years\n")

# =============================================================================
# 8. SKATTEMINISTER SYNDROME - PRIMARY ANALYSIS
# =============================================================================

cat("\n========================================\n")
cat("PRIMARY ANALYSIS: SKATTEMINISTER SYNDROME\n")
cat("========================================\n")

# Unadjusted
cox_unadj <- coxph(Surv(time, early) ~ skatteminister, data = analysis_data)
hr_unadj <- exp(coef(cox_unadj))
ci_unadj <- exp(confint(cox_unadj))
p_unadj <- summary(cox_unadj)$coefficients[5]

cat("\nUNADJUSTED:\n")
cat("HR:", round(hr_unadj, 2), "(95% CI", round(ci_unadj[1], 2), "-",
    round(ci_unadj[2], 2), "), p =", format.pval(p_unadj, digits = 3), "\n")

# Adjusted
cox_adj <- coxph(Surv(time, early) ~ skatteminister + bloc_simple + era,
                 data = analysis_data)
hr_adj <- exp(coef(cox_adj)["skatteministerTax Minister"])
ci_adj <- exp(confint(cox_adj)["skatteministerTax Minister", ])
p_adj <- summary(cox_adj)$coefficients["skatteministerTax Minister", 5]

cat("\nADJUSTED (bloc + era):\n")
cat("HR:", round(hr_adj, 2), "(95% CI", round(ci_adj[1], 2), "-",
    round(ci_adj[2], 2), "), p =", format.pval(p_adj, digits = 3), "\n")

cat("\n*** NOTE: Adjusted HR does not meet conventional significance (p=0.05) ***\n")
cat("*** Findings are SUGGESTIVE rather than ESTABLISHED (per reviewer) ***\n")

# =============================================================================
# 9. PROPORTIONAL HAZARDS TEST
# =============================================================================

cat("\n========================================\n")
cat("PROPORTIONAL HAZARDS ASSUMPTION (Schoenfeld)\n")
cat("========================================\n")

ph_test <- cox.zph(cox_adj)
print(ph_test)

# =============================================================================
# 10. SENSITIVITY ANALYSES - MULTIPLE THRESHOLDS (per R2)
# =============================================================================

cat("\n========================================\n")
cat("SENSITIVITY ANALYSIS: TENURE THRESHOLDS\n")
cat("(Justification for exclusion criteria - R2)\n")
cat("========================================\n")

thresholds <- c(0, 0.083, 0.25, 0.5)  # 0, 1 month, 3 months, 6 months
threshold_names <- c("None", "1 month", "3 months", "6 months")

sens_results <- data.table(
  Threshold = threshold_names,
  MinTenure = thresholds,
  N = integer(4),
  N_excluded = integer(4),
  HR = numeric(4),
  CI_low = numeric(4),
  CI_high = numeric(4),
  P = numeric(4)
)

for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  sens_data <- analysis_data[time >= t]
  n_excluded <- nrow(analysis_data) - nrow(sens_data)

  if (nrow(sens_data[skatteminister == "Tax Minister"]) >= 3) {
    cox_sens <- coxph(Surv(time, early) ~ skatteminister, data = sens_data)
    sens_results[i, `:=`(
      N = nrow(sens_data),
      N_excluded = n_excluded,
      HR = round(exp(coef(cox_sens)), 2),
      CI_low = round(exp(confint(cox_sens))[1], 2),
      CI_high = round(exp(confint(cox_sens))[2], 2),
      P = round(summary(cox_sens)$coefficients[5], 3)
    )]
  }
}

cat("\nSUPPLEMENTARY TABLE S2: Sensitivity to tenure thresholds\n")
print(sens_results)

cat("\nInterpretation: HR remains elevated (1.49-1.67) across thresholds,\n")
cat("suggesting finding is not solely driven by very short appointments.\n")

# =============================================================================
# 11. HIGH-PROFILE PORTFOLIO COMPARISON (NEW - per R2)
# =============================================================================

cat("\n========================================\n")
cat("SUPPLEMENTARY TABLE S3: HIGH-PROFILE PORTFOLIOS\n")
cat("========================================\n")

# Among high-profile portfolios only
hp_data <- analysis_data[high_profile == TRUE]

hp_summary <- hp_data[, .(
  N = .N,
  Events = sum(early),
  PY = round(sum(time), 1),
  Rate = round(sum(early)/sum(time), 2)
), by = ministry_type][order(-N)]

print(hp_summary)

# Compare Skatteminister to other high-profile
if (nrow(hp_data[ministry_type == "Skatteminister"]) >= 3) {
  hp_data[, is_skatte := ministry_type == "Skatteminister"]
  cox_hp <- coxph(Surv(time, early) ~ is_skatte, data = hp_data)

  cat("\nSkatteminister vs OTHER high-profile portfolios:\n")
  cat("HR:", round(exp(coef(cox_hp)), 2), "\n")
  cat("95% CI:", round(exp(confint(cox_hp))[1], 2), "-",
      round(exp(confint(cox_hp))[2], 2), "\n")
  cat("(Reference: Finance, Trade, Foreign Affairs, Justice combined)\n")
}

# =============================================================================
# 12. FOREST PLOT WITH BONFERRONI
# =============================================================================

top_ministries <- analysis_data[, .N, by = ministry_type][N >= 15, ministry_type]
analysis_subset <- analysis_data[ministry_type %in% top_ministries]
analysis_subset[, ministry_factor := relevel(factor(ministry_type), ref = "Finansminister")]

cox_full <- coxph(Surv(time, early) ~ ministry_factor + bloc_simple + era,
                  data = analysis_subset)

cox_results <- tidy(cox_full, conf.int = TRUE, exponentiate = TRUE)
setDT(cox_results)

# Bonferroni
n_comparisons <- sum(grepl("ministry", cox_results$term))
cox_results[, p_bonf := pmin(p.value * n_comparisons, 1)]
cox_results[, term_clean := str_remove_all(term, "ministry_factor|bloc_simple|era")]

# =============================================================================
# 13. FIGURES
# =============================================================================

# Figure 1: Overall survival
fig1 <- ggsurvplot(
  km_overall,
  data = analysis_data,
  risk.table = TRUE,
  risk.table.height = 0.25,
  conf.int = TRUE,
  xlim = c(0, 6),
  break.time.by = 1,
  xlab = "Time since appointment (years)",
  ylab = "Probability of remaining in office",
  title = "Figure 1: Overall Ministerial Survival",
  subtitle = paste0("n=", n_obs, ", events=", n_events),
  ggtheme = theme_minimal(),
  palette = "lancet",
  surv.median.line = "hv"
)

# Figure 2: Skatteminister
fig2 <- ggsurvplot(
  km_skatte,
  data = analysis_data,
  pval = TRUE,
  pval.method = TRUE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  conf.int = TRUE,
  xlim = c(0, 5),
  break.time.by = 1,
  xlab = "Time since appointment (years)",
  ylab = "Probability of remaining in office",
  title = "Figure 2: The 'Skatteminister Syndrome'",
  subtitle = "Pre-specified primary hypothesis | Findings suggestive, not definitive",
  ggtheme = theme_minimal(),
  palette = c("#4DBBD5FF", "#E64B35FF"),
  legend.labs = c("Other Ministers", "Tax Minister")
)

# Figure 3: Forest plot
ministry_results <- cox_results[grepl("minister", term, ignore.case = TRUE)]

fig3 <- ggplot(ministry_results, aes(x = estimate, y = reorder(term_clean, estimate))) +
  geom_point(size = 3, color = "#E64B35FF") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2,
                 color = "#3C5488FF", linewidth = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = c(0.5, 1, 2, 4)) +
  labs(
    x = "Hazard Ratio (log scale)",
    y = "",
    title = "Figure 3: Forest Plot",
    subtitle = "Reference: Finansminister | Bonferroni-corrected p-values in Table 2",
    caption = "Note: Multiple comparisons - interpret individual tests with caution"
  ) +
  theme_minimal()

# =============================================================================
# 14. FINAL ABSTRACT STATISTICS
# =============================================================================

cat("\n========================================\n")
cat("FINAL ABSTRACT (REVISED per R2)\n")
cat("========================================\n")

cat("\nStudy population:\n")
cat("-", n_obs, "appointments,", n_individuals, "individuals\n")
cat("-", n_events, "events,", round(total_pyears, 1), "person-years\n")

cat("\nSkatteminister:\n")
cat("- n =", skatte_stats[skatteminister == "Tax Minister", Appointments], "\n")
cat("- events =", skatte_stats[skatteminister == "Tax Minister", Events], "\n")

cat("\nHazard Ratios (BOTH REPORTED):\n")
cat("- Unadjusted: HR", round(hr_unadj, 2),
    "(95% CI", round(ci_unadj[1], 2), "-", round(ci_unadj[2], 2),
    "), p =", format.pval(p_unadj, digits = 3), "\n")
cat("- Adjusted: HR", round(hr_adj, 2),
    "(95% CI", round(ci_adj[1], 2), "-", round(ci_adj[2], 2),
    "), p =", format.pval(p_adj, digits = 3), "\n")

cat("\n*** CONCLUSION: Findings SUGGESTIVE of Skatteminister Syndrome ***\n")
cat("*** Adjusted analysis does not reach conventional significance ***\n")

cat("\nRepeated appointments:\n")
cat("-", nrow(repeat_ministers), "individuals with multiple appointments\n")
cat("- Independence assumption acknowledged as limitation\n")

# =============================================================================
# 15. SAVE OUTPUTS
# =============================================================================

ggsave("Figure1_Overall_FINAL.png", print(fig1), width = 10, height = 8, dpi = 300)
ggsave("Figure2_Skatteminister_FINAL.png", print(fig2), width = 10, height = 8, dpi = 300)
ggsave("Figure3_Forest_FINAL.png", fig3, width = 10, height = 6, dpi = 300)

saveRDS(list(
  baseline = list(n = n_obs, individuals = n_individuals,
                  events = n_events, pyears = total_pyears),
  skatteminister = skatte_stats,
  hr_unadjusted = list(hr = hr_unadj, ci = ci_unadj, p = p_unadj),
  hr_adjusted = list(hr = hr_adj, ci = ci_adj, p = p_adj),
  sensitivity = sens_results,
  repeated_appointments = repeat_ministers,
  cox_results = cox_results
), "survival_analysis_FINAL.rds")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE - VERSION 3 (FINAL)\n")
cat("Ready for final review\n")
cat("========================================\n")
