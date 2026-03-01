# =============================================================================
# INTERNATIONAL EXPLORATION: Ministerial Survival Across Countries
# =============================================================================

library(data.table)
library(lubridate)
library(survival)
library(ggplot2)

cat("\n")
cat("================================================================\n")
cat("  INTERNATIONAL EXPLORATION: Ministerial Survival Patterns\n")
cat("================================================================\n\n")

# =============================================================================
# 1. LOAD WHOGOV DATA
# =============================================================================

whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")

cat("WhoGov dataset dimensions:", nrow(whogov), "rows x", ncol(whogov), "columns\n")
cat("Countries:", uniqueN(whogov$country_name), "\n")
cat("Years covered:", min(whogov$year), "-", max(whogov$year), "\n\n")

# Key variables in WhoGov:
# - retention_rate_minister: proportion of ministers retained from previous year
# - average_minister: average tenure of ministers
# - n_minister: number of ministers

# =============================================================================
# 2. CALCULATE COUNTRY-LEVEL "SURVIVAL" METRICS
# =============================================================================

cat("================================================================\n")
cat("  COUNTRY-LEVEL MINISTERIAL TURNOVER\n")
cat("================================================================\n\n")

# Calculate average retention rate by country (retention = survival proxy)
# Lower retention = higher turnover = more "dangerous"

country_stats <- whogov[year >= 1960, .(
  Years = .N,
  Avg_Retention = mean(retention_rate_minister, na.rm = TRUE),
  SD_Retention = sd(retention_rate_minister, na.rm = TRUE),
  Avg_Tenure = mean(average_minister, na.rm = TRUE),
  Avg_Cabinet_Size = mean(n_minister, na.rm = TRUE)
), by = .(country_name, country_isocode)][Years >= 20]  # At least 20 years of data

country_stats <- country_stats[order(Avg_Retention)]

cat("COUNTRIES WITH LOWEST RETENTION (Most Dangerous - more turnover):\n")
print(head(country_stats[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                              Avg_Tenure = round(Avg_Tenure, 2))], 15))

cat("\n\nCOUNTRIES WITH HIGHEST RETENTION (Safest - less turnover):\n")
print(tail(country_stats[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                              Avg_Tenure = round(Avg_Tenure, 2))], 15))

# =============================================================================
# 3. FIND OUTLIERS
# =============================================================================

cat("\n\n================================================================\n")
cat("  OUTLIERS: Countries with Unusual Patterns\n")
cat("================================================================\n\n")

# Calculate z-scores for retention
country_stats[, z_retention := (Avg_Retention - mean(Avg_Retention, na.rm = TRUE)) /
                sd(Avg_Retention, na.rm = TRUE)]

outliers_low <- country_stats[z_retention < -2]
outliers_high <- country_stats[z_retention > 2]

cat("UNUSUALLY LOW RETENTION (z < -2):\n")
if (nrow(outliers_low) > 0) {
  print(outliers_low[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                          z_retention = round(z_retention, 2))])
} else {
  cat("None found\n")
}

cat("\nUNUSUALLY HIGH RETENTION (z > 2):\n")
if (nrow(outliers_high) > 0) {
  print(outliers_high[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                           z_retention = round(z_retention, 2))])
} else {
  cat("None found\n")
}

# =============================================================================
# 4. TEMPORAL TRENDS: 1960 to 2020
# =============================================================================

cat("\n\n================================================================\n")
cat("  TEMPORAL TRENDS: Has ministerial survival changed over time?\n")
cat("================================================================\n\n")

# Global trend
global_trend <- whogov[year >= 1960, .(
  N_Countries = uniqueN(country_name),
  Avg_Retention = mean(retention_rate_minister, na.rm = TRUE),
  Avg_Tenure = mean(average_minister, na.rm = TRUE)
), by = year][order(year)]

cat("GLOBAL AVERAGE RETENTION BY DECADE:\n")
decade_trend <- whogov[year >= 1960, .(
  N = .N,
  Avg_Retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  Avg_Tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = .(Decade = floor(year/10)*10)][order(Decade)]
print(decade_trend)

# Test for linear trend
if (nrow(global_trend[!is.na(Avg_Retention)]) > 10) {
  trend_model <- lm(Avg_Retention ~ year, data = global_trend)
  cat("\n\nLINEAR TREND TEST (retention ~ year):\n")
  cat("Slope:", round(coef(trend_model)[2], 5), "per year\n")
  cat("P-value:", round(summary(trend_model)$coefficients[2,4], 4), "\n")

  if (coef(trend_model)[2] > 0) {
    cat("Interpretation: Retention INCREASING over time (ministers surviving longer)\n")
  } else {
    cat("Interpretation: Retention DECREASING over time (more turnover)\n")
  }
}

# =============================================================================
# 5. COMPARE DEMOCRACIES VS DICTATORSHIPS
# =============================================================================

cat("\n\n================================================================\n")
cat("  DEMOCRACIES VS DICTATORSHIPS\n")
cat("================================================================\n\n")

system_stats <- whogov[year >= 1960, .(
  N = .N,
  Countries = uniqueN(country_name),
  Avg_Retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  Avg_Tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = system_category][order(-Avg_Retention)]

print(system_stats)

# =============================================================================
# 6. REGIONAL COMPARISON
# =============================================================================

cat("\n\n================================================================\n")
cat("  REGIONAL PATTERNS\n")
cat("================================================================\n\n")

# Group by region (first letter of ISO code is rough proxy, but let's use continent)
# Better: group Nordic, Western Europe, etc.

# Nordic countries
nordic <- c("DNK", "SWE", "NOR", "FIN", "ISL")
# Western Europe
west_eu <- c("DEU", "FRA", "GBR", "NLD", "BEL", "AUT", "CHE", "IRL", "LUX")
# Southern Europe
south_eu <- c("ITA", "ESP", "PRT", "GRC")
# Eastern Europe
east_eu <- c("POL", "CZE", "HUN", "SVK", "ROM", "BGR")

whogov[, region := fcase(
  country_isocode %in% nordic, "Nordic",
  country_isocode %in% west_eu, "Western Europe",
  country_isocode %in% south_eu, "Southern Europe",
  country_isocode %in% east_eu, "Eastern Europe",
  country_isocode == "USA", "USA",
  country_isocode == "CAN", "Canada",
  country_isocode == "AUS", "Australia",
  grepl("^(ARG|BRA|CHL|COL|MEX|PER|VEN)", country_isocode), "Latin America",
  default = "Other"
)]

region_stats <- whogov[year >= 1960 & region != "Other", .(
  Countries = uniqueN(country_name),
  Obs = .N,
  Avg_Retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  Avg_Tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = region][order(-Avg_Retention)]

print(region_stats)

# =============================================================================
# 7. DENMARK IN CONTEXT
# =============================================================================

cat("\n\n================================================================\n")
cat("  DENMARK IN INTERNATIONAL CONTEXT\n")
cat("================================================================\n\n")

denmark <- country_stats[country_isocode == "DNK"]
cat("Denmark:\n")
cat("- Average retention rate:", round(denmark$Avg_Retention, 3), "\n")
cat("- Average tenure:", round(denmark$Avg_Tenure, 2), "years\n")
cat("- Rank among", nrow(country_stats), "countries:",
    which(country_stats$country_isocode == "DNK"), "\n")
cat("- Percentile:", round(100 * which(country_stats$country_isocode == "DNK") / nrow(country_stats)), "%\n")

# Compare to Nordic neighbors
cat("\nNordic comparison:\n")
nordic_comp <- country_stats[country_isocode %in% nordic]
print(nordic_comp[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                       Avg_Tenure = round(Avg_Tenure, 2))])

# =============================================================================
# 8. INTERESTING CASE STUDIES
# =============================================================================

cat("\n\n================================================================\n")
cat("  INTERESTING CASE STUDIES\n")
cat("================================================================\n\n")

# Countries with high variance (unstable)
country_stats[, cv_retention := SD_Retention / Avg_Retention]
high_variance <- country_stats[order(-cv_retention)][1:10]

cat("MOST VOLATILE (highest coefficient of variation in retention):\n")
print(high_variance[, .(country_name, Avg_Retention = round(Avg_Retention, 3),
                         CV = round(cv_retention, 2))])

# =============================================================================
# 9. TEMPORAL TRENDS BY REGION
# =============================================================================

cat("\n\n================================================================\n")
cat("  TEMPORAL TRENDS BY REGION (1960s vs 2010s)\n")
cat("================================================================\n\n")

temporal_region <- whogov[year >= 1960 & region != "Other", .(
  Avg_Retention = mean(retention_rate_minister, na.rm = TRUE)
), by = .(region, Period = fifelse(year < 1990, "1960-1989", "1990-2020"))]

temporal_wide <- dcast(temporal_region, region ~ Period, value.var = "Avg_Retention")
setnames(temporal_wide, c("1960-1989", "1990-2020"), c("Early", "Late"))
temporal_wide[, Change := round(Late - Early, 3)]
temporal_wide[, Early := round(Early, 3)]
temporal_wide[, Late := round(Late, 3)]

print(temporal_wide[order(-Change)])

cat("\nPositive change = retention improved (longer survival)\n")
cat("Negative change = retention worsened (more turnover)\n")

# =============================================================================
# 10. SAVE RESULTS
# =============================================================================

saveRDS(list(
  country_stats = country_stats,
  decade_trend = decade_trend,
  region_stats = region_stats,
  system_stats = system_stats
), "output/international_exploration.rds")

# =============================================================================
# 11. CREATE VISUALIZATION
# =============================================================================

# Plot global trend
p1 <- ggplot(global_trend[year >= 1960], aes(x = year, y = Avg_Retention)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink") +
  labs(
    title = "Global Ministerial Retention Rate Over Time",
    subtitle = "Average across all countries with data",
    x = "Year",
    y = "Average Retention Rate"
  ) +
  theme_minimal()

ggsave("figures/global_retention_trend.png", p1, width = 10, height = 6, dpi = 300)

cat("\n\n================================================================\n")
cat("  EXPLORATION COMPLETE\n")
cat("================================================================\n")
cat("\nResults saved to output/international_exploration.rds\n")
cat("Figure saved to figures/global_retention_trend.png\n")
