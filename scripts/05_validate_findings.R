# =============================================================================
# VALIDATION: Check Transport Ministry naming and International comparison
# =============================================================================

library(data.table)
library(lubridate)

cat("\n================================================================\n")
cat("  PART 1: VALIDATING DANISH TRANSPORT MINISTRY FINDING\n")
cat("================================================================\n\n")

# =============================================================================
# 1. Check if Transport ministry was renamed/restructured
# =============================================================================

data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")
Encoding(data$ministerpost) <- "latin1"

# Find ALL transport-related positions
transport <- data[grepl("transport|trafik", ministerpost, ignore.case = TRUE)]

cat("TRANSPORT-RELATEREDE MINISTERPOSTER:\n\n")
cat("Antal poster:", nrow(transport), "\n\n")

cat("Unikke titler:\n")
print(sort(unique(transport$ministerpost)))

cat("\n\nTidsfordeling af transport-poster:\n")
transport[, decade := floor(year(start)/10)*10]
print(transport[, .N, by = decade][order(decade)])

# Check: Were transport ministers "fired" because the ministry was abolished?
# If so, we'd see clusters of exits at specific dates

cat("\n\nAfgangsdatoer for transportministre:\n")
transport_exits <- transport[, .(navn, start, stop, ministerpost)]
print(transport_exits[order(stop)])

# Check if multiple transport ministers left on the same date (restructuring signal)
cat("\n\nDatoer hvor flere transportministre stoppede samtidigt:\n")
exit_counts <- transport[, .N, by = stop][N > 1][order(-N)]
print(exit_counts)

# =============================================================================
# 2. Compare to other ministries - did they also have restructuring?
# =============================================================================

cat("\n\n================================================================\n")
cat("  SAMMENLIGNING: Afgang på samme dato (tegn på omstrukturering)\n")
cat("================================================================\n\n")

# For each ministry type, count how many times multiple ministers left same day
extract_ministry <- function(x) {
  x <- tryCatch(tolower(x), error = function(e) x)
  if (is.na(x) || x == "") return("Other")
  if (grepl("statsminister", x)) return("Statsminister")
  if (grepl("skatteminister", x)) return("Skatteminister")
  if (grepl("finansminister", x)) return("Finansminister")
  if (grepl("transport|trafik", x)) return("Transportminister")
  if (grepl("indenrigsminister", x)) return("Indenrigsminister")
  if (grepl("arbejdsminister", x)) return("Arbejdsminister")
  return("Other")
}

data[, ministry_type := sapply(ministerpost, extract_ministry)]

# Count simultaneous exits by ministry
simultaneous <- data[ministry_type != "Other", .(
  N_exits = .N,
  Ministers = paste(unique(navn), collapse = ", ")
), by = .(ministry_type, stop)][N_exits > 1]

cat("Ministerier med flest samtidige afgange (omstrukturering?):\n")
print(simultaneous[, .(Simultaneous_exits = .N), by = ministry_type][order(-Simultaneous_exits)])

# =============================================================================
# PART 2: INTERNATIONAL ANALYSIS - ALL COUNTRIES
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 2: INTERNATIONAL ANALYSIS - ALL COUNTRIES\n")
cat("================================================================\n\n")

# Load WhoGov individual-level data if available, or use cross-sectional
whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")

cat("WhoGov data loaded:\n")
cat("- Countries:", uniqueN(whogov$country_name), "\n")
cat("- Years:", min(whogov$year), "-", max(whogov$year), "\n")
cat("- Observations:", nrow(whogov), "\n\n")

# The cross-sectional data has retention rates, not individual minister data
# retention_rate_minister = share of ministers retained from previous year

# =============================================================================
# 2.1 Global patterns over time
# =============================================================================

cat("GLOBAL RETENTION TRENDS (all countries, all years):\n\n")

global_trend <- whogov[, .(
  N_countries = uniqueN(country_name),
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE),
  SD_retention = sd(retention_rate_minister, na.rm = TRUE),
  Mean_tenure = mean(average_minister, na.rm = TRUE)
), by = year][order(year)]

cat("By year (sample):\n")
print(global_trend[year %in% c(1966, 1980, 1990, 2000, 2010, 2020)])

# Test for trend
trend_model <- lm(Mean_retention ~ year, data = global_trend)
cat("\n\nLinear trend test:\n")
cat("Slope:", round(coef(trend_model)[2], 5), "per year\n")
cat("P-value:", round(summary(trend_model)$coefficients[2, 4], 4), "\n")

# =============================================================================
# 2.2 By political system
# =============================================================================

cat("\n\n================================================================\n")
cat("  RETENTION BY POLITICAL SYSTEM (all countries, all years)\n")
cat("================================================================\n\n")

system_analysis <- whogov[!is.na(system_category), .(
  Countries = uniqueN(country_name),
  Country_years = .N,
  Mean_retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  Mean_tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = system_category][order(-Mean_retention)]

print(system_analysis)

# =============================================================================
# 2.3 Country rankings (all time)
# =============================================================================

cat("\n\n================================================================\n")
cat("  COUNTRY RANKINGS (entire period)\n")
cat("================================================================\n\n")

country_all <- whogov[, .(
  Years = .N,
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE),
  Mean_tenure = mean(average_minister, na.rm = TRUE)
), by = .(country_name, country_isocode)][Years >= 10]  # At least 10 years data

country_all <- country_all[order(Mean_retention)]

cat("LOWEST RETENTION (most turnover) - Bottom 20:\n")
print(head(country_all[, .(country_name, Years, Mean_retention = round(Mean_retention, 3))], 20))

cat("\n\nHIGHEST RETENTION (least turnover) - Top 20:\n")
print(tail(country_all[, .(country_name, Years, Mean_retention = round(Mean_retention, 3))], 20))

# Denmark's position
denmark_rank <- which(country_all$country_isocode == "DNK")
cat("\n\nDENMARK'S POSITION:\n")
cat("Rank:", denmark_rank, "of", nrow(country_all), "\n")
cat("Percentile:", round(100 * denmark_rank / nrow(country_all)), "%\n")
cat("Retention:", round(country_all[country_isocode == "DNK", Mean_retention], 3), "\n")

# =============================================================================
# 2.4 Changes over time by region
# =============================================================================

cat("\n\n================================================================\n")
cat("  CHANGES OVER TIME BY REGION\n")
cat("================================================================\n\n")

# Define regions
nordic <- c("DNK", "SWE", "NOR", "FIN", "ISL")
west_eu <- c("DEU", "FRA", "GBR", "NLD", "BEL", "AUT", "CHE", "IRL", "LUX")
south_eu <- c("ITA", "ESP", "PRT", "GRC")
east_eu <- c("POL", "CZE", "HUN", "SVK", "ROU", "BGR", "UKR", "RUS")
latin_am <- c("ARG", "BRA", "CHL", "COL", "MEX", "PER", "VEN", "ECU", "BOL")
asia <- c("JPN", "KOR", "CHN", "IND", "THA", "IDN", "MYS", "PHL", "VNM")
africa <- c("ZAF", "NGA", "KEN", "EGY", "ETH", "GHA", "TZA", "UGA")

whogov[, region := fcase(
  country_isocode %in% nordic, "Nordic",
  country_isocode %in% west_eu, "Western Europe",
  country_isocode %in% south_eu, "Southern Europe",
  country_isocode %in% east_eu, "Eastern Europe",
  country_isocode %in% latin_am, "Latin America",
  country_isocode %in% asia, "Asia",
  country_isocode %in% africa, "Africa",
  country_isocode == "USA", "USA",
  country_isocode %in% c("AUS", "NZL", "CAN"), "Anglo settler",
  default = "Other"
)]

# Compare periods: 1966-1989 vs 1990-2023
whogov[, period := fifelse(year < 1990, "1966-1989", "1990-2023")]

period_comparison <- whogov[region != "Other", .(
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE)
), by = .(region, period)]

period_wide <- dcast(period_comparison, region ~ period, value.var = "Mean_retention")
setnames(period_wide, c("1966-1989", "1990-2023"), c("Early", "Late"))
period_wide[, Change := round(Late - Early, 3)]
period_wide[, Early := round(Early, 3)]
period_wide[, Late := round(Late, 3)]

print(period_wide[order(-Change)])

cat("\nPositive = retention improved (longer tenure)\n")
cat("Negative = retention worsened (more turnover)\n")

# =============================================================================
# 2.5 Outlier countries
# =============================================================================

cat("\n\n================================================================\n")
cat("  OUTLIER COUNTRIES (unusual patterns)\n")
cat("================================================================\n\n")

# Z-scores
country_all[, z_score := (Mean_retention - mean(Mean_retention)) / sd(Mean_retention)]

cat("EXTREME LOW RETENTION (z < -2):\n")
print(country_all[z_score < -2, .(country_name, Mean_retention = round(Mean_retention, 3), z = round(z_score, 2))])

cat("\n\nEXTREME HIGH RETENTION (z > 2):\n")
print(country_all[z_score > 2, .(country_name, Mean_retention = round(Mean_retention, 3), z = round(z_score, 2))])

# =============================================================================
# 2.6 Democracies only - focus analysis
# =============================================================================

cat("\n\n================================================================\n")
cat("  DEMOCRACIES ONLY - DETAILED ANALYSIS\n")
cat("================================================================\n\n")

democracies <- whogov[system_category %in% c("Parliamentary democracy",
                                              "Presidential democracy",
                                              "Mixed democratic")]

dem_countries <- democracies[, .(
  Years = .N,
  System = first(system_category),
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE),
  Mean_tenure = mean(average_minister, na.rm = TRUE)
), by = country_name][Years >= 10][order(Mean_retention)]

cat("DEMOCRACIES WITH LOWEST RETENTION:\n")
print(head(dem_countries[, .(country_name, System, Mean_retention = round(Mean_retention, 3))], 15))

cat("\n\nDEMOCRACIES WITH HIGHEST RETENTION:\n")
print(tail(dem_countries[, .(country_name, System, Mean_retention = round(Mean_retention, 3))], 15))

# =============================================================================
# SAVE RESULTS
# =============================================================================

saveRDS(list(
  global_trend = global_trend,
  system_analysis = system_analysis,
  country_all = country_all,
  period_wide = period_wide,
  dem_countries = dem_countries
), "output/international_validation.rds")

cat("\n\n================================================================\n")
cat("  ANALYSIS COMPLETE\n")
cat("================================================================\n")
