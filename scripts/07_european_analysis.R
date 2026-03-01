# =============================================================================
# EUROPEAN ANALYSIS: Patterns across European countries
# =============================================================================

library(data.table)
library(lubridate)
library(ggplot2)

cat("\n================================================================\n")
cat("  EUROPEAN MINISTERIAL PATTERNS\n")
cat("================================================================\n\n")

# Load WhoGov data
whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")

cat("WhoGov data loaded:", nrow(whogov), "observations\n\n")

# =============================================================================
# 1. DEFINE EUROPEAN COUNTRIES
# =============================================================================

# European country codes
nordic <- c("DNK", "SWE", "NOR", "FIN", "ISL")
western_eu <- c("DEU", "FRA", "GBR", "NLD", "BEL", "AUT", "CHE", "IRL", "LUX")
southern_eu <- c("ITA", "ESP", "PRT", "GRC", "MLT", "CYP")
eastern_eu <- c("POL", "CZE", "HUN", "SVK", "ROU", "BGR", "SRB", "HRV", "SVN",
                "EST", "LVA", "LTU", "UKR", "BLR", "MDA", "MKD", "ALB", "BIH", "MNE", "XKX")
all_europe <- c(nordic, western_eu, southern_eu, eastern_eu)

whogov[, region := fcase(
  country_isocode %in% nordic, "Nordic",
  country_isocode %in% western_eu, "Western Europe",
  country_isocode %in% southern_eu, "Southern Europe",
  country_isocode %in% eastern_eu, "Eastern Europe",
  default = "Other"
)]

europe <- whogov[region != "Other"]

cat("European countries in dataset:", uniqueN(europe$country_name), "\n")
cat("European observations:", nrow(europe), "\n\n")

# =============================================================================
# 2. EUROPEAN OVERVIEW
# =============================================================================

cat("================================================================\n")
cat("  EUROPEAN RETENTION RATES BY COUNTRY\n")
cat("================================================================\n\n")

eu_countries <- europe[, .(
  Region = first(region),
  Years = .N,
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE),
  SD_retention = sd(retention_rate_minister, na.rm = TRUE),
  Mean_tenure = mean(average_minister, na.rm = TRUE)
), by = country_name][order(Mean_retention)]

cat("EUROPEAN COUNTRIES - LOWEST RETENTION (most turnover):\n")
print(head(eu_countries[, .(country_name, Region, Mean_retention = round(Mean_retention, 3))], 15))

cat("\n\nEUROPEAN COUNTRIES - HIGHEST RETENTION (most stable):\n")
print(tail(eu_countries[, .(country_name, Region, Mean_retention = round(Mean_retention, 3))], 15))

# =============================================================================
# 3. REGIONAL PATTERNS IN EUROPE
# =============================================================================

cat("\n\n================================================================\n")
cat("  REGIONAL PATTERNS IN EUROPE\n")
cat("================================================================\n\n")

eu_regions <- europe[, .(
  Countries = uniqueN(country_name),
  Years = .N,
  Mean_retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  Mean_tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = region][order(-Mean_retention)]

print(eu_regions)

# =============================================================================
# 4. TEMPORAL CHANGES IN EUROPE
# =============================================================================

cat("\n\n================================================================\n")
cat("  TEMPORAL CHANGES IN EUROPE\n")
cat("================================================================\n\n")

# Decades
europe[, decade := floor(year/10)*10]

eu_decades <- europe[, .(
  N = .N,
  Mean_retention = round(mean(retention_rate_minister, na.rm = TRUE), 3)
), by = .(region, decade)][order(region, decade)]

cat("Retention by decade and region:\n\n")
eu_decades_wide <- dcast(eu_decades, region ~ decade, value.var = "Mean_retention")
print(eu_decades_wide)

# Pre/post 1990 (end of Cold War)
europe[, era := fifelse(year < 1990, "Pre-1990", "Post-1990")]

eu_era <- europe[, .(
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE)
), by = .(region, era)]

eu_era_wide <- dcast(eu_era, region ~ era, value.var = "Mean_retention")
eu_era_wide[, Change := round(`Post-1990` - `Pre-1990`, 3)]
eu_era_wide[, `Pre-1990` := round(`Pre-1990`, 3)]
eu_era_wide[, `Post-1990` := round(`Post-1990`, 3)]

cat("\n\nChange in retention: Pre-1990 vs Post-1990:\n")
print(eu_era_wide[order(Change)])

# =============================================================================
# 5. VOLATILITY ANALYSIS - WHO HAS MOST UNSTABLE CABINETS?
# =============================================================================

cat("\n\n================================================================\n")
cat("  VOLATILITY: Countries with most unstable cabinets\n")
cat("================================================================\n\n")

# Coefficient of variation
eu_countries[, CV := SD_retention / Mean_retention]

cat("Most volatile cabinets (highest CV in retention):\n")
print(head(eu_countries[!is.na(CV), .(country_name, Region,
                                       Mean_retention = round(Mean_retention, 3),
                                       CV = round(CV, 2))][order(-CV)], 15))

# =============================================================================
# 6. OUTLIER YEARS - When did specific countries have massive turnover?
# =============================================================================

cat("\n\n================================================================\n")
cat("  OUTLIER YEARS: When did European countries have massive turnover?\n")
cat("================================================================\n\n")

# Find years with retention < 0.3 (very low)
low_retention_years <- europe[retention_rate_minister < 0.3 & !is.na(retention_rate_minister)]

cat("European country-years with retention < 30%:\n\n")
print(low_retention_years[, .(country_name, year, retention = round(retention_rate_minister, 2),
                               govern_name)][order(retention)])

# Count low retention years by country
low_count <- low_retention_years[, .N, by = country_name][order(-N)]
cat("\n\nCountries with most low-retention years:\n")
print(head(low_count, 10))

# =============================================================================
# 7. POLITICAL SYSTEM EFFECTS IN EUROPE
# =============================================================================

cat("\n\n================================================================\n")
cat("  POLITICAL SYSTEM EFFECTS IN EUROPE\n")
cat("================================================================\n\n")

eu_system <- europe[!is.na(system_category), .(
  Countries = uniqueN(country_name),
  Years = .N,
  Mean_retention = round(mean(retention_rate_minister, na.rm = TRUE), 3)
), by = system_category][order(-Mean_retention)]

print(eu_system)

# =============================================================================
# 8. SPECIFIC COUNTRY DEEP DIVES
# =============================================================================

cat("\n\n================================================================\n")
cat("  SPECIFIC COUNTRY ANALYSIS\n")
cat("================================================================\n\n")

# Italy - known for unstable governments
cat("ITALY - The classic case of instability:\n")
italy <- europe[country_name == "Italy"]
cat("Mean retention:", round(mean(italy$retention_rate_minister, na.rm = TRUE), 3), "\n")
cat("Mean tenure:", round(mean(italy$average_minister, na.rm = TRUE), 2), "years\n")
cat("Years with retention < 50%:", sum(italy$retention_rate_minister < 0.5, na.rm = TRUE), "\n\n")

# Belgium - also known for instability
cat("BELGIUM - Complex coalition politics:\n")
belgium <- europe[country_name == "Belgium"]
cat("Mean retention:", round(mean(belgium$retention_rate_minister, na.rm = TRUE), 3), "\n")
cat("Mean tenure:", round(mean(belgium$average_minister, na.rm = TRUE), 2), "years\n\n")

# Germany - known for stability
cat("GERMANY - The stable anchor:\n")
germany <- europe[country_name == "Germany"]
cat("Mean retention:", round(mean(germany$retention_rate_minister, na.rm = TRUE), 3), "\n")
cat("Mean tenure:", round(mean(germany$average_minister, na.rm = TRUE), 2), "years\n\n")

# UK
cat("UNITED KINGDOM:\n")
uk <- europe[country_name == "United Kingdom"]
cat("Mean retention:", round(mean(uk$retention_rate_minister, na.rm = TRUE), 3), "\n")
cat("Mean tenure:", round(mean(uk$average_minister, na.rm = TRUE), 2), "years\n\n")

# =============================================================================
# 9. NORDIC COMPARISON
# =============================================================================

cat("\n\n================================================================\n")
cat("  NORDIC COMPARISON\n")
cat("================================================================\n\n")

nordic_detail <- europe[region == "Nordic", .(
  Years = .N,
  Mean_retention = round(mean(retention_rate_minister, na.rm = TRUE), 3),
  SD = round(sd(retention_rate_minister, na.rm = TRUE), 3),
  Mean_tenure = round(mean(average_minister, na.rm = TRUE), 2)
), by = country_name][order(-Mean_retention)]

print(nordic_detail)

# Nordic over time
nordic_time <- europe[region == "Nordic", .(
  Mean_retention = mean(retention_rate_minister, na.rm = TRUE)
), by = .(country_name, decade)]

nordic_time_wide <- dcast(nordic_time, country_name ~ decade, value.var = "Mean_retention")
cat("\n\nNordic retention by decade:\n")
print(nordic_time_wide)

# =============================================================================
# 10. EASTERN EUROPE - DEMOCRATIZATION EFFECT
# =============================================================================

cat("\n\n================================================================\n")
cat("  EASTERN EUROPE: The Democratization Shock\n")
cat("================================================================\n\n")

eastern <- europe[region == "Eastern Europe"]

# Before and after 1990
eastern_change <- eastern[, .(
  Pre_1990 = mean(retention_rate_minister[year < 1990], na.rm = TRUE),
  Post_1990 = mean(retention_rate_minister[year >= 1990], na.rm = TRUE)
), by = country_name]

eastern_change[, Change := Post_1990 - Pre_1990]
eastern_change[, Pre_1990 := round(Pre_1990, 3)]
eastern_change[, Post_1990 := round(Post_1990, 3)]
eastern_change[, Change := round(Change, 3)]

cat("Eastern European countries - retention before/after 1990:\n")
print(eastern_change[order(Change)])

# =============================================================================
# 11. CREATE VISUALIZATION
# =============================================================================

# European map of retention rates
eu_plot_data <- eu_countries[, .(country_name, Mean_retention, Region)]

p1 <- ggplot(eu_plot_data, aes(x = reorder(country_name, Mean_retention), y = Mean_retention, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Nordic" = "#4DBBD5FF",
                                "Western Europe" = "#00A087FF",
                                "Southern Europe" = "#E64B35FF",
                                "Eastern Europe" = "#F39B7FFF")) +
  labs(x = "", y = "Mean retention rate",
       title = "European Ministerial Stability",
       subtitle = "Average retention rate by country, 1963-2023") +
  theme_minimal()

ggsave("figures/european_retention.png", p1, width = 10, height = 12, dpi = 300)

# Time series by region
eu_time <- europe[, .(Mean_retention = mean(retention_rate_minister, na.rm = TRUE)),
                   by = .(region, year)]

p2 <- ggplot(eu_time[year >= 1970], aes(x = year, y = Mean_retention, color = region)) +
  geom_line(size = 1) +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Nordic" = "#4DBBD5FF",
                                 "Western Europe" = "#00A087FF",
                                 "Southern Europe" = "#E64B35FF",
                                 "Eastern Europe" = "#F39B7FFF")) +
  labs(x = "Year", y = "Mean retention rate",
       title = "European Ministerial Retention Over Time",
       subtitle = "By region, 1970-2023",
       color = "Region") +
  theme_minimal()

ggsave("figures/european_retention_time.png", p2, width = 10, height = 6, dpi = 300)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n\n================================================================\n")
cat("  SUMMARY: KEY EUROPEAN FINDINGS\n")
cat("================================================================\n\n")

cat("1. MOST UNSTABLE EUROPEAN COUNTRIES:\n")
print(head(eu_countries[, .(country_name, Mean_retention = round(Mean_retention, 3))], 5))

cat("\n2. MOST STABLE EUROPEAN COUNTRIES:\n")
print(tail(eu_countries[, .(country_name, Mean_retention = round(Mean_retention, 3))], 5))

cat("\n3. REGIONAL RANKING:\n")
print(eu_regions[, .(region, Mean_retention)])

cat("\n4. EASTERN EUROPE SHOCK:\n")
cat("   Pre-1990 average:", round(mean(eastern[year < 1990, retention_rate_minister], na.rm = TRUE), 3), "\n")
cat("   Post-1990 average:", round(mean(eastern[year >= 1990, retention_rate_minister], na.rm = TRUE), 3), "\n")

cat("\n5. NORDIC AVERAGE:", round(mean(europe[region == "Nordic", retention_rate_minister], na.rm = TRUE), 3), "\n")
cat("   (Denmark:", round(mean(europe[country_name == "Denmark", retention_rate_minister], na.rm = TRUE), 3), ")\n")

# Save results
saveRDS(list(
  eu_countries = eu_countries,
  eu_regions = eu_regions,
  eu_decades_wide = eu_decades_wide,
  low_retention_years = low_retention_years,
  eastern_change = eastern_change,
  nordic_detail = nordic_detail
), "output/european_analysis.rds")

cat("\n\nResults saved to output/european_analysis.rds\n")
cat("Figures saved to figures/\n")
cat("================================================================\n")
