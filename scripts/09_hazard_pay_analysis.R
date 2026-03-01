# Hazard Pay Analysis for BMJ Christmas Article
# Comparing ministerial "occupational hazards" to actual dangerous professions

library(data.table)

setwd("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/R/danish-cabinets")

# =============================================================================
# 1. DANISH MINISTERIAL SALARY DATA (2025)
# =============================================================================
minister_salaries <- data.table(
  position = c("Prime Minister", "Top Ministers (Finance, Foreign)", "Regular Minister"),
  annual_salary_dkk = c(1958176, 1723195, 1566541),
  annual_salary_eur = c(1958176/7.46, 1723195/7.46, 1566541/7.46),  # ~7.46 DKK/EUR
  annual_salary_usd = c(1958176/7.46*1.08, 1723195/7.46*1.08, 1566541/7.46*1.08)  # EUR to USD
)

cat("=== DANISH MINISTERIAL SALARIES (2025) ===\n")
print(minister_salaries)

# =============================================================================
# 2. DANISH MINISTERIAL MORTALITY DATA
# =============================================================================
# Load our Danish data
dk_data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")

# Count deaths in office
deaths <- dk_data[grepl("†", ministerpost)]
n_deaths <- nrow(deaths)

# Total unique ministers post-1945
dk_data[, start_year := year(as.Date(start))]
post_war <- dk_data[start_year >= 1945]
n_unique_ministers <- uniqueN(post_war$navn)

# Time period
years_covered <- 2022 - 1945  # 77 years

# Calculate mortality rate in office
death_rate_per_year <- n_deaths / years_covered
death_rate_per_minister <- n_deaths / n_unique_ministers

cat("\n=== DANISH MINISTERIAL DEATHS IN OFFICE ===\n")
cat("Deaths in office:", n_deaths, "\n")
cat("Unique ministers (1945-2022):", n_unique_ministers, "\n")
cat("Years covered:", years_covered, "\n")
cat("Deaths per year:", round(death_rate_per_year, 3), "\n")
cat("Death rate per minister:", round(death_rate_per_minister * 100, 2), "%\n")

# PM-specific analysis
pm_deaths <- deaths[grepl("statsminister", tolower(ministerpost))]
n_pm_deaths <- nrow(pm_deaths)
n_unique_pms <- uniqueN(post_war[grepl("statsminister", tolower(ministerpost))]$navn)

cat("\n=== PRIME MINISTER DEATHS ===\n")
cat("PM deaths in office:", n_pm_deaths, "\n")
cat("Unique PMs (1945-2022):", n_unique_pms, "\n")
cat("PM death rate:", round(n_pm_deaths / n_unique_pms * 100, 2), "%\n")

# But actually 3 PM deaths were 1942, 1955, 1960 - within 18 years!
pm_death_years <- c(1942, 1955, 1960)
pm_death_interval <- 18  # years between first and last
cat("3 PM deaths in", pm_death_interval, "years (1942-1960)\n")
cat("That's one PM death every", round(pm_death_interval / 3, 1), "years\n")

# =============================================================================
# 3. OCCUPATIONAL HAZARD COMPARISONS (US BLS Data 2023)
# =============================================================================
# Fatality rates per 100,000 full-time equivalent workers
occupation_hazards <- data.table(
  occupation = c(
    "Fishing workers",
    "Logging workers",
    "Aircraft pilots",
    "Roofers",
    "Garbage collectors",
    "Steel workers",
    "Truck drivers",
    "Farmers/ranchers",
    "Construction workers",
    "All workers average",
    "Office workers",
    "Danish PM (1942-1960)",
    "Danish Minister (overall)"
  ),
  fatality_rate_per_100k = c(
    145,     # Fishing
    82,      # Logging
    58,      # Pilots
    47,      # Roofers
    34,      # Garbage collectors
    25,      # Steel workers
    24,      # Truck drivers
    20,      # Farmers
    10,      # Construction
    3.5,     # All workers
    0.5,     # Office (estimated)
    # Danish PM: 3 deaths / 18 years, assuming ~1 PM at risk = 3/18 * 100000 = 16,667
    16667,
    # Danish ministers: 8 deaths / 77 years / ~25 ministers at risk = 8/77/25*100000 = 415
    415
  )
)

cat("\n=== OCCUPATIONAL FATALITY RATES (per 100,000 workers per year) ===\n")
print(occupation_hazards[order(-fatality_rate_per_100k)])

# =============================================================================
# 4. ACTUARIAL COMPARISON
# =============================================================================
# Denmark mortality rates (from georank.org data)
# Annual death probability at different ages:
# Age 50: ~0.3%
# Age 60: ~0.64%
# Age 70: ~1.5%

# Average minister age (estimate ~50)
expected_annual_death_rate_age_50 <- 0.003  # 0.3%
expected_annual_death_rate_age_55 <- 0.004  # 0.4%
expected_annual_death_rate_age_60 <- 0.0064 # 0.64%

# Average tenure ~3 years
average_tenure_years <- 3

# Expected deaths during 77 years with ~25 ministers at any time
# If avg age 55, expected deaths = 77 * 25 * 0.004 = 7.7
expected_deaths_77_years <- 77 * 25 * expected_annual_death_rate_age_55

cat("\n=== ACTUARIAL COMPARISON ===\n")
cat("Expected deaths (actuarial, age 55):", round(expected_deaths_77_years, 1), "\n")
cat("Observed deaths:", n_deaths, "\n")
cat("Standardized Mortality Ratio:", round(n_deaths / expected_deaths_77_years, 2), "\n")

# For PMs specifically (1942-1960, 18 years)
# Expected: 18 years * 1 PM * 0.006 (avg age ~60) = 0.11 deaths
expected_pm_deaths_18_years <- 18 * 1 * 0.006
observed_pm_deaths_18_years <- 3

cat("\nPrime Minister (1942-1960):\n")
cat("Expected deaths (actuarial):", round(expected_pm_deaths_18_years, 2), "\n")
cat("Observed deaths:", observed_pm_deaths_18_years, "\n")
cat("Excess mortality ratio:", round(observed_pm_deaths_18_years / expected_pm_deaths_18_years, 1), "x\n")

# =============================================================================
# 5. HAZARD PAY CALCULATION
# =============================================================================
# If we treat ministerial dismissal as an occupational hazard...
# Dismissal rate: 22.4%
dismissal_rate <- 0.224

# Average tenure: ~3 years
# "Hazard" (dismissal) per year: 22.4% / 3 = 7.5% per year

# Compare to fishing industry: 0.145% fatality per year
# Ministerial "career death" rate is 52x higher than fishing fatality rate!

career_death_rate <- dismissal_rate / 3  # per year
fishing_fatality_rate <- 145 / 100000

cat("\n=== HAZARD PAY ANALYSIS ===\n")
cat("Annual ministerial 'career death' rate:", round(career_death_rate * 100, 1), "%\n")
cat("Annual fishing worker fatality rate:", round(fishing_fatality_rate * 100, 3), "%\n")
cat("Ratio (career death vs actual death):", round(career_death_rate / fishing_fatality_rate, 0), "x\n")

# What hazard pay would be justified?
# Fishing workers get premium of ~$20k/year for danger
# If ministerial career hazard is 52x higher...
fishing_hazard_premium <- 20000  # USD estimate
implied_career_hazard_premium <- fishing_hazard_premium * (career_death_rate / fishing_fatality_rate)

cat("\nImplied 'career hazard' premium:", format(round(implied_career_hazard_premium), big.mark=","), "USD/year\n")
cat("Current minister salary:", format(round(minister_salaries$annual_salary_usd[3]), big.mark=","), "USD/year\n")

# =============================================================================
# 6. INTERNATIONAL COMPARISON - HEAD OF GOVERNMENT MORTALITY
# =============================================================================
cat("\n=== INTERNATIONAL RESEARCH FINDINGS ===\n")
cat("\nFrom BMJ study (Olenski et al. 2015):\n")
cat("- Elected heads of government lived 4.4 fewer years than runners-up\n")
cat("- Mortality hazard ratio: 1.23 (23% elevated risk)\n")
cat("- Sample: 540 candidates across 17 countries (1722-2015)\n")

cat("\nFrom PMC study (Clarke et al. 2022):\n")
cat("- Politicians overall have LOWER mortality (SMR 0.45-0.82)\n")
cat("- But this advantage is for all politicians, not leaders specifically\n")
cat("- US politicians: 7.8 years longer life expectancy at age 45\n")

cat("\nThe paradox: Politicians live longer, but LEADERS die sooner!\n")

# =============================================================================
# 7. SUMMARY FOR BMJ ARTICLE
# =============================================================================
cat("\n" , rep("=", 60), "\n")
cat("SUMMARY: KEY FIGURES FOR BMJ HAZARD PAY ARTICLE\n")
cat(rep("=", 60), "\n")

cat("\n1. SALARY:\n")
cat("   Prime Minister: 1,958,176 DKK (~262,500 USD)\n")
cat("   Regular Minister: 1,566,541 DKK (~210,000 USD)\n")

cat("\n2. ACTUAL DEATHS:\n")
cat("   8 ministers died in office (1945-2022)\n")
cat("   3 Prime Ministers died 1942-1960 (one every 6 years!)\n")
cat("   - Thorvald Stauning (1942) - illness during occupation\n")
cat("   - Hans Hedtoft (1955) - heart attack at Nordic Council\n")
cat("   - H.C. Hansen (1960) - illness\n")

cat("\n3. CAREER MORTALITY:\n")
cat("   22.4% of ministers are 'killed' (fired mid-term)\n")
cat("   Interior Minister: 55.6% career fatality rate\n")
cat("   Agriculture Minister: 51.9% career fatality rate\n")

cat("\n4. COMPARISONS:\n")
cat("   Danish PM death rate 1942-1960: 27x expected actuarial rate\n")
cat("   Ministerial 'career death' rate: 52x fishing worker fatality rate\n")
cat("   Elected leaders die 4.4 years sooner than losing candidates\n")

cat("\n5. HAZARD PAY JUSTIFIED:\n")
cat("   Based on career hazard alone: ~$1 million/year premium\n")
cat("   Current salary covers only 21% of implied hazard premium\n")

# Save results
results <- list(
  salaries = minister_salaries,
  deaths = n_deaths,
  pm_deaths = n_pm_deaths,
  dismissal_rate = dismissal_rate,
  occupation_hazards = occupation_hazards
)
saveRDS(results, "output/hazard_pay_analysis.rds")

cat("\n\nResults saved to output/hazard_pay_analysis.rds\n")
