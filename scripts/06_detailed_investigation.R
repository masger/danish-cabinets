# =============================================================================
# DETAILED INVESTIGATION: Agriculture, Environment, Ambiguous exits, Deaths
# =============================================================================

library(data.table)
library(lubridate)

cat("\n================================================================\n")
cat("  DETAILED INVESTIGATION\n")
cat("================================================================\n\n")

# Load data
data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]
Encoding(data$ministerpost) <- "latin1"

# =============================================================================
# 1. CHECK FOR ACTUAL DEATHS IN OFFICE
# =============================================================================

cat("================================================================\n")
cat("  PART 1: Did any ministers ACTUALLY DIE in office?\n")
cat("================================================================\n\n")

# Look for death indicators in the data
# Check ministerpost column for death symbols (†, død, death)
deaths <- data[grepl("†|død|death|afgået ved døden", ministerpost, ignore.case = TRUE)]

cat("Ministers with death indicators in their record:\n\n")
if (nrow(deaths) > 0) {
  print(deaths[, .(navn, ministerpost, start, stop)])
} else {
  cat("No explicit death markers found in ministerpost column\n")
}

# Also check for very short tenures ending abruptly
# Or ministers whose stop date is their death date
cat("\n\nChecking for potential deaths (stop date patterns):\n")

# Look for ministers where stop date might indicate death
# (e.g., odd dates, mid-week, no obvious political event)
data[, weekday := weekdays(stop)]
data[, year_stop := year(stop)]

# Ministers who stopped on unusual dates (not typical reshuffle days)
# Government changes usually happen on specific dates

cat("\nMinisters with '†' symbol (death marker):\n")
death_symbol <- data[grepl("†", ministerpost)]
if (nrow(death_symbol) > 0) {
  print(death_symbol[, .(navn, start, stop, ministerpost)])
}

# =============================================================================
# 2. AGRICULTURE AND ENVIRONMENT MINISTERS
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 2: Agriculture and Environment Ministers\n")
cat("================================================================\n\n")

# Classify ministries more precisely
extract_ministry_detailed <- function(x) {
  x <- tryCatch(tolower(x), error = function(e) x)
  if (is.na(x) || x == "") return("Other")
  if (grepl("landbrug|f.devare|fiskeri", x)) return("Agriculture/Food")
  if (grepl("milj|natur|klima|energi", x)) return("Environment/Climate")
  if (grepl("statsminister", x)) return("Prime Minister")
  if (grepl("skatteminister", x)) return("Tax Minister")
  if (grepl("finansminister", x)) return("Finance Minister")
  if (grepl("transport|trafik", x)) return("Transport Minister")
  if (grepl("indenrigsminister", x)) return("Interior Minister")
  if (grepl("udenrigsminister", x)) return("Foreign Minister")
  return("Other")
}

data[, ministry_detailed := sapply(ministerpost, extract_ministry_detailed)]

# Political bloc
data[, bloc := fifelse(grepl("Venstre|Konservative|Liberal|Dansk Folkeparti|Nye Borgerlige",
                              `Parti(er)`, ignore.case = TRUE), "Blue", "Red")]

# Calculate government dates
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]
data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]

# Analysis dataset
dat <- data[decimal_date(start) > 1945 & !is.na(time) & time > 0]

# Person-government level
person_govt <- dat[, .(
  first_start = min(start, na.rm = TRUE),
  last_stop = max(stop, na.rm = TRUE),
  total_time = sum(time, na.rm = TRUE),
  positions = paste(unique(ministry_detailed), collapse = " + ")
), by = .(navn, regering, r_start, r_stop, bloc)]

person_govt[, days_before_govt_end := as.numeric(difftime(r_stop, last_stop, units = "days"))]
person_govt[, fired := as.numeric(days_before_govt_end > 30)]

# Add ministry
first_ministry <- dat[, .(ministry_detailed = ministry_detailed[which.min(start)]), by = .(navn, regering)]
person_govt <- merge(person_govt, first_ministry, by = c("navn", "regering"), all.x = TRUE)

# Agriculture analysis
cat("AGRICULTURE/FOOD MINISTERS:\n\n")
agri <- person_govt[ministry_detailed == "Agriculture/Food"]
cat("Total appointments:", nrow(agri), "\n")
cat("By bloc:\n")
print(agri[, .(N = .N, Fired = sum(fired, na.rm = TRUE),
               Rate = round(100 * mean(fired, na.rm = TRUE), 1)), by = bloc])

cat("\n\nAgriculture ministers who were 'fired':\n")
print(agri[fired == 1, .(navn, regering, bloc, days_before_govt_end)])

# Environment analysis
cat("\n\nENVIRONMENT/CLIMATE MINISTERS:\n\n")
env <- person_govt[ministry_detailed == "Environment/Climate"]
cat("Total appointments:", nrow(env), "\n")
cat("By bloc:\n")
print(env[, .(N = .N, Fired = sum(fired, na.rm = TRUE),
              Rate = round(100 * mean(fired, na.rm = TRUE), 1)), by = bloc])

cat("\n\nEnvironment ministers who were 'fired':\n")
print(env[fired == 1, .(navn, regering, bloc, days_before_govt_end)])

# Compare Blue vs Red across all ministries
cat("\n\n================================================================\n")
cat("  BLUE vs RED: Firing rates by ministry\n")
cat("================================================================\n\n")

bloc_ministry <- person_govt[ministry_detailed != "Other", .(
  N = .N,
  Fired = sum(fired, na.rm = TRUE),
  Rate = round(100 * mean(fired, na.rm = TRUE), 1)
), by = .(ministry_detailed, bloc)]

bloc_wide <- dcast(bloc_ministry, ministry_detailed ~ bloc, value.var = c("N", "Rate"))
setcolorder(bloc_wide, c("ministry_detailed", "N_Blue", "Rate_Blue", "N_Red", "Rate_Red"))

print(bloc_wide[order(-Rate_Blue)])

# Statistical test for Agriculture
cat("\n\nChi-square test: Agriculture firing rate Blue vs Red:\n")
agri_table <- agri[, .(Fired = sum(fired, na.rm = TRUE),
                        NotFired = sum(1 - fired, na.rm = TRUE)), by = bloc]
if (nrow(agri_table) == 2) {
  m <- matrix(c(agri_table$Fired, agri_table$NotFired), nrow = 2)
  test <- fisher.test(m)
  cat("Fisher's exact test p-value:", round(test$p.value, 3), "\n")
}

# =============================================================================
# 3. DETAILED LOOK AT AMBIGUOUS CATEGORY
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 3: What happens in the 'AMBIGUOUS' period (8-30 days)?\n")
cat("================================================================\n\n")

person_govt[, departure_detailed := fcase(
  days_before_govt_end <= 0, "Left WITH government (or after)",
  days_before_govt_end <= 7, "Left within 1 week of govt end",
  days_before_govt_end <= 14, "Left 1-2 weeks before govt end",
  days_before_govt_end <= 30, "Left 2-4 weeks before govt end",
  days_before_govt_end <= 60, "Left 1-2 months before govt end",
  days_before_govt_end <= 180, "Left 2-6 months before govt end",
  default = "Left >6 months before govt end"
)]

cat("Detailed departure timing:\n\n")
departure_detail <- person_govt[, .N, by = departure_detailed][order(-N)]
departure_detail[, Pct := round(100 * N / sum(N), 1)]
print(departure_detail)

# Look at specific cases in ambiguous period (8-30 days)
cat("\n\nMinisters who left 8-30 days before government end:\n")
cat("(These are classified as 'ambiguous')\n\n")
ambiguous <- person_govt[days_before_govt_end > 7 & days_before_govt_end <= 30]
print(ambiguous[, .(navn, regering, days_before_govt_end, ministry_detailed)][order(days_before_govt_end)])

# What ministries are over-represented in ambiguous?
cat("\n\nMinistries over-represented in ambiguous category:\n")
ambig_by_ministry <- person_govt[ministry_detailed != "Other", .(
  Total = .N,
  Ambiguous = sum(days_before_govt_end > 7 & days_before_govt_end <= 30, na.rm = TRUE),
  Rate = round(100 * mean(days_before_govt_end > 7 & days_before_govt_end <= 30, na.rm = TRUE), 1)
), by = ministry_detailed][order(-Rate)]
print(ambig_by_ministry)

# =============================================================================
# 4. REASONS FOR DEPARTURE (from ministerpost text)
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 4: Can we identify REASONS for departure?\n")
cat("================================================================\n\n")

# Look for patterns in ministerpost that indicate reason
cat("Ministerpost entries with dates or qualifiers:\n\n")

# Find entries with specific date mentions (often indicate mid-term changes)
dated_entries <- dat[grepl("\\(fra |\\(til |\\(fra\\.|\\(til\\.", ministerpost)]
cat("Entries with 'fra/til' (from/to) date specifications:", nrow(dated_entries), "\n\n")

# Sample of these
cat("Sample of ministerpost entries with date specifications:\n")
print(head(unique(dated_entries$ministerpost), 20))

# Look for scandal-related exits (look for names that appear in news)
cat("\n\nMinisters with very short tenures (< 6 months) who were 'fired':\n")
short_fired <- person_govt[fired == 1 & total_time < 0.5]
print(short_fired[, .(navn, regering, total_time_months = round(total_time * 12, 1),
                       ministry_detailed, days_before_govt_end)][order(total_time_months)])

# =============================================================================
# 5. SPECIFIC PATTERN: Agriculture in Blue governments
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 5: AGRICULTURE IN BLUE GOVERNMENTS - Deep dive\n")
cat("================================================================\n\n")

agri_blue <- dat[ministry_detailed == "Agriculture/Food" &
                  grepl("Venstre|Konservative|Liberal", `Parti(er)`, ignore.case = TRUE)]

cat("All Agriculture ministers in Blue governments:\n\n")
print(agri_blue[, .(navn, start, stop, regering, `Parti(er)`,
                     time_years = round(time, 2))][order(start)])

# Calculate early exits for these
agri_blue[, days_to_govt_end := as.numeric(difftime(r_stop, stop, units = "days"))]
cat("\n\nDays before government ended:\n")
print(agri_blue[, .(navn, start, stop, days_to_govt_end)][order(-days_to_govt_end)])

# =============================================================================
# 6. ENVIRONMENT IN BLUE GOVERNMENTS - Deep dive
# =============================================================================

cat("\n\n================================================================\n")
cat("  PART 6: ENVIRONMENT IN BLUE GOVERNMENTS - Deep dive\n")
cat("================================================================\n\n")

env_blue <- dat[ministry_detailed == "Environment/Climate" &
                 grepl("Venstre|Konservative|Liberal", `Parti(er)`, ignore.case = TRUE)]

cat("All Environment ministers in Blue governments:\n\n")
print(env_blue[, .(navn, start, stop, regering, `Parti(er)`,
                    time_years = round(time, 2))][order(start)])

env_blue[, days_to_govt_end := as.numeric(difftime(r_stop, stop, units = "days"))]
cat("\n\nDays before government ended:\n")
print(env_blue[, .(navn, start, stop, days_to_govt_end)][order(-days_to_govt_end)])

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n\n================================================================\n")
cat("  SUMMARY OF FINDINGS\n")
cat("================================================================\n\n")

cat("1. DEATHS IN OFFICE:\n")
cat("   Ministers with † symbol found:", nrow(death_symbol), "\n")

cat("\n2. AGRICULTURE MINISTERS:\n")
cat("   Blue firing rate:", agri[bloc == "Blue", round(100 * mean(fired, na.rm = TRUE), 1)], "%\n")
cat("   Red firing rate:", agri[bloc == "Red", round(100 * mean(fired, na.rm = TRUE), 1)], "%\n")

cat("\n3. ENVIRONMENT MINISTERS:\n")
cat("   Blue firing rate:", env[bloc == "Blue", round(100 * mean(fired, na.rm = TRUE), 1)], "%\n")
cat("   Red firing rate:", env[bloc == "Red", round(100 * mean(fired, na.rm = TRUE), 1)], "%\n")

cat("\n4. AMBIGUOUS CATEGORY (8-30 days before govt end):\n")
cat("   N:", nrow(ambiguous), "observations\n")
cat("   Likely explanation: Pre-election reshuffles or resignations\n")

# Save results
saveRDS(list(
  death_symbol = death_symbol,
  agri = agri,
  env = env,
  bloc_ministry = bloc_ministry,
  ambiguous = ambiguous
), "output/detailed_investigation.rds")

cat("\n\nResults saved to output/detailed_investigation.rds\n")
cat("================================================================\n")
