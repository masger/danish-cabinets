# =============================================================================
# HVEM FYRER FLEST: Rød eller Blå?
# =============================================================================

library(data.table)

cat("\n================================================================\n")
cat("  HVEM FYRER FLEST MINISTRE: RØD ELLER BLÅ REGERING?\n")
cat("================================================================\n\n")

# Load data
data <- fread("data/danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]

# Beregn datoer
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]
data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]

# Post-1945 data
dat <- data[as.numeric(format(start, "%Y")) > 1945 & !is.na(time) & time > 0]

# Person-government level
person_govt <- dat[, .(
  total_time = sum(time, na.rm = TRUE),
  parti = first(`Parti(er)`)
), by = .(navn, regering, r_start, r_stop)]

person_govt[, days_before := as.numeric(difftime(r_stop,
  dat[.BY, max(stop, na.rm = TRUE), on = .(navn, regering)], units = "days")),
  by = .(navn, regering)]

person_govt[, fired := as.numeric(days_before > 30)]

# Klassificer REGERINGEN (ikke ministeren)
# Socialdemokratisk-ledede = Rød
# Andre = Blå
person_govt[, govt_bloc := fifelse(
  grepl("Socialdemokratiet", parti, ignore.case = TRUE),
  "Rød regering",
  "Blå regering"
)]

# =============================================================================
# HOVEDRESULTAT
# =============================================================================

cat("FYRINGSRATE EFTER REGERINGSFARVE:\n\n")

result <- person_govt[, .(
  Ministre = .N,
  Fyrede = sum(fired, na.rm = TRUE),
  Fyringsrate = round(100 * mean(fired, na.rm = TRUE), 1)
), by = govt_bloc][order(-Fyringsrate)]

print(result)

cat("\n")
diff <- result[1, Fyringsrate] - result[2, Fyringsrate]
cat("Forskel:", diff, "procentpoint\n")

# =============================================================================
# STATISTISK TEST
# =============================================================================

cat("\n\nSTATISTISK TEST (Chi-square):\n")

m <- matrix(c(
  result[govt_bloc == "Blå regering", Fyrede],
  result[govt_bloc == "Rød regering", Fyrede],
  result[govt_bloc == "Blå regering", Ministre - Fyrede],
  result[govt_bloc == "Rød regering", Ministre - Fyrede]
), nrow = 2, byrow = TRUE)

rownames(m) <- c("Fyret", "Ikke fyret")
colnames(m) <- c("Blå", "Rød")
print(m)

test <- chisq.test(m)
cat("\nChi-square:", round(test$statistic, 2), "\n")
cat("P-værdi:", round(test$p.value, 4), "\n")

if (test$p.value < 0.05) {
  cat("\n*** STATISTISK SIGNIFIKANT FORSKEL (p < 0.05) ***\n")
} else {
  cat("\n*** Ikke statistisk signifikant (p >= 0.05) ***\n")
}

# =============================================================================
# DETALJER PER REGERING
# =============================================================================

cat("\n\n================================================================\n")
cat("  DETALJER: Fyringsrate per regering\n")
cat("================================================================\n\n")

govt_detail <- person_govt[, .(
  Ministre = .N,
  Fyrede = sum(fired, na.rm = TRUE),
  Fyringsrate = round(100 * mean(fired, na.rm = TRUE), 1),
  Blok = first(govt_bloc)
), by = regering][order(-Fyringsrate)]

cat("Top 10 regeringer med HØJEST fyringsrate:\n")
print(head(govt_detail, 10))

cat("\n\nTop 10 regeringer med LAVEST fyringsrate:\n")
print(tail(govt_detail[Ministre >= 5], 10))

# =============================================================================
# KONKLUSION
# =============================================================================

cat("\n\n================================================================\n")
cat("  KONKLUSION\n")
cat("================================================================\n\n")

blue_rate <- result[govt_bloc == "Blå regering", Fyringsrate]
red_rate <- result[govt_bloc == "Rød regering", Fyringsrate]

if (blue_rate > red_rate) {
  cat("BLÅ REGERINGER fyrer flere ministre!\n")
  cat("- Blå fyringsrate:", blue_rate, "%\n")
  cat("- Rød fyringsrate:", red_rate, "%\n")
  cat("- Forskel:", round(blue_rate - red_rate, 1), "procentpoint\n")
} else {
  cat("RØDE REGERINGER fyrer flere ministre!\n")
  cat("- Rød fyringsrate:", red_rate, "%\n")
  cat("- Blå fyringsrate:", blue_rate, "%\n")
  cat("- Forskel:", round(red_rate - blue_rate, 1), "procentpoint\n")
}

cat("\n================================================================\n")
