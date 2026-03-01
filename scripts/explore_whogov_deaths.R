# Explore WhoGov dataset for death-related information
library(data.table)

setwd("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/R/danish-cabinets")

# Redirect output to file
sink("output/whogov_death_exploration.txt")

whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")

cat("=== Column names ===\n")
print(names(whogov))

cat("\n=== Columns containing 'death' or 'died' ===\n")
death_cols <- names(whogov)[grepl("death|died|dead|mortality", names(whogov), ignore.case = TRUE)]
print(death_cols)

cat("\n=== Columns containing 'exit' or 'leave' or 'end' ===\n")
exit_cols <- names(whogov)[grepl("exit|leave|end|termin", names(whogov), ignore.case = TRUE)]
print(exit_cols)

cat("\n=== Columns containing 'reason' ===\n")
reason_cols <- names(whogov)[grepl("reason", names(whogov), ignore.case = TRUE)]
print(reason_cols)

cat("\n=== First few rows of potentially relevant columns ===\n")
if (length(exit_cols) > 0) {
  print(head(whogov[, ..exit_cols], 20))
}

cat("\n=== Unique values in exit/termination columns ===\n")
for (col in exit_cols) {
  cat("\n", col, ":\n")
  print(table(whogov[[col]], useNA = "ifany"))
}

sink()
cat("Done. Output written to output/whogov_death_exploration.txt\n")
