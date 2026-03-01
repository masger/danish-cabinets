# Render the BMJ Christmas report
setwd("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/R/danish-cabinets")

tryCatch({
  cat("Starting render...\n")
  rmarkdown::render("reports/BMJ_Christmas_2024.Rmd")
  cat("Render completed successfully!\n")
  cat("Output file:", file.path(getwd(), "reports", "BMJ_Christmas_2024.html"), "\n")
}, error = function(e) {
  cat("Error during render:\n")
  cat(conditionMessage(e), "\n")
})
