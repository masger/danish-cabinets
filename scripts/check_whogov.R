library(data.table)
setwd("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/R/danish-cabinets")

whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")

# Write column names to file
writeLines(names(whogov), "output/whogov_columns.txt")

# Check for death-related columns
death_cols <- names(whogov)[grepl("death|died|exit|reason|end",
                                   names(whogov), ignore.case = TRUE)]
writeLines(c("Death/exit related columns:", death_cols),
           "output/whogov_death_cols.txt")
