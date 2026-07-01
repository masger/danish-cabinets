library(data.table)
library(lubridate)
library(survival)

data <- fread("c:/Users/mand0579/OneDrive - Region Hovedstaden (1)/Projekter/danish-cabinets/data/danish_cabinets.csv", encoding = "Latin-1")
data[, V1 := NULL]

Encoding(data$ministerpost) <- "latin1"

# Fix dates
data[year(stop) == 2051, stop := stop - years(200)]
future_rows <- which(data$stop > Sys.Date())
if (length(future_rows) > 0) data[future_rows, stop := stop - years(100)] 

# Calculate terms
data[, r_start := min(start, na.rm = TRUE), by = regering]
data[, r_stop := max(stop, na.rm = TRUE), by = regering]
data[, p_start := min(start, na.rm = TRUE), by = .(regering, navn)]
data[, p_stop := max(stop, na.rm = TRUE), by = .(regering, navn)]

data[, time := as.numeric(difftime(stop, start, units = "days")) / 365.25]
data[is.na(time) & !is.na(p_stop), time := as.numeric(difftime(p_stop, start, units = "days")) / 365.25]
data[is.na(time) & !is.na(r_stop), time := as.numeric(difftime(r_stop, start, units = "days")) / 365.25]
data[!is.na(time) & time <= 0, time := 0.01]

data[, early := as.numeric(p_stop < r_stop)]
data[is.na(early) | is.infinite(p_stop) | is.infinite(r_stop), early := 0]

# Ministry types
extract_ministry <- function(x) {
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

analysis_data <- data[decimal_date(start) > 1945 & !is.na(time) & time > 0 & !is.na(early) & (is.na(start) | is.na(stop) | start <= stop)]

analysis_data[, bloc_simple := fifelse(grepl("Socialdemokratiet", `Parti(er)`, ignore.case = TRUE), "Red", "Blue")]
analysis_data[, era := cut(decimal_date(start), breaks = c(1945, 1970, 1990, 2010, 2025), labels = c("1945-1970", "1970-1990", "1990-2010", "2010+"), include.lowest = TRUE)]

ministries <- unique(analysis_data$ministry_type)
results <- list()

for (min_type in ministries) {
  if (min_type == "Anden minister") next
  
  # create binary indicator for this ministry vs all others
  analysis_data[, is_target := fifelse(ministry_type == min_type, "Target", "Other")]
  
  # Require at least 5 events in the target group to be stable
  n_target <- nrow(analysis_data[is_target == "Target"])
  events_target <- sum(analysis_data[is_target == "Target"]$early)
  
  if (n_target < 10 || events_target < 5) next
  
  # Adjusted Cox Model
  tryCatch({
    mod <- coxph(Surv(time, early) ~ is_target + bloc_simple + era, data = analysis_data)
    res <- summary(mod)$coefficients
    # "is_targetTarget" is the term we want to look at
    idx <- which(rownames(res) == "is_targetTarget")
    if (length(idx) > 0) {
      hr <- exp(res[idx, "coef"])
      p <- res[idx, "Pr(>|z|)"]
      results[[min_type]] <- data.frame(Ministry = min_type, HR = hr, P_Value = p, N = n_target, Events = events_target)
    }
  }, error = function(e) {})
}

res_df <- do.call(rbind, results)
res_df <- res_df[order(res_df$P_Value), ]
print("--- ALL MINISTRIES vs OTHERS ---")
print(res_df, row.names = FALSE)
