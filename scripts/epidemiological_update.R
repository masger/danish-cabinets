# epidemiological_update.R
library(data.table)
library(survival)
library(lubridate)
library(ggplot2)
library(knitr)

# Helper for parsing
parse_dt <- function(d_str) {
    if (is.null(d_str)) return(as.POSIXct(NA))
    d_str <- trimws(d_str)
    d_str[d_str %in% c("Incumbent", "Present", "")] <- NA
    parse_date_time(d_str, orders = c("d B Y", "Y-m-d", "d m Y", "B d, Y", "Y"))
}

target_countries <- c("Denmark", "France", "Germany", "Greece", "Italy", "Netherlands", "Norway", "Spain", "Sweden", "United Kingdom")
csv_files <- list.files("data/international", pattern = "_enriched\\.csv$", full.names = TRUE)
intl <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)
intl[, start_dt := parse_dt(start_date)]
intl[, end_dt := parse_dt(end_date)]
intl[, dob_dt := parse_dt(dob)]
intl[, dod_dt := parse_dt(dod)]
intl[, country_name := country]
intl[country == "UK", country_name := "United Kingdom"]
intl <- intl[!is.na(start_dt)]

# Gov dates
whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")
gov_dates <- whogov[country_name %in% target_countries & !is.na(govern_start_date), 
                    .(country_name, govern_name, govern_start_date, govern_end_date)]
gov_dates[, govern_start_date := parse_dt(govern_start_date)]
gov_dates[, govern_end_date := parse_dt(govern_end_date)]
gov_dates <- unique(gov_dates, by = c("country_name", "govern_name", "govern_start_date"))

intl[, gov_end_dt := as.POSIXct(NA)]
for(c in target_countries) {
    c_govs <- gov_dates[country_name == c]
    if(nrow(c_govs) == 0) next
    for(i in 1:nrow(c_govs)) {
        intl[country_name == c & start_dt >= c_govs$govern_start_date[i] & start_dt <= c_govs$govern_end_date[i], 
                 gov_end_dt := c_govs$govern_end_date[i]]
    }
}

intl[, fired := 0]
intl[!is.na(gov_end_dt) & !is.na(end_dt), fired := ifelse(as.numeric(difftime(gov_end_dt, end_dt, units="days")) > 30, 1, 0)]
intl[, career_time := as.numeric(difftime(end_dt, start_dt, units = "days")) / 365.25]
intl[!is.na(career_time) & career_time <= 0, career_time := 0.01]

# Portfolio Categorization
categorize_min <- function(x) {
    x <- tolower(x)
    if(grepl("transport|trafik|infrastructure|verkeer|roads", x)) return("Transport")
    if(grepl("finance|finans|budget|trésor|economie", x)) return("Finance")
    if(grepl("defense|forsvar|war|krig|armée", x)) return("Defense")
    if(grepl("health|sundhed|santé|salute", x)) return("Health")
    if(grepl("interior|justice|home|justits|intérieur", x)) return("Interior/Justice")
    return("Other")
}
intl[, portfolio_group := sapply(portfolio, categorize_min)]

# --- 1. Epidemiological Table 5 ---
table5_epi <- intl[!is.na(career_time), .(
    Person_Years = sum(career_time, na.rm=T),
    Unique_Persons = length(unique(name)),
    N_Appts = .N,
    N_Fired = sum(fired, na.rm=T)
), by = country_name]
table5_epi[, IR_per_100py := round(100 * N_Fired / Person_Years, 2)]
table5_epi[, Pers_Years := round(Person_Years, 1)]
setorder(table5_epi, -IR_per_100py)

# --- 2. Multi-Ministry Forest Plot (Fig 4) ---
# Filter to major portfolios
plot_dat_all <- intl[portfolio_group != "Other" & !is.na(career_time)]
fit_all <- coxph(Surv(career_time, fired) ~ portfolio_group, data = intl)
sum_fit <- summary(fit_all)

forest_plot_dat <- data.table(
    Portfolio = c("Defense", "Finance", "Interior/Justice", "Transport"),
    HR = exp(sum_fit$coefficients[, 1]),
    L = exp(sum_fit$coefficients[, 1] - 1.96 * sum_fit$coefficients[, 3]),
    H = exp(sum_fit$coefficients[, 1] + 1.96 * sum_fit$coefficients[, 3]),
    P = sum_fit$coefficients[, 5]
)
# Add Health as reference (1.0)
forest_plot_dat <- rbind(data.table(Portfolio="Health (Ref)", HR=1, L=1, H=1, P=NA), forest_plot_dat)

p4_new <- ggplot(forest_plot_dat, aes(x = HR, y = Portfolio)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = L, xmax = H), size = 1, color = "#DC0000FF") +
    scale_x_log10(breaks = c(0.5, 1, 1.5, 2, 3), limits=c(0.4, 4)) +
    labs(title = "Hazard Ratio for Political Termination by Portfolio",
         subtitle = "Pooled analysis of 10 European nations (Reference: Health)",
         x = "Hazard Ratio (95% CI, Log Scale)",
         y = "") +
    theme_minimal(base_size = 14)

ggsave("reports/figures/fig4_all_ministries_forest.png", p4_new, width = 10, height = 6, dpi = 300)

# --- Output Report ---
cat("# Epidemiological Results: Ministerial Hazard\n\n", file="reports/epidemiological_report.md")
cat("## Table 5. Epidemiological characteristics of political mortality\n", file="reports/epidemiological_report.md", append=T)
cat(kable(table5_epi[, .(Country=country_name, `Person-Years`=Pers_Years, `Unique Persons`=Unique_Persons, `Fired (N)`=N_Fired, `Incidence Rate (per 100 PY)`=IR_per_100py)], format="markdown"), file="reports/epidemiological_report.md", append=T)
cat("\n\n![Figure 4. Forest plot of hazards for early career termination across major portfolios. Reference: Health portfolio.](file:///c:/Users/mand0579/OneDrive%20-%20Region%20Hovedstaden%20(1)/Projekter/danish-cabinets/reports/figures/fig4_all_ministries_forest.png)\n", file="reports/epidemiological_report.md", append=T)
