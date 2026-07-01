# generate_plots.R
library(data.table)
library(survival)
library(ggplot2)
library(lubridate)
library(scales)

# Helper for parsing
parse_dt <- function(d_str) {
    if (is.null(d_str)) return(as.POSIXct(NA))
    d_str <- trimws(d_str)
    d_str[d_str %in% c("Incumbent", "Present", "")] <- NA
    parse_date_time(d_str, orders = c("d B Y", "Y-m-d", "d m Y", "B d, Y", "Y"))
}

target_countries <- c("Denmark", "France", "Germany", "Greece", "Italy", "Netherlands", "Norway", "Spain", "Sweden", "United Kingdom")
csv_files <- list.files("data/international", pattern = "_enriched\\.csv$", full.names = TRUE)
intl_min <- rbindlist(lapply(csv_files, fread, colClasses = "character"), fill = TRUE)
intl_min[, start_dt := parse_dt(start_date)]
intl_min[, end_dt := parse_dt(end_date)]
intl_min[, country_name := country]
intl_min[country == "UK", country_name := "United Kingdom"]
intl_min <- intl_min[!is.na(start_dt)]

# Match gov dates
whogov <- fread("data/WhoGov_crosssectional_V3.1.csv")
gov_dates <- whogov[country_name %in% target_countries & !is.na(govern_start_date), 
                    .(country_name, govern_name, govern_start_date, govern_end_date)]
gov_dates[, govern_start_date := parse_dt(govern_start_date)]
gov_dates[, govern_end_date := parse_dt(govern_end_date)]
gov_dates <- unique(gov_dates, by = c("country_name", "govern_name", "govern_start_date"))

intl_min[, gov_end_dt := as.POSIXct(NA)]
for(c in target_countries) {
    c_govs <- gov_dates[country_name == c]
    if(nrow(c_govs) == 0) next
    for(i in 1:nrow(c_govs)) {
        intl_min[country_name == c & start_dt >= c_govs$govern_start_date[i] & start_dt <= c_govs$govern_end_date[i], 
                 gov_end_dt := c_govs$govern_end_date[i]]
    }
}

intl_min[, fired := 0]
intl_min[!is.na(gov_end_dt) & !is.na(end_dt), fired := ifelse(as.numeric(difftime(gov_end_dt, end_dt, units="days")) > 30, 1, 0)]
intl_min[, career_time := as.numeric(difftime(end_dt, start_dt, units = "days")) / 365.25]
intl_min[!is.na(career_time) & career_time <= 0, career_time := 0.01]

# --- Figure 4: The Global Transport Curse (Forest Plot) ---
plot_data <- data.table(Country = target_countries, HR = as.numeric(NA), L = as.numeric(NA), H = as.numeric(NA), P = as.numeric(NA))
for(c in target_countries) {
    c_dat <- intl_min[country_name == c & !is.na(career_time)]
    c_dat[, is_transport := grepl("transport|trafik|infrastructure|verkeer", tolower(portfolio))]
    if(sum(c_dat$is_transport) >= 3 && sum(c_dat$fired) > 5) {
        fit <- coxph(Surv(career_time, fired) ~ is_transport, data = c_dat)
        s <- summary(fit)
        plot_data[Country == c, HR := exp(coef(fit))]
        plot_data[Country == c, L := exp(coef(fit) - 1.96 * s$coefficients[1, 3])]
        plot_data[Country == c, H := exp(coef(fit) + 1.96 * s$coefficients[1, 3])]
        plot_data[Country == c, P := s$coefficients[1, 5]]
    }
}

plot_data <- plot_data[!is.na(HR)]
setorder(plot_data, HR)
plot_data$Country <- factor(plot_data$Country, levels = plot_data$Country)

p4 <- ggplot(plot_data, aes(x = HR, y = Country)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(xmin = L, xmax = H, color = HR > 1), size = 1) +
    scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10)) +
    scale_color_manual(values = c("#E64B35FF", "#00A087FF"), guide = "none") + # Opposite logic? No, let's just use Lancet
    labs(title = "The Global Transport Minister Curse",
         subtitle = "Hazard of early career termination (Transport vs Other portfolios)",
         x = "Hazard Ratio (Log Scale)",
         y = "") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())

ggsave("reports/figures/fig4_transport_curse_forest.png", p4, width = 10, height = 6, dpi = 300)

# --- Figure 5: Fatal Attraction (Lollipops) ---
# Fired vs Died in Office count comparison
intl_min[, dod_y := year(parse_dt(dod))]
intl_min[, end_y := year(end_dt)]
intl_min[, died_in_office := 0]
intl_min[(!is.na(dod_y) & !is.na(end_y) & dod_y == end_y), died_in_office := 1]

fatal_plot <- intl_min[, .(Fired = sum(fired), Died = sum(died_in_office)), by = country_name]
fatal_long <- melt(fatal_plot, id.vars = "country_name")

p5 <- ggplot(fatal_long, aes(x = country_name, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#3C5488FF", "#DC0000FF"), name = "Outcomes", labels = c("Politically Fired", "Biologically Dead")) +
    labs(title = "Fatal Attraction in European Cabinets",
         subtitle = "Incidence of political vs biological 'death' while in office",
         x = "",
         y = "Total Number of Events") +
    theme_minimal(base_size = 14) +
    coord_flip() +
    theme(legend.position = "bottom")

ggsave("reports/figures/fig5_fatal_attraction.png", p5, width = 10, height = 6, dpi = 300)
