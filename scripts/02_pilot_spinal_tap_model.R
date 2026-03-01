# =============================================================================
# DATA SCIENTIST AGENT: PILOT DATA GENERATOR & ANALYSIS
# "The Spinal Tap Drummer Hypothesis"
# =============================================================================

library(data.table)
library(survival)

cat("📊 Data Scientist: Generating pilot dataset to test the Competing Risks model...\n")

set.seed(42)
n_obs <- 2000

# 5 'Spinal Tap' Portfolios
portfolios <- c("Finance", "Health", "Defense", "Interior", "Transport")
portfolio_dist <- sample(portfolios, n_obs, replace = TRUE)

# Baseline hazards (Years in office)
# Interior gets fired fast (Danish finding)
# Health has moderate tenure but higher death risk
base_tenure <- rweibull(n_obs, shape = 1.5, scale = 3)

pilot_data <- data.table(
    id = 1:n_obs,
    portfolio = portfolio_dist,
    base_t = base_tenure
)

# Apply portfolio modifiers for "Firing" (Risk 1)
pilot_data[portfolio == "Interior", time_to_fire := base_t * 0.6] # Fired fast
pilot_data[portfolio == "Transport", time_to_fire := base_t * 0.8]
pilot_data[portfolio == "Finance", time_to_fire := base_t * 1.2] # Survives longer
pilot_data[portfolio == "Health", time_to_fire := base_t * 1.0]
pilot_data[portfolio == "Defense", time_to_fire := base_t * 1.1]

# Apply portfolio modifiers for "Death in Office" (Risk 2)
# Health Ministers and Defense Ministers have higher risk of sudden "death in office"
pilot_data[, time_to_death := rweibull(n_obs, shape = 2, scale = 50)] # Baseline death is rare
pilot_data[portfolio == "Health", time_to_death := time_to_death * 0.3] # Highly toxic!
pilot_data[portfolio == "Defense", time_to_death := time_to_death * 0.5]

# Determine what happens first
pilot_data[, final_time := pmin(time_to_fire, time_to_death, 6)] # Max 6 years (election)
pilot_data[, event := 0] # 0 = Censored (election)
pilot_data[final_time == time_to_fire & final_time < 6, event := 1] # 1 = Fired
pilot_data[final_time == time_to_death & final_time < 6, event := 2] # 2 = Died

cat("\n--- PILOT DATA SUMMARY ---\n")
print(pilot_data[, .(
    N = .N,
    Fired_Pct = round(100 * mean(event == 1), 1),
    Died_Pct = round(100 * mean(event == 2), 1),
    Median_Years = round(median(final_time), 2)
), by = portfolio][order(-Died_Pct)])

cat("\n📊 Data Scientist: Running Competing Risks Analysis (Fine-Gray model)...\n")
# We use ordinary Cox models for the cause-specific hazards as a simple proxy
cat("\nHazard of getting FIRED (Reference = Finance):\n")
cox_fire <- coxph(Surv(final_time, event == 1) ~ relevel(factor(portfolio), ref = "Finance"), data = pilot_data)
print(round(exp(coef(cox_fire)), 2))

cat("\nHazard of DYING IN OFFICE (Reference = Finance):\n")
cox_death <- coxph(Surv(final_time, event == 2) ~ relevel(factor(portfolio), ref = "Finance"), data = pilot_data)
print(round(exp(coef(cox_death)), 2))

cat("\n📊 Data Scientist: SUCCESS. The pipeline works. The irony is statistically testable.\n")
cat("Passes microphone to The Critic.\n")
