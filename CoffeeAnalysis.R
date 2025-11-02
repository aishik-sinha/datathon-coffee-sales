dat <- read.csv("Coffe_sales.csv")

# Filter for Morning customers
morning_data <- subset(dat, Time_of_Day == "Morning")

# Calculate mean spending for Morning customers
mean_morning_spending <- mean(morning_data$money)
mean_morning_spending

night_data <- subset(dat, Time_of_Day == "Night")

# Calculate mean spending for Night customers
mean_night_spending <- mean(night_data$money)
mean_night_spending

diff_mean <- mean_night_spending - mean_morning_spending
diff_mean

# Calculate standard error (SE)
z_out <- ztest(x = morning_data$money, y = night_data$money, alternative = "greater")

# Z-score for (Night - Morning)
z_score <- z_out$z

# One-tailed p-value originally (assuming H1: Night > Morning)
# If your hypothesis is H1: Morning > Night, use pnorm(z_score) instead (see below).
p_value <- z_out$p.value

z_score
p_value

# Sample size (total observations)
n <- nrow(dat)



##### 1) PERMUTATION TEST#####
# Test statistic = mean(Night) - mean(Morning), consistent with your diff_mean
x <- morning_data$money
y <- night_data$money
obs_diff <- mean(y) - mean(x)  # Night - Morning

# (Replaced manual shuffle loop with course permutation_test helper)
perm_out_two <- permutation_test(y, x, alternative = "two.sided", B = 1000)

# Two-tailed permutation p-value (difference in either direction)
p_perm_two_tailed <- perm_out_two$p.value


perm_out_one_m_gt_n <- permutation_test(y, x, alternative = "less", B = 1000)
p_perm_one_tailed_morning_gt_night <- perm_out_one_m_gt_n$p.value

p_perm_two_tailed
p_perm_one_tailed_morning_gt_night




##### 2) CONFIDENCE INTERVAL FOR A MEAN (95%) #####
# Choose one numeric variable; I chose overall spending `money`
n_all   <- nrow(dat)
mean_all <- mean(dat$money)
# (Compute CI via course ztest helper to avoid disallowed sd/sqrt)
z_ci <- ztest(x = dat$money, conf.level = 0.95)

# 95% CI using z* = 1.96 (large n)
margin <- (z_ci$conf.int[2] - z_ci$conf.int[1]) / 2
ci_lower <- z_ci$conf.int[1]
ci_upper <- z_ci$conf.int[2]

n_all
mean_all
ci_lower
ci_upper

# (If you want CI for Morning only, uncomment below)
# n_m <- nrow(morning_data)
# m_m <- mean(morning_data$money)
# sd_m <- sd(morning_data$money)
# margin_m <- 1.96 * (sd_m / sqrt(n_m))
# c(lower_morning = m_m - margin_m, upper_morning = m_m + margin_m)



##### 3) CHI-SQUARE TEST OF INDEPENDENCE #####
# Example: Is coffee choice related to time of day?
tab <- table(dat$coffee_name, dat$Time_of_Day)  # contingency table
chi_out <- chisq.test(tab)                      # chi-square test

# Report: X-squared, df, p-value
chi_out$statistic   # Chi-square value
chi_out$parameter   # Degrees of freedom
chi_out$p.value     # p-value



# ===== 4) BAYESIAN REASONING — Odds Version (allowed commands only) =====
# Belief (A): customer orders a Latte        -> A ≡ (coffee_name == "Latte")
# Evidence (B): sale occurs in the Morning   -> B ≡ (Time_of_Day == "Morning")
# Prior P(A): choose a subjective prior (assignment asks for a belief) — change if you want
prior_A <- 0.25
prior_odds <- prior_A / (1 - prior_A)

dat <- read.csv("Coffe_sales.csv")

# --- Likelihoods from data using only subset(), mean(), nrow(), table() ---
A_rows    <- subset(dat, coffee_name == "Latte")
notA_rows <- subset(dat, coffee_name != "Latte")

# P(B|A)  = proportion of Morning among Latte orders
p_B_given_A <- mean(subset(A_rows, TRUE)$Time_of_Day == "Morning")

# P(B|¬A) = proportion of Morning among non-Latte orders
p_B_given_notA <- mean(subset(notA_rows, TRUE)$Time_of_Day == "Morning")

# Likelihood ratio and posterior (odds form)
likelihood_ratio <- p_B_given_A / p_B_given_notA
posterior_odds   <- prior_odds * likelihood_ratio
posterior_prob   <- posterior_odds / (1 + posterior_odds)

# (Optional counts to report, using only allowed commands)
counts_A_B     <- nrow(subset(A_rows, Time_of_Day == "Morning"))
counts_A_total <- nrow(A_rows)
counts_notA_B  <- nrow(subset(notA_rows, Time_of_Day == "Morning"))
counts_notA_total <- nrow(notA_rows)

# View results by typing the variable names:
prior_A
prior_odds
p_B_given_A
p_B_given_notA
likelihood_ratio
posterior_odds
posterior_prob
