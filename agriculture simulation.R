

# Libraries
library(robustbase)
library(ggplot2)
library(RColorBrewer)

set.seed(123)

# Simulate agriculture-like data
n <- 100
x <- runif(n, 0, 100)  # e.g., fertilizer in kg/ha
epsilon <- rnorm(n, mean = 0, sd = 10)
y <- 30 + 0.8 * x + epsilon

# Inject 10% extreme outliers in y (simulate wrong yield entries)
outlier_indices <- sample(1:n, 10)
y[outlier_indices] <- y[outlier_indices] + rnorm(10, mean = 100, sd = 30)

# Create data frame
df <- data.frame(x = x, y = y)

# Summary stats
x_bar <- mean(x)
y_bar <- mean(y)
Sx <- sd(x)
Sy <- sd(y)
Cx <- Sx / x_bar
Cy <- Sy / y_bar
rho <- cor(x, y)

# Regressions
fit_ols <- lm(y ~ x)
fit_huber <- lmrob(y ~ x)

beta_ols <- coef(fit_ols)[2]
beta_huber <- coef(fit_huber)[2]

# MSEs using your formula

# Estimator 1 - OLS
mse_t1 <- (y_bar^2 * Cy^2) / n +
  (beta_ols^2 * exp(2 * x_bar) * Cx^2) / n -
  (2 * y_bar * beta_ols * exp(x_bar) * rho * Cy * Cx) / n

# Estimator 2 - OLS
mse_t2 <- (y_bar^2 * Cy^2) / n +
  (y_bar^2 * beta_ols^2 * Cx^2) / n -
  (2 * y_bar^2 * beta_ols * rho * Cy * Cx) / n

# Estimator 1 - Huber
mse_t1_robust <- (y_bar^2 * Cy^2) / n +
  (beta_huber^2 * exp(2 * x_bar) * Cx^2) / n -
  (2 * y_bar * beta_huber * exp(x_bar) * rho * Cy * Cx) / n

# Estimator 2 - Huber
mse_t2_robust <- (y_bar^2 * Cy^2) / n +
  (y_bar^2 * beta_huber^2 * Cx^2) / n -
  (2 * y_bar^2 * beta_huber * rho * Cy * Cx) / n

# Output
cat("Simulated Agriculture Data with Strong Outliers:\n")
cat(sprintf("Estimator 1 OLS    MSE: %.4f\n", mse_t1))
cat(sprintf("Estimator 1 Huber  MSE: %.4f\n", mse_t1_robust))
cat(sprintf("Estimator 2 OLS    MSE: %.4f\n", mse_t2))
cat(sprintf("Estimator 2 Huber  MSE: %.4f\n", mse_t2_robust))

# MSE barplot
mse_df <- data.frame(
  Estimator = c("Est 1 OLS", "Est 2 OLS", "Est 1 Huber", "Est 2 Huber"),
  MSE = c(mse_t1^2, mse_t2^2, mse_t1_robust, mse_t2_robust)
)

ggplot(mse_df, aes(x = Estimator, y = MSE, fill = Estimator)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(title = "Simulated Agriculture Data: MSE Comparison",
       y = "MSE (log10 scale)", x = "") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Outlier detection plot
df$outlier <- ifelse(abs(resid(fit_ols)) > 1.5 * IQR(resid(fit_ols)), "Outlier", "Normal")

ggplot(df, aes(x = x, y = y, color = outlier)) +
  geom_point(size = 3) +
  labs(title = "Simulated Agriculture Data with Injected Outliers",
       x = "Fertilizer (kg/ha)", y = "Yield (q/ha)") +
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  theme_minimal()

