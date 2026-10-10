
# Load libraries
library(carData)
library(robustbase)
library(ggplot2)
library(RColorBrewer)

# Load education-related dataset
data("Prestige")
df <- na.omit(Prestige[, c("education", "income")])
names(df) <- c("x", "y")  # x = education, y = income
data("Prestige")
?Prestige
# Summary statistics
n <- nrow(df)
x <- df$x
y <- df$y
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


# --- Outlier Detection and Plot ---
# Mark outliers using IQR method on residuals from OLS
residuals_ols <- resid(fit_ols)
threshold <- 1.5 * IQR(residuals_ols)
outlier_labels <- ifelse(abs(residuals_ols) > threshold, "Outlier", "Normal")

df$outlier <- outlier_labels

ggplot(df, aes(x = x, y = y, color = outlier)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot: Education vs Income with Outliers (based on OLS residuals)",
       x = "Education", y = "Income") +
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  theme_minimal()

# --- Theoretical MSEs based on given formulas ---

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

# --- Print results ---
cat("THEORETICAL MSEs based on given formulas:\n")
cat(sprintf("MSE(t1)         [OLS]:   %.4f\n", mse_t1))
cat(sprintf("MSE(t2)         [OLS]:   %.4f\n", mse_t2))
cat(sprintf("MSE(t1_robust)  [Huber]: %.4f\n", mse_t1_robust))
cat(sprintf("MSE(t2_robust)  [Huber]: %.4f\n", mse_t2_robust))

# --- Bar plot of MSEs ---
mse_df <- data.frame(
  Estimator = c("Estimator 1 - OLS", "Estimator 2 - OLS",
                "Estimator 1 - Huber", "Estimator 2 - Huber"),
  MSE = c(mse_t1, mse_t2, mse_t1_robust, mse_t2_robust)
)

ggplot(mse_df, aes(x = Estimator, y = MSE, fill = Estimator)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(title = "Theoretical MSE Comparison: OLS vs Huber (Education Data)",
       y = "Mean Squared Error (log10 scale)", x = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

