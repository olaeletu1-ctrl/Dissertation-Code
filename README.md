# Dissertation-Code
# Dissertation Code: Poverty Rate Regression Analysis

## 📊 Full R Code

```r
# ----------------------------------
# Load required libraries
# ----------------------------------
library(readxl)
library(summarytools)
library(lmtest)
library(dplyr)
library(car)
library(ggplot2)

# ----------------------------------
# Load data
# ----------------------------------
ngapovrate <- read_xlsx("Research Outline Regression.xlsx", col_names = TRUE)

# Keep relevant columns
ngapovrate <- ngapovrate[, 1:4]

# ----------------------------------
# Initial regression model
# ----------------------------------
poverty_model <- lm(povertyrate ~ after2010 + inflationrate, data = ngapovrate)
summary(poverty_model)

# ----------------------------------
# Descriptive statistics
# ----------------------------------
ngapovsummary <- descr(ngapovrate)
view(ngapovsummary)

# ----------------------------------
# Diagnostic tests (initial model)
# ----------------------------------
resettest(poverty_model)
dwtest(poverty_model)
bptest(poverty_model)
pacf(residuals(poverty_model))

# ----------------------------------
# Create lag variable
# ----------------------------------
ngapovrate <- ngapovrate %>%
  mutate(povertyrate_lag1 = lag(povertyrate, 1))

ngapovrate <- na.omit(ngapovrate)

# ----------------------------------
# Regression with lag
# ----------------------------------
poverty_model <- lm(
  povertyrate ~ after2010 + inflationrate + povertyrate_lag1,
  data = ngapovrate
)
summary(poverty_model)

# ----------------------------------
# Diagnostic tests (updated model)
# ----------------------------------
resettest(poverty_model)
bptest(poverty_model)
dwtest(poverty_model)
pacf(residuals(poverty_model))

# Multicollinearity check
vif(poverty_model)  # 1–5 indicates moderate correlation

# ----------------------------------
# Plot: Actual vs Fitted
# ----------------------------------
ngapovrate$fitted_values <- fitted(poverty_model)

ggplot(ngapovrate, aes(x = povertyrate, y = fitted_values)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Actual vs. Fitted Values for Poverty Rate",
    x = "Actual Poverty Rate",
    y = "Fitted Poverty Rate"
  ) +
  theme_minimal()

# ----------------------------------
# Cochrane-Orcutt Transformation
# ----------------------------------
residuals_model <- resid(poverty_model)
rho <- cor(residuals_model[-1], residuals_model[-length(residuals_model)])

n <- nrow(ngapovrate)

transformed_y  <- ngapovrate$povertyrate[-1] - rho * ngapovrate$povertyrate[-n]
transformed_x1 <- ngapovrate$after2010[-1] - rho * ngapovrate$after2010[-n]
transformed_x2 <- ngapovrate$inflationrate[-1] - rho * ngapovrate$inflationrate[-n]
transformed_x3 <- ngapovrate$povertyrate_lag1[-1] - rho * ngapovrate$povertyrate_lag1[-n]

transformed_data <- data.frame(
  povertyrate = transformed_y,
  after2010 = transformed_x1,
  inflationrate = transformed_x2,
  povertyrate_lag1 = transformed_x3
)

# ----------------------------------
# Transformed regression model
# ----------------------------------
transformed_model <- lm(
  povertyrate ~ after2010 + inflationrate + povertyrate_lag1,
  data = transformed_data
)

summary(transformed_model)

# ----------------------------------
# Diagnostics (transformed model)
# ----------------------------------
resettest(transformed_model)
bptest(transformed_model)
dwtest(transformed_model)
pacf(residuals(transformed_model))
vif(transformed_model)

# ----------------------------------
# Plot: Transformed model
# ----------------------------------
transformed_data$fitted_values <- fitted(transformed_model)

ggplot(transformed_data, aes(x = povertyrate, y = fitted_values)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Actual vs. Fitted Values for Transformed Poverty Rate",
    x = "Actual Poverty Rate",
    y = "Fitted Poverty Rate"
  ) +
  theme_minimal()

# ----------------------------------
# Summary statistics
# ----------------------------------
ngapovsummary_with_lag <- descr(ngapovrate)
summary(ngapovsummary_with_lag)

transformed_summary <- descr(transformed_data)
summary(transformed_summary)
```
