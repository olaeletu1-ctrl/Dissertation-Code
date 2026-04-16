# Project Overview
This project analyzes the relationship between poverty rates and key economic indicators using linear regression models in R. The goal is to evaluate how factors such as inflation and policy changes (post-2010) influence poverty levels, and to assess model performance through diagnostic testing and transformation techniques.

# Methodology

The methodology involves the following steps:
1. Data Collection
2. Data Processing
3. Data Analysis
4. Model Evaluation
5. Results Interpretation

# Data Description
- The dataset includes the following key variables:
- povertyrate: Percentage of the population living in poverty
- after2010: Dummy variable indicating post-2010 period
- inflationrate: Inflation rate (%)
- povertyrate_lag1: Lagged poverty rate (previous period)

# Requirements

The following packages are required to run the code:

readxl
summarytools
lmtest
dplyr
car
ggplot2

# Usage Instructions

To use this code, follow these steps:
1. Clone the repository
git clone <repository_url>
2. Open R or RStudio and install required packages
install.packages(c("readxl", "summarytools", "lmtest", "dplyr", "car", "ggplot2"))
3. Ensure the dataset file (Research Outline Regression.xlsx) is in your working directory
4. Run the script
source("analysis.R")

# Full R Code

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

# Conclusion
This project provides a regression-based analysis of poverty dynamics. The results demonstrate how economic variables and past poverty levels influence current poverty rates. Diagnostic testing and model refinement improve reliability, while transformation techniques address statistical issues such as autocorrelation, resulting in a more robust model.
