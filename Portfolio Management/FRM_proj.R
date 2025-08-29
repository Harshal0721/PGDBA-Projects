# Required libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(uroot)
library(urca)

# Load the data
data <- read_excel("Asset-2.xlsx")
data <- data %>% mutate(Date = as.Date(Date))

# Split into training and testing based on date
split_index <- which(data$Date == as.Date("2025-04-30"))
data_train <- data[1:(split_index - 1), ]
data_test  <- data[split_index:nrow(data), ]

# 1. Plot raw data (long format)
data_long_train <- data_train %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Value")

# Plot raw time series for train
ggplot(data_long_train, aes(x = Date, y = Value, color = Asset)) +
  geom_line() +
  facet_wrap(~ Asset, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Training Data: Time Series Plots of Assets", x = "Date", y = "Value") +
  theme(legend.position = "none")

# 2. Compute continuously compounded returns for training data
returns_data_train <- data_train %>%
  arrange(Date) %>%
  transmute(Date = Date, 
            across(-Date, ~ c(NA, diff(log(.x))), .names = "{.col}_return")) %>%
  slice(-1)

# 3. Convert to long format
returns_long_train <- returns_data_train %>%
  pivot_longer(cols = -Date, 
               names_to = "Asset", 
               values_to = "Return") %>%
  mutate(Asset = str_remove(Asset, "_return"))

# 4. Plot returns
ggplot(returns_long_train, aes(x = Date, y = Return, color = Asset)) +
  geom_line() +
  facet_wrap(~ Asset, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Training Data: Log Returns of Assets", x = "Date", y = "Log Return") +
  theme(legend.position = "none")

# 5. Summary stats and t-test
return_stats <- returns_long_train %>%
  group_by(Asset) %>%
  summarise(
    Mean = mean(Return, na.rm = TRUE),
    T_statistic = t.test(Return, mu = 0)$statistic,
    P_value = t.test(Return, mu = 0)$p.value
  )

print(return_stats)

# 6. ADF test
ts_list <- split(returns_long_train, returns_long_train$Asset)
run_adf_test <- function(df) {
  ts_data <- na.omit(df$Return)
  test <- ur.df(ts_data, type = "drift", lags = 30, selectlags = "AIC")
  summary(test)
}
for (asset_name in names(ts_list)) {
  cat("===== ADF Test for:", asset_name, "=====\n")
  print(run_adf_test(ts_list[[asset_name]]))
  cat("\n\n")
}

# 7. Ljung-Box test on unadjusted training returns
ts_list_train <- split(returns_long_train, returns_long_train$Asset)

run_lb_test <- function(df) {
  ts_data <- na.omit(df$Return)
  Box.test(ts_data, lag = 30, type = "Ljung-Box")
}

for (asset_name in names(ts_list_train)) {
  cat("===== Ljung-Box Test for:", asset_name, "=====\n")
  print(run_lb_test(ts_list_train[[asset_name]]))
  cat("\n\n")
}

# 8. ACF/PACF Plots on training data (unadjusted)
asset_cols <- names(returns_data_train)[names(returns_data_train) != "Date"]

plot_acf_pacf <- function(ts_data, asset_name) {
  acf_plot <- ggAcf(ts_data, lag.max = 60) +
    ggtitle(paste("ACF for", asset_name)) +
    theme_minimal()
  
  pacf_plot <- ggPacf(ts_data, lag.max = 60) +
    ggtitle(paste("PACF for", asset_name)) +
    theme_minimal()
  
  grid.arrange(acf_plot, pacf_plot, nrow = 2)
}

for (asset_name in asset_cols) {
  cat("=== Plotting ACF and PACF for:", asset_name, "===\n")
  ts_data <- na.omit(returns_data_train[[asset_name]])
  plot_acf_pacf(ts_data, asset_name)
  readline(prompt = "Press [Enter] to continue to next asset...")
}

# 10. Loop through each asset and fit auto.arima while storing in named objects
for (asset_name in asset_cols) {
  cat("===== Fitting ARIMA for:", asset_name, "=====\n")
  
  # Prepare time series data
  ts_data <- ts(na.omit(returns_data_train[[asset_name]]), frequency = 30)
  
  # Fit ARIMA model with specified controls
  model <- auto.arima(
    ts_data,
    ic = "aic",
    allowmean = TRUE,
    allowdrift = TRUE,
    stepwise=TRUE,
    seasonal = TRUE,
    stationary = TRUE,
    test='adf'
  )
  
  # Assign the model to a named object: model_<asset_name>
  assign(paste0("model_", asset_name), model)
  
  # Print model summary
  print(summary(model))
  
  readline(prompt = "Press [Enter] to continue to next asset...")
}

library(FinTS)  # for ArchTest

# Loop through each fitted model
for (asset_name in asset_cols) {
  model_obj <- get(paste0("model_", asset_name))  # get the model from environment
  residuals_ts <- residuals(model_obj)  # extract residuals
  
  cat("===== Residual Diagnostics for:", asset_name, "=====\n")
  
  # Ljung-Box Test
  lb_test <- Box.test(residuals_ts, lag = 30, type = "Ljung-Box")
  cat("Ljung-Box Test:\n")
  print(lb_test)
  
  # ARCH Test (heteroskedasticity)
  arch_test <- ArchTest(residuals_ts, lags = 30)
  cat("ARCH Test:\n")
  print(arch_test)
  
  cat("\n\n")
  readline(prompt = "Press [Enter] to continue to next asset...")
}

library(fGarch)  # for garchFit()

# Get all asset return columns (excluding Date)
all_assets <- names(returns_data_train)[names(returns_data_train) != "Date"]

# Loop through each asset and fit GARCH(1,1) to ARIMA residuals
for (asset_return in all_assets) {
  # Corresponding ARIMA model name
  model_var_name <- paste0("model_", asset_return)
  
  # Try to get the ARIMA model object
  if (exists(model_var_name)) {
    model_obj <- get(model_var_name)
    resid_ts <- residuals(model_obj)
    
    # Fit GARCH(1,1) model to the residuals
    garch_model <- tryCatch({
      garchFit(~ garch(1, 1), data = resid_ts, trace = FALSE)
    }, error = function(e) {
      warning(paste("GARCH model failed for", asset_return, ":", e$message))
      return(NULL)
    })
    
    # Store the GARCH model if it succeeded
    if (!is.null(garch_model)) {
      assign(paste0("model_", asset_return, "_resid"), garch_model)
      
      # Print summary
      cat("===== GARCH(1,1) Model for Residuals of", asset_return, "=====\n")
      print(summary(garch_model))
      cat("\n\n")
      readline(prompt = "Press [Enter] to continue to next asset...")
    }
  } else {
    warning(paste("ARIMA model not found for:", asset_return))
  }
}

library(FinTS)  # for ArchTest

# Get all return columns (excluding Date)
all_assets <- names(returns_data_train)[names(returns_data_train) != "Date"]

# Loop through each asset's GARCH residuals
for (asset_return in all_assets) {
  # Construct GARCH model name
  garch_model_name <- paste0("model_", asset_return, "_resid")
  
  # Check if GARCH model exists
  if (exists(garch_model_name)) {
    garch_model <- get(garch_model_name)
    
    # Get standardized residuals
    resid_ts <- residuals(garch_model, standardize = TRUE)
    
    # Ljung-Box Test
    lb_test <- Box.test(resid_ts, lag = 30, type = "Ljung-Box")
    
    # ARCH Test
    arch_test <- ArchTest(resid_ts, lags = 30)
    
    # Print diagnostics
    cat("===== Residual Diagnostics for GARCH Model:", asset_return, "=====\n")
    cat("---- Ljung-Box Test (lag = 30) ----\n")
    print(lb_test)
    cat("\n---- ARCH Test (lag = 30) ----\n")
    print(arch_test)
    cat("\n\n")
    readline(prompt = "Press [Enter] to continue to next asset...")
  } else {
    warning(paste("GARCH model not found for:", asset_return))
  }
}

# Append the last row of training data to the top of test data
data_test_extended <- bind_rows(tail(data_train, 1), data_test)

# Compute log returns from extended test data
returns_data_test <- data_test_extended %>%
  arrange(Date) %>%
  mutate(across(-Date, ~ log(.x))) %>%              # Apply log
  mutate(across(-Date, ~ c(NA, diff(.x)))) %>%      # Compute differences
  slice(-1) %>%                                      # Remove the first row (NA after diff)
  rename_with(~ paste0(.x, "_return"), -Date)        # Append _return to column names

library(forecast)
library(fGarch)
library(ggplot2)
library(gridExtra)

# Extract asset column names (excluding Date)
asset_cols <- names(data_test)[names(data_test) != "Date"]

# Forecast horizon
h <- nrow(data_test)

# Loop through each asset to forecast recursively
for (asset_name in asset_cols) {
  cat("===== Forecasting for:", asset_name, "=====\n")
  
  # Get fitted ARIMA model
  arima_model <- get(paste0("model_", asset_name, "_return"))
  
  # Get fitted GARCH model
  garch_model <- get(paste0("model_", asset_name, "_return_resid"))
  
  # Perform ARIMA forecast
  arima_fc <- forecast(arima_model, h = h)
  point_forecast <- as.numeric(arima_fc$mean)
  
  # Extract GARCH conditional sigma forecasts (volatility)
  garch_fc <- predict(garch_model, n.ahead = h)
  sigma_fc <- garch_fc$standardDeviation
  
  # Compute 80% and 95% confidence intervals
  lower_80 <- point_forecast - qnorm(0.90) * sigma_fc
  upper_80 <- point_forecast + qnorm(0.90) * sigma_fc
  lower_95 <- point_forecast - qnorm(0.975) * sigma_fc
  upper_95 <- point_forecast + qnorm(0.975) * sigma_fc
  
  # Prepare data for plotting
  plot_df <- data.frame(
    Date = data_test$Date,
    Forecast = point_forecast,
    Lower_80 = lower_80,
    Upper_80 = upper_80,
    Lower_95 = lower_95,
    Upper_95 = upper_95,
    Actual = returns_data_test[[paste0(asset_name, "_return")]]
  )
  
  # Plot forecasts
  p <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Forecast), color = "blue") +
    geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "skyblue", alpha = 0.5) +
    geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = Actual), color = "black", linetype = "dashed") +
    labs(title = paste("Forecast with Confidence Intervals:", asset_name),
         y = "Log Return", x = "Date") +
    theme_minimal()
  
  print(p)
  readline(prompt = "Press [Enter] to continue to next asset...")
}

# Calculate correlation matrix (excluding Date column)
cor_matrix <- cor(returns_data_train %>% select(-Date), use = "pairwise.complete.obs")

# Print correlation matrix
print(cor_matrix)


