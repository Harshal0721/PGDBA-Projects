

# Required libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(uroot)
library(urca)
library(FinTS)
library(fGarch)

# Load and prepare data
data <- Pharma_data
data <- data %>% mutate(Date = as.Date(Date))

# Split into training and test sets
split_index <- which(data$Date == as.Date("2025-04-01"))
data_train <- data[1:(split_index - 1), ]
data_test  <- data[split_index:nrow(data), ]



#ACF AND PACF PLOTS
ts_data1 <- ts(na.omit(data_train$PHARMA), frequency = 30)

acf_plot1 <- ggAcf(ts_data1, lag.max = 60) + 
  ggtitle("ACF - PHARMA") + theme_minimal()
pacf_plot1 <- ggPacf(ts_data1, lag.max = 60) + 
  ggtitle("PACF - PHARMA") + theme_minimal()

grid.arrange(acf_plot1, pacf_plot1, nrow = 2)





#plot
ggplot(data_train, aes(x = Date, y = PHARMA)) +
  geom_line(color = "steelblue") +
  labs(title = "PHARMA: Raw Time Series", x = "Date", y = "Price") +
  theme_minimal()

#log-differencing and plot
returns_data_train <- data_train %>%
  arrange(Date) %>%
  mutate(PHARMA_return = c(NA, diff(log(PHARMA)))) %>%
  slice(-1)  # Remove NA row

ggplot(returns_data_train, aes(x = Date, y = PHARMA_return)) +
  geom_line(color = "darkgreen") +
  labs(title = "PHARMA: Log Returns", x = "Date", y = "Log Return") +
  theme_minimal()

t_result <- t.test(returns_data_train$PHARMA_return, mu = 0)
data.frame(
  Mean = mean(returns_data_train$PHARMA_return, na.rm = TRUE),
  T_statistic = t_result$statistic,
  P_value = t_result$p.value
)



# ADF Test
adf_test <- ur.df(na.omit(returns_data_train$PHARMA_return), type = "drift", lags = 30)
summary(adf_test)

# Ljung-Box Test
Box.test(na.omit(returns_data_train$PHARMA_return), lag = 30, type = "Ljung-Box")

#ACF AND PACF PLOTS
ts_data <- ts(na.omit(returns_data_train$PHARMA_return), frequency = 30)

acf_plot <- ggAcf(ts_data, lag.max = 60) + 
  ggtitle("ACF - PHARMA after differencing") + theme_minimal()
pacf_plot <- ggPacf(ts_data, lag.max = 60) + 
  ggtitle("PACF - PHARMA after differencing") + theme_minimal()

grid.arrange(acf_plot, pacf_plot, nrow = 2)


#Auto-ARIMA
model_PHARMA <- auto.arima(
  ts_data,
  ic = "aic",
  allowmean = TRUE,
  allowdrift = TRUE,
  stepwise = TRUE,
  seasonal = TRUE,
  stationary = TRUE,
  test = 'adf'
)
summary(model_PHARMA)


#Auto-ARIMA
model_PHARMA1 <- auto.arima(
  ts_data,
  ic = "bic",
  allowmean = TRUE,
  allowdrift = TRUE,
  stepwise = TRUE,
  seasonal = TRUE,
  stationary = TRUE,
  test = 'adf'
)
summary(model_PHARMA1)


#ARIMA Residual Diagnostics
resid_ts <- residuals(model_PHARMA)

# Ljung-Box Test
Box.test(resid_ts, lag = 30, type = "Ljung-Box")

# ARCH Test
ArchTest(resid_ts, lags = 30)

garch_model_PHARMA <- garchFit(~ garch(1, 1), data = resid_ts, trace = FALSE)
summary(garch_model_PHARMA)

# Residual Diagnostics
garch_resid <- residuals(garch_model_PHARMA, standardize = TRUE)
Box.test(garch_resid, lag = 30, type = "Ljung-Box")
ArchTest(garch_resid, lags = 30)




# Extend test set by one day from training to compute returns
data_test_extended <- bind_rows(tail(data_train, 1), data_test)

# Compute test log returns
returns_data_test <- data_test_extended %>%
  arrange(Date) %>%
  mutate(PHARMA = log(PHARMA),
         PHARMA_return = c(NA, diff(PHARMA))) %>%
  slice(-1)

# Forecast ARIMA component
h <- nrow(data_test)
arima_fc <- forecast(model_PHARMA, h = h)
point_forecast <- as.numeric(arima_fc$mean)

# Forecast GARCH sigma
garch_fc <- predict(garch_model_PHARMA, n.ahead = h)
sigma_fc <- garch_fc$standardDeviation

# Compute confidence intervals
lower_80 <- point_forecast - qnorm(0.90) * sigma_fc
upper_80 <- point_forecast + qnorm(0.90) * sigma_fc
lower_95 <- point_forecast - qnorm(0.975) * sigma_fc
upper_95 <- point_forecast + qnorm(0.975) * sigma_fc

# Prepare plot data
plot_df <- data.frame(
  Date = data_test$Date,
  Forecast = point_forecast,
  Lower_80 = lower_80,
  Upper_80 = upper_80,
  Lower_95 = lower_95,
  Upper_95 = upper_95,
  Actual = returns_data_test$PHARMA_return
)

# Plot
ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Forecast), color = "blue") +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "skyblue", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightblue", alpha = 0.3) +
  geom_line(aes(y = Actual), color = "black", linetype = "dashed") +
  labs(title = "PHARMA: Forecast with Confidence Intervals", y = "Log Return", x = "Date") +
  theme_minimal()




# Remove any NA values for safe metric calculation
valid_idx <- which(!is.na(returns_data_test$PHARMA_return))

# RMSE
rmse_log <- sqrt(mean((point_forecast[valid_idx] - returns_data_test$PHARMA_return[valid_idx])^2))

# MAPE (multiply by 100 to express in %)
mape_log <- mean(abs((returns_data_test$PHARMA_return[valid_idx] - point_forecast[valid_idx]) /
                       returns_data_test$PHARMA_return[valid_idx])) * 100

cat("Log Returns RMSE:", round(rmse_log, 6), "\n")
cat("Log Returns MAPE:", round(mape_log, 4), "%\n")




# Get last actual PHARMA price from training data
last_price <- tail(data_train$PHARMA, 1)

# Reconstruct forecasted prices from log returns
forecast_prices <- numeric(length(point_forecast))
forecast_prices[1] <- last_price * exp(point_forecast[1])

for (i in 2:length(point_forecast)) {
  forecast_prices[i] <- forecast_prices[i-1] * exp(point_forecast[i])
}


# Actual test prices (original scale)
actual_prices <- data_test$PHARMA

# Ensure matching lengths (safety check)
actual_prices <- actual_prices[1:length(forecast_prices)]

# RMSE
rmse_price <- sqrt(mean((forecast_prices - actual_prices)^2))

# MAPE
mape_price <- mean(abs((forecast_prices - actual_prices) / actual_prices)) * 100

cat("Original Price RMSE:", round(rmse_price, 4), "\n")
cat("Original Price MAPE:", round(mape_price, 4), "%\n")


library(ggplot2)

# Prepare DataFrame for plotting
plot_df_price <- data.frame(
  Date = data_test$Date,
  Actual = actual_prices,
  Forecast = forecast_prices
)

# Plot actual vs predicted prices
ggplot(plot_df_price, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1, linetype = "dashed") +
  geom_line(aes(y = Forecast), color = "blue", size = 1) +
  labs(title = "Actual vs Forecasted Prices (PHARMA)",
       x = "Date", y = "Price",
       caption = "Dashed = Actual | Solid = Forecast") +
  theme_minimal()









