library(readxl)
data <- read_excel("forecasting.xlsx")
str(data)
head(data)
data$date <- as.Date(data$date)

library(dplyr)
install.packages("lubridate")
library(lubridate)
monthly_data <- data %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE)) %>%
  arrange(year, month)

ts_data <- ts(monthly_data$pm25,
              start = c(min(monthly_data$year), 1),
              frequency = 12)

train <- window(ts_data, end = c(2022, 12))
test  <- window(ts_data, start = c(2023, 1))

model_hw <- HoltWinters(train)
library(forecast)
forecast_hw <- forecast(model_hw, h = length(test))


plot(ts_data, col = "black", lwd = 1.5,
     main = "PM2.5: Actual vs Smoothed & Forecast",
     ylab = "PM2.5", xlab = "Time")

# Smoothed (train period)
lines(fitted(model_hw)[,1], col = "blue", lwd = 2)

# Forecast (automatically placed in future time!)
lines(forecast_hw$mean, col = "blue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Actual", "Smoothed", "Forecast"),
       col = c("black", "blue", "blue"),
       lwd = 2,
       lty = c(1,1,2))

errors <- test - forecast_hw$mean

MAE  <- mean(abs(errors))
RMSE <- sqrt(mean(errors^2))
MAPE <- mean(abs(errors / test)) * 100

MAE; RMSE; MAPE
