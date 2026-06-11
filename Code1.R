# =========================
# 1. LOAD LIBRARIES
# =========================
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)

# =========================
# 2. LOAD DATA
# =========================
data <- read_excel("data 1.xlsx", sheet = "Sheet 1")

# =========================
# 3. CLEAN DATA
# =========================
data <- na.omit(data)

# =========================
# 4. CREATE TREND VARIABLE
# =========================
data <- data %>%
  mutate(trend = 1:n())

# =========================
# 5. LOG TRANSFORMATION
# =========================
data <- data %>%
  mutate(
    log_pm25 = log(pm25),
    log_pm25_lag = log(pm25_lag)
  )

# =========================
# 6. ESTIMATE MODELS
# =========================

# Model 1: baseline
model1 <- lm(pm25 ~ pm25_lag + windsp + hum + pressure + precp + trend, data = data)

# Model 2: log model (preferred)
model2 <- lm(log_pm25 ~ log_pm25_lag + windsp + hum + pressure + precp + trend, data = data)

# Model 3: full model with seasonality
model3 <- lm(log_pm25 ~ log_pm25_lag + windsp + hum + pressure + precp + trend +
               spring + summer + fall, data = data)

# Model 4: with 2nd lag
data <- data %>%
  mutate(log_pm25_lag2 = lag(log_pm25, 2))

model4 <- lm(log_pm25 ~ log_pm25_lag + log_pm25_lag2 + windsp + hum + pressure + precp + trend + spring + summer + fall, data = data)
# =========================
# 7. RESULTS
# =========================
summary(model4)

# =========================
# 8. DIAGNOSTIC TESTS
# =========================

# ADF tests
library(tseries)
adf.test(data$log_pm25)
adf.test(data$log_pm25_lag)
adf.test(data$log_pm25_lag2)
adf.test(data$windsp)
adf.test(data$hum)
adf.test(data$pressure)
adf.test(data$precp)

# Heteroskedasticity
bptest(model4)

# Robust standard errors
coeftest(model4, vcov = vcovHC(model4, type = "HC1"))

# Autocorrelation
dwtest(model4)

# Multicollinearity
vif(model4)

# Normality
shapiro.test(residuals(model3))

# RESET test
library(lmtest)
resettest(model4, power = 2:3, type = "fitted")

# =========================
# 9. MODEL COMPARISON
# =========================
AIC(model1, model2, model3)
BIC(model1, model2, model3)

# =========================
# 10. OUTPUT TABLE
# =========================
stargazer(model1, model2, model3, type = "text")