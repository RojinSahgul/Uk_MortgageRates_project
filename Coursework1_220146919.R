#' ---
#' title: Time Series Week NNN
#' author: You
#' date: Today
#' ---

# 1. R Scratchpad ------------------------------------------------------------
install.packages("prophet")
remotes::install_github('facebook/prophet@*release', subdir='R')
1

UK_Mortgage_Interest_Rates <- read_csv("data/UK Mortgage Interest Rates.csv")
head(UK_Mortgage_Interest_Rates)
mortgage_data <- UK_Mortgage_Interest_Rates[, c("Date", "Bank_Rate")]
head(mortgage_data)
mortgage_data$Date <- as.Date(mortgage_data$Date, format = "%d/%m/%Y")
head(mortgage_data)
library(dplyr)
mortgage_data <- mortgage_data %>%rename(ds = Date, y = Bank_Rate)
head(mortgage_data)
str(mortgage_data)
library(prophet)
str(mortgage_data)
InterestRateModel<-prophet(mortgage_data)
FutureRates=make_future_dataframe(InterestRateModel, periods=24, freq='month')
PredictedRates=predict(InterestRateModel,FutureRates)
plot(InterestRateModel, PredictedRates)


prophet_plot_components(InterestRateModel, PredictedRates)


install.packages("lmtest")
library(lmtest)
model_lm <- lm(y ~ ds, data = mortgage_data)
bp_test <- bptest(model_lm)
print(bp_test)

