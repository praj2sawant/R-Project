# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(forecast)
library(tidyverse)
library(tseries)

#load the dataset
data = read.csv("Carsales.csv")

# Convert 'Month' column to Date format
str(data)
data$month = strptime(data$month, format = "%d/%m/%Y")
data$month = as.Date(data$month)
str(data)

# Overview of the data
head(data)

# Summary statistics
summary(data)

#Descriptive Statistics
describer::describe(data)

#Change variable name
colnames(data)[2] = "y"
colnames(data)[3] = "rr"
colnames(data)[4] = "ur"
colnames(data)[5] = "ir"
colnames(data)[6] = "er"
colnames(data)[7] = "pp"
colnames(data)[8] = "sp"

#Create bubble plot
ggplot(data , aes(x = month, y = y)) +
  geom_line() +
  xlab("Time") +
  ylab("Car Sales") +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_labels = "%Y %b" )

#Transform Time-series object
y = ts(data = data$y,
       start = c(2019, 1),
       frequency = 12)
plot.ts(y)

# Automatic detection of outliers for y
myts = tsoutliers(y)
myts

#Checking whether y is Stationary
result = adf.test(y)
result

#Using First Order Differencing on y
y = diff(y)

#Rechecking Stationarity on y
s_differenced_y = adf.test(y)
s_differenced_y                                        


# Check the p-value to determine if the data is now stationary
if (s_differenced_y$p.value <= 0.05) {
  print("y is stationary after first order differencing.")
} else {
  print("y is still not stationary after first order differencing.")
}

#Seasonality plot
ggseasonplot(x = y,
             main = "Seasonality graph")

#Multiplicative Decomposition
decomposition_multiplicative = decompose(x = y,
                                   type = "multiplicative")
plot(decomposition_multiplicative)

#Training and Test Set
training = data %>% filter(month < '2021-06-01')
test = data %>% filter(month >= '2021-06-01')

#Time series object
#if daily, then 7 or 365, 12 if monthly, 4 if quarterly
training_y = ts(data = training$y,
                frequency = 12)

#Auto-correlation plot
acf(training_y)

#Stationarity
#ndiffs stands for number of differencing
ndiffs(x = training_y,
       test = "adf")
#output is 1 which means your data is not stationary

#Get the regressors
training_reg = as.matrix(training[,3:8])
test_reg = as.matrix(test[,3:8])

#SARIMAX model
model = auto.arima(y = training_y,
                   stepwise = FALSE,
                   approximation = FALSE,
                   xreg = training_reg)
summary(model)

#Forecasting
predictions_sarimax = forecast(model, xreg = test_reg)
predictions_sarimax

#plotting
autoplot(predictions_sarimax)

#accuracy
accuracy(predictions_sarimax$mean, test$y)

# Correlation Heatmap
correlation_matrix = cor(data[, -1]) # Excluding 'Month' column for correlation computation
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = 'black')

# Multiple Linear Regression Model
model = lm(y ~ rr+ur+ir+er+pp+sp, data = data)
summary(model)



