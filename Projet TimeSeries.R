######## Airport traffic in CDG Airport #########

# Import data
# -----------

# ------------
# Read the data as a time series 
# ------------

str(paris_cdg)

paris_cdg$CDG <- as.numeric(gsub(",","",paris_cdg$CDG))
paris_cdg

class(paris_cdg)
trafic.ts <- ts(paris_cdg$CDG, start=c(2000,1), frequency=12)
trafic.ts
str(trafic.ts)
class(trafic.ts)
start(trafic.ts)
end(trafic.ts)
frequency(trafic.ts)
time(trafic.ts)

plot(trafic.ts)
abline(reg=lm(trafic.ts~time(trafic.ts)),col="red")

######## What we observe : #######################
# we observe several patterns of non stationarity:
# increasing variance
# trend
# regular oscillations (seasonal effects)
# oscillation regularity 
# increasing var => Seasonal effect 
# We observe several pattern of non stationary 
##################################################


# Decompose the serie into trend+seasonality+random compo
#########################################################
x <- decompose(trafic.ts)
plot(x)

# Performing month plot 
#######################
monthplot(trafic.ts)
cycle(trafic.ts)
boxplot(trafic.ts~cycle(trafic.ts))

#Log transformation
#######################

ltraf <- log(paris_cdg$CDG)

# x dimentionel series 
tr.traf <- cbind(paris_cdg$CDG, ltraf)
colnames(tr.traf) <- c("traffic", "log(traffic)")
tr.traf.ts <- ts(tr.traf, start=c(2000  ,1), frequency=12)
plot.ts(tr.traf.ts, main="")

############################################
### Method ACF auto correlation function ###
############################################

par(mfrow=c(2,1))
acf(ts(trafic.ts, frequency=1), main="Autocorrelation main series")
pacf(ts(trafic.ts, frequency=1), main="Partial autocorrelation main series")
par(mfrow=c(1,1))

# The ACF confirms non stationary
# No more increasing variance 
# Persistent of significant coef in the ACF
# The log is still non stationary

###########################################
## Second transform : 1st order difference ###
###########################################


dltraf <- diff(ltraf, 1)
# main autocorrelation 1st order dif of log  and parcial auto

plot(dltraf)
par(mfrow=c(2,1))
acf(ts(dltraf, frequency = 1))
pacf(ts(dltraf, frequency = 1))
par(mfrow=c(1,1))

# We observe seasonality on the graph
# the series is still no stationary 


###########################################
# 3rd transfo : difference of order 12 ######
###########################################
dltraf_12 <- diff(dltraf, 12)
# main auto and Partial acf of order 12
plot(dltraf_12)
par(mfrow=c(2,1))
acf(ts(dltraf_12, frequency = 1))
pacf(ts(dltraf_12, frequency = 1))
par(mfrow=c(1,1))

# plot no more patterns of non stationarity 
# on the ACF : one significant coeff and additional coeff 
#around lags 12,

# Option : fit AR(1), MA(1) or seasonal versions of these models 

##########################################################################
# Box-Jenkins methodology to build an ARMA process on the stationary data#
##########################################################################


library(tseries)
library(forecast)

# Convert the "Date" column to a date format
#############################################
paris_cdg$Date <- as.Date(paris_cdg$Date)

# Create a time series object
#############################
ts_data <- ts(paris_cdg$CDG, frequency = 12)

# Check for stationarity and difference the data if necessary
#############################################################

adf_test <- adf.test(ts_data)
if (adf_test$p.value > 0.05) {
  diff_data <- diff(ts_data)
} else {
  diff_data <- ts_data
}

# Identify the orders p and q
#############################

acf_data <- acf(diff_data)
pacf_data <- pacf(diff_data)
p <- which.max(pacf_data$acf[-1] < 0.05)
q <- which.max(acf_data$acf[-1] < 0.05)

# Fit the ARMA model
####################
arma_model <- arima(diff_data, order = c(p, 0, q))

# Check the significance of the coefficients
############################################
summary(arma_model)

# estimated value of ar1 is close to 1, the current value of the series is highly correlated with the previous value 
# the series is persistent 

# estimated values of the moving average terms (ma1 to ma5) are all negative
# the current value of the series is negatively influenced by past error terms

# the t-values of the coefficients are all greater than 1.96
# they are statistically significant at the 5% level

# The estimated intercept (mean value) is 4769100.2, relativley large compared to the other coefficients
# there is more uncertainty associated with the estimate of the mean.

# The log likelihood is -3076.34 and the AIC is 6170.69
# the current model is a good balance between goodness of fit and model complexity.

# The training set error measures indicate that the model has a relatively small MAE and RMSE but a large MPE and MAPE
# It suggests that the model may have a tendency to overestimate or underestimate the actual values

# The MASE is close to 1
# It suggests that the model has a similar out-of-sample performance as a naive model that simply uses the last observation as a forecast

# The ACF1 value is negative but relatively small
# It suggests that the model residuals have little autocorrelation

# Check the residuals for normality, non-autocorrelation, and homoscedasticity
################################################################################
checkresiduals(arma_model)



# Calculate the AIC and SBC
###########################
aic <- AIC(arma_model)
sbc <- BIC(arma_model)

# we observe that the values are close but the AIC is lower so the model is preferred

# Model validation with in-sample and out-of-sample analysis
############################################################
train_size <- round(length(diff_data) * 0.8)
train_data <- diff_data[1:train_size]
test_data <- diff_data[(train_size+1):length(diff_data)]
arma_model <- arima(train_data, order = c(p, 0, q))
arma_forecast <- forecast(arma_model, h = 3)
arma_insample <- fitted(arma_model)


# Plot the results
##################
plot(diff_data, type = "l", col = "blue", ylim = range(c(diff_data, arma_insample, arma_forecast$mean)))
lines(arma_insample, col = "red")
lines(arma_forecast$mean, col = "yellow")
lines(test_data, col = "black")

# Add legend and labels
#######################

legend("topright", legend = c("Actual Data", "ARMA In-Sample", "ARMA Forecast", "ARMA Out-of-Sample"), col = c("blue", "red", "yellow", "black"), lty = 1, cex = 0.8)
title(main = "ARMA Model Validation with In-Sample and Out-of-Sample Analysis", sub = "Forecast for the Next Three Periods")
x_axis_label <- paste("time (", time_unit, ")", sep = "")
y_axis_label <- "CDG"



     
     
     
     
     