################
# Chicken data #
################

library(astsa) # Loads data set 

# Plot chicken price over time
plot(x=time(chicken), y=chicken, type='l',
     xlab='Time', ylab='Cents per pound',
     main='Price of chicken')

# Fit a linear model 
fit <- lm(chicken ~ time(chicken), na.action=NULL)
summary(fit) # Print out the summary of the linear model fit

# Plot the fitted line
abline(fit, col='blue')

# Look at residuals 
w <- chicken - fit$coefficients[1] - time(chicken)*fit$coefficients[2]
plot(x=time(chicken), y=w, type='p', 
     xlab='Time', ylab='w')


#################################
# Plot Student's t-distribution #
#################################
x <- seq(-5,5,.01)
plot(x, y=dt(x, df=1), type='l', 
     ylab='Density', ylim=c(0,.5))
for(df in 2:10){
  points(x, y=dt(x, df=df), type='l', col=df)
}

#########################################
# Pollution, Temperature, and Mortality #
#########################################
library(astsa) # Loads data set 

# Plot the data 
#dev.new()
par(mfrow=c(3,1)) 
plot(cmort, main='Cardiovascular Mortality', xlab='', ylab='')
plot(tempr, main='Temperature', xlab='', ylab='')
plot(part, main='Pollution', xlab='', ylab='')

# Fit the model 
temp <- tempr - mean(tempr)
temp2 <- temp^2
trend <- time(cmort)
fit1 <- lm(cmort ~ trend, na.action=NULL)
fit2 <- lm(cmort ~ trend + temp, na.action=NULL)
fit3 <- lm(cmort ~ trend + temp + temp2, na.action=NULL)
fit4 <- lm(cmort ~ trend + temp + temp2 + part, 
          na.action=NULL)
summary(fit3)

# Calculate the quantities in the Table 2.2 in S&S for fit3
n <- length(tempr)
q3 <- 3 # Number of variables included in fit3
k3 <- 4 # Number of regression coefficients (includes the intercept)

# SSE 
SSE3 <- sum(fit3$residuals^2)

# MSE 
MSE3 <- sum(fit3$residuals^2)/(n - (q3 + 1))

# AIC 
AIC3 <- log(SSE3/n) + (n+2*k3)/n

# Compare fit3 to fit4 with an F test using anova function: 
anova(fit3, fit4)
q <- 4 
r <- 3
Fnumerator <- (sum(fit3$residuals^2) - sum(fit4$residuals^2))/(q-r)
Fdenominator <- sum(fit4$residuals^2)/(n-q-1)

# Compare to F statistic we compute ourselves
F <- Fnumerator/Fdenominator 

# Choose a level alpha for our hypothesis test
alpha <- .05

# Find the p-value 
p <- 1-pf(F, df1=q-r, df2=n-q-1) # It's essentially zero. 

# If p < alpha, we reject the null hypothesis. 

# OR 

# Find F(alpha) , i.e. the 1 - alpha percentile of the relevant F dist. 

F_alpha <- qf(p=1-alpha,df1=q-r, df2=n-q-1)

# If F* > F_alpha, we reject the null hypothesis. 

#####################################
# Scatterplot matrices for PTM data #
#####################################

library(astsa) # Loads data set 

# Look at scatterplots of "cmort" vs. its lags
lag1.plot(cmort, 6) 

# Look at scatterplots of "cmort" vs. lags of "part"
lag2.plot(part, cmort, 6) 

# Construct variables again
temp <- tempr - mean(tempr)
temp2 <- temp^2
trend <- time(cmort)

# We're going to add the 4th lag of "part" as a variable
# The explanation of this code is on page 53 of S&S 
ptm <- 
  ts.intersect(cmort, trend, temp, temp2, part, part4=lag(part,-4), 
               dframe=TRUE)
fit5 <- lm(cmort ~ trend + temp + temp2 + part + part4, data=ptm, 
           na.action=NULL)

# Calculate AIC of fit5 
n <- length(tempr)
q5 <- 5 # Number of variables included in fit3
k5 <- 6 # Number of regression coefficients (includes the intercept)

# SSE 
SSE5 <- sum(fit5$residuals^2)

# AIC 
AIC5 <- log(SSE5/n) + (n+2*k5)/n

# The model with part4 added has an improved AIC 

# Other scatter plots #

# The two following commands do the same thing: 
pairs(ptm[,c("cmort","trend","temp","temp2", "part", "part4")])
pairs(cmort ~ trend + temp + temp2 + part + part4, data=ptm)

cor(ptm$cmort, ptm$part)
cor(ptm$cmort, ptm$part4)
cor(ptm$cmort, ptm$trend)
cor(ptm$cmort, ptm$temp)
cor(ptm$cmort, ptm$temp2)

##########################################
# Detrending / differencing chicken data #
##########################################

library(astsa) # Loads data set 

# Plot chicken price over time
plot(x=time(chicken), y=chicken, type='l',
     xlab='Time', ylab='Cents per pound',
     main='Price of chicken')

# Fit a linear model 
fit <- lm(chicken ~ time(chicken), na.action=NULL)
summary(fit) # Print out the summary of the linear model fit

# Plot the fitted line
abline(fit, col='blue')

# Look at detrended chicken price compared to original series
y <- chicken - fit$coefficients[1] - time(chicken)*fit$coefficients[2]

dev.new()
par(mfrow=c(2,1))
plot(x=time(chicken), y=chicken, type='l',
     xlab='Time', ylab='Cents per pound',
     main='Price of chicken')
plot(x=time(chicken), y=y, 
     xlab='Time', ylab='y', type='l', 
     main='Detrended chicken price')

# Look at acf of detrended chicken price
par(mfrow=c(1,1))
acf(y)

# Look at differenced vs detrended chicken price 
dev.new()
par(mfrow=c(2,1))
plot(x=time(chicken), y=diff(chicken), type='l',
     xlab='Time', ylab='Cents per pound',
     main='Differenced chicken price')
plot(x=time(chicken), y=y, 
     xlab='Time', ylab='y', type='l', 
     main='Detrended chicken price')

# Compare ACFS of differenced vs detrended chicken price 
par(mfrow=c(2,1))
acf(diff(chicken), main='ACF of differenced price')
acf(y, main='ACF of detrended price')


###########################################
# Eliminating trends through differencing #
###########################################

# Linear trend 
t <- seq(1, 20, 1)
mu <- 1.5 + 3*t
dev.new()
par(mfrow=c(2,1))
plot(mu, main='Linear trend')
plot(diff(mu), main='First difference')

# Quadratic trend 
t <- seq(1, 20, 1)
mu <- 1.5 - t + t^2
dev.new()
par(mfrow=c(3,1))
plot(mu, main='Quadratic trend')
plot(diff(mu), main='First difference')
plot(diff(diff(mu)), main='Second difference')

####################################
# Log transformation of J&J series #
####################################

library(astsa)
plot(jj, type='o', main='Quarterly Earnings per Share')
plot(log(jj), type='o', main='Log of J&J series ')
fit <- lm(log(jj) ~ time(jj), na.action=NULL)
y <- log(jj) - (fit$coefficients[1] +  fit$coefficients[2]*time(jj))
plot(y, type='o', main='Residuals of linear fit')


