######################################
# Chicken data #
######################################

library(astsa)

# plot chicken price data
plot(x = time(chicken),
     y = chicken,
     type = "l",
     xlab = "time",
     ylab = "price (cents per pound)",
     main = "price of chicken over time")

# fit a linear model
fit <- lm(formula = chicken ~ time(chicken),
          na.action = NULL)

summary(fit)

# plot fitted line
abline(fit, col="blue")

# look at residuals
w <- chicken - fit$coefficients[1] - time(chicken)*fit$coefficients[2]

plot(x = time(chicken),
     y = w,
     type = "p",
     xlab = "time",
     ylab = "residuals")


######################################
# LA Mortality, Temp, Pollution Data #
######################################

# data
cmort
class(cmort) # ts (so, time series data is a type apparently)

# plot the data
par(mfrow = c(3,1))
plot(cmort, main = "Cardiovascular Mortality", xlab = "time", ylab = "# deaths")
plot(tempr, main = "Temperature", xlab = "time", ylab = "temp")
plot(part, main = "Pollution", xlab = "time", ylab = "pol.")

# create all predictor variables for the model
temp <- tempr - mean(tempr)
temp2 <- temp^2
trend <- time(cmort)

# make models
m1 <- lm(formula = cmort ~ trend, na.action = NULL)

# random note:
# 1. hot key for assignment operator (i.e. <-) is alt + -