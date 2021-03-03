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
m2 <- lm(formula = cmort ~ trend + temp, na.action = NULL)
m3 <- lm(formula = cmort ~ trend + temp + temp2, na.action = NULL)
m4 <- lm(formula = cmort ~ trend + temp + temp2 + part, na.action = NULL)

# my models
m_seb1  <- lm(formula = cmort ~ 1, na.action = NULL)
m_seb2  <- lm(formula = cmort ~ trend, na.action = NULL)
m_seb3  <- lm(formula = cmort ~ tempr, na.action = NULL)
m_seb4  <- lm(formula = cmort ~ part, na.action = NULL)
m_seb5  <- lm(formula = cmort ~ tempr + part, na.action = NULL)
m_seb3a <- lm(formula = cmort ~ trend + tempr, na.action = NULL)
m_seb4a <- lm(formula = cmort ~ trend + part, na.action = NULL)
m_seb6  <- lm(formula = cmort ~ trend + tempr + part, na.action = NULL)

# model summary
summary(m3)
summary(m4)
summary(m_seb6)

# compare models
anova(m1, m4)

# comparisons: my models
# NOTE: i think this is all kosher; bc all comparisons are between "nested"/subsetted models
anova(m_seb1, m_seb2)
anova(m_seb1, m_seb3)
anova(m_seb1, m_seb4)

anova(m_seb6, m_seb3)
anova(m_seb6, m_seb4)

anova(m_seb6, m_seb3a)
anova(m_seb6, m_seb4a)

# ----------------- (seb curiosity) -----------------
# SEB, compare models
# 1. twitter post: https://twitter.com/patilindrajeets/status/1366681760944304128

# packages
library(performance)
library(parameters)

# compare models (mine)
# NOTE: not all of these are nested, relative to each other
performance::compare_performance(m_seb1, m_seb2, m_seb3, m_seb3a, m_seb4, m_seb4a, m_seb5, m_seb6)

# compare models (michael's)
performance::compare_performance(m1, m2, m3, m4)
# ----------------- (seb curiosity) -----------------


# random note:
# 1. hot key for assignment operator (i.e. <-) is alt + -