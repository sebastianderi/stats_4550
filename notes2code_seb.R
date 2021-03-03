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
