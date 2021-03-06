###################################
# Author: Megan Gelsinger
# Date: 3/4/21
# Title: STSCI 4550 Lab Session 4 
###################################

# In this lab, we are going to fit a basic structural model (see Problem 2.1)
# which accounts for seasonality in the data. 
# Note: This is just one way of handling a seasonal trend in the time series data.
# We are going to learn other techniques as the semester progresses. 

### Seasonal Trend Examples ###
# A seasonal trend looks like a "copy-and-paste" cycle repeated throughout the time series.  The length of the cycle is constant throughout the time series. When inspecting
# a time series for a seasonal trend it is useful to think about the frequency of the 
# data and what a "logical" seasonal cycle to that data might be, in terms of the frequency of the data.
# Let's look at some examples (built-in to R).

# Ex. 1 ##
?fdeaths
fdeaths
is.ts(fdeaths)
var <- fdeaths
time(fdeaths)

attributes(fdeaths)
time(fdeaths)
cycle(fdeaths)

windows()
plot(fdeaths)

# What might be a logical seasonal period for this data set? 

## Ex. 2 ##
?nhtemp
nhtemp
is.ts(nhtemp)
time(nhtemp)
cycle(nhtemp)

plot(nhtemp)

# What might be a logical seasonal period for this data set? 

## Ex. 3 ##
?UKgas
UKgas
time(UKgas)
cycle(UKgas)

plot(UKgas,
     xaxt = "n")
axis(1,
     las = 2,
     at = seq(min(time(UKgas)), max(time(UKgas)), 1))

# What might be a logical seasonal period for this data set? 


### Fitting Structural Model for Seasonal Trend ###
# The actual time series we are going to attempt to model is the Johnson & Johnson 
# data set from the book (that we have also seen in class)
library(astsa)
?jj
jj
time(jj)
par(mfrow = c(2,1))
plot(jj,
     xaxt = "n",
     main = "earnings yearly")
axis(1,
     las = 2,
     at = seq(min(time(jj)), max(time(jj)), 1))
plot(jj,
     xaxt = "n",
     main = "earnings quarterly")
axis(1,
     las = 2,
     at = seq(min(time(jj)), max(time(jj)), 0.25))

# What might be a logical seasonal period for this data set? 

# Let's write the model on the board and discuss the components before we try and code it up.

# Now that we understand the model, let's try and code it up!
# First, we need to make the factor variable for the different quarters.
# Luckily, this is easy to do in R.
cycle(jj)

Q <- factor(cycle(jj))
Q


# Next, we need to extract our trend.
trend_jj <- time(jj) - 1970 # center (personally, would rather start at 1960, but whatevs)
time(jj)
trend_jj

# Next, we transform the time series data to remove the time-varying variance.
log_jj <- log(jj)
log_jj

par(mfrow = c(1,2))
plot(jj, main = "raw data")
plot(log_jj, main = "log transformed data")


# Finally, we can fit the model
m_jj <- lm(log_jj ~ 0 + trend_jj + Q) #zero forces model to exclude intercept
summary(m_jj)


# Let's look at the residuals of this model.  If we modelled the data well, we should
# have white noise residuals. 


# Bonus: What happens if fit with intercept?

