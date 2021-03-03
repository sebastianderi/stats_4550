######################################
# Produce figure for Nile river data #
######################################

library(longmemo)
data(NileMin)
#pdf(file='~/Desktop/ATSA_code/notes1/nile.pdf', width=7, height=7)
plot(x=seq(622,1284), y=NileMin, type='l', lwd=1.5 , xlab='Year', ylab='Minimum')
#dev.off()
acf(NileMin)

#############################################
# Produce figure for J&J quarterly earnings #
#############################################

library(astsa)
#pdf(file='~/Desktop/ATSA_code/notes1/JJearnings.pdf', width=7, height=7)
plot(jj, type='o', ylab='Quarterly Earnings per Share')
#dev.off()

#####################################
# Produce figure for Dow Jones data #
#####################################

#library(TTR)
#djia <- getYahooData('^DJI', start=20060420, end=20160420, freq='daily')
library(xts)
djiar <- diff(log(djia$Close))[-1]
plot(djiar, main='Dow Jones returns', type='n')
lines(djiar)

##########################################
# Simulate and plot Gaussian white noise #
##########################################

T <- 500
w <- rnorm(T)
plot(w, type='l', xlab='t', ylab=expression(w[t]), 
     main='Gaussian white noise', ylim=c(-4,4), lwd=1.5)

acf(w)
###################################################
# Simulate and plot filtered Gaussian white noise #
###################################################

T <- 200
w <- rnorm(T)
x <- stats::filter(w, sides=2, filter=rep(1/3,3))
x
plot(x, type='l', xlab='t', ylab=expression(x[t]), 
     main='Filtered Gaussian white noise', ylim=c(-2.5,2.5), lwd=1.5)

# with ggplot
ggplot(data = NULL,
       aes(y = x,
           x = 1:length(x))) + geom_line() + theme_bw()

# Comparing theoretical ACF to sample ACF
par(mfrow=c(1,1)) # Lets us put two plots on the same figure
lagmax <- 10 # Maximum lag plotted
y <- rep(0, lagmax)
y[1] <- 1; y[2] <- 2/3; y[3] <-1/3 # Creating a vector y with ACF values
plot(1:lagmax, y, type='h', main='Theoretical ACF', xlab='Lag', ylab='ACF')
acf(x, na.action=na.omit, lag.max=lagmax, main='Sample ACF')

####################################
# Simulate and plot autoregression #
####################################

T <- 100
w <- rnorm(T)
phi <- -0.5
x <- stats::filter(w, filter=c(phi), method='recursive')
plot(x, type='l', xlab='t', ylab=expression(x[t]), 
     main='Autoregression', 
     ylim=c(-4*sqrt(1/(1-phi^2)),
            4*sqrt(1/(1-phi^2))), lwd=1.5)

acf(x, na.action=na.omit)
##############################################
# Simulate and plot a random walk with drift #
##############################################

T <- 500
delta <- -.1
w <- rnorm(T)
s <- cumsum(w)
x <- delta*seq(1,T,1) + s
plot(x, type='l', xlab='t', ylab=expression(x[t]), 
     main='Random walk with drift', 
     ylim=c(min(0,delta*T) - 4*sqrt(T), 
            max(0,delta*T) + 4*sqrt(T)), lwd=1.5)

acf(x)

#############################################
# Simulate and plot signal in noise example #
#############################################

T <- 500
A <- 2
omega <- 1/50
phi <- 0 
s <- A*cos(2*pi*omega*1:T + phi)
w <- 0.5*rnorm(T)
x <- s + w 
plot(x, type='l', xlab='t', ylab=expression(x[t]), 
     main='Signal in noise', 
     ylim=c(-4*sqrt(A + 1), 4*sqrt(A + 1)), lwd=1.5)





