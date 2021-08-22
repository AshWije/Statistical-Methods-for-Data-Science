# MonteCarloEstimateFunction.R
#
# Description:
#	Computes E(T) and P(T>15) for a given function using the Monte Carlo approach.


# Set the number of replications
n <- 10000

#   Draw xA and xB lifetime values and simulate the satellite's lifetime (the time for both xA and xB to fail)
t <- pmax((xA=rexp(n, 0.1)), (xB=rexp(n, 0.1)))

#   Create the histogram for satellite lifetime
hist(t, main="Histogram of T", xlab="T in years", prob=TRUE, ylim=c(0,0.05))

#   Add the probability density function to the histogram plot
f <- function(t) 0.2 * exp(-0.1*t) - 0.2 * exp(-0.2*t)
curve(f, add=TRUE)

#   Estimate E(T)
eT <- mean(t)

#   Estimate P(T > 15)
p <- sum(t > 15) / n