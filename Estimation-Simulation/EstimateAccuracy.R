# EstimateAccuracy.R
#
# Description:
#	Used to determine the optimal n value such that the mean of
#		the estimate is most accurate.


lambda.all <- c(0.01, 0.1, 1, 10)
n.all <- c(5, 10, 30, 100)

# Write a function to simulate one resample and compute mu
mu.star <- function(n, xbar){
  xstar <- rexp(n, 1/xbar)
  mstar <- mean(xstar)
  return(mstar)
}

numCorrect <- array(dim=c(length(lambda.all), length(n.all), 2))
for (j in 1:length(lambda.all)) {
  for (k in 1:length(n.all)) {
    lambda <- lambda.all[j]
    n <- n.all[k]
    total <- c(0, 0)
    for (i in 1:5000) {
      # Original data
      x <- rexp(n, lambda)
      
      # INTERVAL 1
      xbar <- mean(x)
      s <- sd(x)
      ci1 <- xbar + c(-1,1) * qnorm(1-0.05/2) * (s/sqrt(n))

      # INTERVAL 2
      # Simulate bootstrap distribution
      nboot <- 1000
      mu.boot.dist <- replicate(nboot, mu.star(n, xbar))

      # Percentile bootstrap CI for population mean
      ci2 <- sort(mu.boot.dist)[c(nboot*0.025, nboot*0.975)]
      
      # Count total number of correct intervals
      if (1/lambda >= ci1[1] && 1/lambda <= ci1[2]) total[1] <- total[1] + 1
      if (1/lambda >= ci2[1] && 1/lambda <= ci2[2]) total[2] <- total[2] + 1
    }
    numCorrect[j, k,] <- total
  }
}
percentCorrect <- numCorrect/5000