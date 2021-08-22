# EstimateComparison.R
#
# Description:
#	Compares two Monte Carlo estimates for a uniform population and graphically show the results.


parVal <- c(1,5,50,100)
nVal <- c(1,2,3,5,10,30)
nsim <- 1000

# Initialization
mle.mse <- array(dim=c(4,6,nsim))
mme.mse <- array(dim=c(4,6,nsim))

# Compute MSE of two estimates for all n and parameter combinations
for (i in 1:4) {
  for (j in 1:6) {
    for (k in 1:nsim) {
      x <- runif(nVal[j], 0, parVal[i])
      
      mle <- max(x)
      mme <- 2*mean(x)
      
      mle.mse[i, j, k] <- (parVal[i] - mle) ** 2
      mme.mse[i, j, k] <- (parVal[i] - mme) ** 2
    }
  }
}

#   Show graphically (4 graphs for 4 parameters)
#     Graph for Parameter = 1
mean1 <- c(mean(mme.mse[1,1,]), mean(mme.mse[1,2,]), mean(mme.mse[1,3,]), mean(mme.mse[1,4,]), mean(mme.mse[1,5,]), mean(mme.mse[1,6,]))
mean2 <- c(mean(mle.mse[1,1,]), mean(mle.mse[1,2,]), mean(mle.mse[1,3,]), mean(mle.mse[1,4,]), mean(mle.mse[1,5,]), mean(mle.mse[1,6,]))
plot(nVal, mean1, type="o", col="red", lty=6, lwd=2, pch=16,
     main="Average Mean Squared Error of Parameter Estimates with Parameter = 1", xlab="n", ylab="Mean Squared Error")
lines(nVal, mean2, type="o", col="blue", lwd=2, pch=16)
axis(side=1, at=nVal)
legend("topright", c("Estimator 1 (MLE)", "Estimator 2 (MME)"), lty=c(1,6), lwd=c(2,2), col=c("blue","red"))

#     Graph for Parameter = 5
mean1 <- c(mean(mme.mse[2,1,]), mean(mme.mse[2,2,]), mean(mme.mse[2,3,]), mean(mme.mse[2,4,]), mean(mme.mse[2,5,]), mean(mme.mse[2,6,]))
mean2 <- c(mean(mle.mse[2,1,]), mean(mle.mse[2,2,]), mean(mle.mse[2,3,]), mean(mle.mse[2,4,]), mean(mle.mse[2,5,]), mean(mle.mse[2,6,]))
plot(nVal, mean1, type="o", col="red", lty=6, lwd=2, pch=16,
     main="Average Mean Squared Error of Parameter Estimates with Parameter = 5", xlab="n", ylab="Mean Squared Error")
lines(nVal, mean2, type="o", col="blue", lwd=2, pch=16)
axis(side=1, at=nVal)
legend("topright", c("Estimator 1 (MLE)", "Estimator 2 (MME)"), lty=c(1,6), lwd=c(2,2), col=c("blue","red"))

#     Graph for Parameter = 50
mean1 <- c(mean(mme.mse[3,1,]), mean(mme.mse[3,2,]), mean(mme.mse[3,3,]), mean(mme.mse[3,4,]), mean(mme.mse[3,5,]), mean(mme.mse[3,6,]))
mean2 <- c(mean(mle.mse[3,1,]), mean(mle.mse[3,2,]), mean(mle.mse[3,3,]), mean(mle.mse[3,4,]), mean(mle.mse[3,5,]), mean(mle.mse[3,6,]))
plot(nVal, mean1, type="o", col="red", lty=6, lwd=2, pch=16,
     main="Average Mean Squared Error of Parameter Estimates with Parameter = 50", xlab="n", ylab="Mean Squared Error")
lines(nVal, mean2, type="o", col="blue", lwd=2, pch=16)
axis(side=1, at=nVal)
legend("topright", c("Estimator 1 (MLE)", "Estimator 2 (MME)"), lty=c(1,6), lwd=c(2,2), col=c("blue","red"))

#     Graph for Parameter = 100
mean1 <- c(mean(mme.mse[4,1,]), mean(mme.mse[4,2,]), mean(mme.mse[4,3,]), mean(mme.mse[4,4,]), mean(mme.mse[4,5,]), mean(mme.mse[4,6,]))
mean2 <- c(mean(mle.mse[4,1,]), mean(mle.mse[4,2,]), mean(mle.mse[4,3,]), mean(mle.mse[4,4,]), mean(mle.mse[4,5,]), mean(mle.mse[4,6,]))
plot(nVal, mean1, type="o", col="red", lty=6, lwd=2, pch=16,
     main="Average Mean Squared Error of Parameter Estimates with Parameter = 100", xlab="n", ylab="Mean Squared Error")
lines(nVal, mean2, type="o", col="blue", lwd=2, pch=16)
axis(side=1, at=nVal)
legend("topright", c("Estimator 1 (MLE)", "Estimator 2 (MME)"), lty=c(1,6), lwd=c(2,2), col=c("blue","red"))
  