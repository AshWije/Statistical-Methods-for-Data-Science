# MLE.R
#
# Description:
#	Calculating the maximum likelihood estimate of given data by maximizing the log likelihood function
#		via the optim function.


n <- 5
x <- c(21.72, 14.65, 50.42, 28.78, 11.23)

# Obtain MLE by maximizing the log likelihood function
neg.loglik.fun <- function(par)
{
  # This check is in place since optim only allows finite values (prevent -Inf value from returning at par=0)
  if(par!=0)
    result <- n*log(par)-(par+1)*log(prod(x))
  else
    result <- -999999
  return(-result)
}
ml.est <- optim(par=0.1, fn=neg.loglik.fun, lower=0, upper=Inf, method="L-BFGS-B", hessian=TRUE)

# Approximate the standard error
se <- 1 / sqrt(ml.est$hessian[1,1])

# Approximate the confidence interval
alpha <- 0.05
ci <- ml.est$par + c(-1, 1) * qnorm(1 - (alpha/2)) * se
