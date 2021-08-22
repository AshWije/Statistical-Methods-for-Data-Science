# MonteCarloEstimatePi.R
#
# Description:
#	Estimates the value of pi using a Monte Carlo approach and
#		10,000 replications.

# Draw random (x,y) coordinates within the square
squarePoints <- 10000
pX <- runif(squarePoints)
pY <- runif(squarePoints)

# Determine distance of the point from the center (0.5, 0.5)
d <- sqrt(((pX-0.5)^2)+((pY-0.5)^2))

# The points within the circle will have a distance <= 0.5
circlePoints <- length(which(d<=0.5))

# Estimate pi
pi <- 4 * circlePoints / squarePoints