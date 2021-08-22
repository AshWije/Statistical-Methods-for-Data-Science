# GPAvsACTRelationship.R
#
# Description:
#	Compares the relationship between GPA and ACT test scores and visually displays this relationship via
#		a scatterplot. Displays bootstrap estimates of bias and standard error as well as the 95%
#		confidence interval.


library(boot)
set.seed(1)

data <- read.csv("input.csv")
x <- data$act
y <- data$gpa

# Scatterplot and determine strength of linear relationship
plot(x, y, main="GPA at the end of freshman year vs. ACT test score", xlab="ACT test score", ylab="GPA")
abline(lm(y ~ x))
cor(x, y)

# Point estimate of population correlation
p.est <- cov(x, y) / (sd(x)*sd(y))

# Function for bootstrap
corFunction <- function(x, i) {
  row <- x[i,] # get row from data
  return(cor(row$act, row$gpa))
}

# Bootstrap estimate of bias and standard error
p.boot <- boot(data, corFunction, R=999, sim="ordinary", stype="i")
p.boot

# 95% confidence interval using percentile bootstrap
boot.ci(p.boot, type = "perc")
