# RemotevsLocalVoltageComparison.R
#
# Description:
#	Analysis and comparison of two distributions visually via histograms and
#		boxplots and mathematically via a t-test for the 95% confidence interval.


data <- read.csv("input.csv")
remote <- data$voltage[data$location==0]
local <- data$voltage[data$location==1]

# Examining the distributions to analyze
hist(remote, main="Histogram of Voltage for the Remote Location", xlab="Voltage", xlim=c(8, 11))
hist(local, main="Histogram of Voltage for the Local Location", xlab="Voltage", xlim=c(8, 11))
boxplot(remote, local, main="Side-By-Side Boxplots for Voltage", ylab="Voltage", names=c("Remote location", "Local location"))
mean(remote)
median(remote)
mean(local)
median(local)

# 95% Confidence Interval
t.test(remote, local, conf.level = 0.95, var.equal = FALSE)