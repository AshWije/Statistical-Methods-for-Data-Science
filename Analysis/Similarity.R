# Similarity.R
#
# Description:
#	Determine the level of similarity between two distributions visually via histograms and
#		boxplots and mathematically via a t-test for the 95% confidence interval.


data <- read.csv("input.csv")

# Analysis of the Data
hist(data$temperature, data$theoretical, breaks = seq(90, 150, by = 10),
     main="Histogram of Theoretical Vapor Pressure for Temperatures", xlab="Temperature")
hist(data$temperature, data$experimental, breaks = seq(90, 150, by = 10),
     main="Histogram of Experimental Vapor Pressure for Temperatures", xlab="Temperature")
boxplot(data$theoretical, data$experimental, main="Side-By-Side Boxplots for Vapor Pressure",
        ylab="Vapor Pressure", names=c("Theoretical", "Experimental"))
mean(data$theoretical)
median(data$theoretical)
mean(data$experimental)
median(data$experimental)

# 95% Confidence Interval
t.test(data$theoretical, data$experimental, conf.level = 0.95, var.equal = FALSE)
