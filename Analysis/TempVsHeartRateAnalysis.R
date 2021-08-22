# TempVsHeartRateAnalysis.R
#
# Description:
#	Performs analysis on male and female temperatures and heart rates and
#		their relationships. This is done via histograms, boxplots, and CI
#		through bootstrap.


library(boot)
set.seed(1)

data <- read.csv("input.csv")

male.bodyTemp <- data$body_temperature[data$gender==1]
male.heartRate <- data$heart_rate[data$gender==1]
female.bodyTemp <- data$body_temperature[data$gender==2]
female.heartRate <- data$heart_rate[data$gender==2]

# Exploratory analysis of data for body temperature
hist(male.bodyTemp, main="Histogram of Male Body Temperatures", xlab="Body Temperature", xlim=c(96, 101))
hist(female.bodyTemp, main="Histogram of Female Body Temperatures", xlab="Body Temperature", xlim=c(96, 101))
boxplot(male.bodyTemp, female.bodyTemp, main="Side-By-Side Boxplots for Body Temperature",
        ylab="Body Temperature", names=c("Male", "Female"))
mean(male.bodyTemp)
median(male.bodyTemp)
mean(female.bodyTemp)
median(female.bodyTemp)

# 95% confidence interval for the difference of means
t.test(male.bodyTemp, female.bodyTemp, conf.level = 0.95, var.equal = FALSE)

# Exploratory analysis of data for heart rate
hist(male.heartRate, main="Histogram of Male Heart Rates", xlab="Heart Rate")
hist(female.heartRate, main="Histogram of Female Heart Rates", xlab="Heart Rate")
boxplot(male.heartRate, female.heartRate, main="Side-By-Side Boxplots for Heart Rate",
        ylab="Heart Rate", names=c("Male", "Female"))
mean(male.heartRate)
median(male.heartRate)
mean(female.heartRate)
median(female.heartRate)

# 95% confidence interval for the difference of means
t.test(male.heartRate, female.heartRate, conf.level = 0.95, var.equal = FALSE)

# Linear relationship between body temperature and heart rate
plot(data$body_temperature, data$heart_rate, main="Heart Rate vs. Body Temperature",
     xlab="Body Temperature", ylab="Heart Rate")
abline(lm(data$heart_rate ~ data$body_temperature))
cor(data$body_temperature, data$heart_rate)

# Linear relationship between body temperature and heart rate for males
plot(male.bodyTemp, male.heartRate, main="Heart Rate vs. Body Temperature for Males",
     xlab="Body Temperature", ylab="Heart Rate")
abline(lm(male.heartRate ~ male.bodyTemp))
cor(male.bodyTemp, male.heartRate)

# Linear relationship between body temperature and heart rate for females
plot(female.bodyTemp, female.heartRate, main="Heart Rate vs. Body Temperature for Females",
     xlab="Body Temperature", ylab="Heart Rate")
abline(lm(female.heartRate ~ female.bodyTemp))
cor(female.bodyTemp, female.heartRate)

# Function for bootstrap
corFunction.male <- function(x, i) {
  bodyTemp <- x[i] # get row from data
  return(cor(bodyTemp, male.heartRate[i]))
}

# 95% confidence interval using percentile bootstrap
p.boot <- boot(male.bodyTemp, corFunction.male, R=999, sim="ordinary", stype="i")
boot.ci(p.boot, type = "perc")


# Function for bootstrap
corFunction.female <- function(x, i) {
  bodyTemp <- x[i] # get row from data
  return(cor(bodyTemp, female.heartRate[i]))
}

# 95% confidence interval using percentile bootstrap
p.boot <- boot(female.bodyTemp, corFunction.female, R=999, sim="ordinary", stype="i")
boot.ci(p.boot, type = "perc")
