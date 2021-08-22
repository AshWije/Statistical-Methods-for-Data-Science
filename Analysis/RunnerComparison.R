# RunnerComparison.R
#
# Description:
#	Display relevant statistics for four different distributions
#		and compare them using bar graphs and side-by-side boxplots.


# COMPARE FIRST TWO DISTRIBUTIONS
# Create a bar graph of the variable 'Maine'
data <- read.csv("input.csv")
maine <- table(data$Maine)
barplot(maine, main="Barplot of Runners' Origins")

# Histogram of runners time for Maine group
maineMinutes <- data$Time..minutes.[data$Maine == 'Maine']
hist(maineMinutes, main="Histogram of Runners Time for Maine Group", xlab="Time (minutes)", xlim=c(0, 160))

# Histogram of runners time for Away group
awayMinutes <- data$Time..minutes.[data$Maine == 'Away']
hist(awayMinutes, main="Histogram of Runners Time for Away Group", xlab="Time (minutes)", xlim=(c(0, 160)))

# Relevant statistics for Maine group
length(maineMinutes)
mean(maineMinutes)
sd(maineMinutes)
range(maineMinutes)
median(maineMinutes)
IQR(maineMinutes)
quantile(maineMinutes, 0.25)
quantile(maineMinutes, 0.75)

# Relevant statistics for Away group
length(awayMinutes)
mean(awayMinutes)
sd(awayMinutes)
range(awayMinutes)
median(awayMinutes)
IQR(awayMinutes)
quantile(awayMinutes, 0.25)
quantile(awayMinutes, 0.75)

# Side-by-side boxplots for Maine and Away groups' running times
boxplot(maineMinutes, awayMinutes, main="Side-By-Side Boxplots for Running Times", ylab="Time (minutes)", names=c("Maine", "Away"))


# COMPARE SECOND TWO DISTRIBUTIONS
# Side-by-side boxplots for the runners' ages for Male and Female runners
maleAges <- as.integer(data$Age[data$Sex=='M'])
femaleAges <- as.integer(data$Age[data$Sex=='F'])
boxplot(maleAges, femaleAges, main="Side-By-Side Boxplots for Runners' Ages", ylab="Age (years)", names=c("Male", "Female"))

# Relevant statistics for Male runners
length(maleAges)
mean(maleAges)
sd(maleAges)
range(maleAges)
median(maleAges)
IQR(maleAges)
quantile(maleAges, 0.25)
quantile(maleAges, 0.75)

# Relevant statistics for Female runners
length(femaleAges)
mean(femaleAges)
sd(femaleAges)
range(femaleAges)
median(femaleAges)
IQR(femaleAges)
quantile(femaleAges, 0.25)
quantile(femaleAges, 0.75)