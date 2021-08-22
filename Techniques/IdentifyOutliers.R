# IdentifyOutliers.R
#
# Description:
#	Identify the outliers of the given input data of fatal motorcycle accidents.
#		This is done visually via a boxplot.


# Boxplot of data
data <- read.csv("input.csv")
fatalAccidents <- as.integer(data$Fatal.Motorcycle.Accidents)
boxplot(fatalAccidents, main="Boxplot for Fatal Motorcycle Accidents", ylab="Number of Fatal Motorcycle Accidents")

# Provide relevant summary statistics
length(fatalAccidents)
mean(fatalAccidents)
sd(fatalAccidents)
range(fatalAccidents)
median(fatalAccidents)
iqr <- IQR(fatalAccidents)
q <- quantile(fatalAccidents)

# Identify which counties may be considered outliers
data$County[fatalAccidents <= q[2]-1.5*iqr | fatalAccidents >= q[4]+1.5*iqr]
#   > identifies counties Greenville and Horry as outliers

# View number of fatal motorcycle accidents for Greenville and Horry counties
fatalAccidents[data$County == 'GREENVILLE']
fatalAccidents[data$County == 'HORRY']