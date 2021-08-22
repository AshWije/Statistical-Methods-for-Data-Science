# ModelCreation.R
#
# Description:
#	Step-by-step process of creating a linear model for a given data set on PSA level.
#		This model is then used to make predictions for a specific patient.

data <- read.csv("input.csv")
str(data)

# Observe the distributions of the predictors
#   Exclude 'subject', since we don't expect it to have an impact
boxplot(data$cancervol)
hist(data$cancervol)

boxplot(data$weight)
hist(data$weight)

boxplot(data$age)
hist(data$age)

boxplot(data$benpros)
hist(data$benpros)

table(data$vesinv)

boxplot(data$capspen)
hist(data$capspen)

table(data$gleason)

# Notice that the psa data is not distributed very well:
boxplot(data$psa, main="Boxplot for PSA Level", ylab="PSA Level")

# So, use a transformation: Take the log(psa) as the response
boxplot(log(data$psa), main="Boxplot for PSA Level", ylab="PSA Level")
y <- log(data$psa)

# Look at the relationship between response and each predictor one by one
plot(data$cancervol, y)
fit1 <- lm(y ~ cancervol, data = data)
abline(fit1)
summary(fit1)
#   Adjusted R-squared = 0.4258

plot(data$weight, y)
fit2 <- lm(y ~ weight, data = data)
abline(fit2)
summary(fit2)
#   Adjusted R-squared = 0.004446

plot(data$age, y)
fit3 <- lm(y ~ age, data = data)
abline(fit3)
summary(fit3)
#   Adjusted R-squared = 0.01865

plot(data$benpros, y)
fit4 <- lm(y ~ benpros, data = data)
abline(fit4)
summary(fit4)
#   Adjusted R-squared = 0.01451

plot(data$vesinv, y)
fit5 <- lm(y ~ factor(vesinv), data = data)
abline(fit5)
summary(fit5)
#   Adjusted R-squared = 0.3136

plot(data$capspen, y)
fit6 <- lm(y ~ capspen, data = data)
abline(fit6)
summary(fit6)
#   Adjusted R-squared = 0.2606

plot(data$gleason, y)
fit7 <- lm(y ~ gleason, data = data)
abline(fit7)
summary(fit7)
#   Adjusted R-squared = 0.2831

# We see a positive trend in all of the cases
# We observe that cancer volume is important and add it to the model: y ~ cancervol
#   (by its adjusted R-squared value being the highest at 0.4258 and simply by observing its plot)
# Let's try adding vesinv, the predictor with the next highest R-squared value at 0.3136
fit8 <- lm(y ~ cancervol+factor(vesinv), data = data)
summary(fit8)
#   Adjusted R-squared = 0.472

# Perform a partial F-test to check significance of vesinv
anova(fit1, fit8)
#   p-value = 0.002953 (this is a small value, therefore we conclude that vesinv is significant)


# Now, let's try adding gleason, the predictor with the next highest R-squared value at 0.2831
fit9 <- lm(y ~ cancervol+factor(vesinv)+gleason, data = data)
summary(fit9)
#   Adjusted R-squared = 0.5125

# Perform a partial F-test to check significance of gleason
anova(fit8, fit9)
#   p-value = 0.003804 (this is a small value, therefore we conclude that gleason is significant)


# Now, let's try adding capspen, the predictor with the next highest R-squared value at 0.2606
fit10 <- lm(y ~ cancervol+factor(vesinv)+gleason+capspen, data = data)
summary(fit10)
#   Adjusted R-squared = 0.5097

# Perform a partial F-test to check significance of vesinv
anova(fit9, fit10)
#   p-value = 0.4985 (this is a large value, therefore we conclude that capspen is not significant)


# Now, let's try adding age, the predictor with the next highest R-squared value at 0.01865
fit11 <- lm(y ~ cancervol+factor(vesinv)+gleason+age, data = data)
summary(fit11)
#   Adjusted R-squared = 0.513

# Perform a partial F-test to check significance of age
anova(fit9, fit11)
#   p-value = 0.2995 (this is a large value, therefore we conclude that age is not significant)


# Now, let's try adding benpros, the predictor with the next highest R-squared value at 0.01451
fit12 <- lm(y ~ cancervol+factor(vesinv)+gleason+benpros, data = data)
summary(fit12)
#   Adjusted R-squared = 0.5653

# Perform a partial F-test to check significance of benpros
anova(fit9, fit12)
#   p-value = 0.0007054 (this is a small value, therefore we conclude that benpros is significant)


# Now, let's try reconsidering capspen
fit13 <- lm(y ~ cancervol+factor(vesinv)+gleason+benpros+capspen, data = data)
summary(fit13)
#   Adjusted R-squared = 0.5637

# Perform a partial F-test to check significance of capspen
anova(fit12, fit13)
#   p-value = 0.4132 (this is a large value, therefore we conclude that capspen is not significant)


# Now, let's try reconsidering age
fit14 <- lm(y ~ cancervol+factor(vesinv)+gleason+benpros+age, data = data)
summary(fit14)
#   Adjusted R-squared = 0.5607

# Perform a partial F-test to check significance of age
anova(fit12, fit14)
#   p-value = 0.8367 (this is a large value, therefore we conclude that age is not significant)


# Now, let's try adding weight, the predictor with the next highest R-squared value at 0.004446
fit15 <- lm(y ~ cancervol+factor(vesinv)+gleason+benpros+weight, data = data)
summary(fit15)
#   Adjusted R-squared = 0.5632

# Perform a partial F-test to check significance of weight
anova(fit12, fit15)
#   p-value = 0.4527 (this is a large value, therefore we conclude that weight is not significant)

# This means our final model is fit12: y ~ cancervol + vesinv + gleason + benpros


# Forward selection based on AIC
fit16.forward <- step(lm(y ~ 1, data = data), 
                      scope = list(upper = ~cancervol+weight+age+benpros+factor(vesinv)+capspen+gleason),
                      direction = "forward")
# This returns the same resulting model: y ~ cancervol + gleason + benpros + vesinv


# Backward elimination based on AIC
fit17.backward <- step(lm(y ~ cancervol+weight+age+benpros+factor(vesinv)+capspen+gleason, data = data), 
                       scope = list(lower = ~1), direction = "backward")
# This returns the same resulting model: y ~ cancervol + benpros + vesinv + gleason


# Both forward/backward
fit18.both <- step(lm(y ~ 1, data = data), 
                   scope = list(lower = ~1, upper = ~cancervol+weight+age+benpros+factor(vesinv)+capspen+gleason),
                   direction = "both")
# This returns the same resulting model: y ~ cancervol + gleason + benpros + vesinv


# All models are the same, therefore our final model is: y ~ cancervol + gleason + benpros + vesinv
# Model Visualization:
#   Residual Plot
plot(fitted(fit12), resid(fit12), main="Residual Plot", xlab="Fitted Value (log(PSA level))", ylab="Residual")
abline(h = 0)

#   Plot of Absolute Residuals
plot(fitted(fit12), abs(resid(fit12)), main="Plot of Absolute Residuals",
     xlab="Fitted Value (log(PSA level))", ylab="Absolute Residual")

#   Normal QQ Plot
qqnorm(resid(fit12))
qqline(resid(fit12))

#   Time series plot
plot(1:length(resid(fit12)),resid(fit12), type="l", main="Time Series Plot", xlab="Index", ylab="Residual")
abline(h=0)

# Finding the patient predictor values
x <- data.frame(cancervol=mean(data$cancervol), gleason=mean(data$gleason), benpros=mean(data$benpros), vesinv=0)

# Prediction: log(psa)=2.330541; psa=10.2835
exp(predict(fit12, x))
