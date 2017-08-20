#####################################
#
# Regression Analysis of Boston Data
#
#####################################


library(MASS) 
#Remove record "Boston$medv == 50" because seems like outlier.
i = which(Boston$medv == 50) 
boston = Boston[-i,] # delete cases with medv=50
boston$chas = factor(boston$chas)
boston$rad = factor(boston$rad)


fit.all = lm(medv ~ ., data = boston) # fit a linear model with all variables
fit.step = step(fit.all, direction="both") # stepwise variable selection
fit.step$anova

summary(fit.step) # print the fitted model

yhat = predict(fit.step, newdata=boston, type="response") # predictions
print(yhat)

plot(boston$medv, fit.step$fitted, xlim=c(0,50), ylim=c(0,50), xlab="Observed Values", ylab="Fitted Values")
abline(a=0, b=1)

mean((boston$medv - yhat)^2) # MSE
