#############################################
#
# Regression Anlsysis of German Credit Data
#
#############################################


##### Data

german = read.table("c:/germandata.txt", header=T) 
# If you do not covert data to factor, get a wrong model due to integer value.
german$numcredits = factor(german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)
german$y = ifelse(german$y=="good", 1, 0)


##### Fit a logit model

fit.all = glm(y ~ ., family = binomial, data = german)
fit.step = step(fit.all, direction="both") # stepwise vaiable selection
fit.step$anova

summary(fit.step)


###### Make predictions

p = predict(fit.step, newdata=german, type="response") # prediction
threshold = 0.5 #cutoff
yhat = ifelse(p > threshold, 1, 0)

class.tab = table(german$y, yhat, dnn=c("Actual","Predicted"))
print(class.tab) # classification table
sum(german$y==yhat)/length(german$y) #Prediction Accuracy
sum(german$y!=yhat)/length(german$y) #Misclassification Rate
class.tab[1,1]/apply(class.tab, 1, sum)[1] #Specificity
class.tab[2,2]/apply(class.tab, 1, sum)[2] #Sensitivity


##### ROC Curve & AUC
install.packages('ROCR')

library(ROCR)

pred = prediction(p, german$y)
perf = performance(pred,"tpr","fpr")

plot(perf, lty=1, col=2, xlim=c(0,1), ylim=c(0,1), xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
lines(x = c(0, 1), y = c(0, 1), col = "grey")

performance(pred,"auc")@y.values #AUC




