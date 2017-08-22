#####################################
#
# Model Comparison with German Data
#
#####################################


##### Data Import

german = read.table("germandata.txt",header=T) 
german$numcredits = factor(german$numcredits)
german$residence = factor(german$residence)
german$residpeople = factor(german$residpeople)
german$y = relevel(german$y, ref="bad") #reference level 0="bad"
summary(german)

threshold = 0.5 #cutoff



##### Regression

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) 
german.train = german[i,] # training data (70%)
german.test = german[-i,] # testd ata (30%)

fit.reg = glm(y ~ ., family = binomial(link = "logit"), data = german.train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #stepwise selection

p.test.reg = predict(fit.step.reg, newdata=german.test, type="response") #probabilities
yhat.test.reg = ifelse(p.test.reg > threshold, levels(german$y)[2], levels(german$y)[1]) 

tab = table(german.test$y, yhat.test.reg, dnn=c("Observed","Predicted"))
print(tab) # classification table
1-sum(diag(tab))/sum(tab) # misclassification rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity



##### Decision Tree

library(rpart)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) 
german.train = german[i,] # training data (70%)
german.test = german[-i,] # test data (30%)

my.control = rpart.control(xval=10, cp=0, minsplit=5)
fit.tree = rpart(y ~ ., data = german.train, method="class", control=my.control)
#start prune
ii = which.min(fit.tree$cp[,4]) #find min xerror; set prune point
fit.prun.tree = prune(fit.tree, cp = fit.tree$cp[ii,1])

p.test.tree = predict(fit.prun.tree, newdata=german.test, type="prob")[,2] #probabilities
#levels(german$y)[2] is good, levels(german$y)[1] is bad
yhat.test.tree = ifelse(p.test.tree > threshold, levels(german$y)[2], levels(german$y)[1])

tab = table(german.test$y, yhat.test.tree, dnn=c("Observed","Predicted"))
print(tab) # classification table
1-sum(diag(tab))/sum(tab) # misclassification rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity



##### Bagging
# no prune
# cannot plot
library(rpart)
library(adabag)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) 
german.train = german[i,] # training data (70%)
german.test = german[-i,] # testd ata (30%)

my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit.bag = bagging(y ~ ., data = german.train, mfinal=50, control=my.control)

p.test.bag = predict.bagging(fit.bag, newdata=german.test)$prob[,2] #probabilities
yhat.test.bag = ifelse(p.test.bag > threshold, levels(german$y)[2], levels(german$y)[1])

tab = table(german.test$y, yhat.test.bag, dnn=c("Observed","Predicted"))
print(tab) # classification table
1-sum(diag(tab))/sum(tab) # misclassification rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity



##### Boosting
# no prune
# cannot plot
library(rpart)
library(adabag)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) 
german.train = german[i,] # training data (70%)
german.test = german[-i,] # testd ata (30%)

my.control = rpart.control(xval=0, cp=0, maxdepth=1)
fit.boo = boosting(y ~ ., data = german.train, boos=T, mfinal=100, control=my.control)

p.test.boo = predict.boosting(fit.boo, newdata=german.test)$prob[,2] #probabilities
yhat.test.boo = ifelse(p.test.boo > threshold, levels(german$y)[2], levels(german$y)[1])

tab = table(german.test$y, yhat.test.boo, dnn=c("Observed","Predicted"))
print(tab) # classification table
1-sum(diag(tab))/sum(tab) # misclassification rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity



##### Random Forest
# no prune
# cannot plot
library(randomForest)

set.seed(1234)
i = sample(1:nrow(german), round(nrow(german)*0.7)) 
german.train = german[i,] # training data (70%)
german.test = german[-i,] # testd ata (30%)

fit.rf = randomForest(y ~ ., data = german.train, ntree=100, mtry=5, importance=T, na.action=na.omit)

p.test.rf = predict(fit.rf, newdata=german.test , type="prob")[,2] #probabilities
yhat.test.rf = ifelse(p.test.rf > threshold, levels(german$y)[2], levels(german$y)[1])

tab = table(german.test$y, yhat.test.rf, dnn=c("Observed","Predicted"))
print(tab) # classification table
1-sum(diag(tab))/sum(tab) # misclassification rate
tab[,2]/apply(tab, 1, sum) #specificity/sensitivity



##### ROC and AUC 

library(ROCR)

pred.reg = prediction(p.test.reg, german.test$y); perf.reg = performance(pred.reg,"tpr","fpr")
pred.tree = prediction(p.test.tree, german.test$y); perf.tree = performance(pred.tree,"tpr","fpr")
pred.nn = prediction(p.test.nn, german2.test$y); perf.nn = performance(pred.nn,"tpr","fpr")
pred.bag = prediction(p.test.bag, german.test$y); perf.bag = performance(pred.bag,"tpr","fpr")
pred.boo = prediction(p.test.boo, german.test$y); perf.boo = performance(pred.boo,"tpr","fpr")
pred.rf = prediction(p.test.rf, german.test$y); perf.rf = performance(pred.rf,"tpr","fpr")

#Draw ROC
plot(perf.reg, lty=1, col=1, xlim=c(0,1), ylim=c(0,1),xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
plot(perf.tree, lty=2, col=2, add=TRUE)
plot(perf.nn, lty=3, col=3, add=TRUE)
plot(perf.bag, lty=4, col=4, add=TRUE)
plot(perf.boo, lty=5, col=5, add=TRUE)
plot(perf.rf, lty=6, col=6, add=TRUE)
lines(x = c(0, 1), y = c(0, 1), col = "grey")
legend(0.6,0.3, c("Regression","Decision Tree","Neural Network","Bagging","Boosting","Random Forest"), lty=1:6, col=1:6)

#Compute AUC
performance(pred.reg,"auc")@y.values #Regression
performance(pred.tree,"auc")@y.values #Decision Tree
performance(pred.nn,"auc")@y.values #Neural Network
performance(pred.bag,"auc")@y.values #Bagging
performance(pred.boo,"auc")@y.values #Boosting
performance(pred.rf,"auc")@y.values #Random Forest

##### END

