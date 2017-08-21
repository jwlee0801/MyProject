source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("OutlierD")
library(Biobase)
openVignette("Biobase")
biocLite("ALL")
library(ALL)
data(ALL)
dat = exprs(ALL)
f.name = featureNames(ALL)
head(f.name)
s.name = sampleNames(ALL)
head(s.name)
mat = phenoData(ALL)
cl = c(rep(0, 95), rep(1, 33))
#multtest를_이용한_유의한_유전자_탐색
biocLite("multtest")
library(multtest)
resT = mt.maxT(dat, classlabel=cl, B=1000)
head(resT)
#Heatmap
library(ALL)
data(ALL)
dat = exprs(ALL)
x = dat[1:50, 96:128]
standardize <- function(x) { (x-mean(x,na.rm=T))/sqrt(var(x,na.rm=T)) }
x = t(apply(x, 1, standardize))
heatmap(x, main="Heatmap")
#outlierD를_이용한_이상치_탐색
library(OutlierD)
?OutlierD
install.packages("MatrixModels")
#install anothor packages
library(MatrixModels)
data(lcms)
x <- log2(lcms) #log2-tranformation, do normalization if necessary

fit1 <- OutlierD(x1=x[,1], x2=x[,2], method="constant")
fit2 <- OutlierD(x1=x[,1], x2=x[,2], method="linear")
fit3 <- OutlierD(x1=x[,1], x2=x[,2], method="nonlin")
fit4 <- OutlierD(x1=x[,1], x2=x[,2], method="nonpar")

fit3$x[1:10,]
