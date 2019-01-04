#References
#http://amunategui.github.io/google-trends-walkthrough/
#https://www.r-bloggers.com/download-and-plot-google-trends-data-with-r/
#https://github.com/PMassicotte/gtrendsR/issues/108

#Library installations (if required)

#Load libraries for analysis
library("gtrendsR")
library("quantmod")
library("stockPortfolio")
library("lubridate")
library("zoo")
library("ggplot2")
library("class")
library("readr")
library("data.table")
library("plyr")
library("leaps")
library("tree")
library("MASS")
library("class")
library(randomForest)

###MCCREE
library(dplyr)
library(reshape2)

#Set your wd
#Here I set my wd to save the cleaned file. For additional analysis you will set your wd to load the cleaned file
setwd("C:/Data/R")

#Cleaned data set is saved to my local
save(clean.data,file = "cleandata.Rda")

#Removing all variables prior to loading the saved data
rm(list=ls()) 

#Clean data set is loaded from my local
#Make sure to run all library packages at the top of the code, even when loading this data.
load("cleandata.Rda")

#After loading data, the dim shows 517 rows and 27 columns as expected
dim(clean.data)
head(clean.data)

#Analysis using hits to predict return response value

#Create training and testing sets for cross validation
#training set is 1/2 the testing set
n = 1:floor(nrow(clean.data)/2)
TrainingSet = clean.data[n, ]
ValidationSet = clean.data[ - n, ]
dim(TrainingSet)
dim(ValidationSet)


#scatterplot between all variables to look at initial relationships
attach(clean.data)
names(clean.data)
pairs(~buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + hig_return + trv_return + pgr_return + hig_r_Lag1 + hig_r_Lag2 + trv_r_Lag1 + trv_r_Lag2 + pgr_r_Lag1 + pgr_r_Lag2, clean.data)

#***************************************************************
# HIG ANALYSIS
#***************************************************************

#Best Model Subset selection for each dependent variable (non Lag).
#Since hig_return is not lagged, we will not use Lag variables in this analysis
#Best model sbuset for hig_return
hig.return=regsubsets(hig_return ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return, data=clean.data, nvmax=9)
t(summary(hig.return)$which)
hig.return.summary=summary(hig.return)
Models=c(1:9)
CP=hig.return.summary$cp
plot(CP)
ADJ=hig.return.summary$adjr2
plot(ADJ)
BIC=hig.return.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(hig.return,which.min(hig.return.summary$bic))
coef(hig.return,which.max(hig.return.summary$adjr2))

#Results --> To measure hig_return use the 3 model order because it minimizes CP and maximizes ADJ. Although it does not minimize BIC, it is the second lowest BIC
#Selected variables for hig_return: trv_hit, trv_return, pgr_return

#Lag 1 anslysis
#Will will use Lag1 variables and non lags in this analysis
#Best model sbuset for hig_r_Lag1
hig.r.lag1=regsubsets(hig_r_Lag1 ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1, data=clean.data, nvmax=12)
t(summary(hig.r.lag1)$which)
hig.r.lag1.summary=summary(hig.r.lag1)
Models=c(1:12)
CP=hig.r.lag1.summary$cp
plot(CP)
ADJ=hig.r.lag1.summary$adjr2
plot(ADJ)
BIC=hig.r.lag1.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(hig.r.lag1,which.min(hig.r.lag1.summary$bic))
coef(hig.r.lag1,which.max(hig.r.lag1.summary$adjr2))

##Results --> Use the 8 model order. Min CP is 8, Max ADJ is 9, Min BIc is 4
#8 is 3rd lowest max ADJ. BIC minimizes at smaller orders so it makes more sense to chose the lower of the two values closest together (8 compared to 9)
#bull_hit, hig_hit, pgr_hit, trv_return, pgr_return,hig_return, pgr_r_Lag1, trv_r_Lag1


#Lag2 analysis
#Will will use Lag2 variables. Lag 1 variables, and non lags in this analysis
#Best model sbuset for hig_r_Lag2
hig.r.lag2=regsubsets(hig_r_Lag2 ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2 + trv_r_Lag2, data=clean.data, nvmax=15)
t(summary(hig.r.lag2)$which)
hig.r.lag2.summary=summary(hig.r.lag2)
Models=c(1:15)
CP=hig.r.lag2.summary$cp
plot(CP)
ADJ=hig.r.lag2.summary$adjr2
plot(ADJ)
BIC=hig.r.lag2.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(hig.r.lag2,which.min(hig.r.lag2.summary$bic))
coef(hig.r.lag2,which.max(hig.r.lag2.summary$adjr2))

##Results --> Use the 9 model order. Min CP is 9, Max ADJ is 10, Min BIc is 6
#9 is 3rd lowest max ADJ. BIC minimizes at smaller orders so it makes more sense to chose the lower of the two values closest together (9 compared to 10)
#sell_hit, hig_hit, trv_hit, trv_return, pgr_return,hig_return, pgr_r_Lag1, hig_r_Lag1, pgr_r_Lag2


##Model with the lowest MSE (mean squared error) is the best model
#Linear
#Linear for hig_return
linear.fit=lm(hig_return~trv_hit + trv_return + pgr_return,data=TrainingSet)
summary(linear.fit)
#Only pgr_return and trv_return are significant
linear.fit=lm(hig_return~trv_return + pgr_return,data=TrainingSet)
summary(linear.fit)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$hig_return
hig.MSE.linear=mean(err^2)
hig.MSE.linear
#MSE of 0.001088611

##MCCREE
pred.linear.hig.nl=data.frame(pred.linear)
colnames(pred.linear.hig.nl) = c('pred.linear.hig.nl')
err.linear.hig.nl = err

#Linear for hig_r_Lag1
linear.fit=lm(hig_r_Lag1~bull_hit + hig_hit + pgr_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1,data=TrainingSet)
summary(linear.fit)
#bul_hit and trv_return are not significant
linear.fit=lm(hig_r_Lag1~hig_hit + pgr_hit + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1,data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$hig_return
hig.Lag1.MSE.linear=mean(err^2)
hig.Lag1.MSE.linear
#MSE of 0.006277151

##MCCREE
pred.linear.hig.l1=data.frame(pred.linear)
colnames(pred.linear.hig.l1) = c('pred.linear.hig.l1')
err.linear.hig.l1 = err

#Linear for hig_r_Lag2
linear.fit=lm(hig_r_Lag2~sell_hit + hig_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2, data=TrainingSet)
summary(linear.fit)
#sell_hit, trv_hit, trv_return, and pgr_return are not significant
linear.fit=lm(hig_r_Lag2~hig_hit + hig_return + pgr_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2, data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$hig_return
hig.Lag2.MSE.linear=mean(err^2)
hig.Lag2.MSE.linear
#MSE of 0.003818571

##MCCREE
pred.linear.hig.l2=data.frame(pred.linear)
colnames(pred.linear.hig.l2) = c('pred.linear.hig.l2')
err.linear.hig.l2 = err

#Polynomial (Linear Basis Expansion with Restriction and Selection Methods)

#Polynomial for hig_return
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(hig_return~poly(trv_return + pgr_return,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$hig_return
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
hig.MSE.df = MSE.df[which.min(MSE.poly), ]
hig.MSE.df
#Lowest df MSE is 4 with MSE of 0.001023044
poly.fit=lm(hig_return~poly(trv_return + pgr_return,4),data=TrainingSet)
summary(poly.fit)
#All are significant

##MCCREE
pred.poly.hig.nl=data.frame(pred.poly)
colnames(pred.poly.hig.nl) = c('pred.poly.hig.nl')
err.poly.hig.nl = err

#Polynomial for hig_r_Lag1
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(hig_r_Lag1~poly(hig_hit + pgr_hit + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$hig_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
hig.Lag1.MSE.df = MSE.df[which.min(MSE.poly), ]
hig.Lag1.MSE.df
#Lowest df is 2 with MSE of 0.001949355
poly.fit=lm(hig_r_Lag1~poly(hig_hit + pgr_hit + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1,k),data=TrainingSet)
summary(poly.fit)
#None are significant

##MCCREE
pred.poly.hig.l1=data.frame(pred.poly)
colnames(pred.poly.hig.l1) = c('pred.poly.hig.l1')
err.poly.hig.l1 = err

#Polynomial for hig_r_Lag2
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(hig_r_Lag2~poly(hig_hit + hig_return + pgr_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$hig_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
hig.Lag2.MSE.df = MSE.df[which.min(MSE.poly), ]
hig.Lag2.MSE.df
#Lowest df is 1 with MSE of 0.001933249
poly.fit=lm(hig_r_Lag2~poly(hig_hit + hig_return + pgr_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2,k),data=TrainingSet)
summary(poly.fit)
#df of 3, 4, 5, and 9 are significant
#df of 3 has lowest MSE of significant
hig.Lag2.MSE.df = MSE.df[df == 3]
hig.Lag2.MSE.df
#When df = 3, MSE is 0.002805688

##MCCREE
pred.poly.hig.l2=data.frame(pred.poly)
colnames(pred.poly.hig.l2) = c('pred.poly.hig.l2')
err.poly.hig.l2 = err


#Regression Tree with pruning
#Regression Tree with pruning for hig_return
tree.hig.return=tree(hig_return~.,TrainingSet)
plot(tree.hig.return)
text(tree.hig.return,pretty=0)

pred.tree=predict(tree.hig.return,ValidationSet)
tree.test=clean.data[-n, "hig_return"]
higtree = mean((pred.tree - tree.test)^2)
higtree
summary(tree.hig.return)
#Tree with 7 nodes has an MSE of 0.002283092

cv.hig.return=cv.tree(tree.hig.return)
plot(cv.hig.return$size,cv.hig.return$dev,type='b')
#We should prune the tree to 7 nodes (same as above)

prune.hig.return=prune.tree(tree.hig.return,best=7)
plot(prune.hig.return)
text(prune.hig.return,pretty=0)
summary(prune.hig.return)

pred.hig.return.prune=predict(prune.hig.return,newdata=ValidationSet)
tree.test=clean.data[-n, "hig_return"]
higprune = mean((pred.hig.return.prune - tree.test)^2)
higprune
summary(prune.hig.return)
#Nodes reamain at 7 after pruning so MSE remains 0.002283092

##MCCREE
pred.tree.hig.nl=data.frame(pred.hig.return.prune)
colnames(pred.tree.hig.nl) = c('pred.tree.hig.nl')
err=pred.hig.return.prune-ValidationSet$hig_return
err.tree.hig.nl = err

#Regression Tree with pruning for hig_r_Lag1
tree.hig.return=tree(hig_r_Lag1~.,TrainingSet)
plot(tree.hig.return)
text(tree.hig.return,pretty=0)

pred.tree=predict(tree.hig.return,ValidationSet)
tree.test=clean.data[-n, "hig_r_Lag1"]
higtree.Lag1 = mean((pred.tree - tree.test)^2)
higtree.Lag1
summary(tree.hig.return)
#Tree with 7 nodes has an MSE of 0.002059652

cv.hig.return=cv.tree(tree.hig.return)
plot(cv.hig.return$size,cv.hig.return$dev,type='b')
#We should prune the tree to 7 nodes

prune.hig.return=prune.tree(tree.hig.return,best=7)
plot(prune.hig.return)
text(prune.hig.return,pretty=0)
summary(prune.hig.return)

pred.hig.return.prune=predict(prune.hig.return,newdata=ValidationSet)
tree.test=clean.data[-n, "hig_r_Lag1"]
higprune.Lag1 = mean((pred.hig.return.prune - tree.test)^2)
higprune.Lag1
summary(prune.hig.return)
#Nodes did not change with pruning so the MSE remains 0.002059652

##MCCREE
pred.tree.hig.l1=data.frame(pred.hig.return.prune)
colnames(pred.tree.hig.l1) = c('pred.tree.hig.l1')
err=pred.hig.return.prune-ValidationSet$hig_r_Lag1
err.tree.hig.l1 = err

#Regression Tree with pruning for hig_r_Lag2
tree.hig.return=tree(hig_r_Lag2~.,TrainingSet)
plot(tree.hig.return)
text(tree.hig.return,pretty=0)

pred.tree=predict(tree.hig.return,ValidationSet)
tree.test=clean.data[-n, "hig_r_Lag2"]
higtree.Lag2 = mean((pred.tree - tree.test)^2)
higtree.Lag2
summary(tree.hig.return)
#Tree with 7 nodes has an MSE of 0.004800451

cv.hig.return=cv.tree(tree.hig.return)
plot(cv.hig.return$size,cv.hig.return$dev,type='b')
#Cross validation says we should prune the tree to 4 nodes

prune.hig.return=prune.tree(tree.hig.return,best=4)
plot(prune.hig.return)
text(prune.hig.return,pretty=0)
summary(prune.hig.return)

pred.hig.return.prune=predict(prune.hig.return,newdata=ValidationSet)
tree.test=clean.data[-n, "hig_r_Lag2"]
higprune.Lag2 = mean((pred.hig.return.prune - tree.test)^2)
higprune.Lag2
summary(prune.hig.return)
#At 4 nodes, the MSE is 0.004310937

##MCCREE
pred.tree.hig.l2=data.frame(pred.hig.return.prune)
colnames(pred.tree.hig.l2) = c('pred.tree.hig.l2')
err=pred.hig.return.prune-ValidationSet$hig_r_Lag2
err.tree.hig.l2 = err

#Table of hig MSE
higtree = as.data.frame(c(higtree, higtree.Lag1, higtree.Lag2))
higprune = as.data.frame(c(higprune, higprune.Lag1, higprune.Lag2))
higdf = as.data.frame(c(hig.MSE.df[1], hig.Lag1.MSE.df[1], hig.Lag2.MSE.df[1]))
higMSE = as.data.frame(c(hig.MSE.linear, hig.Lag1.MSE.linear, hig.Lag2.MSE.linear))
higMSE = cbind(higMSE,higdf,higtree,higprune)
rownames(higMSE) = c("No Lag","Lag1","Lag2")
colnames(higMSE) = c("Hig Linear","Hig Poly","Hig Tree","Hig Prune") 
higMSE
#For no lag, linear is better
#For lag1, poly is better
#for lag2, poly is better


#Analyze variable statistical Significance
#For hig_return
hig.return.d.glm=glm(hig_return.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return, data=clean.data, family = binomial)
summary(hig.return.d.glm,digits=4)
#Only trv_return and pgr_return are significant for glm.

#For hig_r_Lag1
hig.r.Lag1.d.glm=glm(hig_r_Lag1.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + trv_r_Lag1 + pgr_r_Lag1, data=clean.data, family = binomial)
summary(hig.r.Lag1.d.glm,digits=4)
#Only pgr_return, hig_return, trv_r_Lag1, pgr_r_Lag1 are significant

#For hig_r_Lag2
hig.r.Lag2.d.glm=glm(hig_r_Lag2.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + trv_r_Lag1 + pgr_r_Lag1 + hig_r_Lag1 + trv_r_Lag2 + pgr_r_Lag2, data=clean.data, family = binomial)
summary(hig.r.Lag2.d.glm,digits=4)
#Only pgr_r_Lag1, hig_r_Lag1, trv_r_Lag2, pgr_r_Lag2 are significant

#Logistic Regression
#Logistc Regresssion for hig_return.d.glm
hig_return.d.Val = ValidationSet$hig_return.d
head(ValidationSet)

hig.r.d.glm=glm(hig_return.d~trv_return + pgr_return,data=TrainingSet, family=binomial)
hig.r.d.probs=predict(hig.r.d.glm,data=ValidationSet,type="response")

z=length(hig_return.d.Val)
hig.r.d.pred=rep("Up",z)
hig.r.d.pred[hig.r.d.probs>.5]="Down"
table(hig.r.d.pred,hig_return.d.Val)
Higglm = round(mean(hig.r.d.pred==hig_return.d.Val)*100,4)
Higglm
#Success rate of 44.0154%


#Logistic Regression for hig.r.Lag1.d
hig_return.d.Val = ValidationSet$hig_r_Lag1.d

hig.r.d.glm=glm(hig_r_Lag1.d~pgr_return + hig_return + trv_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
hig.r.d.probs=predict(hig.r.d.glm,data=ValidationSet,type="response")

z=length(hig_return.d.Val)
hig.r.d.pred=rep("Up",z)
hig.r.d.pred[hig.r.d.probs>.5]="Down"
table(hig.r.d.pred,hig_return.d.Val)
Hig.Lag1.glm = round(mean(hig.r.d.pred==hig_return.d.Val)*100,4)
Hig.Lag1.glm
#Success rate of 42.0849%


#Logistic Regression for hig.r.Lag2.d
hig_return.d.Val = ValidationSet$hig_r_Lag2.d
head(ValidationSet)

hig.r.d.glm=glm(hig_r_Lag2.d~pgr_r_Lag1 + hig_r_Lag1 + trv_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
hig.r.d.probs=predict(hig.r.d.glm,data=ValidationSet,type="response")

z=length(hig_return.d.Val)
hig.r.d.pred=rep("Up",z)
hig.r.d.pred[hig.r.d.probs>.5]="Down"
table(hig.r.d.pred,hig_return.d.Val)
Hig.Lag2.glm = round(mean(hig.r.d.pred==hig_return.d.Val)*100,4)
Hig.Lag2.glm
#Success rate of 40.5405%


#LDA
#Use variables identified as statistically significant in glm analysis

#LDA for hig_return.d
hig_return.d.Val = ValidationSet$hig_return.d

hig.r.d.lda.fit=lda(hig_return.d~trv_return + pgr_return,data=TrainingSet, family=binomial)
hig.r.d.lda.pred=predict(hig.r.d.lda.fit,ValidationSet)
hig.r.d.lda.class=hig.r.d.lda.pred$class
table(hig.r.d.lda.class,hig_return.d.Val)
higLDA = round(mean(hig.r.d.lda.class==hig_return.d.Val)*100, 4)
higLDA
#Success rate of 67.9537%


#LDA for hig.r.Lag1.d
hig_return.d.Val = ValidationSet$hig_r_Lag1.d

hig.r.d.lda.fit=lda(hig_r_Lag1.d ~pgr_return + hig_return + trv_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
hig.r.d.lda.pred=predict(hig.r.d.lda.fit,ValidationSet)
hig.r.d.lda.class=hig.r.d.lda.pred$class
table(hig.r.d.lda.class,hig_return.d.Val)
hig.Lag1.LDA = round(mean(hig.r.d.lda.class==hig_return.d.Val)*100, 4)
hig.Lag1.LDA
#Success rate of 68.3398%

#LDA for hig.r.Lag2.d
hig_return.d.Val = ValidationSet$hig_r_Lag2.d

hig.r.d.lda.fit=lda(hig_r_Lag2~pgr_r_Lag1 + hig_r_Lag1 + trv_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
hig.r.d.lda.pred=predict(hig.r.d.lda.fit,ValidationSet)
hig.r.d.lda.class=hig.r.d.lda.pred$class
table(hig.r.d.lda.class,hig_return.d.Val)
hig.Lag2.LDA = round(mean(hig.r.d.lda.class==hig_return.d.Val)*100, 4)
hig.Lag2.LDA
#Success rate of 50.1931%


#QDA
#using significant variables outlined in GLM analysis

#QDA for hig_return.d
hig_return.d.Val = ValidationSet$hig_return.d

hig.r.d.qda.fit=qda(hig_return.d~trv_return + pgr_return,data=TrainingSet, family=binomial)
hig.r.d.qda.pred=predict(hig.r.d.qda.fit,ValidationSet)
hig.r.d.qda.class=hig.r.d.qda.pred$class
table(hig.r.d.qda.class,hig_return.d.Val)
hig.QDA = round(mean(hig.r.d.qda.class==hig_return.d.Val)*100, 4)
hig.QDA
#Success rate of 75.2896%

#QDA for hig.r.Lag1.d
hig_return.d.Val = ValidationSet$hig_r_Lag1.d

hig.r.d.qda.fit=qda(hig_r_Lag1.d ~pgr_return + hig_return + trv_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
hig.r.d.qda.pred=predict(hig.r.d.qda.fit,ValidationSet)
hig.r.d.qda.class=hig.r.d.qda.pred$class
table(hig.r.d.qda.class,hig_return.d.Val)
hig.Lag1.QDA = round(mean(hig.r.d.qda.class==hig_return.d.Val)*100, 4)
hig.Lag1.QDA
#Success rate of 69.112%

#QDA for hig.r.Lag2.d
hig_return.d.Val = ValidationSet$hig_r_Lag2.d


hig.r.d.qda.fit=qda(hig_r_Lag2.d~pgr_r_Lag1 + hig_r_Lag1 + trv_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
hig.r.d.qda.pred=predict(hig.r.d.qda.fit,ValidationSet)
hig.r.d.qda.class=hig.r.d.qda.pred$class
table(hig.r.d.qda.class,hig_return.d.Val)
hig.Lag2.QDA = round(mean(hig.r.d.qda.class==hig_return.d.Val)*100, 4)
hig.Lag2.QDA
#Success rate of 69.4981


#K nearest neighbors
#using significant variables defined in GLM analysis


#KNN for hig_return.d
#No Lag variables: trv_return, pgr_return
vars = c("trv_return","pgr_return")
var2 = c("hig_return.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  hig.knn.pred=knn(train.var,test.var,train.dep$hig_return.d,k = i)
  KNN.Multi[i]=mean(hig.knn.pred==test.dep$hig_return.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
hig.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
hig.knn
hig.knn.NoLag = round(mean(hig.knn[1])*100, 4)
hig.knn.NoLag

#Success rate of 75.6757% when K = 6


#KNN for hig_r_Lag1.d
vars = c("pgr_return","hig_return","trv_r_Lag1","pgr_r_Lag1")
var2 = c("hig_r_Lag1.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  hig.knn.pred=knn(train.var,test.var,train.dep$hig_r_Lag1.d,k = i)
  KNN.Multi[i]=mean(hig.knn.pred==test.dep$hig_r_Lag1.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
hig.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
hig.knn
hig.Lag1.knn = round(mean(hig.knn[1])*100, 4)
hig.Lag1.knn
#Success rate of 72.973% when k = 20


#KNN for hig.r.Lag2.d
vars = c("pgr_r_Lag1","hig_r_Lag1","trv_r_Lag2","pgr_r_Lag2")
var2 = c("hig_r_Lag2.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  hig.knn.pred=knn(train.var,test.var,train.dep$hig_r_Lag2.d,k = i)
  KNN.Multi[i]=mean(hig.knn.pred==test.dep$hig_r_Lag2.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
hig.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
hig.knn
hig.Lag2.knn = round(mean(hig.knn[1])*100, 4)
hig.Lag2.knn
#Success rate of 73.7452% when k = 24

#Table of hig success
higknn = as.data.frame(c(hig.knn.NoLag,hig.Lag1.knn,hig.Lag2.knn))
higqda = as.data.frame(c(hig.QDA, hig.Lag1.QDA, hig.Lag2.QDA))
higlda = as.data.frame(c(higLDA, hig.Lag1.LDA, hig.Lag2.LDA))
higS = as.data.frame(c(Higglm, Hig.Lag1.glm, Hig.Lag2.glm))
higS = cbind(higS, higlda, higqda, higknn)
rownames(higS) = c("No Lag","Lag1","Lag2")
colnames(higS) = c("HIG GLM","HIG LDA","HIG QDA","HIG KNN") 
higS
#For no lag, the best success rate is KNN, followed by QDA, then LDA, then GLM.
#For lag1, the best success rate is KNN, followed by QDA, then LDA, then GLM.  
#for lag2, the best success rate is KNN, followed by QDA, then LDA, then GLM.

#***************************************************************
# TRV ANALYSIS
#***************************************************************

#Best Model Subset selection for each dependent variable (non Lag).
#Since trv_return is not lagged, we will not use Lag variables in this analysis
#Best model sbuset for trv_return
trv.return=regsubsets(trv_return ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + hig_return + pgr_return, data=clean.data, nvmax=9)
t(summary(trv.return)$which)
trv.return.summary=summary(trv.return)
Models=c(1:9)
CP=trv.return.summary$cp
plot(CP)
ADJ=trv.return.summary$adjr2
plot(ADJ)
BIC=trv.return.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(trv.return,which.min(trv.return.summary$bic))
coef(trv.return,which.max(trv.return.summary$adjr2))

#Results --> To measure trv_return use the 2 model order because it minimizes CP and BIC and maximizes ADJ.
#Selected variables for trv_return: hig_return, pgr_return

#Lag 1 anslysis
#Will will use Lag1 variables and non lags in this analysis
#Best model sbuset for hig_r_Lag1
trv.r.lag1=regsubsets(trv_r_Lag1 ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + hig_r_Lag1, data=clean.data, nvmax=12)
t(summary(trv.r.lag1)$which)
trv.r.lag1.summary=summary(trv.r.lag1)
Models=c(1:12)
CP=trv.r.lag1.summary$cp
plot(CP)
ADJ=trv.r.lag1.summary$adjr2
plot(ADJ)
BIC=trv.r.lag1.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(trv.r.lag1,which.min(trv.r.lag1.summary$bic))
coef(trv.r.lag1,which.max(trv.r.lag1.summary$adjr2))

##Results --> Use the 6 model order. At 6, CP is minimized and ADJ is maximized.
#buy_hit, trv_hit, pgr_return, hig_return, pgr_r_Lag1, hig_r_Lag1


#Lag2 analysis
#Will will use Lag2 variables. Lag 1 variables, and non lags in this analysis
#Best model sbuset for trv_r_Lag2
trv.r.lag2=regsubsets(trv_r_Lag2 ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2 + hig_r_Lag2, data=clean.data, nvmax=15)
t(summary(trv.r.lag2)$which)
trv.r.lag2.summary=summary(trv.r.lag2)
Models=c(1:15)
CP=trv.r.lag2.summary$cp
plot(CP)
ADJ=trv.r.lag2.summary$adjr2
plot(ADJ)
BIC=trv.r.lag2.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(trv.r.lag2,which.min(trv.r.lag2.summary$bic))
coef(trv.r.lag2,which.max(trv.r.lag2.summary$adjr2))

##Results --> Use the 6 model order. The 6 model order does not minimize CP (at 8), maximize ADJ (10), or minimize BIC (3), however, 6 is produces favorable results for CP and ADJ while still producing a low BIC
#bear_hit, trv_return, pgr_r_Lag1, trv_r_Lag1, hig_r_Lag1, pgr_r_Lag2

##Model with the lowest MSE (mean squared error) is the best model
#Linear
#Linear for trv_return
linear.fit=lm(trv_return~hig_return + pgr_return,data=TrainingSet)
summary(linear.fit)
#Both are significant
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$trv_return
trv.MSE.linear=mean(err^2)
trv.MSE.linear
#MSE of 0.0003390204

##MCCREE
pred.linear.trv.nl=data.frame(pred.linear)
colnames(pred.linear.trv.nl) = c('pred.linear.trv.nl')
err.linear.trv.nl = err

#Linear for trv_r_Lag1
linear.fit=lm(trv_r_Lag1~buy_hit + trv_hit + pgr_return + hig_return + pgr_r_Lag1 + hig_r_Lag1,data=TrainingSet)
summary(linear.fit)
#buy_hit and trv_hit are not significant
linear.fit=lm(trv_r_Lag1~pgr_return + hig_return + pgr_r_Lag1 + hig_r_Lag1,data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$trv_return
trv.Lag1.MSE.linear=mean(err^2)
trv.Lag1.MSE.linear
#MSE of 0.0007602453

##MCCREE
pred.linear.trv.l1=data.frame(pred.linear)
colnames(pred.linear.trv.l1) = c('pred.linear.trv.l1')
err.linear.trv.l1 = err

#Linear for trv_r_Lag2
linear.fit=lm(trv_r_Lag2~bear_hit + trv_return + pgr_r_Lag1 + trv_r_Lag1 + hig_r_Lag1 + pgr_r_Lag2, data=TrainingSet)
summary(linear.fit)
#only trv_return and pgr_r_Lag2 are significant
linear.fit=lm(trv_r_Lag2~trv_return + pgr_r_Lag2, data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$trv_return
trv.Lag2.MSE.linear=mean(err^2)
trv.Lag2.MSE.linear
#MSE of 0.000957788

##MCCREE
pred.linear.trv.l2=data.frame(pred.linear)
colnames(pred.linear.trv.l2) = c('pred.linear.trv.l2')
err.linear.trv.l2 = err


#Polynomial (Linear Basis Expansion with Restriction and Selection Methods)
#Polynomial for trv_return
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(trv_return~poly(hig_return + pgr_return,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$trv_return
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
trv.MSE.df = MSE.df[which.min(MSE.poly), ]
trv.MSE.df
#Lowest df MSE is 9 with MSE of 0.0003265171
summary(poly.fit)
#df of 9 is significant

##MCCREE
pred.poly.trv.nl=data.frame(pred.poly)
colnames(pred.poly.trv.nl) = c('pred.poly.trv.nl')
err.poly.trv.nl = err

#Polynomial for trv_r_Lag1
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(trv_r_Lag1~poly(
    pgr_return + hig_return + pgr_r_Lag1 + hig_r_Lag1,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$trv_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
trv.Lag1.MSE.df = MSE.df[which.min(MSE.poly), ]
trv.Lag1.MSE.df
#Lowest df is 8 with MSE of 0.0004269293
summary(poly.fit)
#df of 7 is the lowest MSE with significant df
trv.Lag1.MSE.df = MSE.df[df == 7]
trv.Lag1.MSE.df
#When df = 7, MSE is 0.0004327845

##MCCREE
pred.poly.trv.l1=data.frame(pred.poly)
colnames(pred.poly.trv.l1) = c('pred.poly.trv.l1')
err.poly.trv.l1 = err

#Polynomial for trv_r_Lag2
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(trv_r_Lag2~poly(trv_return + pgr_r_Lag2,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$trv_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
trv.Lag2.MSE.df = MSE.df[which.min(MSE.poly), ]
trv.Lag2.MSE.df
#Lowest df is 1 with MSE of 0.0005931612
summary(poly.fit)
#df of 1 is significant

##MCCREE
pred.poly.trv.l2=data.frame(pred.poly)
colnames(pred.poly.trv.l2) = c('pred.poly.trv.l2')
err.poly.trv.l2 = err


#Regression Tree with pruning
#Regression Tree with pruning for hig_return
tree.trv.return=tree(trv_return~.,TrainingSet)
plot(tree.trv.return)
text(tree.trv.return,pretty=0)

pred.tree=predict(tree.trv.return,ValidationSet)
tree.test=clean.data[-n, "trv_return"]
trvtree = mean((pred.tree - tree.test)^2)
trvtree
summary(tree.trv.return)
#Tree with 8 nodes has an MSE of 0.0004708743


cv.trv.return=cv.tree(tree.trv.return)
plot(cv.trv.return$size,cv.trv.return$dev,type='b')
cv.trv.return$size[which.min(cv.trv.return$dev)]
#We should prune the tree to 4 nodes

prune.trv.return=prune.tree(tree.trv.return,best=4)
plot(prune.trv.return)
text(prune.trv.return,pretty=0)
summary(prune.trv.return)

pred.trv.return.prune=predict(prune.trv.return,newdata=ValidationSet)
tree.test=clean.data[-n, "trv_return"]
trvprune = mean((pred.trv.return.prune - tree.test)^2)
trvprune
summary(prune.trv.return)
#At 4 notes the MSE is 0.0004195233

##MCCREE
pred.tree.trv.nl=data.frame(pred.trv.return.prune)
colnames(pred.tree.trv.nl) = c('pred.tree.trv.nl')
err=pred.trv.return.prune-ValidationSet$trv_r_Lag2
err.tree.trv.nl = err


#Regression Tree with pruning for trv_r_Lag1
tree.trv.return=tree(trv_r_Lag1~.,TrainingSet)
plot(tree.trv.return)
text(tree.trv.return,pretty=0)

pred.tree=predict(tree.trv.return,ValidationSet)
tree.test=clean.data[-n, "trv_r_Lag1"]
trvtree.Lag1 = mean((pred.tree - tree.test)^2)
trvtree.Lag1
summary(tree.trv.return)
#Tree with 8 nodes has an MSE of 0.0005259598

cv.trv.return=cv.tree(tree.trv.return)
plot(cv.trv.return$size,cv.trv.return$dev,type='b')
cv.trv.return$size[which.min(cv.trv.return$dev)]
#The tree should be pruned to 8 notes (no change)

prune.trv.return=prune.tree(tree.trv.return,best=8)
plot(prune.trv.return)
text(prune.trv.return,pretty=0)
summary(prune.trv.return)

pred.trv.return.prune=predict(prune.trv.return,newdata=ValidationSet)
tree.test=clean.data[-n, "trv_r_Lag1"]
trvprune.Lag1 = mean((pred.trv.return.prune - tree.test)^2)
trvprune.Lag1
summary(prune.trv.return)
#Nodes did not change with pruning so the MSE remains 0.0005259598

##MCCREE
pred.tree.trv.l1=data.frame(pred.trv.return.prune)
colnames(pred.tree.trv.l1) = c('pred.tree.trv.l1')
err=pred.trv.return.prune-ValidationSet$trv_r_Lag2
err.tree.trv.l1 = err


#Regression Tree with pruning for trv_r_Lag2
tree.trv.return=tree(trv_r_Lag2~.,TrainingSet)
plot(tree.trv.return)
text(tree.trv.return,pretty=0)

pred.tree=predict(tree.trv.return,ValidationSet)
tree.test=clean.data[-n, "trv_r_Lag2"]
trvtree.Lag2 = mean((pred.tree - tree.test)^2)
trvtree.Lag2
summary(tree.trv.return)
#Tree with 9 nodes has an MSE of 0.0003778998

cv.trv.return=cv.tree(tree.trv.return)
plot(cv.trv.return$size,cv.trv.return$dev,type='b')
cv.trv.return$size[which.min(cv.trv.return$dev)]
#Cross validation says we should prune the tree to 9 nodes

prune.trv.return=prune.tree(tree.trv.return,best=9)
plot(prune.trv.return)
text(prune.trv.return,pretty=0)
summary(prune.trv.return)

pred.trv.return.prune=predict(prune.trv.return,newdata=ValidationSet)
tree.test=clean.data[-n, "trv_r_Lag2"]
trvprune.Lag2 = mean((pred.trv.return.prune - tree.test)^2)
trvprune.Lag2
summary(prune.trv.return)
#At 9 nodes, the MSE is remains 0.0003778998

##MCCREE
pred.tree.trv.l2=data.frame(pred.trv.return.prune)
colnames(pred.tree.trv.l2) = c('pred.tree.trv.l2')
err=pred.trv.return.prune-ValidationSet$trv_r_Lag2
err.tree.trv.l2 = err

#Table of trv MSE
trvtree = as.data.frame(c(trvtree, trvtree.Lag1, trvtree.Lag2))
trvprune = as.data.frame(c(trvprune, trvprune.Lag1, trvprune.Lag2))
trvdf = as.data.frame(c(trv.MSE.df[1], trv.Lag1.MSE.df[1], trv.Lag2.MSE.df[1]))
trvMSE = as.data.frame(c(trv.MSE.linear, trv.Lag1.MSE.linear, trv.Lag2.MSE.linear))
trvMSE = cbind(trvMSE,trvdf,trvtree,trvprune)
rownames(trvMSE) = c("No Lag","Lag1","Lag2")
colnames(trvMSE) = c("TRV Linear","TRV Poly","TRV Tree","TRV Prune") 
trvMSE
#For no lag, poly is better
#For lag1, tree/prune are better
#for lag2, tree/prune are better

#Analyze variable statistical Significance
#For trv_return
trv.return.d.glm=glm(trv_return.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + hig_return + pgr_return, data=clean.data, family = binomial)
summary(trv.return.d.glm,digits=4)
#Only hig_return + pgr_return are significant for glm.

#For trv_r_Lag1
trv.r.Lag1.d.glm=glm(trv_r_Lag1.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + hig_r_Lag1 + pgr_r_Lag1, data=clean.data, family = binomial)
summary(trv.r.Lag1.d.glm,digits=4)
#Only hig_r_Lag1 + pgr_r_Lag1 are significant

#For trv_r_Lag2
trv.r.Lag2.d.glm=glm(trv_r_Lag2.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + trv_r_Lag1 + pgr_r_Lag1 + hig_r_Lag1 + hig_r_Lag2 + pgr_r_Lag2, data=clean.data, family = binomial)
summary(trv.r.Lag2.d.glm,digits=4)
#Only hig_return + hig_r_Lag2 + pgr_r_Lag2 are significant

#Logistic Regression
#Logistc Regresssion for trv_return.d.glm
trv_return.d.Val = ValidationSet$trv_return.d

trv.r.d.glm=glm(trv_return.d~hig_return + pgr_return,data=TrainingSet, family=binomial)
trv.r.d.probs=predict(trv.r.d.glm,data=ValidationSet,type="response")

z=length(trv_return.d.Val)
trv.r.d.pred=rep("Up",z)
trv.r.d.pred[trv.r.d.probs>.5]="Down"
table(trv.r.d.pred,trv_return.d.Val)
Trvglm = round(mean(trv.r.d.pred==trv_return.d.Val)*100,4)
Trvglm
#Success rate of 45.1737%


#Logistic Regression for trv.r.Lag1.d
trv_return.d.Val = ValidationSet$trv_r_Lag1.d

trv.r.d.glm=glm(trv_r_Lag1.d~hig_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
trv.r.d.probs=predict(trv.r.d.glm,data=ValidationSet,type="response")

z=length(trv_return.d.Val)
trv.r.d.pred=rep("Up",z)
trv.r.d.pred[trv.r.d.probs>.5]="Down"
table(trv.r.d.pred,trv_return.d.Val)
Trv.Lag1.glm = round(mean(trv.r.d.pred==trv_return.d.Val)*100,4)
Trv.Lag1.glm
#Success rate of 45.5598%


#Logistic Regression for trv.r.Lag2.d
trv_return.d.Val = ValidationSet$trv_r_Lag2.d

trv.r.d.glm=glm(trv_r_Lag2.d~hig_return + hig_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
trv.r.d.probs=predict(trv.r.d.glm,data=ValidationSet,type="response")

z=length(trv_return.d.Val)
trv.r.d.pred=rep("Up",z)
trv.r.d.pred[trv.r.d.probs>.5]="Down"
table(trv.r.d.pred,trv_return.d.Val)
Trv.Lag2.glm = round(mean(trv.r.d.pred==trv_return.d.Val)*100,4)
Trv.Lag2.glm
#Success rate of 44.7876%


#LDA
#Use variables identified as statistically significant in glm analysis

#LDA for trv_return.d
trv_return.d.Val = ValidationSet$trv_return.d

trv.r.d.lda.fit=lda(trv_return.d~hig_return + pgr_return,data=TrainingSet, family=binomial)
trv.r.d.lda.pred=predict(trv.r.d.lda.fit,ValidationSet)
trv.r.d.lda.class=trv.r.d.lda.pred$class
table(trv.r.d.lda.class,trv_return.d.Val)
trvLDA = round(mean(trv.r.d.lda.class==trv_return.d.Val)*100, 4)
trvLDA
#Success rate of 75.2896%


#LDA for trv.r.Lag1.d
trv_return.d.Val = ValidationSet$trv_r_Lag1.d

trv.r.d.lda.fit=lda(trv_r_Lag1.d ~hig_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
trv.r.d.lda.pred=predict(trv.r.d.lda.fit,ValidationSet)
trv.r.d.lda.class=trv.r.d.lda.pred$class
table(trv.r.d.lda.class,trv_return.d.Val)
trv.Lag1.LDA = round(mean(trv.r.d.lda.class==trv_return.d.Val)*100, 4)
trv.Lag1.LDA
#Success rate of 76.4479%

#LDA for trv.r.Lag2.d
trv_return.d.Val = ValidationSet$trv_r_Lag2.d

trv.r.d.lda.fit=lda(trv_r_Lag2~hig_return + hig_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
trv.r.d.lda.pred=predict(trv.r.d.lda.fit,ValidationSet)
trv.r.d.lda.class=trv.r.d.lda.pred$class
table(trv.r.d.lda.class,trv_return.d.Val)
trv.Lag2.LDA = round(mean(trv.r.d.lda.class==trv_return.d.Val)*100, 4)
trv.Lag2.LDA
#Success rate of 50.9653%


#QDA
#using significant variables outlined in GLM analysis

#QDA for trv_return.d
trv_return.d.Val = ValidationSet$trv_return.d

trv.r.d.qda.fit=qda(trv_return.d~hig_return + pgr_return,data=TrainingSet, family=binomial)
trv.r.d.qda.pred=predict(trv.r.d.qda.fit,ValidationSet)
trv.r.d.qda.class=trv.r.d.qda.pred$class
table(trv.r.d.qda.class,trv_return.d.Val)
trv.QDA = round(mean(trv.r.d.qda.class==trv_return.d.Val)*100, 4)
trv.QDA
#Success rate of 75.6757%

#QDA for trv.r.Lag1.d
trv_return.d.Val = ValidationSet$trv_r_Lag1.d

trv.r.d.qda.fit=qda(trv_r_Lag1.d ~hig_r_Lag1 + pgr_r_Lag1,data=TrainingSet, family=binomial)
trv.r.d.qda.pred=predict(trv.r.d.qda.fit,ValidationSet)
trv.r.d.qda.class=trv.r.d.qda.pred$class
table(trv.r.d.qda.class,trv_return.d.Val)
trv.Lag1.QDA = round(mean(trv.r.d.qda.class==trv_return.d.Val)*100, 4)
trv.Lag1.QDA
#Success rate of 76.834%

#QDA for trv.r.Lag2.d
trv_return.d.Val = ValidationSet$trv_r_Lag2.d


trv.r.d.qda.fit=qda(trv_r_Lag2.d~hig_return + hig_r_Lag2 + pgr_r_Lag2,data=TrainingSet, family=binomial)
trv.r.d.qda.pred=predict(trv.r.d.qda.fit,ValidationSet)
trv.r.d.qda.class=trv.r.d.qda.pred$class
table(trv.r.d.qda.class,trv_return.d.Val)
trv.Lag2.QDA = round(mean(trv.r.d.qda.class==trv_return.d.Val)*100, 4)
trv.Lag2.QDA
#Success rate of 57.9151%


#K nearest neighbors
#using significant variables defined in GLM analysis


#KNN for trv_return.d
#No Lag variables: hig_return, pgr_return
vars = c("hig_return","pgr_return")
var2 = c("trv_return.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  trv.knn.pred=knn(train.var,test.var,train.dep$trv_return.d,k = i)
  KNN.Multi[i]=mean(trv.knn.pred==test.dep$trv_return.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
trv.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
trv.knn
trv.knn.NoLag = round(mean(trv.knn[1])*100, 4)
trv.knn.NoLag

#Success rate of 78.7645% when K = 17


#KNN for trv_r_Lag1.d
vars = c("hig_r_Lag1","pgr_r_Lag1")
var2 = c("trv_r_Lag1.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  trv.knn.pred=knn(train.var,test.var,train.dep$trv_r_Lag1.d,k = i)
  KNN.Multi[i]=mean(trv.knn.pred==test.dep$trv_r_Lag1.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
trv.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
trv.knn
trv.Lag1.knn = round(mean(trv.knn[1])*100, 4)
trv.Lag1.knn
#Success rate of  77.9923% when k = 40


#KNN for trv.r.Lag2.d
vars = c("hig_return","hig_r_Lag2","pgr_r_Lag2")
var2 = c("trv_r_Lag2.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  trv.knn.pred=knn(train.var,test.var,train.dep$trv_r_Lag2.d,k = i)
  KNN.Multi[i]=mean(trv.knn.pred==test.dep$trv_r_Lag2.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
trv.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
trv.knn
trv.Lag2.knn = round(mean(trv.knn[1])*100, 4)
trv.Lag2.knn
#Success rate of 77.2201% when k = 28

#Table of trv success
trvknn = as.data.frame(c(trv.knn.NoLag,trv.Lag1.knn,trv.Lag2.knn))
trvqda = as.data.frame(c(trv.QDA, trv.Lag1.QDA, trv.Lag2.QDA))
trvlda = as.data.frame(c(trvLDA, trv.Lag1.LDA, trv.Lag2.LDA))
trvS = as.data.frame(c(Trvglm, Trv.Lag1.glm, Trv.Lag2.glm))
trvS = cbind(trvS, trvlda, trvqda, trvknn)
rownames(trvS) = c("No Lag","Lag1","Lag2")
colnames(trvS) = c("TRV GLM","TRV LDA","TRV QDA","TRV KNN") 
trvS

#For no lag, the best success rate is KNN, followed by QDA, then LDA, then GLM.
#For lag1, the best success rate is KNN, followed by QDA, then LDA, then GLM.  
#for lag2, the best success rate is KNN, followed by QDA, then LDA, then GLM.

#***************************************************************
# PGR ANALYSIS
#***************************************************************

#Best Model Subset selection for each dependent variable (non Lag).
#Since pgr_return is not lagged, we will not use Lag variables in this analysis
#Best model sbuset for pgr_return
pgr.return=regsubsets(pgr_return ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + hig_return + trv_return, data=clean.data, nvmax=9)
t(summary(pgr.return)$which)
pgr.return.summary=summary(pgr.return)
Models=c(1:9)
CP=pgr.return.summary$cp
plot(CP)
ADJ=pgr.return.summary$adjr2
plot(ADJ)
BIC=pgr.return.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(pgr.return,which.min(pgr.return.summary$bic))
coef(pgr.return,which.max(pgr.return.summary$adjr2))

#Results --> To measure pgr_return use the 3. It does not minimize CP (4), maximize ADJ (7), or minimize BIC (2). However, 3 is still close to the min CP and Max ADJ, while also being closer to the min BIC.
#Selected variables for pgr_return: hig_hit + hig_return + trv_return

#Lag 1 anslysis
#Will will use Lag1 variables and non lags in this analysis
#Best model sbuset for hig_r_Lag1
pgr.r.lag1=regsubsets(pgr_r_Lag1 ~  + buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + trv_r_Lag1 + hig_r_Lag1, data=clean.data, nvmax=12)
t(summary(pgr.r.lag1)$which)
pgr.r.lag1.summary=summary(pgr.r.lag1)
Models=c(1:12)
CP=pgr.r.lag1.summary$cp
plot(CP)
ADJ=pgr.r.lag1.summary$adjr2
plot(ADJ)
BIC=pgr.r.lag1.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(pgr.r.lag1,which.min(pgr.r.lag1.summary$bic))
coef(pgr.r.lag1,which.max(pgr.r.lag1.summary$adjr2))

##Results --> Use the 5 model order because it is a reasonable middle ground between the minimum CP (5), Max ADJ (6), and min BIC (4)
#trv_hit + trv_return + pgr_return + trv_r_Lag1 + hig_r_Lag1


#Lag2 analysis
#Will will use Lag2 variables. Lag 1 variables, and non lags in this analysis
#Best model sbuset for pgr_r_Lag2
pgr.r.lag2=regsubsets(pgr_r_Lag2 ~buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return + hig_return + pgr_r_Lag1 + trv_r_Lag1 + hig_r_Lag1 + trv_r_Lag2 + hig_r_Lag2, data=clean.data, nvmax=15)
t(summary(pgr.r.lag2)$which)
pgr.r.lag2.summary=summary(pgr.r.lag2)
Models=c(1:15)
CP=pgr.r.lag2.summary$cp
plot(CP)
ADJ=pgr.r.lag2.summary$adjr2
plot(ADJ)
BIC=pgr.r.lag2.summary$bic
plot(BIC)
Models.Var=cbind(Models,CP,ADJ,BIC)
Models.Var
Models.Var[which.min(CP), ]
Models.Var[which.min(BIC), ]
Models.Var[which.max(ADJ), ]
coef(pgr.r.lag2,which.min(pgr.r.lag2.summary$bic))
coef(pgr.r.lag2,which.max(pgr.r.lag2.summary$adjr2))

##Results --> Use the 8 model order. It is a middle ground between the minimum CP (at 8), maximum ADJ (10), and minimum BIC (6)
#sell_hit + bull_hit + trv_return + pgr_return + pgr_r_Lag1 + trv_r_Lag1 + trv_r_Lag2 + hig_r_Lag2


##Model with the lowest MSE (mean squared error) is the best model
#Linear
#Linear for pgr_return
linear.fit=lm(pgr_return~hig_hit + hig_return + trv_return,data=TrainingSet)
summary(linear.fit)
#hig_hit is not significant
linear.fit=lm(pgr_return~hig_return + trv_return,data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$pgr_return
pgr.MSE.linear=mean(err^2)
pgr.MSE.linear
#MSE of 0.0003781347

##MCCREE
pred.linear.pgr.nl=data.frame(pred.linear)
colnames(pred.linear.pgr.nl) = c('pred.linear.pgr.nl')
err.linear.pgr.nl = err

#Linear for pgr_r_Lag1
linear.fit=lm(pgr_r_Lag1~trv_hit + trv_return + pgr_return + trv_r_Lag1 + hig_r_Lag1,data=TrainingSet)
summary(linear.fit)
#all except trv_hit are significant
linear.fit=lm(pgr_r_Lag1~trv_return + pgr_return + trv_r_Lag1 + hig_r_Lag1,data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$pgr_return
pgr.Lag1.MSE.linear=mean(err^2)
pgr.Lag1.MSE.linear
#MSE of 0.0009305973

##MCCREE
pred.linear.pgr.l1=data.frame(pred.linear)
colnames(pred.linear.pgr.l1) = c('pred.linear.pgr.l1')
err.linear.pgr.l1 = err

#Linear for pgr_r_Lag2
linear.fit=lm(pgr_r_Lag2~ sell_hit + bull_hit + trv_return + pgr_return + pgr_r_Lag1 + trv_r_Lag1 + trv_r_Lag2 + hig_r_Lag2, data=TrainingSet)
summary(linear.fit)
#all are significant except trv_return
linear.fit=lm(pgr_r_Lag2~ sell_hit + bull_hit + pgr_return + pgr_r_Lag1 + trv_r_Lag1 + trv_r_Lag2 + hig_r_Lag2, data=TrainingSet)
pred.linear=predict(linear.fit,newdata=ValidationSet)
err=pred.linear-ValidationSet$pgr_return
pgr.Lag2.MSE.linear=mean(err^2)
pgr.Lag2.MSE.linear
#MSE of 0.0005618122

##MCCREE
pred.linear.pgr.l2=data.frame(pred.linear)
colnames(pred.linear.pgr.l2) = c('pred.linear.pgr.l2')
err.linear.pgr.l2 = err


#Polynomial (Linear Basis Expansion with Restriction and Selection Methods)
#Polynomial for pgr_return
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(pgr_return~poly(hig_return + trv_return,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$pgr_return
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
pgr.MSE.df = MSE.df[which.min(MSE.poly), ]
pgr.MSE.df
#Lowest df MSE is 10 with MSE of 0.0003665792
summary(poly.fit)
#Df of 10 is significant

##MCCREE
pred.poly.pgr.nl=data.frame(pred.poly)
colnames(pred.poly.pgr.nl) = c('pred.poly.pgr.nl')
err.poly.pgr.nl = err


#Polynomial for pgr_r_Lag1
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(pgr_r_Lag1~poly(trv_return + pgr_return + trv_r_Lag1 + hig_r_Lag1,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$pgr_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
pgr.Lag1.MSE.df = MSE.df[which.min(MSE.poly), ]
pgr.Lag1.MSE.df
#Lowest df is 2 with MSE of 0.0004973148
summary(poly.fit)
#Significant df with lowest MSE is 1
pgr.Lag1.MSE.df = MSE.df[df == 1]
pgr.Lag1.MSE.df
#MSE when df = 1 is 0.0005027214

##MCCREE
pred.poly.pgr.l1=data.frame(pred.poly)
colnames(pred.poly.pgr.l1) = c('pred.poly.pgr.l1')
err.poly.pgr.l1 = err


#Polynomial for pgr_r_Lag2
MSE.poly=rep(NA,10)
for (k in 1:10)
{
  poly.fit=lm(pgr_r_Lag2~poly(sell_hit + bull_hit + pgr_return + pgr_r_Lag1 + trv_r_Lag1 + trv_r_Lag2 + hig_r_Lag2,k),data=TrainingSet)
  pred.poly=predict(poly.fit,newdata=ValidationSet)
  err=pred.poly-ValidationSet$pgr_r_Lag1
  MSE.poly[k]=mean(err^2)
}
df=c(1:10)
MSE.df=cbind(MSE.poly,df)
plot(df, MSE.poly, lwd=1,col="black")
lines(MSE.poly,lwd=1,col="black")
MSE.df[which.min(MSE.poly), ]
pgr.Lag2.MSE.df = MSE.df[which.min(MSE.poly), ]
pgr.Lag2.MSE.df
#Lowest df is 1 with MSE of 0.0006241012
summary(poly.fit)
#only df of 3 is significant
pgr.Lag2.MSE.df = MSE.df[df == 3]
pgr.Lag2.MSE.df
#MSE of 0.001215061

##MCCREE
pred.poly.pgr.l2=data.frame(pred.poly)
colnames(pred.poly.pgr.l2) = c('pred.poly.pgr.l2')
err.poly.pgr.l2 = err


#Regression Tree with pruning
#Regression Tree with pruning for pgr_return
tree.pgr.return=tree(pgr_return~.,TrainingSet)
plot(tree.pgr.return)
text(tree.pgr.return,pretty=0)

pred.tree=predict(tree.pgr.return,ValidationSet)
tree.test=clean.data[-n, "pgr_return"]
pgrtree = mean((pred.tree - tree.test)^2)
pgrtree
summary(tree.pgr.return)
#Tree with 9 nodes has an MSE of 0.001159224

cv.pgr.return=cv.tree(tree.pgr.return)
plot(cv.pgr.return$size,cv.pgr.return$dev,type='b')
cv.pgr.return$size[which.min(cv.pgr.return$dev)]
#We should prune the tree to 4 nodes

prune.pgr.return=prune.tree(tree.pgr.return,best=4)
plot(prune.pgr.return)
text(prune.pgr.return,pretty=0)
summary(prune.pgr.return)

pred.pgr.return.prune=predict(prune.pgr.return,newdata=ValidationSet)
tree.test=clean.data[-n, "pgr_return"]
pgrprune = mean((pred.pgr.return.prune - tree.test)^2)
pgrprune
summary(prune.pgr.return)
#At 4 notes the MSE is 0.00101616

##MCCREE
pred.tree.pgr.nl=data.frame(pred.pgr.return.prune)
colnames(pred.tree.pgr.nl) = c('pred.tree.pgr.nl')
err=pred.pgr.return.prune-ValidationSet$pgr_r_Lag2
err.tree.pgr.nl = err

#Regression Tree with pruning for pgr_r_Lag1
tree.pgr.return=tree(pgr_r_Lag1~.,TrainingSet)
plot(tree.pgr.return)
text(tree.pgr.return,pretty=0)

pred.tree=predict(tree.pgr.return,ValidationSet)
tree.test=clean.data[-n, "pgr_r_Lag1"]
pgrtree.Lag1 = mean((pred.tree - tree.test)^2)
pgrtree.Lag1
summary(tree.pgr.return)
#Tree with 7 nodes has an MSE of 0.0002376342

cv.pgr.return=cv.tree(tree.pgr.return)
plot(cv.pgr.return$size,cv.pgr.return$dev,type='b')
cv.pgr.return$size[which.min(cv.pgr.return$dev)]
#The tree should be pruned to 7 notes (no change)

prune.pgr.return=prune.tree(tree.pgr.return,best=7)
plot(prune.pgr.return)
text(prune.pgr.return,pretty=0)
summary(prune.pgr.return)

pred.pgr.return.prune=predict(prune.pgr.return,newdata=ValidationSet)
tree.test=clean.data[-n, "pgr_r_Lag1"]
pgrprune.Lag1 = mean((pred.pgr.return.prune - tree.test)^2)
pgrprune.Lag1
summary(prune.pgr.return)
#Nodes did not change with pruning so the MSE remains 0.0002376342

##MCCREE
pred.tree.pgr.l1=data.frame(pred.pgr.return.prune)
colnames(pred.tree.pgr.l1) = c('pred.tree.pgr.l1')
err=pred.pgr.return.prune-ValidationSet$pgr_r_Lag2
err.tree.pgr.l1 = err

#Regression Tree with pruning for pgr_r_Lag2
tree.pgr.return=tree(pgr_r_Lag2~.,TrainingSet)
plot(tree.pgr.return)
text(tree.pgr.return,pretty=0)

pred.tree=predict(tree.pgr.return,ValidationSet)
tree.test=clean.data[-n, "pgr_r_Lag2"]
pgrtree.Lag2 = mean((pred.tree - tree.test)^2)
pgrtree.Lag2
summary(tree.pgr.return)
#Tree with 8 nodes has an MSE of 0.000260926

cv.pgr.return=cv.tree(tree.pgr.return)
plot(cv.pgr.return$size,cv.pgr.return$dev,type='b')
cv.pgr.return$size[which.min(cv.pgr.return$dev)]
#Cross validation says we should prune the tree to 4 nodes

prune.pgr.return=prune.tree(tree.pgr.return,best=4)
plot(prune.pgr.return)
text(prune.pgr.return,pretty=0)
summary(prune.pgr.return)

pred.pgr.return.prune=predict(prune.pgr.return,newdata=ValidationSet)
tree.test=clean.data[-n, "pgr_r_Lag2"]
pgrprune.Lag2 = mean((pred.pgr.return.prune - tree.test)^2)
pgrprune.Lag2
summary(prune.pgr.return)
#At 4 nodes, the MSE is remains 0.0002890311

##MCCREE
pred.tree.pgr.l2=data.frame(pred.pgr.return.prune)
colnames(pred.tree.pgr.l2) = c('pred.tree.pgr.l2')
err=pred.pgr.return.prune-ValidationSet$pgr_r_Lag2
err.tree.pgr.l2 = err


#Table of pgr MSE
pgrtree = as.data.frame(c(pgrtree, pgrtree.Lag1, pgrtree.Lag2))
pgrprune = as.data.frame(c(pgrprune, pgrprune.Lag1, pgrprune.Lag2))
pgrdf = as.data.frame(c(pgr.MSE.df[1], pgr.Lag1.MSE.df[1], pgr.Lag2.MSE.df[1]))
pgrMSE = as.data.frame(c(pgr.MSE.linear, pgr.Lag1.MSE.linear, pgr.Lag2.MSE.linear))
pgrMSE = cbind(pgrMSE,pgrdf,pgrtree,pgrprune)
rownames(pgrMSE) = c("No Lag","Lag1","Lag2")
colnames(pgrMSE) = c("PGR Linear","PGR Poly","PGR Tree","PGR Prune") 
pgrMSE

#For no lag, poly is better
#For lag1, linear is better
#for lag2, poly is better


#Analyze variable statistical Significance
#For pgr_return
pgr.return.d.glm=glm(pgr_return.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + hig_return + trv_return, data=clean.data, family = binomial)
summary(pgr.return.d.glm,digits=4)
#Only hig_return + trv_return are significant for glm.

#For pgr_r_Lag1
pgr.r.Lag1.d.glm=glm(pgr_r_Lag1.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + hig_r_Lag1 + trv_r_Lag1, data=clean.data, family = binomial)
summary(pgr.r.Lag1.d.glm,digits=4)
#Only pgr_return + hig_r_Lag1 + trv_r_Lag1 are significant

#For pgr_r_Lag2
pgr.r.Lag2.d.glm=glm(pgr_r_Lag2.d~ buy_hit + sell_hit + bull_hit + bear_hit + hig_hit + pgr_hit + trv_hit + trv_return + pgr_return +hig_return + trv_r_Lag1 + pgr_r_Lag1 + hig_r_Lag1 + hig_r_Lag2 + trv_r_Lag2, data=clean.data, family = binomial)
summary(pgr.r.Lag2.d.glm,digits=4)
#Only bull_hit + pgr_r_Lag1 + hig_r_Lag2 + trv_r_Lag2 are significant

#Logistic Regression
#Logistc Regresssion for pgr_return.d.glm
pgr_return.d.Val = ValidationSet$pgr_return.d

pgr.r.d.glm=glm(pgr_return.d~hig_return + trv_return,data=TrainingSet, family=binomial)
pgr.r.d.probs=predict(pgr.r.d.glm,data=ValidationSet,type="response")

z=length(pgr_return.d.Val)
pgr.r.d.pred=rep("Up",z)
pgr.r.d.pred[pgr.r.d.probs>.5]="Down"
table(pgr.r.d.pred,pgr_return.d.Val)
Pgrglm = round(mean(pgr.r.d.pred==pgr_return.d.Val)*100,4)
Pgrglm

#Success rate of 44.7876%


#Logistic Regression for pgr.r.Lag1.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag1.d

pgr.r.d.glm=glm(pgr_r_Lag1.d~ pgr_return + hig_r_Lag1 + trv_r_Lag1,data=TrainingSet, family=binomial)
pgr.r.d.probs=predict(pgr.r.d.glm,data=ValidationSet,type="response")

z=length(pgr_return.d.Val)
pgr.r.d.pred=rep("Up",z)
pgr.r.d.pred[pgr.r.d.probs>.5]="Down"
table(pgr.r.d.pred,pgr_return.d.Val)
Pgr.Lag1.glm = round(mean(pgr.r.d.pred==pgr_return.d.Val)*100,4)
Pgr.Lag1.glm

#Success rate of 44.7876%


#Logistic Regression for pgr.r.Lag2.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag2.d

pgr.r.d.glm=glm(pgr_r_Lag2.d~pgr_return + hig_r_Lag1 + trv_r_Lag1,data=TrainingSet, family=binomial)
pgr.r.d.probs=predict(pgr.r.d.glm,data=ValidationSet,type="response")

z=length(pgr_return.d.Val)
pgr.r.d.pred=rep("Up",z)
pgr.r.d.pred[pgr.r.d.probs>.5]="Down"
table(pgr.r.d.pred,pgr_return.d.Val)
Pgr.Lag2.glm = round(mean(pgr.r.d.pred==pgr_return.d.Val)*100,4)
Pgr.Lag2.glm
#Success rate of 47.1042%


#LDA
#Use variables identified as statistically significant in glm analysis

#LDA for pgr_return.d
pgr_return.d.Val = ValidationSet$pgr_return.d

pgr.r.d.lda.fit=lda(pgr_return.d~hig_return + trv_return,data=TrainingSet, family=binomial)
pgr.r.d.lda.pred=predict(pgr.r.d.lda.fit,ValidationSet)
pgr.r.d.lda.class=pgr.r.d.lda.pred$class
table(pgr.r.d.lda.class,pgr_return.d.Val)
pgrLDA = round(mean(pgr.r.d.lda.class==pgr_return.d.Val)*100, 4)
pgrLDA
#Success rate of 66.4093%


#LDA for pgr.r.Lag1.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag1.d

pgr.r.d.lda.fit=lda(pgr_r_Lag1.d ~pgr_return + hig_r_Lag1 + trv_r_Lag1,data=TrainingSet, family=binomial)
pgr.r.d.lda.pred=predict(pgr.r.d.lda.fit,ValidationSet)
pgr.r.d.lda.class=pgr.r.d.lda.pred$class
table(pgr.r.d.lda.class,pgr_return.d.Val)
pgr.Lag1.LDA = round(mean(pgr.r.d.lda.class==pgr_return.d.Val)*100, 4)
pgr.Lag1.LDA
#Success rate of 67.9537%


#LDA for pgr.r.Lag2.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag2.d

pgr.r.d.lda.fit=lda(pgr_r_Lag2~ bull_hit + pgr_r_Lag1 + hig_r_Lag2 + trv_r_Lag2,data=TrainingSet, family=binomial)
pgr.r.d.lda.pred=predict(pgr.r.d.lda.fit,ValidationSet)
pgr.r.d.lda.class=pgr.r.d.lda.pred$class
table(pgr.r.d.lda.class,pgr_return.d.Val)
pgr.Lag2.LDA = round(mean(pgr.r.d.lda.class==pgr_return.d.Val)*100, 4)
pgr.Lag2.LDA
#Success rate of 49.4208%


#QDA
#using significant variables outlined in GLM analysis

#QDA for pgr_return.d
pgr_return.d.Val = ValidationSet$pgr_return.d

pgr.r.d.qda.fit=qda(pgr_return.d~hig_return + trv_return,data=TrainingSet, family=binomial)
pgr.r.d.qda.pred=predict(pgr.r.d.qda.fit,ValidationSet)
pgr.r.d.qda.class=pgr.r.d.qda.pred$class
table(pgr.r.d.qda.class,pgr_return.d.Val)
pgr.QDA = round(mean(pgr.r.d.qda.class==pgr_return.d.Val)*100, 4)
pgr.QDA
#Success rate of 69.112%


#QDA for pgr.r.Lag1.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag1.d

pgr.r.d.qda.fit=qda(pgr_r_Lag1.d ~pgr_return + hig_r_Lag1 + trv_r_Lag1,data=TrainingSet, family=binomial)
pgr.r.d.qda.pred=predict(pgr.r.d.qda.fit,ValidationSet)
pgr.r.d.qda.class=pgr.r.d.qda.pred$class
table(pgr.r.d.qda.class,pgr_return.d.Val)
pgr.Lag1.QDA = round(mean(pgr.r.d.qda.class==pgr_return.d.Val)*100, 4)
pgr.Lag1.QDA

#Success rate of 61.0039%

#QDA for pgr.r.Lag2.d
pgr_return.d.Val = ValidationSet$pgr_r_Lag2.d


pgr.r.d.qda.fit=qda(pgr_r_Lag2.d~bull_hit + pgr_r_Lag1 + hig_r_Lag2 + trv_r_Lag2,data=TrainingSet, family=binomial)
pgr.r.d.qda.pred=predict(pgr.r.d.qda.fit,ValidationSet)
pgr.r.d.qda.class=pgr.r.d.qda.pred$class
table(pgr.r.d.qda.class,pgr_return.d.Val)
pgr.Lag2.QDA = round(mean(pgr.r.d.qda.class==pgr_return.d.Val)*100, 4)
pgr.Lag2.QDA
#Success rate of 69.4981%



#K nearest neighbors
#using significant variables defined in GLM analysis


#KNN for pgr_return.d
#No Lag variables: hig_return, trv_return
vars = c("hig_return","trv_return")
var2 = c("pgr_return.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  trv.knn.pred=knn(train.var,test.var,train.dep$pgr_return.d,k = i)
  KNN.Multi[i]=mean(trv.knn.pred==test.dep$pgr_return.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
pgr.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
pgr.knn
pgr.knn.NoLag = round(mean(pgr.knn[1])*100, 4)
pgr.knn.NoLag

#Success rate of 72.2008% when K = 28


#KNN for pgr_r_Lag1.d
#pgr_return + hig_r_Lag1 + trv_r_Lag1
vars = c("pgr_return","hig_r_Lag1","trv_r_Lag1")
var2 = c("pgr_r_Lag1.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  pgr.knn.pred=knn(train.var,test.var,train.dep$pgr_r_Lag1.d,k = i)
  KNN.Multi[i]=mean(pgr.knn.pred==test.dep$pgr_r_Lag1.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
pgr.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
pgr.knn
pgr.Lag1.knn = round(mean(pgr.knn[1])*100, 4)
pgr.Lag1.knn
#Success rate of  70.2703% when k = 15


#KNN for pgr.r.Lag2.d
#bull_hit + pgr_r_Lag1 + hig_r_Lag2 + trv_r_Lag2
vars = c("bull_hit","pgr_r_Lag1","hig_r_Lag2","trv_r_Lag2")
var2 = c("pgr_r_Lag2.d")
train.var = TrainingSet[vars]
test.var = ValidationSet[vars]
test.var = test.var[vars]
train.dep = TrainingSet[var2]
test.dep = ValidationSet[var2]

KNN.Multi=rep(NA,50)
for (i in 1:50)
{
  set.seed(1)
  pgr.knn.pred=knn(train.var,test.var,train.dep$pgr_r_Lag2.d,k = i)
  KNN.Multi[i]=mean(pgr.knn.pred==test.dep$pgr_r_Lag2.d)
}
KN=c(1:50)
KNN.Multi.KN=cbind(KNN.Multi,KN)
pgr.knn = KNN.Multi.KN[which.max(KNN.Multi), ]
pgr.knn
pgr.Lag2.knn = round(mean(pgr.knn[1])*100, 4)
pgr.Lag2.knn
#Success rate of 65.251% when k = 32


#Table of pgr success
pgrknn = as.data.frame(c(pgr.knn.NoLag,pgr.Lag1.knn,pgr.Lag2.knn))
pgrqda = as.data.frame(c(pgr.QDA, pgr.Lag1.QDA, pgr.Lag2.QDA))
pgrlda = as.data.frame(c(pgrLDA, pgr.Lag1.LDA, pgr.Lag2.LDA))
pgrS = as.data.frame(c(Pgrglm, Pgr.Lag1.glm, Pgr.Lag2.glm))
pgrS = cbind(pgrS, pgrlda, pgrqda, pgrknn)
rownames(pgrS) = c("No Lag","Lag1","Lag2")
colnames(pgrS) = c("PGR GLM","PGR LDA","PGR QDA","PGR KNN") 
pgrS


#For no lag, the best success rate is KNN, followed by QDA, then LDA, then GLM.
#For lag1, the best success rate is KNN, followed by LDA, then QDA, then GLM.  
#for lag2, the best success rate is QDA, followed by KNN, then LDA, then GLM.

#***************************************************************
# Consolidated Results
#***************************************************************
higMSE
trvMSE
pgrMSE
higS
trvS
pgrS

