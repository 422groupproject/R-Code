setwd('F:/Users/Annie/Documents/R')
charity <- read.csv('charity.csv')
summary(charity)
library(DataExplorer)
plot_histogram(charity)

charity.t <- charity
# charity.t$avhv <- log(charity.t$avhv)
# charity.t$incm <- log(charity.t$incm)
# charity.t$inca <- log(charity.t$inca)
charity.t$tgif <- log(charity.t$tgif)
charity.t$lgif <- log(charity.t$lgif)
#charity.t$rgif <- log(charity.t$rgif)
#charity.t$tlag <- log(charity.t$tlag)
# charity.t$agif <- log(charity.t$agif)
plot_histogram(charity.t)


library(moments)
skewness(charity$avhv)
skewness(charity$incm)
skewness(charity$inca)
skewness(charity$tgif)
skewness(charity$lgif)
skewness(charity$rgif)
skewness(charity$tlag)
skewness(charity$agif)

# par(mfrow=c(2,6))
# boxplot(charity.t$tgif, col = "cyan")
# title('Lifetime Gifts', cex.main=.85)
# boxplot(charity.t$lgif, col = "cyan")
# title('Largest Gift', cex.main=.85)
# boxplot(charity.t$rgif, col = "cyan")
# title('Most Recent Gift', cex.main=.85)
# boxplot(charity.t$agif, col = "cyan")
# title('Avg Gift', cex.main=.85)
# boxplot(charity.t$tdon, col = "darkseagreen1")
# title('Mnths Since Donation', cex.main=.85)
# boxplot(charity.t$tlag, col = "darkseagreen1")
# title('Mnths BTWN Gifts', cex.main=.85)
# boxplot(charity.t$npro, col = "darkseagreen1")
# title('# of Promotions', cex.main=.85)
# boxplot(charity.t$plow, col = "darkseagreen1")
# title('Low Income Percent', cex.main=.85)
# boxplot(charity.t$wrat, col = "darkseagreen1")
# title('Wealth Rating', cex.main=.85)
# boxplot(charity.t$avhv, col = "darkkhaki")
# title('AvgNghbrdHomeValue', cex.main=.85)
# boxplot(charitynmv$incm, col = "darkkhaki")
# title('MedianNghbrdIncome', cex.main=.85)
# boxplot(charity.t$inca, col = "darkkhaki")
# title('AvgNghbrdIncome', cex.main=.85)

# set up data for analysis
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1
x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1
x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

##### CLASSIFICATION MODELING ######
library(MASS)
# logistic regression
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))
post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
#plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5
cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
#table(chat.valid.log1, c.valid) # classification table

#Logistic regression GAMBut 
library(gam)
model.gam1 <- gam(donr ~ reg1  + reg2+  reg3 + reg4 + home + s(chld,3) + s(hinc,3) + genf + s(wrat,3) + 
                    incm + tgif + rgif  + tdon + s(tlag,3), 
                  data.train.std.c, family=binomial)
post.valid.gam1 <- predict(model.gam1, data.valid.std.c, type="response") 
profit.gam1 <- cumsum(14.5*c.valid[order(post.valid.gam1, decreasing=T)]-2)
#plot(profit.gam1) 
n.mail.valid <- which.max(profit.gam1)
c(n.mail.valid, max(profit.gam1)) 
cutoff.gam1 <- sort(post.valid.gam1, decreasing=T)[n.mail.valid+1] 
chat.valid.gam1 <- ifelse(post.valid.gam1>cutoff.gam1, 1, 0)
table(chat.valid.gam1, c.valid)

#summary(model.log1)$coefficients

# linear discriminant analysis

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2)+ genf + wrat + 
                    avhv + npro  + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()
# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model
post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
#plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5
cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
#table(chat.valid.lda1, c.valid) # classification table

#Quadratic Discriminant Analysis

model.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc  + I(hinc^2)+ genf + wrat + 
                    avhv + npro + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c)
post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2] 
profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.qda1)
c(n.mail.valid, max(profit.qda1))


#K-Nearest Neighbors
library(class)
set.seed(1)
knn.pred1 =knn(data.train.std.c,data.valid.std.c, c.train ,k=1)
knn.pred5 =knn(data.train.std.c,data.valid.std.c, c.train ,k=5)
knn.pred10 =knn(data.train.std.c,data.valid.std.c, c.train ,k=10)
profit.knn1 <- cumsum(14.5*c.valid[order(knn.pred1, decreasing=T)]-2)
profit.knn5 <- cumsum(14.5*c.valid[order(knn.pred5, decreasing=T)]-2)
profit.knn10 <- cumsum(14.5*c.valid[order(knn.pred10, decreasing=T)]-2)
n.mail.valid1 <- which.max(profit.knn1)
c(n.mail.valid1, max(profit.knn1))
n.mail.valid5 <- which.max(profit.knn5)
c(n.mail.valid5, max(profit.knn5))
n.mail.valid10 <- which.max(profit.knn10)
c(n.mail.valid1, max(profit.knn10))

##### PREDICTION MODELING ######
# Least squares regression
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615
# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2)
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error

# Best Subset Selection
library(leaps)
set.seed(1)
regfit.best=regsubsets(damt ???.,data= data.train.std.y,nvmax=21)
test.mat=model.matrix(damt???.,data=data.valid.std.y)
val.errors =rep(NA ,19)
for(i in 1:19){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.valid-pred)^2)
}
min <- which.min(val.errors)
coef(regfit.best, min)
model.ls3 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  wrat + incm + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
pred.valid.ls3 <- predict(model.ls3, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls3)^2)
sd((y.valid - pred.valid.ls3)^2)/sqrt(n.valid.y)


 predict.regsubsets =function (object , newdata ,id ,...){
   form=as.formula(object$call[[2]])
   mat=model.matrix(form ,newdata )
   coefi=coef(object,id=id)
   xvars=names(coefi)
   mat[,xvars]%*%coefi
   }

#K- fold cross-validation
k=10
set.seed(1)
folds=sample(1:k,n.train.y,replace=TRUE)
cv.errors =matrix(NA,k,21,dimnames =list(NULL, paste(1:21)))
for(j in 1:k){
  best.fit=regsubsets(damt~.,data=data.train.std.y[folds!=j,],
                         nvmax=21)
 for(i in 1:21){
   pred=predict(best.fit,data.train.std.y[folds ==j,],id=i)
    cv.errors[j,i]= mean((y.valid[folds==j]-pred)^2)
    }
 }
mean.cv.errors=apply(cv.errors ,2, mean)
min <- which.min(mean.cv.errors)
coef(regfit.best, min)

# Ridge Regression

# Lasso

# Principal Components Regreesion

# Partial Least Squares
