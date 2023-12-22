#Classification 

#KNN Algorithm
library(MASS)
data(fgl)
dim(fgl)

head(fgl, n = 2)

# column 10 is class label, scale converts to mean 0 sd 1
x <- scale(fgl[,1:9])
# apply function sd to columns of x
apply(x,2,sd)

library(class)
test <- sample(1:214,10)
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
data.frame(fgl$type[test],nearest1,nearest5)

#Classification Example
credit <- read.csv("/Users/charlessuey/Downloads/credit.csv")

head(credit)
dim(credit)


library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
default <- credit$Default

credscore <- cv.gamlr(credx, default, family="bin")

plot(credscore$gamlr)

plot(credscore)

###Classification Example###
# min
sum(coef(credscore, s="min")!=0) 
# AICc
sum(coef(credscore$gamlr)!=0) 
#AIC
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0)
# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]


## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

#Classification Rule

# move this around to see how these change
rule <- 1/5 
## false positive rate at 1/5 rule
sum( (pred>rule)[default==0] )/sum(pred>rule) 
## false negative rate at 1/5 rule
sum( (pred<rule)[default==1] )/sum(pred<rule) 

sum( (pred>rule)[default==1] )/sum(default==1)

sum( (pred<rule)[default==0] )/sum(default==0) 



#LASSO penalized multinomial regression example

#Install Glmnet package 
install.packages("glmnet")

library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #Design matrix includes chemical composition variab
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial")

plot(glassfit)

par(mfrow=c(2,3), mai=c(.6,.6,.4,.4))
plot(glassfit$glm, xvar="lambda")

## extract coefficients
B <- coef(glassfit, select="min"); B 

probfgl <- predict(glassfit, xfgl, type="response"); dim(probfgl); head(probfgl,n=2); tail(probfgl,n=2)

probfgl <- drop(probfgl); 
n <- nrow(xfgl)
trueclassprobs <- probfgl[cbind(1:n, gtype)]; head(trueclassprobs,n=3); tail(trueclassprobs,n=3)



