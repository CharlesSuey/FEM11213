# This code will open and read the CSV file 

SC <- read.csv("/Users/charlessuey/Downloads/semiconductor.csv")

full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance 
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
} 


# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
# create an empty dataframe of results
Out <- data.frame(full=rep(NA,K))
# use a for loop to run the experiment
for(k in 1:K){
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit regression on full sample
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  ## get prediction: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  ## calculate and log R2
  Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  ## print progress
  cat(k, " ")
} 

boxplot(Out, col="plum", ylab="R2")

# what are the average Out R2?
colMeans(Out) 

boxplot(Out, col="plum", ylab="R2")

## what are the average Out R2?
colMeans(Out) 

#Regularization Path 

#Foreward Stepwise Regression Example 
null <- glm(FAIL~1, data=SC)
fwd <- step(null, scope=formula(full), dir="forward")
length(coef(fwd))
 
# I Instal gamlr package into system 
install.packages("gamlr")
#this loads the package so it can be used in experiment 
library(gamlr)

## run a lasso path plot
spender <- gamlr(xweb, log(yspend), verb=TRUE); spender

## path plot
plot(spender) 
#K-fold Cross Validation for LASSO
cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)
betamin = coef(cv.spender, select="min"); betamin
#Aikaike's Info Criterion
head(AIC(spender))