#Regular Regression 

# This code will open and read the CSV file 
oj <- read.csv("/Users/charlessuey/Downloads/oj.csv")

head(oj, n=5)
tail(oj, n=5)

glm(log(sales) ~ brand + log(price), data=oj) 

x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)

oj$brand = as.factor(oj$brand)
x <- model.matrix(~ brand + log(price), data=oj); head(x)

oj$mybrand = relevel(oj$brand, "tropicana")
x <- model.matrix(~ mybrand + log(price), data=oj); head(x)

glm(log(sales) ~ log(price)*brand*feat, data=oj)

#End of Regular regression 
#Logit Regression 

# This code will open and read the CSV file 
email <- read.csv("/Users/charlessuey/Downloads/spam.csv")
#The dim command will give us information on the spam.csv
dim(email)
#The data being used has 4601 observations and 58 variables  
colnames(email) 
# The "colnames" command displays the names of the collumns 

glm(spam ~ ., data=email, family='binomial') 

spammy <- glm(spam ~ ., data = email, family = 'binomial')
#This command fits the logistics regression model 
coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])

coef(spammy)["word_george"]; exp(coef(spammy)["word_george"]); 1/exp(coef(spammy)["word_george"])

predict(spammy, newdata = email[c(1,4000),], type="response")
summary(spammy)$deviance
#The deviance statistic measures how well the model fits the data
summary(spammy)$null.deviance
#The null deviance measures the amount deviance with no predictors 

D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2
#You are able to predict approx. 75% of the variation in spam occurence 

### Altered Code 
library(caret)
set.seed(123)
cv_results <- train(spam ~ ., data = email, method = "glm", family = "binomial", trControl = trainControl(method = "cv"))
summary(cv_results)
#for my contribution added a logistics regression to extend this weeks code. 
