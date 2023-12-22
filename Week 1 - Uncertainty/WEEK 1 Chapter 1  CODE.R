##WEEK 1 Chapter 1 

# This code will open and read the CSV file 
browser <- read.csv("/Users/charlessuey/Downloads/web-browsers.csv")
# The "dim" code will display the diminsions of the data frame 
dim(browser)
# The "head" code will show the first few collums and rows of the dataframe in the csv.file 
head(browser)

#Frequentist 

# The "mean" command will display the mean within the spend variable 
mean(browser$spend);
# The "var" caluculates the variance of the spend collumn 
var(browser$spend)/1e4;
#  This calculates the squareroot value will we caluculated above 
sqrt(var(browser$spend)/1e4) 

# Bootstrping 

B <- 1000
mub <- c() 
for (b in 1:1000) {samp_b <- sample.int(nrow(browser), replace = TRUE)
mub <- c(mub, mean(browser$spend[samp_b]))}
sd(mub) 

#
h <- hist(mub)
xfit <- seq(min(mub), max(mub), length = 40)
yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4))
yfit <- yfit * diff(h$mids[1:2]) * length(mub)
#can you explain why we need each term in the last expression?
lines(xfit, yfit, col = "black", lwd = 2) 

#Bootstrapoing Regressions 
B <- 1000
betas <- c()
for (b in 1:1000){ samp_b <- sample.int(nrow(browser), replace=TRUE) 
reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,]) 
betas <- rbind(betas, coef(reg_b))}
head(betas, n=3) 
cov(betas[, "broadband"], betas[, "anychildren"])


#BH Algorithm Example 1 
browser<- read.csv("/Users/charlessuey/Downloads/web-browsers.csv")
spendy <- glm(log(spend) ~ . -id, data=browser)

pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
pvalrank <- rank(pval)
reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1)
png(file="BHAlgoExample.png",
    width=600, height=350)
plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
lines(pvalrank, (0.1/9)*pvalrank)

#BH Algorithm Example 2

SC <- read.csv("/Users/charlessuey/Downloads/semiconductor.csv")

full <- glm(FAIL ~ ., data=SC, family=binomial)
pvals <- summary(full)$coef[-1,4]
hist(pvals, xlab="p-value", main="", col="lightblue")
#-1 to drop the intercept

# END OF WEEK 1 CODE


#My alteration/extension

#Using the data from the web browsers csv i want to run a new linear regression and add a new predictor with that variable being anychildern 
linear_model <- lm(spend ~ broadband + anychildren, data = browser)
summary(linear_model)
#In the extension we see that adding children is not statically signaficant 
