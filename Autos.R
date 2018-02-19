# Import required libraries
library(dplyr)
library(glmnet)
library(mgcv)
library(car)
library(randomForest)

# Change the file location to wherever it is on your computer!
setwd("Directory/to/Autos") # Change to directory of data location
load("Autos/train.dat")
load("Autos/test.dat")

train.df <- data.frame(train)
test.df <- data.frame(test)

summary(train)

# Check for NA
any(is.na(train))
any(is.na(test))

# Missing values
head(train[which(row.names(train) == "125059"),])

# Notice inconsistent values
train$notRepairedDamage = as.factor(ifelse(train$notRepairedDamage == "ja", "yes", ifelse(train$notRepairedDamage == "nein", "no", "unknown")))
train$gearbox = as.factor(ifelse(train$gearbox == "automatik", "automatic", ifelse(train$gearbox == "manuell", "manual", "unknown")))

# Because factors are strange and there are lots of levels,
# we need to try another tactic for changing the level names
vehicleType = sapply(train$vehicleType, as.character)
vehicleType = ifelse(vehicleType == "", "unknown", vehicleType)
train$vehicleType = as.factor(vehicleType)

# Modify empty data entries for fuel type
fuelType = sapply(train$fuelType, as.character)
fuelType = ifelse(fuelType == "", "unknown", fuelType)
train$fuelType = as.factor(fuelType)

# Modify empty data entries for model name
modelName = sapply(train$model, as.character)
modelName = ifelse(modelName == "", "unknown", modelName)
train$model = as.factor(modelName)

# Modify empty data entries for brand name
brands = sapply(train$brand, as.character)
brands = ifelse(brands == "", "unknown", brands)
train$brand = as.factor(brands)

# Summarize training dataset
summary(train)

# Plot log-price for each postal code
train$postalCodeCluster = NULL
for(i in 1:length(train$postalCode)){
  train$postalCodeCluster[i] = floor((train$postalCode[i])/10000)
}
train$postalCodeCluster = as.factor(train$postalCodeCluster)
plot(train$postalCodeCluster, 
     log(train$price), 
     xlab="First digit of postal code", 
     ylab="Log-Price", 
     main="Figure 1. Log Price by Postal Code district")

# Plot log-price again and account for seasonality differences
plot(as.factor(train$monthOfRegistration[which(train$price >= 100)]), 
     log(train$price[which(train$price >= 100)]), 
     xlab="Month of Registration", 
     ylab="Log-Price", 
     main="Figure 2. Log-Price by Month of Registration")

# Plot AB-test against price and log-price
par(mfrow=c(1,2))
plot(train$abtest, train$price, main="Figure 3. ABTest vs. Price")
plot(train$abtest, log(train$price), main="Figure 4. ABTest vs. Log-Price")

# Pairs plot of continuous variables
pairs(train[,c(5,8,10,12)],
      main="Figure 5. Pairs Plot of Continuous Variables")

# The number of records with powerPS > 1000
sum(ifelse(train$powerPS > 1000, 1, 0))

# The number of records with yearOfRegistration > 2018
sum(ifelse(train$yearOfRegistration > 2018, 1, 0))

# Removing these entries and viewing the pairs plot in comparison to log-price may make things clearer
tr_dat = train[which(train$powerPS < 1000),c(5,8,10,12)]
tr_dat = tr_dat[which(tr_dat$yearOfRegistration <= 2018),]
tr_dat = data.frame(log(tr_dat$price), tr_dat)
pairs(tr_dat, main="Figure 6. Pairs Plot with Log-Price (Some Outliers Removed)")

# Plot log-power vs. log-price
plot(log(tr_dat$powerPS), 
     tr_dat$log.tr_dat.price., 
     xlab="LogPowerPS", 
     ylab="LogPrice", 
     main="Figure 7. Log PowerPS vs. Log-Price")

# Pairs plot with log-power and log-kilometers
tr_dat = data.frame(tr_dat, log(tr_dat$powerPS),log(tr_dat$kilometer))
pairs(tr_dat[which(tr_dat$price > 100),c(1,3,6,7)], main="Figure 8. Pairs Plot with Log-PowerPS and Log-Kilometers")

# Number of levels for each categorical variable
nlevels(train$dateCrawled) # 164590 levels
nlevels(train$model) # 251 levels
nlevels(train$name) # 128113 levels
nlevels(train$vehicleType) # 9 levels
nlevels(train$brand) # 40 levels
nlevels(train$dateCreated) #97 levels
nlevels(train$lastSeen) # 111190 levels

# Plot log-price vs. potentially correlated categorical variables
tr_dat2 = train[which(train$price >= 100),]
par(mfrow=c(1,2))
plot(tr_dat2$vehicleType, 
     log(tr_dat2$price), 
     xlab="Vehicle Type", 
     ylab="Log-Price", 
     main="Figure 9. Log-Price by Vehicle Type")
plot(tr_dat2$brand, 
     log(tr_dat2$price), 
     xlab="Brand", 
     ylab="Log-Price", 
     main="Figure 10. Log-Price by Brand")
par(mfrow=c(1,1))
plot(tr_dat2$model, 
     log(tr_dat2$price), 
     xlab="Model", 
     ylab="Log-Price", 
     main="Figure 11. Log-Price by Model")

# Cumulative sum of brand count percentages
#str(train)
brand_counts <- as.data.frame(sort(table(train$brand), decreasing = T))
brand_counts$percentage <- (brand_counts$Freq/sum(brand_counts$Freq))*100
brand_counts$cum <- cumsum(brand_counts$percentage)
#View(brand_counts)

# Plot log-price vs. brand groups
top_brands = brand_counts[1:10, 1]
brand_groups = as.factor(ifelse(as.character(train$brand) %in% as.character(top_brands), 
                                as.character(train$brand), "other"))
train = data.frame(train, brand_groups)
plot(train$brand_groups, 
     log(train$price), 
     xlab="Brand(Groups)", 
     ylab="Log-Price", 
     main="Figure 12. Log-Price by Brand Groups")

# Cumulative sum of model count percentages
model_counts <- as.data.frame(sort(table(train$model), decreasing = T))
model_counts$percentage <- (model_counts$Freq/sum(model_counts$Freq))*100
model_counts$cum <- cumsum(model_counts$percentage)
#View(model_counts)

# Plot log-price by model group
top_models = model_counts[1:24, 1]
model_groups = as.factor(ifelse(as.character(train$model) %in% as.character(top_models), 
                                as.character(train$model), "other"))
train = data.frame(train, model_groups)
par(mfrow=c(1,1))
plot(train$model_groups, 
     log(train$price), 
     xlab="Model (Group)", 
     ylab="Log-Price", 
     main="Figure 13. Log-Price by Model Group")

# Additional plots
par(mfrow=c(1,1))
plot(train$gearbox[which(train$price >= 100)], 
     log(train$price[which(train$price >= 100)]), 
     xlab="Gearbox", 
     ylab="Log-Price",
     main="Figure 14. Gearbox vs. Log-Price")
plot(train$notRepairedDamage[which(train$price >= 100)], 
     log(train$price[which(train$price >= 100)]), 
     xlab="NotRepairedDamage", ylab="Log-Price", 
     main="Figure 15. NotRepairedDamage vs. Log-Price")
plot(train$fuelType[which(train$price >= 100)], 
     log(train$price[which(train$price >= 100)]), 
     xlab="FuelType", ylab="Log-Price", 
     main="Figure 16. FuelType vs. Log-Price")

# Plot log-price by fuel type group
fuelType_group = as.factor(ifelse(as.character(train$fuelType) == "benzin", as.character(train$fuelType), 
                                  ifelse(as.character(train$fuelType) == "diesel", as.character(train$fuelType), "other")))
train = data.frame(train, fuelType_group)
plot(train$fuelType_group, 
     log(train$price), 
     xlab="FuelType Group", 
     ylab="Log-Price", 
     main="Figure 17. Log-Price by FuelType Group")

# Interactions with Kilometer
par(mfrow=c(1,2))
plot(train$kilometer, 
     log(train$price), 
     col=train$fuelType_group, 
     xlab="Kilometer", 
     ylab="Log-Price", 
     main="Figure 18. Kilometer & FuelType Group")
plot(train$kilometer, 
     log(train$price), 
     col=train$brand_groups, 
     xlab="Kilometer", 
     ylab="Log-Price", 
     main="Figure 19. Kilometer & Brand Group")
plot(train$kilometer, 
     log(train$price), 
     col=train$model_groups, 
     xlab="Kilometer", 
     ylab="Log-Price", 
     main="Figure 20. Kilometer & Model Group")
plot(train$kilometer, 
     log(train$price), 
     col=train$gearbox, 
     xlab="Kilometer", 
     ylab="Log-Price", 
     main="Figure 21. Kilometer & Gearbox")
plot(train$kilometer, 
     log(train$price), 
     col=train$notRepairedDamage, 
     xlab="Kilometer", ylab="Log-Price", 
     main="Figure 22. Kilometer & NotRepairedDamage")
plot(train$kilometer, 
     log(train$price), 
     col=train$vehicleType, 
     xlab="Kilometer", 
     ylab="Log-Price", 
     main="Figure 23. Kilometer & VehicleType")

# Interactions with powerPS
plot(log(train$powerPS), 
     log(train$price), 
     col=train$fuelType_group, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 24. Log-PowerPS & FuelType Group")
plot(log(train$powerPS), 
     log(train$price), 
     col=train$brand_groups, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 25. Log-PowerPS & Brand Group")
plot(log(train$powerPS), 
     log(train$price), 
     col=train$model_groups, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 26. Log-PowerPS & Model Group")
plot(log(train$powerPS), 
     log(train$price), 
     col=train$gearbox, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 27. Log-PowerPS & Gearbox")
plot(log(train$powerPS), 
     log(train$price), 
     col=train$notRepairedDamage, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 28. Log-PowerPS & NotRepairedDamage")
plot(log(train$powerPS), 
     log(train$price), 
     col=train$vehicleType, 
     xlab="Log-PowerPS", 
     ylab="Log-Price", 
     main="Figure 29. Log-PowerPS & VehicleType")

# Histograms of price and log-price
par(mfrow=c(1,2))
hist(train$price, xlab="Price", main="Figure 30. Histogram of Price")
hist(log(train$price), xlab="Log-Price", main="Figure 31. Histogram of Log-Price")

# Histogram of log-price without low prices
train2 = train[which(train$price >= 100),]
train2 = data.frame(train2[,c(1,5,7,8,9,10,12,16,17,20,22,23,24)], log(train2$price), log(train2$powerPS))
colnames(train2)[14] = "logPrice"
colnames(train2)[15] = "logPowerPS"
hist(train2$logPrice, 
     xlab="Log-Price (clean)", 
     main="Figure 32. Log-Price without low prices")

# Summary of transformed training dataset
nrow(train2) #1926 rows
summary(train2)

# Plot month of crawl vs log-price
par(mfrow=c(2,2))
dateCrawl_month = as.factor(format(as.Date(train2$dateCrawled), "%Y-%m"))
plot(dateCrawl_month, 
     train2$logPrice, 
     main="Figure 33. Month of crawl vs Log-Price")

# Plot month/day of crawl vs log-price
dateCrawl_day = as.factor(as.Date(train2$dateCrawled, "%Y-%m-%d"))
plot(dateCrawl_day, 
     train2$logPrice, 
     main="Figure 34. Month/Day of crawl vs Log-Price")

# Plot log-price vs num of dats ad was online
adLength = as.double(difftime(as.Date(train2$lastSeen, "%Y-%m-%d"), 
                              as.Date(train2$dateCreated, "%Y-%m-%d"), units="days"))
#summary(adLength)
plot(adLength, 
     train2$logPrice, 
     main="Figure 35. Ad length vs. Log-Price", 
     xlab="Number of days ad was online", 
     ylab="Log-Price")

# Plot ad length vs log-price
ad_data = data.frame(train2, adLength)
ad_data = ad_data[which(adLength < 40),]
plot(ad_data$adLength, 
     ad_data$logPrice, 
     xlab = "Number of days ad was online", 
     ylab="Log-Price",
     main="Figure 36. Ad length (clean) vs. Log-Price")

# Remove year of registration
train2 = train2[which(train2$powerPS < 1000), c(3,4,5,6,7,8,11,12,13,14,15)]
train2 = train2[which(train2$yearOfRegistration <= 2018),]
train2 = train2[,-2] # remove yearOfRegistration

# Final cleaning:
train2.df <- data.frame(train2)

# Delete zeroes from powerPS
train2.df <- train2.df[train2.df$powerPS != 0,]

# Delete powerPS variable
train2.df <- train2.df[,-3]

# Combine "andere" and "other"
train2.df$model_groups[which(train2.df$model_groups == "andere")] <- "other" #combine variables since same meaning

#data cleaning complete
train_final <- train2.df
summary(train_final)
nrow(train_final)

# Additional data cleaning
test$notRepairedDamage = as.factor(ifelse(test$notRepairedDamage == "ja", "yes", 
                                          ifelse(test$notRepairedDamage == "nein", "no", "unknown")))
test$gearbox = as.factor(ifelse(test$gearbox == "automatik", "automatic", 
                                ifelse(test$gearbox == "manuell", "manual", "unknown")))

# Clean vehicle type
vehicleType = sapply(test$vehicleType, as.character)
vehicleType = ifelse(vehicleType == "", "unknown", vehicleType)
test$vehicleType = as.factor(vehicleType)

# Clean fuel type
fuelType = sapply(test$fuelType, as.character)
fuelType = ifelse(fuelType == "", "unknown", fuelType)
test$fuelType = as.factor(fuelType)

# Clean model name
modelName = sapply(test$model, as.character)
modelName = ifelse(modelName == "", "unknown", modelName)
test$model = as.factor(modelName)

# Clean brand
brands = sapply(test$brand, as.character)
brands = ifelse(brands == "", "unknown", brands)
test$brand = as.factor(brands)

# Clean testing data
#str(test)
brand_groups = as.factor(ifelse(as.character(test$brand) %in% as.character(top_brands), as.character(test$brand), "other"))
test = data.frame(test, brand_groups)

# Additional cleaning of testing data
model_groups = as.factor(ifelse(as.character(test$model) %in% as.character(top_models), as.character(test$model), "other"))
test = data.frame(test, model_groups)

# Additional cleaning of testing data
fuelType_group = as.factor(ifelse(as.character(test$fuelType) == "benzin", as.character(test$fuelType), ifelse(as.character(test$fuelType) == "diesel", as.character(test$fuelType), "other")))
test = data.frame(test, fuelType_group)

# Additional cleaning of testing data
test2 = test[which(test$price >= 100),]
test2 = data.frame(test2[,c(5,7,8,9,10,12,16,21,22,23)],log(test2$price), log(test2$powerPS))
colnames(test2)[11] = "logPrice"
colnames(test2)[12] = "logPowerPS"

# Remove year of registration
test2 = test2[which(test2$powerPS < 1000),]
test2 = test2[which(test2$yearOfRegistration <= 2018), -1] #remove price (keeping logprice)
test2 = test2[,-2] # remove yearOfRegistration

# Create data frame for final test dataset
test2.df <- data.frame(test2)

# Delete zeroes from powerPS
test2.df <- test2.df[test2.df$powerPS != 0,]

# Delete powerPS variable
test2.df <- test2.df[,-3]

# Combine "andere" and "other"
test2.df$model_groups[which(test2.df$model_groups == "andere")] <- "other" #combine variables since same meaning

#data cleaning complete
test_final <- test2.df

# Fit linear model
train.lm <- lm(logPrice~., data=train_final)
summary(train.lm) # R^2 = .69

# ANOVA
train.anova <- anova(train.lm)
train.anova

# AIC and BIC
AIC(train.lm) # 3635
BIC(train.lm) # 3913

# MSE
pd <- predict(train.lm, test2.df)
test.mse <- mean((test_final$logPrice - pd)^2)
test.mse # MSE = 0.66

# Residual plots
resf = rstandard(train.lm)
plot(resf, main="Plot of Residuals")
plot(predict(train.lm), resf, xlab="Predicted Values from Linear Model", main="Predicted vs. Residuals")
plot(train_final$kilometer, resf, xlab="Kilometer", main="Kilometer vs. Residuals")
plot(train_final$logPowerPS, resf, xlab="Log-PowerPS", main="Log-PowerPS vs. Residuals")

# Residual boxplots for categorical variables
par(mfrow=c(2,2))
with(train_final,{
  plot(resf~gearbox)
  plot(resf~vehicleType)
  plot(resf~brand_groups)
  plot(resf~model_groups)
})

# qq plot:
par(mfrow=c(1,1))
qqPlot(resf)

#get model matrices set y's
x.train = model.matrix(logPrice~.,train_final)[,-1] 
y.train = train_final$logPrice
x.test = model.matrix(logPrice~.,test_final)[,-1] 
y.test = test_final$logPrice

#observe MSE for many values of lambda
#set seed to replicate 
set.seed(3) 
lambdas=seq(1e-3,300,length=100)
MSE=rep(0,100)
for(i in 1:100)
{
  fit.ridge=glmnet(x.train,y.train,alpha=0,lambda=lambdas[i])
  pred.ridge=predict(fit.ridge,newx=x.test)
  MSE[i]=mean((pred.ridge-y.test)^2)
}
plot(lambdas,MSE,xlab=expression(lambda),ylab="Test set MSE")

#cross validation
ridge.cv = cv.glmnet(x.train,y.train,alpha=0)
plot(ridge.cv) #plot on log-lambda scale
plot(ridge.cv$lambda,ridge.cv$cvm,xlim=c(0,300)) #plot on lambda scale
ridge.lambda.cv = ridge.cv$lambda.min # the minimizing lambda

#fit model 
fit.ridge = glmnet(x.train,y.train,alpha=0,lambda=ridge.lambda.cv)
pred.ridge = predict(fit.ridge,newx=x.test)

#observe MSE for many values of lambda
#set seed to replicate 
set.seed(3) 
lambdas=seq(1e-3,10,length=100)
MSE=rep(0,100)
for(i in 1:100)
{
  fit.lasso=glmnet(x.train,y.train,alpha=1,lambda=lambdas[i])
  pred.lasso=predict(fit.lasso,newx=x.test)
  MSE[i]=mean((pred.lasso-y.test)^2)
}
plot(lambdas,MSE,xlab=expression(lambda),ylab="Test set MSE")

#cross validation
lasso.cv = cv.glmnet(x.train,y.train,alpha=1)
plot(lasso.cv) #plot on log-lambda scale
plot(lasso.cv$lambda,lasso.cv$cvm,xlim=c(0,.7)) #plot on lambda scale 
lasso.lambda.cv = lasso.cv$lambda.min # the minimizing lambda

#fit model 
fit.lasso = glmnet(x.train,y.train,alpha=1,lambda=lasso.lambda.cv)
pred.lasso = predict(fit.lasso,newx=x.test)

#penalties
sqrt(sum(coef(fit.ridge)[-1,1]^2))
sum(abs(coef(fit.lasso)[-1,1]))

#cross validated optimal lambdas
ridge.lambda.cv
lasso.lambda.cv

#MSE's
mean((y.test-pred.ridge)^2) 
mean((y.test-pred.lasso)^2) 

#Compute R^2 from true and predicted values
rsquare <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  
  return (rsq)
}
rsquare(y.test, pred.ridge)
rsquare(y.test, pred.lasso)

#summary of coefficients
coef(fit.ridge)
coef(fit.lasso)

# Find k for continous
train.gam <- gam(logPrice~s(kilometer, k =-1, bs = "cs"), data=train_final)
summary(train.gam)
#gam.check(train.gam) # k = 9

# Find k for continous
train.gam <- gam(logPrice~s(logPowerPS, k =-1, bs = "cs"), data=train_final)
summary(train.gam)
#gam.check(train.gam) # k = 9

# Calculate Gams
train.gam <- gam(logPrice~s(kilometer,k=9)+s(logPowerPS,k=9)+vehicleType+gearbox+notRepairedDamage+brand_groups+model_groups+fuelType_group, data=train_final)
summary(train.gam) # R^2 = 69%
gam.check(train.gam)

# Check AIC and BIC
AIC(train.gam) # 3545
BIC(train.gam) # 3881.148

# Calculate MSE
pd <- predict(train.gam, test2.df)
test.mse <- mean((test_final$logPrice - pd)^2)
test.mse # MSE = 0.59

#attempt bagging 
bag.fit = randomForest(logPrice ~ ., data = train_final, mtry = ncol(train_final)-1, ntree = 500, importance = T)
bag.pred = predict(bag.fit, test_final)

#m-try
ncol(train_final)-1

#mse
mean((test_final$logPrice - bag.pred)^2)

#variable importance
importance(bag.fit) 

#fit a random forest 
rf.fit = randomForest(logPrice ~ ., 
                      data = train_final, 
                      mtry = (ncol(train_final)-1)/2, 
                      ntree = 500, 
                      importance = T)
rf.pred = predict(rf.fit, test_final)

#mtry
(ncol(train_final)-1)/2

#mse 
mean((test_final$logPrice - rf.pred)^2)

#variable importance
importance(rf.fit)

#plots
plot(rf.fit, main="Random Forest Error by Number of Trees")
plot(rf.pred, 
     test_final$logPrice, 
     xlab="Predicted", 
     ylab="Actual",
     main="Random Forest Actual LogPrice vs. Predicted LogPrice")
