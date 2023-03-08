#### LOGISTIC REGRESSION DEMO ####

##### Libraries #####
library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)

# set the seed
set.seed(1)

##### Reading in Data ##### 
# as data table 

dt <- read.csv('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 6\\assignment\\churn.csv', header = TRUE)
dt <- as.data.table(dt)
str(dt)
summary(dt)
dt <- dt[complete.cases(dt),]
summary(dt)

#data cleaning


dtcln <- dt[, c("customerID", "TotalCharges", "MultipleLines", "OnlineSecurity","OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies") := NULL]
dtcln$gender <- as.factor(dtcln$gender)
dtcln$SeniorCitizen <- as.factor(dtcln$SeniorCitizen)
dtcln$Partner <- as.factor(dtcln$Partner)
dtcln$Dependents <- as.factor(dtcln$Dependents)
dtcln$PhoneService <- as.factor(dtcln$PhoneService)
dtcln$InternetService <- as.factor(dtcln$InternetService)
dtcln$Contract <- as.factor(dtcln$Contract)
dtcln$PaperlessBilling <- as.factor(dtcln$PaperlessBilling)
dtcln$PaymentMethod <- as.factor(dtcln$PaymentMethod)
dtcln$Churn <- as.factor(dtcln$Churn)


# Split the data into a train and test set
samp <- sample.split(dt$Churn, SplitRatio = 0.8)
train <- subset(dtcln, samp == TRUE)
test <- subset(dtcln, samp == FALSE)

# Create a multi-linear binomial logistic regression on survived vs sex
model <- glm(Churn ~ ., data = train, family = "binomial")
summary(model)
alias(model)
vif(model)


# Perform step AIC and remove high p-values
stepAIC(model, dirrection = 'both')


#  AIC=4757.64

model <- glm(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + 
               InternetService + Contract + PaperlessBilling + PaymentMethod + 
               MonthlyCharges, data = train, family = "binomial")


summary(model)
vif(model)


# Predict on the train data            
trainpreds <- predict(model, type = 'response', train)
# Round prediction values at 0.5 cutoff factor and change labels
trainp <- factor(trainpreds >= 0.5, labels = c('No', 'Yes'))
# Build a confusion matrix to see results
trainCM <- confusionMatrix(train$Churn, trainp)
trainCM



### Prediction ### 
# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= 0.5, labels = c('No', 'Yes'))

# Build a confusion matrix to see results
testCM <- confusionMatrix(test$Churn, testp)
testCM



# Create a ROC curve and results for the Train data
train_roc_curve <- roc(train$Churn, trainpreds)
train_roc_curve
plot(train_roc_curve)
train_rocc <- coords(roc=train_roc_curve, x = 'best', best.method = 'closest.topleft')
train_rocc


# Create a ROC curve and results for the Test data
test_roc_curve <- roc(test$Churn, testpreds)
test_roc_curve
plot(test_roc_curve)
test_rocc <- coords(roc=test_roc_curve, x = 'best', best.method = 'closest.topleft')
test_rocc



# Predict on the train data using the ROC cut-off            
# Round prediction values at 0.5 cutoff factor and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('No', 'Yes'))

# Build a confusion matrix to see results
trainROCCM <- confusionMatrix(train$Churn, trainrocp)
trainROCCM



# Predict on the test data            
# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('No', 'Yes'))

# Build a confusion matrix to see results
testROCCM <- confusionMatrix(test$Churn, testp)
testROCCM



#View all the Confusion matrices
trainCM
trainROCCM

testCM
testROCCM
