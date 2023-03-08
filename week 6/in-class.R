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

dt <- read.csv('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 6\\breast-cancer-wisconsin.data', header = TRUE)
dt <- as.data.table(dt)
str(dt)
summary(dt)


# change columns names
colnames(dt) = c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
"bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Diagnosis")

# removing id column 
dtcln <- dt[, c("id") := NULL]

# change bare_nucleoli from char to numeric
dtcln$bare_nucleoli = as.numeric(dtcln$bare_nucleoli)

# label our dependent values from 2 and 4 to benign and malignant
dtcln$Diagnosis <- factor(dt$Diagnosis, labels = c('benign', 'malignant')) 

summary(dtcln)

# removing NAs 
dtcln <- dtcln[complete.cases(dtcln),]

# Split the data into a train and test set
samp <- sample.split(dtcln$Diagnosis, SplitRatio = 0.8)
train <- subset(dtcln, samp == TRUE)
test <- subset(dtcln, samp == FALSE)




##### Modeling #####

# Create a multi-linear binomial logistic regression on diagnosed vs sex
model <- glm(Diagnosis ~ ., data = train, family = "binomial")



# Look at the model summary
summary(model)

# Check for collinearity
vif(model)  

# Perform step AIC and remove high p-values
stepAIC(model, dirrection = 'both')


# Update model based on the step AIC
model <- glm(formula = Diagnosis ~ clump_thickness + marginal_adhesion + bare_nucleoli, family = "binomial", data = train)

#Check model summary again
summary(model)


# Predict on the train data 
trainpreds <- predict(model, type = 'response', train)
# Round prediction values at 0.5 cutoff factor and change labels
trainp <- factor(trainpreds >= 0.5, labels = c('benign', 'malignant'))
# Build a confusion matrix to see results
trainCM <- confusionMatrix(train$Diagnosis, trainp)
trainCM


### Prediction ### 
# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= 0.5, labels = c('benign', 'malignant'))

# Build a confusion matrix to see results
testCM <- confusionMatrix(test$Diagnosis, testp)
testCM



# Create a ROC curve and results for the Train data
train_roc_curve <- roc(train$Diagnosis, trainpreds)
train_roc_curve
plot(train_roc_curve)
train_rocc <- coords(roc=train_roc_curve, x = 'best', best.method = 'closest.topleft')
train_rocc



# Create a ROC curve and results for the Test data
test_roc_curve <- roc(test$Diagnosis, testpreds)
test_roc_curve
plot(test_roc_curve)
test_rocc <- coords(roc=test_roc_curve, x = 'best', best.method = 'closest.topleft')
test_rocc




# Predict on the train data using the ROC cut-off            
# Round prediction values at 0.5 cutoff factor and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('benign', 'malignant'))



# Build a confusion matrix to see results
trainROCCM <- confusionMatrix(train$Diagnosis, trainrocp)
trainROCCM


# Predict on the test data            
# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('benign', 'malignant'))

# Build a confusion matrix to see results
testROCCM <- confusionMatrix(test$Diagnosis, testp)
testROCCM

#View all the Confusion matrices
trainCM
trainROCCM

testCM
testROCCM
