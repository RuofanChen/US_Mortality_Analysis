#k-fold cross-validation
#downsample and weighted loss function
#logistic regression with regularization and SVM with regularization 
library(glmnet)
library(caret)
library('kernlab')
library(e1071)
library(tidyverse)
library(DMwR)
library(caret)
#### part 1 read data and data pre-processing
setwd('D:/data/semester3/5665/5665-Project/archive/train')
train <- read.csv('train.csv')
dim(train)
names(train)
# check missing value
sapply(train, function(x)sum(is.na(x)))
# reorder data and factorize categorical variables,dropped 'id'

train1 <- train %>% mutate(`Gender` = as.factor(`Gender`), 
                       Driving_License = as.factor(`Driving_License`),
                       Region_Code = as.factor(`Region_Code`),
                       Previously_Insured = as.factor(`Previously_Insured`),
                       Vehicle_Age = as.factor(`Vehicle_Age`),
                       Vehicle_Damage = as.factor(`Vehicle_Damage`),
                       Policy_Sales_Channel = as.factor(`Policy_Sales_Channel`),
                       Response=as.factor(`Response`))
train1 <- train1 %>% select(`Age`, `Annual_Premium`,`Vintage`: `Gender`,
                        `Driving_License`, `Region_Code`, 
                        `Previously_Insured`, `Vehicle_Age`,`Vehicle_Damage`,
                        `Policy_Sales_Channel`,
                        `Response`)
summary(train1)
print(prop.table(table(train1$Response)))

# count number of level of region_code
nlevels(train1$Region_Code)
table(train1$Region_Code)

# count number of level of policy_sales_channel
nlevels(train1$Policy_Sales_Channel)
table(train1$Policy_Sales_Channel)

# some levels have a small number of observation
# dropped unused levels of policy_sales_channel

# generate new level called '200', if not belongs to
#'152','26','124','160','156','122'
levels(train1$Policy_Sales_Channel) <- c(levels(train1$Policy_Sales_Channel),'200')

train1[!(train1$Policy_Sales_Channel %in% c('152','26',
                                        '124','160','156','122')),4] <- as.factor(200)
train1$Policy_Sales_Channel <- droplevels(train1$Policy_Sales_Channel)

# drop levels of region_code
levels(train1$Region_Code) <- c(levels(train1$Region_Code),'200')

train1[!(train1$Region_Code %in% c('28','8',
                                        '46','41','15','30')),8] <- as.factor(200)
train1$Region_Code <- droplevels(train1$Region_Code)

levels(train1$Response) <- c("No", "Yes")

summary(train1)


#### part 2 descriptive analysis - data summary



#### part 3
# down sample
# use k-fold cross-validation to find hyper-parameter
# on logistic regression


### prepare: use cv and grid search to 
# find the best regularization parameter
## to save time, first using downsample to re-sample the data



# setting seed to generate a  
# reproducible random sampling

# Dummy code categorical predictor variables
x <- model.matrix(Response~., train1)[,-1]
y <- train1$Response

set.seed(123)
down_train <- downSample(train1[,1:10],train1$Response,yname='Response')
table(down_train$Response)
summary(down_train)

# smote_train <- SMOTE(Response ~ ., data  = train1, perc.over = 100,k=5)
# table(smote_train$Response) 


# Dummy code categorical predictor variables
down_x <- model.matrix(Response~., down_train)[,-1]
down_y <- down_train$Response


library(caret)
ctrl <- trainControl(method="cv", number=5,classProbs=TRUE,
                     summaryFunction = twoClassSummary)

set.seed(123)
down_glm_model <- train(down_x, down_y, method = "glmnet", 
                             trControl = ctrl,metric = "ROC",
                             tuneGrid = expand.grid(alpha = 0:1,
                                                    lambda = seq(0,0.001,length=100)))
down_glm_model


library(glmnet)
library(tibble)
library(ggplot2)
library(broom)

# best parameter
down_glm_model$bestTune
down_glm_model$results[131,]

# best coefficient
coef(down_glm_model$finalModel, down_glm_model$bestTune$lambda)

# variable importance
ImpMeasure<-data.frame(varImp(down_glm_model)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
ImpMeasure[order(-ImpMeasure$Overall),][1:10,]
# goodness-of-fit test of models
glance(down_glm_model)
tidy(down_glm_model)




#### part 4
# use k-fold cross-validation on Logistic regression 
# weighted loss function
# with regularization

# Create model weights (they sum to one)
model_weights <- ifelse(train$Response == 0,
                        (1/table(train$Response)[1]) * 0.5,
                        (1/table(train$Response)[2]) * 0.5)


set.seed(123)
weighted_glm_model <- train(x, y, method = "glmnet", 
                             weights=model_weights,
                             trControl = ctrl,metric = "ROC",
                             tuneGrid = expand.grid(alpha = 0:1,
                                                    lambda = seq(0,0.001,length=100)))
weighted_glm_model


# best parameter
weighted_glm_model$bestTune
weighted_glm_model$results[133,]


# best coefficient
coef(weighted_glm_model$finalModel, weighted_glm_model$bestTune$lambda)

# variable importance
ImpMeasure<-data.frame(varImp(weighted_glm_model)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
ImpMeasure[order(-ImpMeasure$Overall),][1:10,]

#### part 5
# down sample method to re-sampling the data
# use k-fold cross-validation on SVM
# with regularization



# For SVM, we use Linear kernel function. Due to limited 
# computation reason, we use 1% of down-sampled data for tuning parameter 
# and all down-sampled data for training
set.seed(123)
down_train_sample <- down_train[sample(nrow(down_train),934,replace=TRUE,
                                         prob=rep(1/nrow(down_train),nrow(down_train))),]
table(down_train_sample$Response)

# Dummy code categorical predictor variables
#smote_x_sample <- model.matrix(Response~., smote_train_sample)[,-1]

#levels(smote_train_sample$Response) <- c("No", "Yes")

#smote_y_sample <- smote_train_sample$Response


# Fit the model on the sample set
set.seed(123)

## SVM
myGrid <- expand.grid(C = 2^(-5:10),sigma = 2^(-10:3))
        
down_sample_svm_model_radial <- caret::train(Response~.,
                                              data=down_train_sample,
                          method = "svmRadial",
                          trControl=ctrl,
                          metric = "ROC",
                          verboseIter = T,
                          #preProcess = c("center","scale"),
                          tuneGrid = myGrid)
down_sample_svm_model_radial

# Print the best tuning parameter sigma and C that
# maximizes model accuracy

down_sample_svm_model_radial$bestTune
max(down_sample_svm_model_radial$results$ROC)#0.7739307
plot(down_sample_svm_model_radial)


# coef(down_svm_model_linear$finalModel, down_svm_model_linear$bestTune$lambda)


library('kernlab')
library(e1071)

# sample for 20% for training
#set.seed(123)
#down_train_sample1 <- down_train[sample(nrow(down_train),18684,replace=TRUE,
#                                       prob=rep(1/nrow(down_train),nrow(down_train))),]
#table(down_train_sample1$Response)




smp_size <- floor(0.7 * nrow(down_train))
set.seed(123)
train_for_training <- sample(seq_len(nrow(down_train)), size = smp_size)
for_training <- down_train[train_for_training, ]
for_testing <- down_train[-train_for_training, ]
dim(for_training)
dim(for_testing)

down_svm_model <- ksvm(Response ~ .,
                 data = for_training,
                kernel = "rbfdot",
                kpar = list(sigma=0.001953125),
                C = 1,
                 prob.model = TRUE,
                 scaled = FALSE)
down_svm_model


# variable importance


library(pROC)
library(kernlab)

predict_Response_down_svm = predict(down_svm_model,for_testing,type="probabilities")
predict_Response_down_svm = as.data.frame(predict_Response_down_svm)$Yes

# set response as numeric

# predict_Response_down_svm <- ifelse(predict_Response_down_svm=="Yes", 1, 0)
rocCurve_down_svm = pROC::roc(response = for_testing$Response,
                   predictor = predict_Response_down_svm,
                   levels=c('No','Yes'))
auc_down_svm = auc(rocCurve_down_svm)
plot(rocCurve_down_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(Down_SVM)")
#### part 6
# use k-fold cross-validation on SVM
# radial kernel
# weighted loss function
# with regularization

# Print the best tuning parameter sigma and C that
# maximizes model accuracy

# choose 20% from train1 to train the model 381109*0.2

set.seed(123)
train_sample <- train1[sample(nrow(train1),76221,replace=TRUE,
                                       prob=rep(1/nrow(train1),nrow(train1))),]
table(train_sample$Response)



smp_size1 <- floor(0.7 * nrow(train_sample))
set.seed(123)
train_for_training1 <- sample(seq_len(nrow(train_sample)), size = smp_size1)
for_training1 <- train_sample[train_for_training1, ]
for_testing1 <- train_sample[-train_for_training1, ]
dim(for_training1)
dim(for_testing1)

weighted_svm_model <- ksvm(Response ~ .,
                       data = for_training1,
                       kernel = "rbfdot",
                       kpar = list(sigma=0.001953125),
                       C = 1,
                       weights=model_weights,
                       prob.model = TRUE,
                       scaled = FALSE)
weighted_svm_model



library(kernlab)
predict_Response_weighted_svm = predict(weighted_svm_model,for_testing1,type="probabilities")
predict_Response_weighted_svm = as.data.frame(predict_Response_weighted_svm)$Yes

# set response as numeric

# predict_Response_down_svm <- ifelse(predict_Response_down_svm=="Yes", 1, 0)
rocCurve_weighted_svm = pROC::roc(response = for_testing1$Response,
                              predictor = predict_Response_weighted_svm,
                              levels=c('No','Yes'))
auc_weighted_svm = auc(rocCurve_weighted_svm)

par(mfrow=c(1,2))
plot(rocCurve_down_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(Down_SVM)")
plot(rocCurve_weighted_svm,legacy.axes = TRUE,print.auc = TRUE,col="blue",main="ROC(Weighted_SVM)")
