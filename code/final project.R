# workplace run above line 88
library(sqldf)
library(glmnet)
library(caret)
library('kernlab')
library(tibble)
library(broom)
library(tidyverse)
library(DMwR)
setwd('/Users/ruofanchen/Downloads')
fixed   <- file("VS19MORT.DUSMCPUB_r20201216")

attr(fixed, "file.format") <- list(sep = ",", header = FALSE) 
US2019 <- sqldf("SELECT 
                   substr(V1, 63, 1) Education_03v,
                   substr(V1, 65, 2) Month,
                   substr(V1, 69, 1) Sex, 
                   substr(V1, 70, 1) Age_detail_type,
                   substr(V1, 71, 3) Age_detail,
                   substr(V1, 84, 1) Marital,
                   substr(V1,107,1) Manner_of_death,
                   substr(V1,146,4) ICD_code,
                   substr(V1,450,1) Bridged_race_5recode
                   FROM fixed")
#US2019_sui <- US2019[US2019$Manner_of_death=='2',]
#US2019_other <- US2019[US2019$Manner_of_death=='1'|US2019$Manner_of_death=='3'|
#                         US2019$Manner_of_death=='6'|US2019$Manner_of_death=='7',]
#write.csv(US2019_sui,file = 'US2019_sui')

US2019_ana <- US2019[US2019$Manner_of_death=='1'|US2019$Manner_of_death=='2'|US2019$Manner_of_death=='3'|
                       US2019$Manner_of_death=='6'|US2019$Manner_of_death=='7',]

US2019_ana$Education_03v <- as.factor(US2019_ana$Education_03v)
US2019_ana$Age_detail <- as.numeric(US2019_ana$Age_detail)
US2019_ana$Age_detail_type <- as.numeric(US2019_ana$Age_detail_type)
US2019_ana$Month <- as.factor(US2019_ana$Month)
US2019_ana$Marital <- as.factor(US2019_ana$Marital)
US2019_ana$Sex <- as.factor(US2019_ana$Sex)
US2019_ana$ICD_code <- as.factor(US2019_ana$ICD_code)
US2019_ana$Bridged_race_5recode <- as.factor(US2019_ana$Bridged_race_5recode)

summary(US2019_ana)

US2019_ana$Manner_of_death <- as.character(US2019_ana$Manner_of_death)
US2019_ana$Manner_of_death[US2019_ana$Manner_of_death!='2'] <- 'N'
US2019_ana$Manner_of_death[US2019_ana$Manner_of_death=='2'] <- 'Y'
US2019_ana$Manner_of_death <- as.factor(US2019_ana$Manner_of_death)

summary(US2019_ana)

nlevels(US2019_ana$Education_03v)
table(US2019_ana$Education_03v)

sort(table(US2019_ana$ICD_code),decreasing = T)

# remove age missing obs
US2019_ana_pre <- subset(US2019_ana,Age_detail_type!=9 &Age_detail!=999)
Newage <- rep(1000,dim(US2019_ana_pre)[1])

Newage[US2019_ana_pre$Age_detail_type==1] <- US2019_ana_pre[US2019_ana_pre$Age_detail_type==1,5]
Newage[US2019_ana_pre$Age_detail_type==2] <- US2019_ana_pre[US2019_ana_pre$Age_detail_type==2,5]/12
Newage[US2019_ana_pre$Age_detail_type==4] <- US2019_ana_pre[US2019_ana_pre$Age_detail_type==4,5]/(12*30)
Newage[US2019_ana_pre$Age_detail_type==5] <- US2019_ana_pre[US2019_ana_pre$Age_detail_type==5,5]/(12*30*24)
Newage[US2019_ana_pre$Age_detail_type==6] <- US2019_ana_pre[US2019_ana_pre$Age_detail_type==6,5]/(12*30*24*60)

# new dataset
US2019_ana_pre_new <- data.frame(US2019_ana_pre$Education_03v,
                                 US2019_ana_pre$Month,
                                 US2019_ana_pre$Sex,
                                 US2019_ana_pre$Marital,
                                 US2019_ana_pre$Bridged_race_5recode,
                                 Newage,
                                 US2019_ana_pre$Manner_of_death)
names(US2019_ana_pre_new) <- c('edu','mon','sex','mar','race','age','manner')
class(US2019_ana_pre_new$manner)

###########################################################################
response <- rep(1000,dim(US2019_ana_pre_new)[1])
response[US2019_ana_pre_new$manner=='N'] <- 0
response[US2019_ana_pre_new$manner=='Y'] <- 1

US2019_ana_pre_new$response <- response



# split the data into training and test
## 70% of the sample size
smp_size <- floor(0.7 * nrow(US2019_ana_pre_new))
## set the seed to make partition reproducible
set.seed(123)
US2019_train <- sample(seq_len(nrow(US2019_ana_pre_new)), size = smp_size)
US2019_ana_train <- US2019_ana_pre_new[US2019_train, ]
US2019_ana_test <- US2019_ana_pre_new[-US2019_train, ]
dim(US2019_ana_train)
dim(US2019_ana_test)

x <- model.matrix(manner~edu+mon+sex+mar+race+age, US2019_ana_train)[,-1]
y <- US2019_ana_train$response


# Create model weights (they sum to one)
model_weights <- ifelse(US2019_ana_train$response==0,
                        (1/table(US2019_ana_train$response)[1]) * 0.5,
                        (1/table(US2019_ana_train$response)[2]) * 0.5)

set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial",nfolds=5,weights = model_weights
                      ,type.measure = 'auc')
# Fit the final model on the training data
set.seed(123)
weighted_glm_model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min,weights = model_weights)

coef(weighted_glm_model)
# Make predictions on the test data
x.test <- model.matrix(manner~edu+mon+sex+mar+race+age, US2019_ana_test)[,-1]
pred <- predict(weighted_glm_model, newx = x.test, type = 'response',s ="lambda.min")
auc(label,pred)





x.test <- model.matrix(manner~edu+mon+sex+mar+race+age, US2019_ana_test)[,-1]
probabilities <- weighted_glm_model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "Y", "N")
# Model accuracy
observed.classes <- US2019_ana_test$manner
mean(predicted.classes == observed.classes)





x.test <- model.matrix(diabetes ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)





ctrl <- trainControl(method="cv", number=1,classProbs=TRUE,
                     summaryFunction = twoClassSummary)
weighted_glm_model <- train(x, y, method = "glmnet", 
                            weights=model_weights,
                            trControl = ctrl,
                            metric = "ROC",
                            tuneGrid = expand.grid(alpha = 1,
                                                   lambda = seq(0,0.001,length=5)))
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













max(US2019$Cause_39_code)
US2019$Cause_39_code <- as.numeric(US2019$Cause_39_code)
US2019$Cause_39_code

US2015 <- sqldf("SELECT 
                   substr(V1, 160, 2) Cause, 
                   substr(V1, 69, 1) Sex, 
                   substr(V1, 70, 4) Age, 
                   substr(V1, 65, 2) Month,
                   substr(V1,109,1) Autopsy,
                   substr(V1,107,1) Manner_of_death,
                   count(*) Deaths 
                 FROM fixed 
                 GROUP BY 
                   Cause, 
                   Sex, 
                   Age, 
                   Month,
                Autopsy,
                Manner_of_death")


dim(US2015[US2015$Autopsy=='Y'&US2015$Manner_of_death==1,])
dim(US2015[US2015$Manner_of_death==1,])

dim(US2015[US2015$Autopsy=='Y'&US2015$Manner_of_death==7,])
dim(US2015[US2015$Manner_of_death==7,])
