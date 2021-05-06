# created by Ruofan Chen 
# US Mortality 2019
# Last modified date: May 02,2021


# load data --------------------------------------------------------------------
# load packages
library(sqldf) # required for read data

fixed   <- file("../data/VS19MORT.DUSMCPUB")

attr(fixed, "file.format") <- list(sep = ",", header = FALSE) 

# read data by select columns
US2019 <- sqldf("SELECT 
                   substr(V1, 63, 1) Education_03v,
                   substr(V1, 65, 2) Month,
                   substr(V1, 69, 1) Sex, 
                   substr(V1, 70, 1) Age_detail_type,
                   substr(V1, 71, 3) Age_detail,
                   substr(V1, 84, 1) Marital,
                   substr(V1, 85, 1) Day_of_week,
                   substr(V1,107,1) Manner_of_death,
                   substr(V1, 145, 1) Place_of_injury,
                   substr(V1,146,4) ICD_code,
                   substr(V1,450,1) Bridged_race_5recode
                   FROM fixed")


# get labeled data set, prepared for analysis-----------------------------------

# 12 columns are loaded, see the values of columns
# education: 1 to 9 (9 means unknown)
# Month: 01 to 12
# Sex: M & F
# Age Type: 1,2,4,5,6,9 (9 means not stated)
# Age Detailed: 999 means not stated
# Marital Status: S M W D U (U means unknown)
# Day of Week: 1 to 7, 9 means unknown
# Manner of Death: 1 to 7 & blank (4 means pending investigation, 5 not determined)
# Place of Injury: 0 to 9(means unspecified place), blanks
# ICD code
# Race: 0-4 (0 means other)


str(US2019)
table(US2019$Education_03v)
table(US2019$Month)
table(US2019$Sex)
table(US2019$Age_detail_type)
table(US2019$Age_detail)
table(US2019$Marital)
table(US2019$Day_of_week)
table(US2019$Manner_of_death)
table(US2019$Place_of_injury)
table(US2019$ICD_code)
table(US2019$Bridged_race_5recode)

# get data prepared for analysis -----------------------------------------------
# remove observations/rows that are not stated(9 and 999) 
# except place of injury and day of week
# note data type of all columns are character

# for education
US2019_ana <- US2019[US2019$Education_03v != '9',]

# for Age type
US2019_ana <- US2019_ana[US2019_ana$Age_detail_type != '9',]

# for Age detail
US2019_ana <- US2019_ana[US2019_ana$Age_detail != '999',]

# for marital
US2019_ana <- US2019_ana[US2019_ana$Marital != 'U',]

# for manner of death
US2019_ana <- US2019_ana[US2019_ana$Manner_of_death=='1'|US2019_ana$Manner_of_death=='2'|US2019_ana$Manner_of_death=='3'|
                           US2019_ana$Manner_of_death=='6'|US2019_ana$Manner_of_death=='7',]


# get frequency of Bridged_race_5recode, there's no 0 occurred
table(US2019_ana$Bridged_race_5recode)


table(US2019_ana$Education_03v)
table(US2019_ana$Month)
table(US2019_ana$Sex)
table(US2019_ana$Age_detail_type)
table(US2019_ana$Age_detail)
table(US2019_ana$Marital)
table(US2019_ana$Day_of_week)
table(US2019_ana$Manner_of_death)
table(US2019_ana$Place_of_injury)
table(US2019_ana$ICD_code)



# set column data type
US2019_ana$Education_03v <- as.factor(US2019_ana$Education_03v)
table(US2019_ana$Education_03v)

US2019_ana$Month <- as.factor(US2019_ana$Month)
table(US2019_ana$Month)

US2019_ana$Sex <- as.factor(US2019_ana$Sex)
table(US2019_ana$Sex)

US2019_ana$Age_detail_type <- as.numeric(US2019_ana$Age_detail_type)
table(US2019_ana$Age_detail_type)

US2019_ana$Age_detail <- as.numeric(US2019_ana$Age_detail)
# table(US2019_ana$Age_detail)

US2019_ana$Marital <- as.factor(US2019_ana$Marital)
table(US2019_ana$Marital)

US2019_ana$ICD_code <- as.factor(US2019_ana$ICD_code)

US2019_ana$Bridged_race_5recode <- as.factor(US2019_ana$Bridged_race_5recode)
table(US2019_ana$Bridged_race_5recode)


US2019_ana$Manner_of_death[US2019_ana$Manner_of_death!='2'] <- 'N'
US2019_ana$Manner_of_death[US2019_ana$Manner_of_death=='2'] <- 'Y'
US2019_ana$Manner_of_death <- as.factor(US2019_ana$Manner_of_death)
table(US2019_ana$Manner_of_death)


# calculate age by year
Newage <- rep(1000,dim(US2019_ana)[1])

Newage[US2019_ana$Age_detail_type==1] <- US2019_ana[US2019_ana$Age_detail_type==1,5]
Newage[US2019_ana$Age_detail_type==2] <- US2019_ana[US2019_ana$Age_detail_type==2,5]/12
Newage[US2019_ana$Age_detail_type==4] <- US2019_ana[US2019_ana$Age_detail_type==4,5]/(12*30)
Newage[US2019_ana$Age_detail_type==5] <- US2019_ana[US2019_ana$Age_detail_type==5,5]/(12*30*24)
Newage[US2019_ana$Age_detail_type==6] <- US2019_ana[US2019_ana$Age_detail_type==6,5]/(12*30*24*60)

# deal with place of injury and day of week

US2019_ana$Day_of_week <- as.factor(US2019_ana$Day_of_week)
table(US2019_ana$Day_of_week)
US2019_ana$Place_of_injury[US2019_ana$Place_of_injury==' '] <- NA

US2019_ana$Place_of_injury <- as.factor(US2019_ana$Place_of_injury)


# new data set
US2019_ana_pre <- data.frame(US2019_ana$Education_03v,
                                 US2019_ana$Month,
                                 US2019_ana$Sex,
                                 Newage,
                                 US2019_ana$Marital,
                                 US2019_ana$Day_of_week,
                                 US2019_ana$Place_of_injury,
                                 US2019_ana$ICD_code,    
                                 US2019_ana$Bridged_race_5recode,
                                 US2019_ana$Manner_of_death)
names(US2019_ana_pre) <- c('edu','mon','sex','age','mar','week','place',
                           'ICD','race','manner')


# get data prepared for descriptive analysis -----------------------------------
# the analysis is based on the suicide subset
US2019_sui <- US2019_ana_pre[US2019_ana_pre$manner=='Y',]

# removed all missing values
US2019_sui <- na.omit(US2019_sui)

# removed the observation including unspecified values or blanks
US2019_sui <- US2019_sui[US2019_sui$place != '9',]
US2019_sui <- US2019_sui[US2019_sui$week != '9',]

sort(table(US2019_sui$ICD),decreasing = T)[1:10]

# create a level: if ICD not in  X74,X70,X72 then belongs to other

levels(US2019_sui$ICD) <- c(levels(US2019_sui$ICD),'Othr')

US2019_sui[!(US2019_sui$ICD %in% c('X74 ','X70 ','X72 ')),8] <- as.factor('Othr')
US2019_sui$ICD <- droplevels(US2019_sui$ICD)
table(US2019_sui$ICD)

# see the age distribution of suicide, compared to overall death
# create a overlaid histogram
# combine your two data frames into one, first make a new column in each.
plot_age1 <- US2019_sui
plot_age2 <- US2019_ana_pre

plot_age1$age_tag <- 'suicide_age'
plot_age2$age_tag <- 'overall_age'

# combine the two data frames plot_age1 and plot_age2
combo_age <- rbind(plot_age1, plot_age2)
library(ggplot2)
ggplot(combo_age, aes(age, fill = age_tag)) + geom_density(alpha = 0.2)


# place of death
barplot(table(US2019_sui$place)) # Home, similar with overall
barplot(table(US2019_ana_pre$place))

# education
barplot(table(US2019_sui$edu))
sort(table(US2019_ana_pre$edu),decreasing = T)# high school, similar with overall

# month
sort(table(US2019_sui$mon),decreasing = T) # 08,07,09,10...
sort(table(US2019_ana_pre$mon),decreasing = T) #12,01,03,11...

# sex
sort(table(US2019_sui$sex),decreasing = T) # male
sort(table(US2019_ana_pre$sex),decreasing = T) # male

# marital
sort(table(US2019_sui$mar),decreasing = T) # single
sort(table(US2019_ana_pre$mar),decreasing = T) # married

# week
sort(table(US2019_sui$week),decreasing = T) # similar for all day of week
sort(table(US2019_ana_pre$week),decreasing = T) # similar for all day of week

# ICD
sort(table(US2019_sui$ICD),decreasing = T)[1:10]


# race
sort(table(US2019_sui$race),decreasing = T) # white similar
sort(table(US2019_ana_pre$race),decreasing = T)


###########################################################################
response <- rep(1000,dim(US2019_ana_pre)[1])
response[US2019_ana_pre$manner=='N'] <- 0
response[US2019_ana_pre$manner=='Y'] <- 1

US2019_ana_pre$response <- response

# save data as 'US2019_ana_pre.csv' file
# write.csv(US2019_ana_pre,file = 'US2019_ana_pre.csv',row.names = F)



# modeling part ----------------------------------------------------------------
data <- US2019_ana_pre

data <- subset(data,select = -c(week,place,ICD))

# down sample modeling ---------------------------------------------------------

# down sample
library(caret)
set.seed(123)
down_data <- downSample(data[,1:6],data$manner,yname='manner')
table(down_data$manner)
summary(down_data)

# get design matrix and y (based on downsampled data)
down_x <- model.matrix(manner~edu+mon+sex+age+mar+race, down_data)[,-1]

# design matrix with beta0 equal to 1
down_xwith <- model.matrix(manner~edu+mon+sex+age+mar+race, down_data)
down_y <- ifelse(down_data$manner=='Y',1,0)

# find the best penalized parameter based on downsampled
library(glmnet)
set.seed(123)
down_cv.lasso <- cv.glmnet(down_x, down_y, alpha = 1, family = "binomial",nfolds=5
                      ,type.measure = 'auc')

# split the downsample into train and test set
down_smp_size <- floor(0.8 * nrow(down_x))
set.seed(123)
down_train_ind <- sample(seq_len(nrow(down_x)), size = down_smp_size)
down_x_train <- down_x[down_train_ind, ]
down_x_test <- down_x[-down_train_ind, ]
down_y_train <- down_y[down_train_ind]
down_y_test <- down_y[-down_train_ind]
down_xwith_train <- down_xwith[down_train_ind, ]
down_xwith_test <- down_xwith[-down_train_ind, ]


# Fit the final model on the training data, based on downsample
#set.seed(123)
#down_glm_model <- glmnet(down_x_train, down_y_train, alpha = 1, family = "binomial",
#                             lambda = down_cv.lasso$lambda.min)
#coef(down_glm_model)

down_loss <- function(par){
  return(sum( (-down_y_train * (down_xwith_train %*% par)  + log( 1+ exp((down_xwith_train %*% (par) )))))/nrow(down_xwith_train) + down_cv.lasso$lambda.min * sum(abs(par[-1])))
}
beta_down <- optim(par = rep(0,ncol(down_xwith_train)), fn=down_loss,method='BFGS',control=(maxit = 1e+05),hessian=T)
beta_down$par


# significant test:
beta_down_fisher <- solve(beta_down$hessian)
prop_sigma <- sqrt(diag(beta_down_fisher)/nrow(down_xwith_train))
upper <- beta_down$par+1.96*prop_sigma
lower <- beta_down$par-1.96*prop_sigma
sign(upper) - sign(lower)

# Make predictions on the test data
down_pred <- exp(down_xwith_test %*% beta_down$par)/(1+exp(down_xwith_test %*% beta_down$par))
library(pROC)
down_g <- roc(down_y_test ~ down_pred, print.auc=T,algorithm=2)
as.numeric(down_g$auc)
plot(down_g,print.thres=TRUE)

# Stage-wise feature adding: mon, race, sex, edu, mar, age 
#1
set.seed(123)
down_train_ind <- sample(seq_len(nrow(down_data)), size = down_smp_size)
down_data_train <- down_data[down_train_ind, ]
down_data_test <- down_data[-down_train_ind, ]
down_glm_model_1 <- glm(manner~mon, family=binomial(link = 'logit'),data = down_data_train)
library(tidyverse)
down_pred_1 <- down_glm_model_1 %>% predict(down_data_test, type = "response")
down_g1 <- roc(down_data_test$manner ~ down_pred_1, print.auc=T,algorithm=2)
plot(down_g1)

#2
down_glm_model_2 <- glm(manner~mon+race, family=binomial(link = 'logit'),data = down_data_train)

# check multicollinearity
library(car)
vif(down_glm_model_2)
down_pred_2 <- down_glm_model_2 %>% predict(down_data_test, type = "response")
down_g2 <- roc(down_data_test$manner ~ down_pred_2, print.auc=T,algorithm=2)
plot(down_g2)

#3
down_glm_model_3 <- glm(manner~mon+race+sex, family=binomial(link = 'logit'),data = down_data_train)

# check multicollinearity
vif(down_glm_model_3)
down_pred_3 <- down_glm_model_3 %>% predict(down_data_test, type = "response")
down_g3 <- roc(down_data_test$manner ~ down_pred_3, print.auc=T,algorithm=2)
plot(down_g3)

#4
down_glm_model_4 <- glm(manner~mon+race+sex+edu, family=binomial(link = 'logit'),data = down_data_train)
vif(down_glm_model_4)
down_pred_4 <- down_glm_model_4 %>% predict(down_data_test, type = "response")
down_g4 <- roc(down_data_test$manner ~ down_pred_4, print.auc=T,algorithm=2)
plot(down_g4)

#5
down_glm_model_5 <- glm(manner~mon+race+sex+edu+mar, family=binomial(link = 'logit'),data = down_data_train)
vif(down_glm_model_5)
down_pred_5 <- down_glm_model_5 %>% predict(down_data_test, type = "response")
down_g5 <- roc(down_data_test$manner ~ down_pred_5, print.auc=T,algorithm=2)
plot(down_g5)

#6
down_glm_model_6 <- glm(manner~mon+race+sex+edu+mar+age, family=binomial(link = 'logit'),data = down_data_train)
vif(down_glm_model_6)
down_pred_6 <- down_glm_model_6 %>% predict(down_data_test, type = "response")
down_g6 <- roc(down_data_test$manner ~ down_pred_6, print.auc=T,algorithm=2)
plot(down_g6)


ggroc(list('+mon' = down_g1, '+mon+race' = down_g2,
           '+mon+race+sex' = down_g3,'+mon+race+sex+edu' = down_g4,
           '+mon+race+sex+edu+mar' = down_g5,'+mon+race+sex+edu+mar+age' = down_g6,
           'all (L1)' = down_g)) # + geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1)) # legacy.axes = FALSE


# weighted loss logistic modeling ----------------------------------------------

# get design matrix and y (based on original data, use weighted loss)
x <- model.matrix(manner~edu+mon+sex+age+mar+race, data)[,-1]
# design matrix with beta0 1
xwith <- model.matrix(manner~edu+mon+sex+age+mar+race, data)
y <- ifelse(data$manner=='Y',1,0)

smp_size <- floor(0.8 * nrow(x))
set.seed(123)
train_ind <- sample(seq_len(nrow(x)), size = smp_size)
x_train <- x[train_ind, ]
x_test <- x[-train_ind, ]
xwith_train <- xwith[train_ind, ]
xwith_test <- xwith[-train_ind, ]
y_train <- y[train_ind]
y_test <- y[-train_ind]

# Create model weights (they sum to one)
model_weights <- ifelse(y_train==0,table(y_train)[2]/nrow(x_train),table(y_train)[1]/nrow(x_train))
scale_weights <- model_weights * nrow(x_train) / sum(model_weights)

set.seed(123)
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial",nfolds=5,weights = scale_weights
                      ,type.measure = 'auc')


# Fit the final model on the training data
#set.seed(123)
#weighted_glm_model <- glmnet(x_train, y_train, alpha = 1, family = "binomial",
#                             lambda = cv.lasso$lambda.min,weights = model_weights[train_ind])
#coef(weighted_glm_model)

weight_loss <- function(par){
  return(sum( (-y_train * (xwith_train %*% par)  + log( 1+ exp((xwith_train %*% (par) )))) * scale_weights )/nrow(xwith_train) + cv.lasso$lambda.min * sum(abs(par[-1])))
}
beta_weight <- optim(par = rep(0,ncol(xwith_train)), fn=weight_loss,method='BFGS',control=(maxit = 1e+05),hessian=T)
beta_weight$par


# significant test:
beta_weight_fisher <- solve(beta_weight$hessian)
prop_sigma <- sqrt(diag(beta_weight_fisher)/nrow(xwith_train))
upper <- beta_weight$par+1.96*prop_sigma
lower <- beta_weight$par-1.96*prop_sigma
sign(upper) - sign(lower)

# Make predictions on the test data
weight_pred <- exp(xwith_test %*% beta_weight$par)/(1+exp(xwith_test %*% beta_weight$par))
library(pROC)
g <- roc(y_test ~ weight_pred, print.auc=T,algorithm=2)
as.numeric(g$auc)
plot(g,print.thres=TRUE)


# Stage-wise feature adding: mon, race, sex, edu, mar, age, 
#1
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
data_train <- data[train_ind, ]
data_test <- data[-train_ind, ]
data_train$weight = ifelse(data_train$response==0,sum(data_train$response==1)/nrow(data_train),sum(data_train$response==0)/nrow(data_train))
weighted_glm_model <- glm(response~mon, family=binomial(link = 'logit'), weights = weight,data = data_train)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g1 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)
plot(g1)

#2
weighted_glm_model <- glm(response~mon+race, family=binomial(link = 'logit'), weights = model_weights[train_ind],data = data_train)
vif(weighted_glm_model)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g2 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)

#3
weighted_glm_model <- glm(response~mon+race+sex, family=binomial(link = 'logit'), weights = model_weights[train_ind],data = data_train)
vif(weighted_glm_model)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g3 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)

#4
weighted_glm_model <- glm(response~mon+race+sex+edu, family=binomial(link = 'logit'), weights = model_weights[train_ind],data = data_train)
vif(weighted_glm_model)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g4 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)

#5
weighted_glm_model <- glm(response~mon+race+sex+edu+mar, family=binomial(link = 'logit'), weights = model_weights[train_ind],data = data_train)
vif(weighted_glm_model)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g5 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)

#6
weighted_glm_model <- glm(response~mon+race+sex+edu+mar+age, family=binomial(link = 'logit'), weights = model_weights[train_ind],data = data_train)
vif(weighted_glm_model)
pred <- weighted_glm_model %>% predict(data_test, type = "response")
g6 <- roc(data_test$response ~ pred, print.auc=T,algorithm=2)

ggroc(list('+mon' = g1, '+mon+race' = g2,
           '+mon+race+sex' = g3,'+mon+race+sex+edu' = g4,
           '+mon+race+sex+edu+mar' = g5,'+mon+race+sex+edu+mar+age' = g6,
           'all (L1)' = g)) # + geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1)) # legacy.axes = FALSE


# end of logistic regression ---------------------------------------------------

# Start of transfer learning ---------------------------------------------------

x_with <- model.matrix(manner~edu+mon+sex+age+mar+race, down_data)
y <- ifelse(down_data$manner=='Y',1,0)
x <- model.matrix(manner~edu+mon+sex+age+mar+race, down_data)[,-1]

# split the sample into two with same number observations
smp_size <- floor(0.5 * nrow(down_data))
set.seed(123)
select_ind <- sample(seq_len(nrow(down_data)), size = smp_size,replace = FALSE)
x_with1 <- x_with[select_ind, ]
x_with2 <- x_with[-select_ind, ]
x_1 <- x[select_ind, ]
x_2 <- x[-select_ind, ]
y_1 <- y[select_ind]
y_2 <- y[-select_ind]

# Determine the appropriate lambda:
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial",nfolds=5
                      ,type.measure = 'auc')
lambda_1 = cv.lasso$lambda.min
cv.lasso <- cv.glmnet(x_1, y_1, alpha = 1, family = "binomial",nfolds=5
                      ,type.measure = 'auc')
lambda_2 = cv.lasso$lambda.min

# calculate beta trans

trans_glm <- function(x_target,y_target,x_aux,y_aux,lambda_w,lambda_delta){
  if (ncol(x_target)!=ncol(x_aux)){
    print("Two numbers of features don't match!")
    break
  }
  # Step 1: 
  x_comb = rbind(x_target,x_aux)
  y_comb = c(y_target,y_aux)
  inner_loss1 <- function(par){
    return(sum( (-y_comb * (x_comb %*% par)  + log( 1+ exp((x_comb %*% par)))))/nrow(x_comb) + lambda_w * sum(abs(par[-1])))
  }
  w_hat <- optim(par = rep(0,ncol(x_comb)), fn=inner_loss1,method='BFGS')$par
  print(w_hat)
  # Step 2:
  inner_loss2 <- function(par){
    return(sum( (-y_target * (x_target %*% par)  + log( 1+ exp((x_target %*% (w_hat+par) )))))/nrow(x_target) + lambda_delta * sum(abs(par[-1])))
  }
  delta_hat <- optim(par = rep(0,ncol(x_target)), fn=inner_loss2,method='BFGS')$par
  print(delta_hat)
  return(w_hat+delta_hat)
}

beta_trans <-trans_glm(x_with1,y_1,x_with2,y_2,lambda_1,lambda_2)

# calculate beta part

inner_loss_part <- function(par){
  return(sum( -y_1 * (x_with1 %*% par)  + log( 1+ exp((x_with1 %*% par))))/nrow(x_with1) + lambda_2 * sum(abs(par[-1])))
}

beta_part <- optim(par = rep(0,ncol(x_with1)), fn=inner_loss_part,method='BFGS')$par

# Model comparison:

sum((beta_trans - beta_down$par)^2)
sum((beta_part - beta_down$par)^2)



