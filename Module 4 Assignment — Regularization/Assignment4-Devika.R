library(ISLR)

college_dataset_ISLR<- College
head(college_dataset_ISLR)
str(college_dataset_ISLR)
#------------------
##Question 1
#------------------
set.seed(20353)
sample_size_75 <- floor(0.75 * nrow(college_dataset_ISLR))

trainingSet_Indices <- sample(seq_len(nrow(college_dataset_ISLR)), size = sample_size_75)
trainingSet_Data_75 <- college_dataset_ISLR[trainingSet_Indices, ]
testSet_Data_25 <- college_dataset_ISLR[-trainingSet_Indices, ]

trainingSet_Data_75_x <-model.matrix(S.F.Ratio ~ .,trainingSet_Data_75)[,-1]
testSet_Data_25_x <-model.matrix(S.F.Ratio ~ .,testSet_Data_25)

trainingSet_Data_75_y <- trainingSet_Data_75$S.F.Ratio
testSet_Data_25_y <- testSet_Data_25$S.F.Ratio

#------------------
## Ridge Regression
#------------------
#------------------
##Question 2
#------------------
library(glmnet)
set.seed(20353)
crossValidation_fit_glmnet_ridge <- cv.glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                                        alpha = 0, nfolds = 10)
crossValidation_fit_glmnet_ridge
cat("Lambda.min:", crossValidation_fit_glmnet_ridge$lambda.min, "\n")
cat("Lambda.1se:", crossValidation_fit_glmnet_ridge$lambda.1se, "\n")

log(crossValidation_fit_glmnet_ridge$lambda.min)
log(crossValidation_fit_glmnet_ridge$lambda.1se)

#------------------
##Question 3
#------------------
plot(crossValidation_fit_glmnet_ridge)

#------------------
##Question 4
#------------------
ridgeModel_q4 <- glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                     alpha = 0, lambda = crossValidation_fit_glmnet_ridge$lambda.min)
options(scipen = 999)
coefficients(ridgeModel_q4)

#------------------
##Question 5
#------------------
predicted_trainingSet_Data_75_y_ridge <- predict(ridgeModel_q4, newx = trainingSet_Data_75_x)
rmse_train_ridge <- sqrt(mean((trainingSet_Data_75_y - predicted_trainingSet_Data_75_y_ridge)^2))
rmse_train_ridge

#------------------
##Question 6
#------------------
predicted_testSet_Data_25_y_ridge <- predict(ridgeModel_q4, newx = testSet_Data_25_x)
rmse_test_ridge <- sqrt(mean((testSet_Data_25_y - predicted_testSet_Data_25_y_ridge)^2))
rmse_test_ridge


#------------------
## LASSO
#------------------
#------------------
##Question 7
#------------------
set.seed(20353)
crossValidation_fit_glmnet_lasso <- cv.glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                                              alpha = 1, nfolds = 10)

cat("Lambda.min:", crossValidation_fit_glmnet_lasso$lambda.min, "\n")
cat("Lambda.1se:", crossValidation_fit_glmnet_lasso$lambda.1se, "\n")

log(crossValidation_fit_glmnet_lasso$lambda.min)
log(crossValidation_fit_glmnet_lasso$lambda.1se)

#------------------
##Question 8
#------------------
plot(crossValidation_fit_glmnet_lasso)

#------------------
##Question 9
#------------------
lassoModel_q9 <- glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                        alpha = 1, lambda = crossValidation_fit_glmnet_lasso$lambda.min)
options(scipen = 999)
coefficients(lassoModel_q9)

#------------------
##Question 10
#------------------
predicted_trainingSet_Data_75_y_lasso <- predict(lassoModel_q9, newx = trainingSet_Data_75_x)
rmse_train_lasso <- sqrt(mean((trainingSet_Data_75_y - predicted_trainingSet_Data_75_y_lasso)^2))
rmse_train_lasso

#------------------
##Question 11
#------------------
predicted_testSet_Data_25_y_lasso <- predict(lassoModel_q9, newx = testSet_Data_25_x)
rmse_test_lasso <- sqrt(mean((testSet_Data_25_y - predicted_testSet_Data_25_y_lasso)^2))
rmse_test_lasso

#------------------
## ElasticNet Let Alpha = 0.5
#------------------
#------------------
##Question 12
#------------------
set.seed(20353)
crossValidation_fit_glmnet_elasticNet <- cv.glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                                              alpha = 0.5, nfolds = 10)
cat("Lambda.min:", crossValidation_fit_glmnet_elasticNet$lambda.min, "\n")
cat("Lambda.1se:", crossValidation_fit_glmnet_elasticNet$lambda.1se, "\n")
log(crossValidation_fit_glmnet_elasticNet$lambda.min)
log(crossValidation_fit_glmnet_elasticNet$lambda.1se)

plot(crossValidation_fit_glmnet_elasticNet)

elasticNetModel_q12 <- glmnet(trainingSet_Data_75_x, trainingSet_Data_75_y, 
                        alpha = 0.5, lambda = crossValidation_fit_glmnet_elasticNet$lambda.min)
options(scipen = 999)
coefficients(elasticNetModel_q12)

predicted_trainingSet_Data_75_y_elasticNet <- predict(elasticNetModel_q12, newx = trainingSet_Data_75_x)
rmse_train_elasticNet <- sqrt(mean((trainingSet_Data_75_y - predicted_trainingSet_Data_75_y_elasticNet)^2))
rmse_train_elasticNet

predicted_testSet_Data_25_y_elasticNet <- predict(elasticNetModel_q12, newx = testSet_Data_25_x)
rmse_test_elasticNet <- sqrt(mean((testSet_Data_25_y - predicted_testSet_Data_25_y_elasticNet)^2))
rmse_test_elasticNet

#------------------
##Question 14
#------------------
set.seed(20353)
stepwise_model_q14 <- step(lm(S.F.Ratio ~ ., data = trainingSet_Data_75), direction = 'both')
summary(stepwise_model_q14)

stepwise_model_q14_prediction_train <- predict(stepwise_model_q14, newdata = trainingSet_Data_75)
rmse_stepwise_model_q14_train <- sqrt(mean((trainingSet_Data_75$S.F.Ratio - stepwise_model_q14_prediction_train)^2))
rmse_stepwise_model_q14_train

stepwise_model_q14_prediction_test <- predict(stepwise_model_q14, newdata = testSet_Data_25)
rmse_stepwise_model_q14_test <- sqrt(mean((testSet_Data_25$S.F.Ratio - stepwise_model_q14_prediction_test)^2))
rmse_stepwise_model_q14_test

