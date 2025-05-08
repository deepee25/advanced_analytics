install.packages("ggplot2")
library(ISLR)
library(ggplot2)
#---------------
#Ques_1
#---------------

#importing data into a dataFrame
college_df_lm <- College
dim(college_df_lm)
head(college_df_lm)

summary(college_df_lm)
str(college_df_lm)

hist(college_df_lm$Outstate, main = "Out-of-state Tuition Histogram with Mean and Median Line",
     xlab = "Out-Of-State Tuition", ylab = "Frequency", col = "cyan")
abline(v = median(college_df_lm$Outstate), col = "orange", lwd = 2)
abline(v = mean(college_df_lm$Outstate), col = "red", lwd = 2)
legend("topright", legend = c("Mean", "Median"),col = c("red", "orange"), lty = 1, lwd = 2, cex = 0.6)

#distribution of the dataset 
#we have records of 565 Private Universities and 212 Public Universities
college_df_lm_plot = college_df_lm
college_df_lm_plot$Type = ifelse(college_df_lm_plot$Private == "Yes", "Private University", "Public University")
typeFreq = table(college_df_lm_plot$Type)
barplot(typeFreq, main = "Distribution of Colleges by Type in Dataset",
        xlab = "Type", ylab = "Frequency", col = "skyblue",
        ylim = c(0, max(typeFreq) + 50))

# In this plot, the box plot for each category (private and non-private) 
# shows the distribution of out-of-state tuition, allowing you to compare 
# the tuition between the two types of colleges.
boxplot(Outstate ~ Type, data = college_df_lm_plot, 
        main = "Outstate Tuition by College Type", 
        xlab = "Type", 
        ylab = "Outstate Tuition",
        col = "lightblue")

qplot(x=Outstate, y=Room.Board, color=Private, 
      shape=Private, data = college_df_lm, geom='point')+
  ggtitle("Out-of-state Tuition vs. Room and Board Costs") +
  xlab("Out-of-state Tuition") +
  ylab("Room and Board Costs") +
  scale_shape(solid=FALSE)

#---------------
#Ques_2
#---------------
college_df_lm

set.seed(20353)
sampSize <- floor(0.75 * nrow(college_df_lm))

trainSetIndices <- sample(seq_len(nrow(college_df_lm)), size = sampSize)
trainSetData <- college_df_lm[trainSetIndices, ]
testSetData <- college_df_lm[-trainSetIndices, ]

cat("Train Data Dimensions:\nNo. Rows =", dim(trainSetData)[1],"\nNo. Columns=",dim(trainSetData)[2],"\n")
cat("Test Data Dimensions:\nNo. Rows =", dim(testSetData)[1],"\nNo. Columns=",dim(testSetData)[2],"\n")

print("Train Data")
head(trainSetData)
print("Test Data")
head(testSetData)

#---------------
#Ques_3
#---------------

#=======================================================================
# library(MASS)
# fullModel <- glm(Private ~ ., 
#                   data = trainSetData, family = binomial(link = "logit"))
# stepModel <- stepAIC(fullModel, direction = "both")
# summary(stepModel)
#Output:
##glm(formula = Private ~ Apps + Top25perc + Outstate + Room.Board + 
##      Books + Personal + PhD + Terminal + perc.alumni + Expend, 
##    family = binomial(link = "logit"), data = trainSetData)
#=======================================================================
logistic_r_model <- glm(Private ~ Apps + Top25perc + Outstate + Room.Board + 
                          Books + Personal + PhD + Terminal + perc.alumni + Expend, data = trainSetData, family = binomial(link = "logit"))
summary(logistic_r_model)
coef(logistic_r_model) # log odds (Display Regression Coefficients)
exp(coef(logistic_r_model)) # odds (Display Regression Coefficients)

#---------------
#Ques_4
#---------------
install.packages("caret")
library(caret)
trainSetData_Pred_Prob <- predict(logistic_r_model, newdata = trainSetData, type = "response")
trainSetData_Pred_Class = as.factor(ifelse(trainSetData_Pred_Prob >= 0.5, "Yes", "No"))

confMatrix_trainSetData = confusionMatrix(trainSetData_Pred_Class, trainSetData$Private, positive = 'Yes')
confMatrix_trainSetData
# The confusion matrix for the logistic regression model on the training set for the College dataset is as follows:
  #                  Reference
  # Prediction    No                  Yes
  #         No   144(True Negative)   16 (False negative)
  #        Yes    20(False Positive)  402(True Positive)
# From this confusion matrix, we can calculate the following metrics:
# -Accuracy: 0.9381
# -Sensitivity (True Positive Rate): 0.9617
# -Specificity (True Negative Rate): 0.8780
# -Precision (Positive Predictive Value): 0.9526
# -Negative Predictive Value: 0.9000
# 
# Interpretation and Discussion:
# -The model has an overall accuracy of 0.9381, indicating that it correctly classifies 93.81% of the observations in the training set.
# -The sensitivity (true positive rate) of 0.9617 suggests that the model performs well in correctly identifying colleges that are private.
# -The specificity (true negative rate) of 0.8780 indicates that the model performs slightly less well in correctly identifying colleges that are not private.
# -The precision (positive predictive value) of 0.9526 indicates that when the model predicts a college is private, it is correct about 95.26% of the time.
# -The negative predictive value of 0.9000 indicates that when the model predicts a college is not private, it is correct about 90.00% of the time.
# 
# Which misclassifications are more damaging for the analysis, False Positives or False Negatives?
# -In this context, the consequences of false positives and false negatives need to be considered.
# -False positives (predicting a college is private when it is not) could lead to misallocation of resources or misinterpretation of the factors influencing college status.False negatives (predicting a college is not private when it is) could lead to missed opportunities for targeted interventions or inaccurate conclusions about the characteristics of private colleges.
# -Depending on the specific goals of the analysis, one type of misclassification may be more damaging than the other. For example, if the goal is to identify factors associated with private colleges for targeted interventions, false negatives may be more damaging.-

#---------------
#Ques_5
#---------------
library(knitr)
precision_trainSetData <- confMatrix_trainSetData$byClass["Pos Pred Value"]
recall_trainSetData <- confMatrix_trainSetData$byClass["Sensitivity"]
f1_score_trainSetData <- (2 * precision_trainSetData * recall_trainSetData) / (precision_trainSetData + recall_trainSetData)
specificity_trainSetData <- confMatrix_trainSetData$byClass["Specificity"]
f2_score_trainSetData <- (5 * precision_trainSetData * recall_trainSetData) / (4 * precision_trainSetData + recall_trainSetData)

metricsMatrix_trainSetData <- matrix(c(
  "Accuracy", confMatrix_trainSetData$overall["Accuracy"],
  "Precision", precision_trainSetData,
  "Recall (Sensitivity)", recall_trainSetData,
  "Specificity", specificity_trainSetData,
  "F1 Score", f1_score_trainSetData,
  "F2 Score", f2_score_trainSetData
), ncol = 2, byrow = TRUE)

knitr::kable(metricsMatrix_trainSetData, col.names = c("Metric", "Value"),
             align = "c")
# 
# Interpretation:
# 
# -Accuracy: The model correctly classified 93.81% of all observations.
# -Precision: The model correctly identified 95.26% of the colleges it predicted as private.
# -Recall (Sensitivity): The model correctly identified 96.17% of all private colleges.
# -Specificity: The model correctly identified 87.80% of all non-private colleges.
# -F1 Score: The harmonic mean of precision and recall is 0.9571, indicating a balance between precision and recall.
# -F2 Score: The F2 score weighs recall higher than precision, useful when false negatives are more concerning. It is 0.9599, indicating high recall.
# ---Overall, the model shows good performance in terms of accuracy, precision, recall, specificity, and F1 and F2 scores.

#---------------
#Ques_6
#---------------

testSetData_Pred_Prob <- predict(logistic_r_model, newdata = testSetData, type = "response")
testSetData_Pred_Class = as.factor(ifelse(testSetData_Pred_Prob >= 0.5, "Yes", "No"))

confMatrix_testSetData = confusionMatrix(testSetData_Pred_Class, testSetData$Private, positive = 'Yes')
confMatrix_testSetData

precision_testSetData <- confMatrix_testSetData$byClass["Pos Pred Value"]
recall_testSetData <- confMatrix_testSetData$byClass["Sensitivity"]
f1_score_testSetData <- (2 * precision_testSetData * recall_testSetData) / (precision_testSetData + recall_testSetData)
specificity_testSetData <- confMatrix_testSetData$byClass["Specificity"]
f2_score_testSetData <- (5 * precision_testSetData * recall_testSetData) / (4 * precision_testSetData + recall_testSetData)

metricsMatrix_testSetData <- matrix(c(
  "Accuracy", confMatrix_testSetData$overall["Accuracy"],
  "Precision", precision_testSetData,
  "Recall (Sensitivity)", recall_testSetData,
  "Specificity", specificity_testSetData,
  "F1 Score", f1_score_testSetData,
  "F2 Score", f2_score_testSetData
), ncol = 2, byrow = TRUE)

knitr::kable(metricsMatrix_testSetData, col.names = c("Metric", "Value"),
             align = "c")

#---------------
#Ques_7
#---------------

library(pROC)

rocCurve_TestSetData = roc(testSetData$Private, testSetData_Pred_Prob)
plot(rocCurve_TestSetData, main = "ROC Curve", col = "blue", 
     lwd = 2, ylab="Sensitivity - True Positive Rate", xlab = "Specificity - False Positive Rate")

# Based on the image you sent, the ROC curve does show good performance in 
# distinguishing between private and public colleges. 
# The curve bows towards the top-left corner, which is ideal for an ROC curve. 
# This indicates the model can effectively classify between private and public colleges.

#---------------
#Ques_8
#---------------
auc_TestSetData = auc(rocCurve_TestSetData)
cat("Area under the curve:",auc_TestSetData)
legend("bottomright", legend = paste("AUC =", round(auc_TestSetData, 2)), col = "blue", lty = 1, cex = 1)


# The AUC (Area Under the Curve) of 0.973356 indicates that the 
# model has very good discriminatory power. AUC values range from 0 to 1, 
# where a value of 1 represents a perfect model 
# (i.e., the model is able to perfectly distinguish between the positive and negative classes), 
# and a value of 0.5 represents a model that performs no better than random chance.

# With an AUC of 0.973356, the model's ability 
# to correctly classify positive instances as positive and negative 
# instances as negative is excellent. This suggests that the model 
# has strong predictive performance and is effective at ranking instances 
# in the test set according to their likelihood of being positive.

#---------------
#Ques_9
#---------------
install.packages("e1071")
library(e1071)
q9_svmModel <- svm(Private ~ Apps + Top25perc + Outstate + Room.Board + 
                     Books + Personal + PhD + Terminal + perc.alumni + Expend, data = trainSetData, kernel = "linear")
q9_svmPrediction <- predict(q9_svmModel, newdata = testSetData)

q9_confusionMatrixSVM <- confusionMatrix(q9_svmPrediction, testSetData$Private, positive = 'Yes')
q9_confusionMatrixSVM
q9_precisionSVM <- q9_confusionMatrixSVM$byClass["Pos Pred Value"]
q9_recallSVM <- q9_confusionMatrixSVM$byClass["Sensitivity"]
q9_f1ScoreSVM <- (2 * q9_precisionSVM * q9_recallSVM) / (q9_precisionSVM + q9_recallSVM)
q9_specificitySVM <- q9_confusionMatrixSVM$byClass["Specificity"]
q9_f2ScoreSVM <- (5 * q9_precisionSVM * q9_recallSVM) / (4 * q9_precisionSVM + q9_recallSVM)

metricsMatrix_testSetData_SVM <- matrix(c(
  "Accuracy", q9_confusionMatrixSVM$overall["Accuracy"],
  "Precision", q9_precisionSVM,
  "Recall (Sensitivity)", q9_recallSVM,
  "Specificity", q9_specificitySVM,
  "F1 Score", q9_f1ScoreSVM,
  "F2 Score", q9_f2ScoreSVM
), ncol = 2, byrow = TRUE)

knitr::kable(metricsMatrix_testSetData_SVM, col.names = c("Metric", "Value"),
             align = "c")

q9_svmPrediction_Prob <- predict(q9_svmModel, newdata = testSetData, decision.values = TRUE)
q9_svmPrediction_Prob <- attr(q9_svmPrediction_Prob, "decision.values")[, 1]
q9_rocCurveSVM <- roc(testSetData$Private, q9_svmPrediction_Prob)
plot(q9_rocCurveSVM, main = "ROC Curve SVM", col = "blue", 
     lwd = 2, ylab="Sensitivity - True Positive Rate", xlab = "Specificity - False Positive Rate")

q9_aucSVM <- auc(q9_rocCurveSVM)
cat("Area under the curve (SVM):",q9_aucSVM)
legend("bottomright", legend = paste("AUC =", round(q9_aucSVM, 2)), col = "blue", lty = 1, cex = 1)

