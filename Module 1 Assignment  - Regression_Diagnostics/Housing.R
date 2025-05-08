library(dplyr)
library(readxl)
library(readr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gmodels)
library(knitr)
library(car)

#1
df_AmesHousing <- read.csv("/Users/devik/Downloads/AmesHousing.csv")

#displaying class of df_AmesHousing
class(df_AmesHousing)


#2 - EDA
summary(df_AmesHousing)
str(df_AmesHousing)

#displaying head
head(df_AmesHousing)

#Finding columns with missing values

#--------replacing empty string values with NA
df_AmesHousing <- df_AmesHousing %>%
  mutate_all(~ifelse(. == "", NA, .))

missing_values_count <- colSums(is.na(df_AmesHousing))
print(missing_values_count)

missing_values_percentage <- colSums(is.na(df_AmesHousing)) / nrow(df_AmesHousing) * 100
print(missing_values_percentage)

print(missing_values_percentage[missing_values_percentage > 0])

columns_missing_values_percentag_gt_10 <- names(missing_values_percentage[missing_values_percentage > 10])
print(columns_missing_values_percentag_gt_10)

#Cleaning data - dropping columns with missing% > 80
columns_missing_values_percentag_gt_80 <- names(missing_values_percentage[missing_values_percentage > 80])
df_AmesHousing_cleaned <- df_AmesHousing[,!names(df_AmesHousing) %in% columns_missing_values_percentag_gt_80]

missing_values_percentage <- colSums(is.na(df_AmesHousing_cleaned)) / nrow(df_AmesHousing_cleaned) * 100
print(missing_values_percentage[missing_values_percentage > 0])

library(dplyr)
#imputing with mean
df_AmesHousing_cleaned_imputed <- df_AmesHousing_cleaned %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

missing_values_percentage <- colSums(is.na(df_AmesHousing_cleaned_imputed)) / nrow(df_AmesHousing_cleaned_imputed) * 100
print(missing_values_percentage[missing_values_percentage > 0])

columns_missing_values_percentag_gt_40 <- names(missing_values_percentage[missing_values_percentage > 40])
df_AmesHousing_cleaned_imputed <- df_AmesHousing_cleaned_imputed[,!names(df_AmesHousing_cleaned_imputed) %in% columns_missing_values_percentag_gt_40]

missing_values_percentage <- colSums(is.na(df_AmesHousing_cleaned_imputed)) / nrow(df_AmesHousing_cleaned_imputed) * 100
print(missing_values_percentage[missing_values_percentage > 0])

dim(df_AmesHousing_cleaned_imputed)

#removing rows with missing values as just columns with very less percentage of missing values are remaining now
df_AmesHousing_cleaned_final <- na.omit(df_AmesHousing_cleaned_imputed)
dim(df_AmesHousing_cleaned_final)

missing_values_percentage <- colSums(df_AmesHousing_cleaned_final == "") / nrow(df_AmesHousing_cleaned_final) * 100
print(missing_values_percentage[missing_values_percentage > 0])

df_AmesHousing_processed <- df_AmesHousing_cleaned_final
#4
df_AmesHousing_processed <- subset(df_AmesHousing_processed, select = -PID)
df_AmesHousing_processed <- subset(df_AmesHousing_processed, select = -Order)
cor_matrix <- cor(select_if(df_AmesHousing_processed, is.numeric))
print(cor_matrix)

#5
library(ggplot2)
library(reshape2)

cor_df <- as.data.frame(cor_matrix)
cor_df$var1 <- rownames(cor_df)
cor_df_long <- melt(cor_df, id.vars = "var1", variable.name = "var2")

cor_df_long$var1 <- factor(cor_df_long$var1, levels = unique(cor_df_long$var1))
cor_df_long$var2 <- factor(cor_df_long$var2, levels = unique(cor_df_long$var2))

ggplot(cor_df_long, aes(x = var1, y = var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), 
                       breaks = seq(-1, 1, by = 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")

#6
#Finding correlation against SalePrice

corr_SalePrice <- sapply(df_AmesHousing_processed[, sapply(df_AmesHousing_processed, is.numeric) & names(df_AmesHousing_processed) != "SalePrice"], 
                         function(x) cor(x, df_AmesHousing_processed$SalePrice, use = "pairwise.complete.obs"))
highest_corr_X <- names(which.max(corr_SalePrice))
highest_corr_X

lowest_corr_X <- names(which.min(corr_SalePrice))
lowest_corr_X

closest_corr_0.5_X <- names(which.min(abs(corr_SalePrice - 0.5)))
closest_corr_0.5_X

df_AmesHousing_processed[[highest_corr_X]] <- factor(df_AmesHousing_processed[[highest_corr_X]])
ggplot(df_AmesHousing_processed, aes(x = !!sym(highest_corr_X), y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot SalesPrice with Highest Correlated X") +
  scale_x_discrete(name = highest_corr_X)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

df_AmesHousing_processed[[lowest_corr_X]] <- factor(df_AmesHousing_processed[[lowest_corr_X]])
ggplot(df_AmesHousing_processed, aes(x = !!sym(lowest_corr_X), y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot SalesPrice with Lowest Correlated X") +
  scale_x_discrete(name = lowest_corr_X) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

df_AmesHousing_processed[[closest_corr_0.5_X]] <- as.numeric(df_AmesHousing_processed[[closest_corr_0.5_X]])
ggplot(df_AmesHousing_processed, aes(x = !!sym(closest_corr_0.5_X), y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot SalePrice with Variable X Correlated Closest to 0.5")+
  scale_x_continuous(name = closest_corr_0.5_X) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


options(scipen = 999)#to avoid scientific notation

df_AmesHousing_processed_numeric <- df_AmesHousing_processed %>%
  select_if(function(x) is.numeric(x) || is.factor(x)) %>%
  mutate_if(is.factor, as.numeric)


# Fit a linear regression model
model <- lm(SalePrice ~ Overall.Qual + BsmtFin.SF.1 + Garage.Cars + Gr.Liv.Area, data = df_AmesHousing_processed_numeric)
# Print the summary of the model
summary(model)

coeff <- coef(model)
intercept <- coeff[1]
terms <- paste(names(coeff)[-1], "*", coeff[-1], collapse = " + ")
equation <- paste("SalePrice =", intercept, "+", terms)
cat(equation, "\n")

# Plotting regression diagnostics
plot(model)

# check for multicollinearity in your regression model
library(car)
vif_values <- vif(model)
print(vif_values)

#Check for outliers
# Extract the residuals from the model
residuals <- resid(model)

# Calculate standardized residuals
standardized_residuals <- rstandard(model)

# Create a dataframe to store residuals and standardized residuals
residual_df <- data.frame(Residuals = residuals, Standardized_Residuals = standardized_residuals)

# Identify potential outliers based on standardized residuals
outliers <- residual_df[abs(residual_df$Standardized_Residuals) > 2, ]

# Print the potential outliers
print(outliers)

# Fit a linear regression model
model <- lm(SalePrice ~ Overall.Qual + BsmtFin.SF.1 + Garage.Cars + Gr.Liv.Area, data = df_AmesHousing_processed_numeric)

# Extract standardized residuals
standardized_residuals <- rstandard(model)

# Identify outliers with standardized residuals greater than 2
outliers <- which(abs(standardized_residuals) > 2)

# Create a new dataframe excluding outliers
df_AmesHousing_no_outliers <- df_AmesHousing_processed_numeric[-outliers, ]

# Refit the model without outliers
model_no_outliers <- lm(SalePrice ~ Year.Built + Year.Remod.Add + Overall.Qual + Overall.Cond, data = df_AmesHousing_no_outliers)

# Print summary of the new model
summary(model_no_outliers)

#13
library(MASS)
library(leaps)
leaps<- regsubsets(SalePrice ~ ., data=df_AmesHousing_processed_numeric, nbest=3)
plot(leaps,scale = "adjr2")
summary(leaps)

#from the Summary of the regsubsets output it is observed that
# Best 1 variable Model is Model with Variable: Overall.Qual
# Best 2 variable Model is Model with Variables: Overall.Qual and Gr.Liv.Area
# Best 3 variable Model is Model with Variables: Overall.Qual, BsmtFin.SF.1 and Gr.Liv.Area

model_var_1 <- lm(SalePrice ~ Overall.Qual, data = df_AmesHousing_processed_numeric)
summary(model_var_1)
coeff <- coef(model_var_1) 
intercept <- coeff[1] 
terms <- paste(names(coeff)[-1], "*", coeff[-1], collapse = " + ") 
equation <- paste("SalePrice =", intercept, "+", terms) 
cat(equation, "\n")

model_var_2 <- lm(SalePrice ~ Overall.Qual + Gr.Liv.Area, data = df_AmesHousing_processed_numeric)
summary(model_var_2)
coeff <- coef(model_var_2) 
intercept <- coeff[1] 
terms <- paste(names(coeff)[-1], "*", coeff[-1], collapse = " + ") 
equation <- paste("SalePrice =", intercept, "+", terms) 
cat(equation, "\n")

model_var_3 <- lm(SalePrice ~ Overall.Qual + BsmtFin.SF.1 + Gr.Liv.Area, data = df_AmesHousing_processed_numeric)
summary(model_var_3)
coeff <- coef(model_var_3) 
intercept <- coeff[1] 
terms <- paste(names(coeff)[-1], "*", coeff[-1], collapse = " + ") 
equation <- paste("SalePrice =", intercept, "+", terms) 
cat(equation, "\n")

#To find Best Model, I am using the stepwise selection method using stepAIC()
complete_model <- lm(SalePrice ~ ., data = df_AmesHousing_processed_numeric)
backward_model <- stepAIC(complete_model, direction = "backward")
forward_model <- stepAIC(complete_model, direction = "forward")
both_model <- stepAIC(complete_model, direction = "both")
models <- list(backward = backward_model, forward = forward_model, both = both_model)
best_model_name <- names(models)[which.min(sapply(models, AIC))]
best_model <- models[[best_model_name]]
summary(best_model)
coeff <- coef(best_model) 
intercept <- coeff[1] 
terms <- paste(names(coeff)[-1], "*", coeff[-1], collapse = " + ") 
equation <- paste("SalePrice =", intercept, "+", terms) 
cat(equation, "\n")

