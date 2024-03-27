###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving#load needed packages. make sure they are installed.
library(tidyverse) #for data processing/cleaning; includes ggplot2, tidyr, readr, dplyr, stringr, purr, forcats
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(renv) #for package management
library(knitr) #for nice tables
library(kableExtra) #for nice tables
library(gt) #for nice tables
library(gtsummary) #for summary tables


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","madaproject.rds")

#load data. 
madaproject <- readRDS(data_location)


############################
## ---- fit-data-logistic --------

# second model has all variables as predictors
log_mod <- logistic_reg() %>% set_engine("glm")
logfit <- log_mod %>% fit(tb ~ ., data = madaproject)

# Load required libraries
library(tidymodels)
library(pROC)

# Define the logistic regression model with glm engine
log_mod <- logistic_reg() %>% 
  set_engine("glm")

# Fit the model
logfit <- log_mod %>% 
  fit(tb ~ ., data = madaproject)

# Predict probabilities
predicted_probabilities <- predict(logfit, new_data = madaproject, type = "prob")

# Compute ROC curve and AUC
roc_result <- roc(madaproject$tb, predicted_probabilities$.pred_TB)

# Compute accuracy
predicted_classes <- ifelse(predicted_probabilities$.pred_TB > 0.5, "Yes", "No")
accuracy <- mean(predicted_classes == madaproject$tb)

# Display AUC and accuracy
print(paste("AUC:", round(auc(roc_result), 3)))
print(paste("Accuracy:", round(accuracy, 3)))








# Compute the accuracy and AUC for model 1
m1_acc <- logfit %>% 
  predict(madaproject) %>% 
  bind_cols(madaproject) %>% 
  metrics(truth = tb, estimate = .pred_class) %>% 
  filter(.metric == "accuracy") 
m1_auc <-  logfit %>%
  predict(madaproject, type = "prob") %>%
  bind_cols(madaproject) %>%
  roc_auc(truth = madaproject$tb, .pred_class)

# Print the results
print(m1_acc)

print(m1_auc)


# save fit results table  
table_file2 = here("results", "tables", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

  