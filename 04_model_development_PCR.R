## Load required libraries ##
library(stats)
library(dplyr)
library(readxl)
library(caret)

##Set working directory##
##Locate the dataset file location, below is just an example##
setwd("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")


##Set as MydataAIC or any other name##
MydataPCR <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_afterAICselection.xlsx")

##OPTIONAL, but helpful if you have multiples datasheets in one excel file##
sheets <- excel_sheets("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_afterAICselection.xlsx")
print(sheets)

##OPTIONAL, Read a specific sheet by name##
MydataPCR <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_afterAICselection.xlsx", 
                        sheet = "curvularia_data_afterAICselecti")  # Replace "curvularia_data_afterAICselecti" with your actual sheet name

##To make sure that you are using the right dataset##
View (MydataPCR)
head(MydataPCR)

##Attached the dataset that you are working on##
attach(MydataPCR)


##Based on AIC Result, include only significant variables, in this case for Curvularia during ripening phase as an example ##
##Step:  AIC=571.78 Cur ~ `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0) ##

## Model development using Principal Component Regression using Selected Significant variables##
## To make it easier make sure to rearrange all the selected variables to front column, ##
## In this case, `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0)` were put in front column ## 


###########################################################################
#################################### PCA ##################################

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#### The  columns number 2-8 includes the Spore concentration and selected variables ####
modelPCR <- prcomp(MydataPCR[, 2:8], center = TRUE, scale. = TRUE)
modelPCR$scale
print(modelPCR)
summary(modelPCR)

##############################################################################
######################## Transform PC for PCA + MLR ##########################

pc_scores<-as.data.frame(modelPCR$x)
summary(pc_scores)

#### PCs are selected based on Recursive Feature Elimination (RFE) #########
modelPCR<-lm(Cur ~ PC1 + PC2 + PC4 + PC5 + PC7, data = pc_scores)
summary(modelPCR)

###############################################################################
############################ Recursive Feature Elimination (RFE) ##############

library(caret)
library(randomForest)  # Optional, for RF-based RFE

## Define control function for RFE ##
rfe_control <- rfeControl(
  functions = lmFuncs,  # Use linear regression for selection #For random forest, change functions = rfFuncs.
  method = "cv",        # Cross-validation
  number = 10           # 10-fold CV
)

## Perform RFE ##
pc_scores$Cur <- MydataPCR$Cur  # If Cur is a separate vector
pc_scores
set.seed(1)  # For reproducibility
rfe_result <- rfe(
  x = pc_scores[, c("PC1", "PC2", "PC3", "PC4","PC5","PC6","PC7")], 
  y = pc_scores$Cur,
  sizes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),  # Try different numbers of features
  rfeControl = rfe_control
)

## "PC1", "PC2", "PC3", "PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15" ##

## Print results ##
print(rfe_result)

plot(rfe_result, type = c("g", "o"))

optimal_features <- predictors(rfe_result)
print(optimal_features)

## After Recursive Feature Elimination (RFE), Train and Evaluate the Models until requirements are met ##
## In some cases, you have to try different combination of PCs to get better models ##

#######################################################################
###########################TRAINING THE MODEL##########################

## To make it easier make sure to rearrange all the selected variables to front column, ##
## In this case, `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0)` were put in first front column ## 

#######################################################################################
####################### K-Fold Cross-Validation #######################################

library(caret)
set.seed(1) #set once ONLY
pc_scores$Cur <- MydataPCR$Cur  # If Cur is a separate vector
pc_scores

train_index <- createDataPartition(MydataPCR$Cur, p = 0.75, list = FALSE)
train_data <- pc_scores[train_index, ]
test_data <- pc_scores[-train_index, ]


## Set up training control with 10-fold cross-validation repeated 3 times ##
train_control <- trainControl(method = "repeatedcv", 
                              number = 10,         # 10-fold cross-validation
                              repeats = 5,         # Repeat 3 times
                              savePredictions = "final",  # Save final model predictions for analysis
                              verboseIter = FALSE,
                              summaryFunction = defaultSummary)  # Display training process

## Fit the model using cross-validation ##
model_PCR <- train(Cur ~   PC1 + PC2 + PC4 + PC5 + PC7, data = train_data, 
                  method = "lm", trControl = train_control)


########################################################################
#################### EVALUATION OF MODEL ###############################

## Predict on test (validation) data ##
predictions <- predict(model_PCR, newdata = test_data)

## Calculate RMSE and Rsquared for the test data ##
test_rmse <- sqrt(mean((predictions - test_data$Cur)^2))
test_r_squared <- 1 - sum((predictions - test_data$Cur)^2) / sum((test_data$Cur - mean(test_data$Cur))^2)

## Print test performance ##
print(paste("Test RMSE: ", test_rmse))
print(paste("Test R-Squared: ", test_r_squared))


###############################################################################
#########################################   HISTOGRAM  ########################

## Load ggplot2 library (if not already loaded) ##
library(ggplot2)

## Convert residuals into a data frame ##
residuals_df <- data.frame(residuals = residuals(model_PCR$finalModel))

# Plot histogram with density curve
ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..),  # Use density instead of count
                 bins = 5, 
                 fill = "lightblue", 
                 color = "black") +
  geom_density(color = "red", linewidth = 1) +  # Overlay density curve
  labs(title = "Residual Histogram for FusRip PCR Model_Corr", 
       x = "Residuals", 
       y = "Density") +
  theme_minimal()


###########################################################
######################### QQ normality ####################

qqnorm(residuals(model_PCR$finalModel))
qqline(residuals(model_PCR$finalModel), col = "red", lwd = 2)


## VIF ##
library(car)
vif(model_PCR$finalModel)  # Values > 10 indicate multicollinearity issues

## Durbin-Watson test ##
dw_test <- dwtest(model_PCR$finalModel) ## near 2 is good
print(dw_test)

## Breusch-pagan test ##
library(lmtest)
bptest(model_PCR$finalModel)  # p-value < 0.05 indicates heteroscedasticity

## Shapiro-wilk normality test ##
shapiro.test(residuals(model_PCR$finalModel))


