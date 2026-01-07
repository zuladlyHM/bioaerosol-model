## Load required libraries ##
library(stats)
library(dplyr)
library(readxl)
library(caret)

##Set working directory##
##Locate the dataset file location, below is just an example##
setwd("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")


##Set as MydataAIC or any other name##
MydataAIC <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data)

##OPTIONAL, but helpful if you have multiple datasheets in one excel file##
sheets <- excel_sheets("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")
print(sheets)

##OPTIONAL, Read a specific sheet by name##
MydataAIC <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data", 
                        sheet = "curvularia_data_afterAICselecti")  # Replace "curvularia_data_afterAICselecti" with your actual sheet name

##To make sure that you are using the right dataset##
View (MydataAIC)
head(MydataAIC)

##Attached the dataset that you are working on##
attach(MydataAIC)


##Based on AIC Result, include only significant variables, in this case for Curvularia during ripening phase as an example ##
##Step:  AIC=571.78 Cur ~ `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0) ##

## Model development using Multiple linear regression using Selected Significant variables##
Curspore.lm <- lm(Cur ~ `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0)`, 
                  data = MydataAIC[, 3:17])

summary (Curspore.lm)


#######################################################################
###########################TRAINING THE MODEL##########################

## To make it easier make sure to rearrange all the selected variables to front column, ##
## In this case, `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0)` were put in front column ##
## Or remove the non-selected variables ##

install.packages("caret")  # if not yet installed
library(caret)

# Set seed for reproducibility
set.seed(1)

## ===== Step 1: Train-Test Split (75% Train, 25% Test) ##
trainIndex <- createDataPartition(MydataAIC$Cur, p = 0.75, list = FALSE)
trainData <- MydataAIC[trainIndex, 2:8]
testData  <- MydataAIC[-trainIndex, 2:8]

## ===== Step 2: Repeated Cross-Validation on Training Set ##
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5)

## Train the model with repeated CV ##
model_cv <- train(Cur ~ ., 
                  data = trainData, 
                  method = "lm", 
                  trControl = ctrl)

## View CV results ##
print(model_cv)
cat("Cross-validated RMSE:", model_cv$results$RMSE, "\n")
cat("Cross-validated R²:", model_cv$results$Rsquared, "\n")

## Extract the final linear model from caret ##
final_model <- model_cv$finalModel

## Load car package for VIF ##
install.packages("car")  # Run once
library(car)

## Check Variance Inflation Factor ##
vif(final_model)

## Load lmtest package ##
install.packages("lmtest")
library(lmtest)

## Run Durbin-Watson test ##
dwtest(final_model)

## Still using lmtest package ##
bptest(final_model)

## Run Shapiro-Wilk test on residuals ##
shapiro.test(residuals(final_model))


###############################################################################
#########################################   HISTOGRAM  ########################

## Load ggplot2 library (if not already loaded) ##
library(ggplot2)

## Convert residuals into a data frame ##
residuals_df <- data.frame(residuals = residuals(model_cv$finalModel))

## Plot histogram with density curve ##
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

###################################
########### QQ normality ##########

qqnorm(residuals(model_cv$finalModel))
qqline(residuals(model_cv$finalModel), col = "red", lwd = 2)
