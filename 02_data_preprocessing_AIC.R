## Load required libraries ##
library(stats)
library(dplyr)
library(readxl)
library(caret)

##Set working directory##
##Locate the dataset file location, below is just an example##
setwd("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")


##Set as MydataAIC or any other name##
##Use only the significant variables (Based on Spearman's correlation test), p<0.10, p<0.05, p<0.01##
MydataAIC <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")

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


##To check the AIC, lowest value is better##
lmfull <- lm(Cur ~ ., data=MydataAIC[,3:17])
summary (lmfull)
AIC(lmfull)

##To remove predictors one at at time## ##Cross-validate model performance##
##To get the lowest-AIC model## ##Bidirectional stepwise selection was used due to large sample size##
step(lmfull, direction = "both")

##Results from curvularia_data_afterAICselecti##
##Step:  AIC=572.25 Cur ~ `Rain (3)` + `WD (1)` + `WS (7)` + `MinT (0)` + `MaxT (7)` + `MeanT (0)` + `MeanT (1)##
## Use the significant variables from AIC for model development ##



