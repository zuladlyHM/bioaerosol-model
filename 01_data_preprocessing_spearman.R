## Load required libraries ##
library(stats)
library(dplyr)
library(readxl)
library(caret)

##Set working directory##
##Locate the dataset file location, below is just an example##
setwd("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data")


##Set as Mydata or any other name##
Mydata <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_original.xlsx")

##OPTIONAL, but helpful if you have multiple datasheets in one excel file##
sheets <- excel_sheets("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_original.xlsx")
print(sheets)

##OPTIONAL, Read a specific sheet by name##
Mydata <- read_excel("C:/Users/Predator Helios 300/OneDrive/Desktop/bioaerosol model/data/curvularia_data_original.xlsx", 
                        sheet = "curvularia_data_original")  # Replace "curvularia_data_original" with your actual sheet name

##To make sure that you are using the right dataset##
View (Mydata)
head(Mydata)

##Attached the dataset that you are working on##
attach(Mydata)


## Spearman correlation test ##
## Example Mean Temperature vs Spore Concentration ##
## MeanT (0)= average temperature at 0-day
## MeanT (1)= average temperature 1 day before
## MeanT (2)= average temperature 2 day before and so forth...###

cor.test(Mydata[["Cur"]], Mydata[["MeanT (0)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (1)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (2)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (3)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (4)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (5)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (6)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")
cor.test(Mydata[["Cur"]], Mydata[["MeanT (7)"]], 
         method = "spearman",
         exact = FALSE,
         alternative = "two.sided")

