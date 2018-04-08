##### Clean workspace #####
rm(list=ls(all=TRUE)) #start with empty workspace
set.seed(1)
cat("\014")  
##########

library(readr)
library(forecast)
library(tseries)
library(ggplot2)

source("fun_getSalesByLevel.R")
source("fun_getSalesBySubLevel.R")

## Reading Data ##
rod <- read_csv(file.choose())

## Preparing Data ##
rod$`Sub-Brand`           <- as.factor(rod$`Sub-Brand`)
rod$Brand                 <- as.factor(rod$Brand)
rod$`Regional Warehouse`  <- as.factor(rod$`Regional Warehouse`)
rod$`Dealer Code`         <- as.factor(rod$`Dealer Code`)
rod$`Retail Quantity`     <- as.numeric(rod$`Retail Quantity`)
rod                       <- rod[!is.na(rod$`Regional Warehouse`),]
rod$date <-
  as.Date(as.yearmon(as.character(paste(rod$Year , rod$Month)), "%Y%m"))

## Removing negetive quantities and making them 0 ##
rod[rod$`Retail Quantity` < 0, 7] <-
  rod[rod$`Retail Quantity` < 0, 7] * 0

summary(rod)

getSalesByLevel(rod, "Brand", 6)                                #### National Brand
getSalesBySubLevel(rod,"Regional Warehouse", "Brand", 6)        #### Regional Warehouse Brand
getSalesBySubLevel(rod,"Dealer Code", "Brand", 6)               #### Dealer Brand
getSalesByLevel(rod, "Sub-Brand", 6)                            #### National Sub-Brand
getSalesBySubLevel(rod,"Regional Warehouse", "Sub-Brand", 6)    #### Regional Warehouse Sub-Brand
getSalesBySubLevel(rod,"Dealer Code", "Sub-Brand", 6)           #### Dealer Sub-Brand



