cat("\014") # To clear the console
#setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/project/")
setwd("C:/Users/91843/Desktop/IDA/datasets/")
# set working directory based on your installation of R

library(tidyverse) 
library(janitor)
library(ggplot2)
#------------------StarBuck Menu Nutrition Drinks ----------------------------------

#drinks <- read.csv("datasets/starbucks-menu-nutrition-drinks.csv")
drinks <- read.csv("starbucks-menu-nutrition-drinks.csv")
summary(drinks)
head(drinks,6)
names(drinks)
drinks$Carb...g.
ncol(drinks)
colnames(drinks)
drinks[sample(nrow(drinks), 3), ]
drinks_exp[sample(nrow(drinks_exp), 3), ]


#------------------StarBuck Drinks Menu----------------------------------

drinks_exp <- read.csv("starbucks_drinkMenu_expanded.csv")
summary(drinks_exp)
head(drinks_exp,6)
names(drinks_exp)
ncol(drinks_exp)
colnames(drinks_exp)
drinks_exp[sample(nrow(drinks_exp), 3), ]
ggplot(drinks_exp, aes(x = Sodium..mg.)) +
  geom_histogram(bins=30) +
  ggtitle("Sodium Content: Starbucks Menu")

#------------------StarBuck Menu Nutrition Food ----------------------------------


food <- read.csv("starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
summary(food)
head(food,6)
names(food)
food$Carb...g.
ncol(food)
colnames(food)
drinks[sample(nrow(food), 3), 5]


