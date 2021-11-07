cat("\014") # To clear the console
#setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/project/")
setwd("C:/Users/91843/Desktop/IDA/datasets/")
# set working directory based on your installation of R

library(tidyverse) 
library(janitor)
library(ggplot2)

#------------------StarBuck Menu Nutrition Food ----------------------------------


food <- read.csv("starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
summary(food)
head(food,6)
names(food)
drinks[sample(nrow(food), 3), 5]
colA<-food$Calories
colB<-food$Carb...g.
cor.test(colA,colB)
#cor.test(fcolA,colB,alternative="less")
#cor.test(colA,colB,alternative="greater")
#cor.test(colA,colB,alternative="two.sided")
ggplot(data=food, aes(x=colA, y=colB, group=1)) +
  geom_line(color="blue",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Food and Carbs..")

#------------------StarBuck Menu Nutrition Drinks ----------------------------------

#drinks <- read.csv("datasets/starbucks-menu-nutrition-drinks.csv")
drinks <- read.csv("starbucks-menu-nutrition-drinks.csv")
summary(drinks)
head(drinks,6)
names(drinks)
drinks$Carb...g.
ncol(drinks)
drinks[sample(nrow(drinks), 3), ]
is.numeric(colC)
class(colC)
typeof(colC)
typeof(colD)
length(colC)
length(colD)
drinks$Calories <- as.numeric(drinks$Calories)
drinks$Carb...g. <- as.numeric(drinks$Carb...g.)
colC<-drinks$Calories
colD<-drinks$Carb...g.
cor.test(colC,colD)
typeof(colC)
typeof(colD)

#------------------StarBuck Drinks Menu----------------------------------

drinks_exp <- read.csv("starbucks_drinkMenu_expanded.csv")
summary(drinks_exp)
head(drinks_exp,6)
names(drinks_exp)
drinks_exp[sample(nrow(drinks_exp), 3), ]
ggplot(drinks_exp, aes(x = Sodium..mg.)) +
  geom_histogram(bins=30) +
  ggtitle("Sodium Content: Starbucks Menu")




