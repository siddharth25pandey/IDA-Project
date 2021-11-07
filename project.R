setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/IDA/")
# set working directory based on your installation of R

drinks <- read.csv("datasets/starbucks-menu-nutrition-drinks.csv")
drinks$Carb...g.
ncol(drinks)
colnames(drinks)
drinks[sample(nrow(drinks), 3), ]

drinks_exp <- read.csv("datasets/starbucks_drinkMenu_expanded.csv")
ncol(drinks_exp)
colnames(drinks_exp)
drinks_exp[sample(nrow(drinks_exp), 3), ]


food <- read.csv("datasets/starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
food$Carb...g.
ncol(food)
colnames(food)
drinks[sample(nrow(food), 3), ]


