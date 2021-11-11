cat("\014") # To clear the console
#setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/project/")
setwd("C:/Users/91843/Desktop/IDA/datasets/")
# set working directory based on your installation of R

library(tidyverse) 
library(janitor)
library(ggplot2)

variance <- function(X){
  n <- length(X)
  mX <- sum(X)/n
  tot <- 0.0
  for (i in X){
    tot <- tot + (i-mX)^2
  }
  var <- tot/(n-1)
  return(var)
}

pearson_corr <- function(X,Y) {
  if (length(X) != length(Y)){
    return("unequal columns")
  }
  n <- length(X)
  mX <- sum(X)/n
  mY <- sum(Y)/n
  vX <- variance(X)
  vY <- variance(Y)
  ssX <- (vX*(n-1))^0.5
  ssY <- (vY*(n-1))^0.5
  
  tot <- 0.0
  for (i in 1:length(X)){
    tot <- tot + (X[i]-mX)*(Y[i]-mY)
  }
  
  r <- tot/(ssX*ssY)
  return(r)
}
degree_of_freedom <- function(X,Y)
{ if (length(X) != length(Y)){
  return("unequal columns")
  }
  return ((length(X)-2))
}

two_sample_t_test <- function(X,Y)
{
  if (length(X) != length(Y)){
    return("unequal columns")
  }
    n <- length(X)
    mX <- sum(X)/n
    mY <- sum(Y)/n
    vX <- variance(X)
    vY <- variance(Y)
    ssX <- (vX*(n-1))^0.5
    ssY <- (vY*(n-1))^0.5
    #pooled_variance <- ((n-1)*vX+(n-1)*vY)/(2*n-2)
    pooled_variance <- (vX+vY)/2
    t <- (mX-mY)/(pooled_variance)
    return (t)
    
  
}

t_value <-function(X,Y)
{
  if (length(X) != length(Y)){
    return("unequal columns")
  }
  n <- length(X)
  p <-pearson_corr(X,Y)
  value_of_t <- p *((n-2)/(1-(p^2)))^0.5
  return (value_of_t)
  
}


#------------------StarBuck Menu Nutrition Food ----------------------------------


food <- read.csv("starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
lapply(food,function(x) { length(which(is.na(x)))})
colSums(is.na(food))
summary(food)
head(food,6)
names(food)
food[sample(nrow(food), 3), 5]
colA<-food$Calories
colB<-food$Carb...g.
cor.test(colA,colB)
r<-pearson_corr(colA,colB)
print(r)
df<-degree_of_freedom(colA,colB)
print(df)
tstt_test_statistics <- two_sample_t_test(colA,colB)
print(tstt_test_statistics)
value_of_t <-t_value(colA,colB)
print(value_of_t)
#cor.test(fcolA,colB,alternative="less")
#cor.test(colA,colB,alternative="greater")
#cor.test(colA,colB,alternative="two.sided")
ggplot(data=food, aes(x=colA, y=colB, group=1)) +
  geom_line(color="blue",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Food and Carbs..")

#------------------StarBuck Menu Nutrition Drinks ----------------------------------

drinks <- read.csv("starbucks-menu-nutrition-drinks.csv")
drinks<-drinks[!(drinks$Calories=="-"),]
summary(drinks)
head(drinks,6)
names(drinks)
drinks$Carb...g.
ncol(drinks)
drinks[sample(nrow(drinks), 3), ]
colC<-drinks$Calories
colD<-drinks$Carb...g.
is.numeric(colC)
is.numeric(colD)
class(colC)
typeof(colC)
typeof(colD)
length(colC)
length(colD)
drinks$Calories <- as.numeric(drinks$Calories)
drinks$Carb...g. <- as.numeric(drinks$Carb...g.)

typeof(colC)
typeof(colD)
cor.test(colC,colD)
r<-pearson_corr(colC,colD)
print(r)
df<-degree_of_freedom(colC,colD)
print(df)
tstt_test_statistics <- two_sample_t_test(colC,colD)
print(tstt_test_statistics)
value_of_t <-t_value(colC,colD)
print(value_of_t)


#------------------StarBuck Drinks Menu----------------------------------

drinks_exp <- read.csv("starbucks_drinkMenu_expanded.csv")
lapply(drinks_exp,function(x) { length(which(is.na(x)))})
colSums(is.na(drinks_ex))
summary(drinks_exp)
head(drinks_exp,6)
names(drinks_exp)
drinks_exp[sample(nrow(drinks_exp), 3), ]
ggplot(drinks_exp, aes(x = Sodium..mg.)) +
  geom_histogram(bins=30) +
  ggtitle("Sodium Content: Starbucks Menu")

