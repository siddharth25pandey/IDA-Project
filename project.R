cat("\014") # To clear the console
# set working directory based on your installation of R
#setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/project/")
setwd("C:/Users/91843/Desktop/IDA/datasets/")
#setwd("C:/Users/Priyam Bajpai/Desktop/IDA/datasets/")

# Library for plotting the plot
library(ggplot2)

# Functions for Correlation analysis
#------------------------------------

# Variance function
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

# Pearson Correlation (Degree of Correlation)
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

# Degree of Freedon ((m-1)*(n-1))
degree_of_freedom <- function(X,Y)
{ if (length(X) != length(Y)){
  return("unequal columns")
}
  return ((length(X)-2))
}

# T value from degree of correlation
t_value <-function(X,Y)
{
  if (length(X) != length(Y)){
    return("unequal columns")
  }
  n <- length(X)
  r <-pearson_corr(X,Y)
  value_of_t <- r *((n-2)/(1-(r^2)))^0.5
  return (value_of_t)
  
}

# Coefficient of determination
coefficient_of_detemination <-function(X,Y)
{
  if (length(X) != length(Y)){
    return("unequal columns")
  }
  xy <- 0.0
  x2 <-0.0
  y2 <-0.0
  n <- length(X)
  for (i in 1:n){
    xy <- xy + X[i]*Y[i]
  }
  for (i in 1:n){
    x2 <- x2 + X[i]*X[i]
  }
  for (i in 1:n){
    y2 <- y2 + Y[i]*Y[i]
  }
  
  r_2=((n*xy)-(sum(X)*sum(Y)))/(((n*x2)-(sum(X)^2))*((n*y2)-(sum(Y)^2))^0.5)
  return (r_2^2)
}


#------------------StarBuck Menu Nutrition Food ----------------------------------

food <- read.csv("starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
lapply(food,function(x) { length(which(is.na(x)))})
colSums(is.na(food))
str(food)
summary(food)
head(food,6)
colA<-food$Calories
colB<-food$Carb...g.
cor.test(colA,colB)
r<-pearson_corr(colA,colB)
print(r)
df<-degree_of_freedom(colA,colB)
print(df)
t <-t_value(colA,colB)
print(t)
r2 <-coefficient_of_detemination(colA,colB)
print(r2)
r_square_in_built = lm(colA ~ colB, data=food)
summary(r_square_in_built)$r.squared 
#value_of_t <-t_value(colA,colB)
#print(value_of_t)
#p_value <- pt(t, df=df, lower.tail=FALSE)
#print(p_value)
#cor.test(fcolA,colB,alternative="less")
#cor.test(colA,colB,alternative="greater")
#cor.test(colA,colB,alternative="two.sided")
ggplot(data=food, aes(x=colA, y=colB, group=1)) +
  geom_line(color="blue",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Food and Carbs..")

#------------------StarBuck Menu Nutrition Drinks ----------------------------------

drinks <- read.csv("starbucks-menu-nutrition-drinks.csv")
summary(drinks)
head(drinks,6)
names(drinks)
drinks<-drinks[!(drinks$Calories=="-"),]
head(drinks,6)
drinks[sample(nrow(drinks), 3), 5]
colC<-drinks$Calories
colC<-as.integer(colC)
colD<-drinks$Carb...g.
colD<-as.integer(colD)
cor.test(colC,colD)
r_d<-pearson_corr(colC,colD)
print(r_d)
#cor.test(fcolA,colB,alternative="less")
#cor.test(colA,colB,alternative="greater")
#cor.test(colA,colB,alternative="two.sided")
ggplot(data=drinks, aes(x=colC, y=colD, group=1)) +
  geom_line(color="red",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Drinks and Carbs..")


