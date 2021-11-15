cat("\014") # To clear the console
# set working directory based on your installation of R
#setwd("/Users/shreyash/Documents/UG3/sem5/ida/R/project/IDA/datasets")
#setwd("C:/Users/91843/Desktop/IDA/datasets/")
#setwd("C:/Users/Priyam Bajpai/Desktop/IDA/datasets/")

# Library for plotting the plot
library(ggplot2)

# Functions for Correlation analysis
#------------------------------------
total <- function(X){
  tot <- 0.0
  for(i in X){
    tot <- tot + i
  }
  return(tot)
}


# Variance function
variance <- function(X){
  n <- length(X)  #length of column
  mX <- total(X)/n  #mean of column
  tot <- 0.0      
  for (i in X){
    tot <- tot + (i-mX)^2  #sum of squared deviation
  }
  var <- tot/(n-1)  #get variance of column
  return(var)
}

# Pearson Correlation (Degree of Correlation)
pearson_corr <- function(X,Y) {
  if (length(X) != length(Y)){ #if columns are not equal, return error
    return("Error: unequal columns")
  }
  n <- length(X)  #length of column
  mX <- total(X)/n  #mean of column X
  mY <- total(Y)/n  #mean of column Y
  vX <- variance(X) #variance of column X
  vY <- variance(Y) #variance of column Y
  ssX <- (vX*(n-1))^0.5  #squared deviation of column X
  ssY <- (vY*(n-1))^0.5  #squared deviation of column Y
  
  tot <- 0.0
  for (i in 1:length(X)){
    tot <- tot + (X[i]-mX)*(Y[i]-mY)  #paired sum of deviation about mean
  }
  
  r <- tot/(ssX*ssY)  #get pearson coefficient 
  return(r)
}

# Degree of Freedom ((m-1)*(n-1))
degree_of_freedom <- function(X,Y){ 
  if (length(X) != length(Y)){  #if columns are not equal, return error
    return("unequal columns")
  }
  return ((length(X)-2))  #return degree of freedom
}

# T value from degree of correlation
t_value <-function(X,Y){
  if (length(X) != length(Y)){  #if columns are not equal, return error
    return("unequal columns")
  }
  n <- length(X)  #length of column
  r <-pearson_corr(X,Y)  #get pearson correlation coefficient
  value_of_t <- r *((n-2)/(1-(r^2)))^0.5  #get t value for correlation
  return (value_of_t)
  
}

# Coefficient of determination
coefficient_of_detemination <-function(X,Y){
  if (length(X) != length(Y)){  #if columns are not equal, return error
    return("unequal columns")
  }
  xy <- 0.0
  x2 <-0.0
  y2 <-0.0
  n <- length(X)  #length of column
  mX <- total(X)/n  #mean of column X
  mY <- total(Y)/n  #mean of column Y
  for (i in 1:n){
        xy <- xy + ((X[i]-mX)*(Y[i]-mY))  #paired sum of deviation about mean
  }
  for (i in 1:n){
    x2 <- x2 + ((X[i]-mX)*(X[i]-mX))  #squared sum of deviation about mean of column X
  }
  for (i in 1:n){
    y2 <- y2 + ((Y[i]-mY)*(Y[i]-mY))  #squared sum of deviation about mean of column Y
  }
  r_2=xy/((x2*y2)^0.5)  #get coefficient of determination
  return (r_2^2)
}


#------------------StarBuck Menu Nutrition Food ----------------------------------

sprintf("Food data")
food <- read.csv("starbucks-menu-nutrition-food.csv",fileEncoding = "UTF-16")
colSums(is.na(food))  #check which(if) columns contain null values
summary(food)   #get statistics about each attribute
head(food,6)  #get some idea how dataset is like
colA<-food$Calories  #attribute 1 for checking correlation
colB<-food$Carb...g. #attribute 2 for checking correlation
cor.test(colA,colB)  #built-in function for pearson correlation, used to verify our result
r<-pearson_corr(colA,colB)  #get pearson coefficient
sprintf("Pearson coefficient for Calories and Carb attributes: %f", r)
df<-degree_of_freedom(colA,colB)  #get degree of freedom
sprintf("Degrees of freedom: %d",df)
t <-t_value(colA,colB) #get t value
sprintf("T score for pearson correlation between Calories and Carb attributes: %f",t)
r2 <-coefficient_of_detemination(colA,colB)  #get coefficient of determination
sprintf("Coefficient of determination between Calories and Carb attributes: %f",r2)
r_square_in_built = lm(colA ~ colB, data=food)  #built-in function for Coefficient of determination
summary(r_square_in_built)$r.squared  #used built in function to check our result
ggplot(data=food, aes(x=colA, y=colB, group=1)) +   #plot column data to check if Pearson correlation is valid
  geom_line(color="blue",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Food and Carbs..")

#------------------StarBuck Menu Nutrition Drinks ----------------------------------

sprintf("Beverage data")
drinks <- read.csv("starbucks-menu-nutrition-drinks.csv")
summary(drinks)
head(drinks,6)
drinks<-drinks[!(drinks$Calories=="-"),]
head(drinks,6)
colC<-drinks$Calories
colC<-as.integer(colC)
colD<-drinks$Carb...g.
colD<-as.integer(colD)
cor.test(colC,colD)
r_d<-pearson_corr(colC,colD)
sprintf("Pearson coefficient for Calories and Carb attributes: %f", r_d)
df<-degree_of_freedom(colC,colD)  #get degree of freedom
sprintf("Degrees of freedom: %d",df)
t <-t_value(colC,colD) #get t value
sprintf("T score for pearson correlation between Calories and Carb attributes: %f",t)
r2 <-coefficient_of_detemination(colC,colD)  #get coefficient of determination
sprintf("Coefficient of determination between Calories and Carb attributes: %f",r2)
r_square_in_built = lm(colC ~ colD, data=drinks)  #built-in function for Coefficient of determination
summary(r_square_in_built)$r.squared  #used built in function to check our result

ggplot(data=drinks, aes(x=colC, y=colD, group=1)) +
  geom_line(color="red",linetype="dashed")+
  geom_point()+ggtitle("Relation Between Calories of Drinks and Carbs..")
