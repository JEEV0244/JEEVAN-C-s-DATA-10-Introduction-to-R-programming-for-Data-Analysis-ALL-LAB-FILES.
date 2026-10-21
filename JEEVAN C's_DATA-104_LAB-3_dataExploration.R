
### LAB-3 
### PART-2
### DATA_EXPLORATION
### JEEVAN C
### DATA_104_INTRODUCTION TO R PROGRAM

getwd()
rm(list = ls())

# A file picker window opens: select bike_buyers.csv
mydata <- read.csv(file.choose(), header = TRUE,
                   na.strings = "", stringsAsFactors = FALSE)

# Check that it loaded
head(mydata)
str(mydata)
library(dplyr)

glimpse(mydata)

dim(mydata)

names(mydata)

str(mydata)

head(mydata)

summary(mydata)
colSums(is.na(mydata))

table(mydata$Gender)
table(mydata$Marital.Status)

mydata%>%count(Gender)

#install.packages("ggvis")

#install.packages("ggplot2")

#install.packages("tidyverse")

library(ggvis)
library(tidyverse)
library(ggplot2)

hist(mydata$Income)

hist(mydata$Age)

hist(mydata$Children,breaks = 25)

hist(mydata$Cars, breaks = 20)

mydata$Marital.Status = as.factor(mydata$Marital.Status)
mydata$Gender = as.factor(mydata$Gender)
mydata$Occupation = as.factor(mydata$Occupation)

colSums(is.na(mydata))

median(na.omit(mydata$Income))
median(na.omit(mydata$Age))

mydata_cleaned = mydata

mydata_cleaned$Income[is.na(mydata_cleaned$Income)] = 
  median(na.omit(mydata_cleaned$Income))

colSums(is.na(mydata_cleaned))

mydata_cleaned$Age[is.na(mydata_cleaned$Age)] = median(na.omit(mydata_cleaned$Age))

colSums(is.na(mydata_cleaned))

### mode function ## def

get_mode = function(x){
  unique_x = unique(x)
  tabulate_x = tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
  
}

### bike buyers: Marital.Status, Gender, Children. Home.Owner , Cars

mydata_cleaned$Marital.Status[is.na(mydata_cleaned$Marital.Status)] = 
  get_mode(na.omit(mydata_cleaned$Marital.Status))

colSums(is.na(mydata_cleaned))

mydata_cleaned$Gender[is.na(mydata_cleaned$Gender)] = 
  get_mode(na.omit(mydata_cleaned$Gender))

mydata_cleaned$Children[is.na(mydata_cleaned$Children)] = 
  get_mode(na.omit(mydata_cleaned$Children))

mydata_cleaned$Home.Owner[is.na(mydata_cleaned$Home.Owner)] = 
  get_mode(na.omit(mydata_cleaned$Home.Owner))

mydata_cleaned$Cars[is.na(mydata_cleaned$Cars)] = 
  get_mode(na.omit(mydata_cleaned$Cars))

colSums(is.na(mydata_cleaned))

counts = table(mydata_cleaned$Cars, mydata_cleaned$Gender)

counts

barplot(counts, main = 'Barplot for number of cars', xlab = 'No of Cars', 
        legend = rownames(counts))

hist(mydata_cleaned$Income)
plot(density(mydata_cleaned$Income))
plot(density(mydata_cleaned$Age), main = "Density plot for Age")

ggplot(mydata_cleaned, aes(y = Income, x = Age))+geom_point()

boxplot(mydata_cleaned$Income, main = 'Boxplot for Income', 
        xlab = "Data", ylab = "Income", ylim= c(10000,150000))



 