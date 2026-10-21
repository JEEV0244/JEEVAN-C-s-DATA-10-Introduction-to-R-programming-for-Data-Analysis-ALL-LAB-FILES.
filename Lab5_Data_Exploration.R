############################################################
#Topic: tidyverse pacakegae ...Data manipulation
# Lab5: Data manipulation in R
# Author: Durga Sharma

############################################################
#install.packages("tidyverse")

rm(list = ls())

library("dplyr")
library("tidyr")
library("tidyverse")

getwd()
setwd("C:/MyLaptop/GGU/RProgramming/Spring2026/babynames.csv")

df = read.csv("C:/MyLaptop/GGU/RProgramming/Spring2026/babynames.csv/babynames.csv")

str(df)

View(df)

names(df)

### select

head(select(df,name, year),10)

filter(df, sex =="M")

male_names = df%>%filter(sex=="M")%>%select(name, year)

head(male_names,10)

### filter by specific name may be take John

df%>%filter(name=="John")

### group_by
### count

df%>%group_by(name)%>%summarize(names_counts = sum(n))


df%>%count(name)


df%>%count(year)

### rename  new bame , old name 

df%>%rename(BabyName = name)
head(df)


df1 = df%>%select(name,year, Birth_Year = year)
View(df1)

library(dplyr)
library(babynames)

df%>%group_by(name)%>%summarise(name_counts = sum(n))

df2 = df%>%filter(year == 1880)%>%mutate(year_total = sum(n))
head(df2)

df3 = df%>%filter(year==1880)%>%transmute(year_prop = n/sum(n))
head(df3)

df%>%count(year, sort = T)

install.packages("ggplot2")

library("ggplot2")

Mary = df%>%filter(name=="Mary")
ggplot(Mary, aes(x = year, y = n)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Trend of the Name Mary Over Time",
    x = "Year",
    y = "Number of Occurrences"
  ) 


ggplot(mtcars, aes(x= mpg,y = qsec,size = wt*1000, color= as.factor(cyl)))+geom_point(alpha = 0.8)+
  xlab("Miles per gallon")+
  ylab("Discharge")

ggplot(mtcars, aes( x= as.factor(cyl), y = hp, fill = as.factor(cyl)))+
  geom_boxplot()

ggplot(mtcars, aes(x = hp/wt, y = qsec))+geom_point(size = 4, color = as.factor(cyl), alpha = 0.8)


