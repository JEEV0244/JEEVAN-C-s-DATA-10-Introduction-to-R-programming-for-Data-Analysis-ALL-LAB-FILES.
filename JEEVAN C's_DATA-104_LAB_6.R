#install.packages("tm")
#install.packages("Matrix")
#install.packages("nnet")
#install.packages("LiblineaR")



library(tm)
library(Matrix)
library(LiblineaR)
library(nnet)

rm(list = ls())

getwd()
setwd("C:/MyLaptop/GGU/RProgramming/Spring2026")

### Read the data

tweet = read.csv("GOP_REL_ONLY.csv",stringsAsFactors = FALSE)

head(tweet)
View(tweet)

tweet$text= as.character(tweet$text)
tweet$sentiment = as.factor(tweet$sentiment)

tweet$sentiment = relevel(tweet$sentiment, ref = "Neutral")
View(tweet)

summary(tweet$sentiment)
head(tweet$text)

corpus = VCorpus(VectorSource(tweet$text))

control = list(tolower = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, 
               stopwords = TRUE, wordLengths = c(3, Inf))

dtm = DocumentTermMatrix(corpus, control = control)
dtm$ncol
dtm$nrow
dtm$dimnames
View(dtm$terms)
