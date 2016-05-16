library(ggplot2)
library(rmongodb)
library(parallel)
library(foreach)
library(doMC)

source('R/MongoTweets.R')

registerDoMC(2)
tasks <- list("meteor.tweets_atlanta",
              "meteor.tweets_boston" ,
              "meteor.tweets_newyork",
              "meteor.tweets_seattle",
              "meteor.tweets_houston")

#system.time(m <- foreach(i=1:5) %do% predictionsForCity(tasks[[i]]))
mongoTestCity(city="newyork",w1=1,w2=33.4,s1=48.48,s2=74.78,t1=49,t2=61,a1=86,a2=93,num=100)
mongoTestCity(city="atlanta",w1=71,w2=82,s1=24,s2=60,t1=45,t2=62,a1=86,a2=93,num=100)
mongoTestCity(city="boston" ,w1=3,w2=32,s1=59.69,s2=86.73,t1=43,t2=52,a1=86,a2=93,num=100)
mongoTestCity(city="seattle",w1=5,w2=73.4,s1=50.00,s2=79.91,t1=53,t2=75,a1=86,a2=93,num=100)
mongoTestCity(city="houston",w1=83,w2=95,s1=25.68,s2=56.51,t1=43,t2=66,a1=86,a2=93,num=100)

# Parallel Process... seems buggy now.
#system.time(m <- foreach(i=1:5) %dopar% predictionsForCity(tasks[[i]]))
