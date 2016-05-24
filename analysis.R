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
mongoTestCity(city="newyork",w1=1.00, w2=33.40,s1=48.48,s2=74.78,t1=49,t2=61,a1=86,a2=93,num=100)
mongoTestCity(city="atlanta",w1=71.00,w2=82.00,s1=24.00,s2=60.00,t1=45,t2=62,a1=81,a2=92,num=100)
mongoTestCity(city="denver" ,w1=3.00, w2=32.00,s1=59.69,s2=86.73,t1=43,t2=52,a1=82,a2=91,num=100)
mongoTestCity(city="seattle",w1=5.00, w2=73.40,s1=50.00,s2=79.91,t1=53,t2=75,a1=83,a2=93,num=100)
mongoTestCity(city="houston",w1=83.00,w2=95.00,s1=25.68,s2=56.51,t1=43,t2=66,a1=84,a2=93,num=100)
mongoTestCity(city="angeles",w1=70.00,w2=92.00,s1=41.80,s2=67.19,t1=60,t2=81,a1=81,a2=92,num=100)

# Parallel Process... seems buggy now.
#system.time(m <- foreach(i=1:5) %dopar% predictionsForCity(tasks[[i]]))
