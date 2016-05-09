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

system.time(m <- foreach(i=1:5) %do% predictionsForCity(tasks[[i]]))
# Parallel Process... seems buggy now.
#system.time(m <- foreach(i=1:5) %dopar% predictionsForCity(tasks[[i]]))
