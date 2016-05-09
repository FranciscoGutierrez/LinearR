library(ggplot2)
library(rmongodb)

# Calculates the prediction values for any city and pushes to MongoDB.

# Input: Name of collection. (Default: "meteor.tweets_atlanta")
# Test:

# tweets_atlanta # tweets_boston
# tweets_houston # tweets_newyork
# tweets_seattle

predictionsForCity <- function(coll = "meteor.tweets_atlanta"){
    opinion<- c()
    safety <- c()
    health <- c()
    qlindx <- c()
    traffi <- c()
    pollut <- c()
    track  <- ""
    mongo <- mongo.create("127.0.0.1:3001",name="meteor")
     if(mongo.is.connected(mongo) == TRUE) {
        mongo.get.database.collections(mongo, "meteor")
        cursor <- mongo.find(mongo, coll, fields = list("polarity"=1L, "quality_of_life_index"=1L, "health_care_index"=1L, "safety_index"=1L, "traffic_index"=1L, "pollution_index"=1L, "track"=1L, "_id"= 0))
        while (mongo.cursor.next(cursor)) {
            value  <- mongo.bson.to.list(mongo.cursor.value(cursor))
            opinion<- c(opinion,value$polarity)
            safety <- c(safety, value$safety_index)
            health <- c(health, value$health_care_index)
            traffi <- c(traffi, value$traffic_index)
            pollut <- c(pollut, value$pollution_index)
            qlindx <- c(qlindx, value$quality_of_life_index)
            track  <- value$track
        }

        s_model <- lm(safety ~ opinion)
        h_model <- lm(health ~ opinion)
        t_model <- lm(traffi ~ opinion)
        p_model <- lm(pollut ~ opinion)
        q_model <- lm(qlindx ~ opinion)
        
        s_int = predict(s_model, interval="prediction")
        h_int = predict(h_model, interval="prediction")
        t_int = predict(t_model, interval="prediction")
        p_int = predict(p_model, interval="prediction")
        q_int = predict(q_model, interval="prediction")
        
        s_fit = s_int[,1]
        s_lwr = s_int[,2]
        s_upr = s_int[,3]
        
        h_fit = h_int[,1]
        h_lwr = h_int[,2]
        h_upr = h_int[,3]
        
        t_fit = t_int[,1]
        t_lwr = t_int[,2]
        t_upr = t_int[,3]
        
        p_fit = p_int[,1]
        p_lwr = p_int[,2]
        p_upr = p_int[,3]
        
        q_fit = q_int[,1]
        q_lwr = q_int[,2]
        q_upr = q_int[,3]
        
        # MISSING CODE TO INSERT TO MONGODB
        #print(c(min(fitted.values),max(fitted.values),min(pred.lower),max(pred.lower),min(pred.upper),max(pred.upper)))
        #tweets <- data.frame(opinion = opinion, index = index)        
        #g <- ggplot(tweets, aes(x=index, y=opinion)) + scale_colour_gradient(low="lightblue", high="white") + geom_point() + stat_smooth(method = "lm", formula=y~x)
        #print(g)
        
        print(s_model)
        print(h_model)
        print(t_model)
        print(p_model)
        print(q_model)
        print(track)
    }
}