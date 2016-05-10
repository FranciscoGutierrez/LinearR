# Calculates the prediction values for any city and pushes to MongoDB.

# Input: Name of collection. (Default: "meteor.tweets_atlanta")
# Test:

# tweets_atlanta # tweets_boston
# tweets_houston # tweets_newyork
# tweets_seattle

predictionsForCity <- function(coll = "meteor.tweets_atlanta"){
    print(paste("Process Started...", coll))
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
        cursor <- mongo.find(mongo, coll, fields = list("polarity"=1L, 
                                                        "quality_of_life_index"=1L,
                                                        "health_care_index"=1L,
                                                        "safety_index"=1L,
                                                        "traffic_index"=1L,
                                                        "pollution_index"=1L,
                                                        "track"=1L, "_id"= 0))
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
        
        # Normalize the data...
        opinion <- (opinion-min(opinion))/(max(opinion)-min(opinion))
        # Normalize the data...
        safety <- (safety-min(16.64))/(max(85.72)-min(16.64))
        health <- (health-min(32.29))/(max(89.81)-min(32.29))
        qlindx <- (qlindx-min(8.92))/(max(241.56)-min(8.92))
        traffi <- (traffi-min(51.41))/(max(363.15)-min(51.41))
        pollut <- (pollut-min(10.47))/(max(99.24)-min(10.47))
        
        print(opinion)
        
        # Setup models...
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
        
        mongo.remove(mongo, "meteor.cities", list(city=track))
        mongo.insert(mongo, "meteor.cities", list(city=track,
                                                  s_fit_min = min(s_fit),
                                                  s_fit_max = max(s_fit),
                                                  s_lwr_min = min(s_lwr),
                                                  s_lwr_max = max(s_lwr),
                                                  s_upr_min = min(s_upr),
                                                  s_upr_max = max(s_upr),
                                                  h_fit_min = min(h_fit),
                                                  h_fit_max = max(h_fit),
                                                  h_lwr_min = min(h_lwr),
                                                  h_lwr_max = max(h_lwr),
                                                  h_upr_min = min(h_upr),
                                                  h_upr_max = max(h_upr),
                                                  t_fit_min = min(t_fit),
                                                  t_fit_max = max(t_fit),
                                                  t_lwr_min = min(t_lwr),
                                                  t_lwr_max = max(t_lwr),
                                                  t_upr_min = min(t_upr),
                                                  t_upr_max = max(t_upr),
                                                  p_fit_min = min(p_fit),
                                                  p_fit_max = max(p_fit),
                                                  p_lwr_min = min(p_lwr),
                                                  p_lwr_max = max(p_lwr),
                                                  p_upr_min = min(p_upr),
                                                  p_upr_max = max(p_upr),
                                                  q_fit_min = min(q_fit),
                                                  q_fit_max = max(q_fit),
                                                  q_lwr_min = min(q_lwr),
                                                  q_lwr_max = max(q_lwr),
                                                  q_upr_min = min(q_upr),
                                                  q_upr_max = max(q_upr),
                                                  num_twets = length(opinion)
                                                  ))
        
        height <- 300
        width  <- 300
        track  <- paste("figs//",track, sep="")
        ##
        png_s <- paste(track,"_s.png", sep="")
        png_q <- paste(track,"_q.png", sep="")
        png_h <- paste(track,"_h.png", sep="")
        png_t <- paste(track,"_t.png", sep="")
        png_p <- paste(track,"_p.png", sep="")
        ## Safety
        png(file=png_s, width,height,units="px",bg = "transparent")
        par(mar=c(0, 0, 0, 0))
        plot(safety,opinion, axes=FALSE, bty="n", ann=FALSE, xlim=c(0,1), ylim=c(0,1), pch=20, bg="#cacaca", col="#676767", cex=0.3)
        dev.off()
        ## Quality
        png(file=png_q, width,height,units="px",bg = "transparent")
        par(mar=c(0, 0, 0, 0))
        plot(qlindx,opinion, axes=FALSE, bty="n", ann=FALSE, xlim=c(0,1), ylim=c(0,1), pch=20, bg="#cacaca", col="#676767", cex=0.3)
        dev.off()
        ## Health
        png(file=png_h, width,height,units="px",bg = "transparent")
        par(mar=c(0, 0, 0, 0))
        plot(health,opinion, axes=FALSE, bty="n", ann=FALSE, xlim=c(0,1), ylim=c(0,1), pch=20, bg="#cacaca", col="#676767", cex=0.3)
        dev.off()
        ## Traffic
        png(file=png_t, width,height,units="px",bg = "transparent")
        par(mar=c(0, 0, 0, 0))
        plot(traffi,opinion,axes=FALSE, bty="n", ann=FALSE, xlim=c(0,1), ylim=c(0,1), pch=20, bg="#cacaca", col="#676767", cex=0.3)
        dev.off()
        ## Pollution
        png(file=png_p, width,height,units="px",bg = "transparent")
        par(mar=c(0, 0, 0, 0))
        plot(pollut,opinion,axes=FALSE, bty="n", ann=FALSE, xlim=c(0,1), ylim=c(0,1), pch=20, bg="#cacaca", col="#676767", cex=0.3)
        dev.off()
        #plot(opinion,qlindx,axes=FALSE, bty="n", ann=FALSE, xaxt="n", yaxt="n",xlim=c(-1,1), ylim=c(0,200), xaxs = "i", yaxs = "i", pch=20, bg="#cacaca", col="#676767", cex=0.5)
        # performance,score
        # opinion, health, qlindx, traffi, pollut
        #print(mongo.find.one(mongo, "meteor.cities", list(city=track)))
    }
}

#tweets <- data.frame(opinion = opinion, index = index)        
#g <- ggplot(tweets, aes(x=index, y=opinion)) + 
#scale_colour_gradient(low="lightblue", high="white") + 
#geom_point() +
#stat_smooth(method = "lm", formula=y~x)
#print(g)
#print(s_model)
#print(h_model)
#print(t_model)
#print(p_model)
#print(q_model)



