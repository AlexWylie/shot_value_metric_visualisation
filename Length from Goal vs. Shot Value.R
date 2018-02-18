library(REdaS)
library(ggplot2)
DF <- read.csv("Shot Value Database.csv", header = T, na.strings = "NA")
DF1 <- read.csv("Post Coordinates.csv", header = T)
DF$Y <- DF$Y*68/105
Xcoordinates <- as.numeric((DF$X))
leftpostX <- as.numeric((DF1$Left.Post.X))
rightpostX <- as.numeric((DF1$Right.Post.X))
Ycoordinates <- as.numeric(DF$Y)
leftpostY <- as.numeric((DF1$Left.Post.Y*68/105))
rightpostY <- as.numeric((DF1$Right.Post.Y*68/105))
DF$leftpostangle <- 0
DF$rightpostangle <- 0
for(i in 1:nrow(DF)){
        v1 <- sqrt((leftpostX - Xcoordinates[i])^2 + (leftpostY - Ycoordinates[i])^2)
        v2 <- sqrt((leftpostX - rightpostX)^2 + (leftpostY - rightpostY)^2)
        v3 <-sqrt((Xcoordinates[i] - rightpostX)^2 + (Ycoordinates[i] - rightpostY)^2)
        leftpostangle <- acos((v1^2 + v2^2 - v3^2) / (2 * v1 * v2))
        leftpostangle <- rad2deg(leftpostangle)
        v1 <- sqrt((rightpostX - Xcoordinates[i])^2 + (rightpostY - Ycoordinates[i])^2)
        v2 <- sqrt((rightpostX - leftpostX)^2 + (rightpostY - leftpostY)^2)
        v3 <-sqrt((Xcoordinates[i] - leftpostX)^2 + (Ycoordinates[i] - leftpostY)^2)
        rightpostangle <- acos((v1^2 + v2^2 - v3^2) / (2 * v1 * v2))
        rightpostangle <- rad2deg(rightpostangle)
        DF$leftpostangle[i] <- leftpostangle
        DF$rightpostangle[i] <- rightpostangle
}
DF[is.na(DF$leftpostangle), "leftpostangle"] <- 90
DF[is.na(DF$rightpostangle), "rightpostangle"] <- 90
for(i in 1:nrow(DF)){
        if(DF$leftpostangle[i] > 90){
                relativeangle <- (90-(((DF$leftpostangle[i]))-90))/90
        }
        else if(DF$rightpostangle[i] > 90){
                relativeangle <- (90-(((DF$rightpostangle[i]))-90))/90
        }
        else {relativeangle <- 1
        }
        DF$relativeangle[i] <- relativeangle
}
middlegoalxy <- (c((leftpostX + rightpostX)/2, (leftpostY + rightpostY)/2))
DF$length <- 0
for(i in 1:nrow(DF)){
        shotxy <- c(DF$X[i], DF$Y[i])
        DF$length[i] <- dist(rbind(shotxy, middlegoalxy))
}
shotvaluemodel <- glm(Goal ~ Header + Volley + X1.on.1 + From.Deflection + From.Cross + From.Throughball + Penalty + relativeangle + length, family = binomial(link='logit'), data = DF)
DF$shotvalue <- predict(shotvaluemodel, DF, type = "response")
DF <- DF[!DF$Penalty == 1,]
DF$length <- DF$length*1.05
png(filename = "Length from Goal (M) vs. Shot Value Plot.png", units = "in", width = 10, height = 8, res = 300)
g <- ggplot(DF, aes(x = length, y = shotvalue))
g + geom_point(alpha = 2/3, colour = "orange") + geom_smooth(method = "auto") + labs(x = "Length from Centre of Goal (M)", y = "Shot Values", title = "Relationship Between Length from Centre of Goal (M) and Shot Values") +
        theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 10, family = "Arial"))
dev.off()