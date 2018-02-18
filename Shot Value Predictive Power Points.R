library(REdaS)
library(psychometric)
library(ggplot2)
library(boot)
library(reshape2)
library(dplyr)
DF <- read.csv("Shot Value Database.csv", header = T, na.strings = c("NA", "#N/A"))
DF1 <- read.csv("Post Coordinates.csv", header = T)
DF$Y1 <- DF$Y*68/105
Xcoordinates <- as.numeric((DF$X))
leftpostX <- as.numeric((DF1$Left.Post.X))
rightpostX <- as.numeric((DF1$Right.Post.X))
Ycoordinates <- as.numeric(DF$Y1)
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
R2 <- data.frame(table(DF$Game[DF$Team == "Leicester City F.C."])/(table(DF$Game[DF$Team == "Leicester City F.C."])+table(DF$Game[!DF$Team == "Leicester City F.C."])))
DFmelt <- melt(DF, id.vars = c("Game", "Team"), measure.vars = c("Goal", "shotvalue"))
DFcast <- dcast(DFmelt, Game + Team ~ variable, sum)
goalRatioFunction <- function(DFcast){
        DFsplit <- split(DFcast, DFcast$Game)
        sapply(DFsplit, function(DFsplit) DFsplit$Goal[DFsplit$Team == "Leicester City F.C."]/(DFsplit$Goal[DFsplit$Team == "Leicester City F.C."]+DFsplit$Goal[!DFsplit$Team == "Leicester City F.C."]))
}
R2$goalRatio <- goalRatioFunction(DFcast)
R2$goalRatio[is.na(R2$goalRatio)] <- 0
shotValueRatioFunction <- function(DFcast){
        DFsplit <- split(DFcast, DFcast$Game)
        sapply(DFsplit, function(DFsplit) DFsplit$shotvalue[DFsplit$Team == "Leicester City F.C."]/(DFsplit$shotvalue[DFsplit$Team == "Leicester City F.C."]+DFsplit$shotvalue[!DFsplit$Team == "Leicester City F.C."]))
}
R2$shotValueRatio <- shotValueRatioFunction(DFcast)
R2$shotValueRatio[is.na(R2$shotValueRatio)] <- 0
colnames(R2) <- c("Game", "shotRatio", "goalRatio", "shotvalueRatio")
R2$point <- 0
for(i in 1:nrow(R2)){
        if(R2$goalRatio[i] > 0.5){
                R2$point[i] <- 3
        } else if(R2$goalRatio[i] == 0.5){
                R2$point[i] <- 1
        } else {R2$point[i] <- 0
        }}
R2$pointsPerGameSample1 <- 0
R2$pointsPerGameSample2 <- 0
R2$pointsPerGameR2 <- 0
for(i in 1:nrow(R2)){
        R2$pointsPerGameSample1[i] <- mean(R2$point[1:i])
}
for(i in nrow(R2):1){
        R2$pointsPerGameSample2[i] <- mean(R2$point[32:i])
}
for(i in 1:nrow(R2)){
        R2$pointsPerGameR2[i] <- summary(lm(pointsPerGameSample1 ~ pointsPerGameSample2, R2[1:i,]))$r.squared
}
R2$pointsPerGameR2[is.na(R2$pointsPerGameR2)] <- 0
R2$goalRatioPerGame <- 0
R2$goalRatioPerGameR2 <- 0
for(i in 1:nrow(R2)){
        R2$goalRatioPerGame[i] <- mean(R2$goalRatio[1:i])
}
for(i in 1:nrow(R2)){
        R2$goalRatioPerGameR2[i] <- summary(lm(goalRatioPerGame ~ pointsPerGameSample2, R2[1:i,]))$r.squared
}
R2$goalRatioPerGameR2[is.na(R2$goalRatioPerGameR2)] <- 0
R2$shotRatioPerGame <- 0
R2$shotRatioPerGameR2 <- 0
for(i in 1:nrow(R2)){
        R2$shotRatioPerGame[i] <- mean(R2$shotRatio[1:i])
}
for(i in 1:nrow(R2)){
        R2$shotRatioPerGameR2[i] <- summary(lm(shotRatioPerGame ~ pointsPerGameSample2, R2[1:i,]))$r.squared
}
R2$shotRatioPerGameR2[is.na(R2$shotRatioPerGameR2)] <- 0
R2$shotValueRatioPerGame <- 0
R2$shotValueRatioPerGameR2 <- 0
for(i in 1:nrow(R2)){
        R2$shotValueRatioPerGame[i] <- mean(R2$shotvalueRatio[1:i])
}
for(i in 1:nrow(R2)){
        R2$shotValueRatioPerGameR2[i] <- summary(lm(shotValueRatioPerGame ~ pointsPerGameSample2, R2[1:i,]))$r.squared
}
R2$shotRatioPerGameR2[is.na(R2$shotRatioPerGameR2)] <- 0
R2$Game <- as.numeric(R2$Game)
R2melt <- melt(R2, id.vars = "Game", measure.vars = c("pointsPerGameR2", "goalRatioPerGameR2", "shotRatioPerGameR2", "shotValueRatioPerGameR2"))
g <- ggplot(R2melt, aes(x = Game, y = value))
g + geom_smooth(aes(colour = variable), method = "loess", se = F) + scale_y_continuous(limits  = c(0, 1)) +
        scale_x_continuous(breaks = seq(0, 32, 4)) + labs(y = "Correlation (R2)", title = "Predictors of Future Points") + scale_colour_discrete("Metric", labels = c("Points per Game", "Goal Ratio per Game", "Shot Ratio per Game", "Shot Value Ratio per Game")) +
        theme(text = element_text(size = 10, family = "Arial"), plot.title = element_text(hjust = 0.5), legend.position = "bottom")
ggsave("Metrics Predictors of Future Points.png", width = 10, height = 8)
Stats <- colMeans(R2)