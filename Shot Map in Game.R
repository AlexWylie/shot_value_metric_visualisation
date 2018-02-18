library(REdaS)
library(ggplot2)
library(RColorBrewer)
DF <- read.csv("Shot Value Database.csv", header = T, na.strings = "NA")
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
DF <- DF[DF$Game == 31,]
Team1 <- head(DF$Team, 1)
Team2 <- tail(DF$Team, 1)
DF$X1 <- ifelse(DF$Team == Team1, 100-DF$X, DF$X)
DF$Y1 <- ifelse(DF$Team == Team1, 100-DF$Y, DF$Y)
Team1DF <- DF[DF$Team == Team1,]
Team2DF <- DF[DF$Team == Team2,]
g.cols <- c("#6D6E72", "#14AFF1")
DF$GoalColour <- factor(DF$Goal, levels=sort(c(unique(DF$Goal))))
scoreline <- paste(Team1, "[", sum(Team1DF$Goal), "]", "-", Team2, "[", sum(Team2DF$Goal), "]", sep = " ")
shotvalueline <- paste((round(sum(Team1DF$shotvalue), 2)), "-", round(sum(Team2DF$shotvalue), 2), sep = " ")
png(filename="Shot Value Map in a Game.png", units="in", width=10, height=8, res=300)
ggplot(DF, aes(x = X1, y = Y1)) + pitch_markings(col_pitch = "white", col_lines = "black") +
        theme_pitch() + xlim(0, 100) + ylim(0, 100) + theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_text(hjust = 0.5, face = "bold"), axis.title.y = element_blank()) +
        geom_point(aes(colour = GoalColour, size = shotvalue), alpha = 2/3) + scale_size(range = c(1,10)) +
        labs(title = scoreline, x = shotvalueline) + scale_color_manual(values = g.cols, drop = FALSE)
dev.off()