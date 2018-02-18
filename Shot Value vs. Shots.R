library(REdaS)
library(ggplot2)
library(gridExtra)
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
DF2 <- DF[!DF$Team == "Leicester City F.C.",]
shotvaluemodel <- glm(Goal ~ Header + Volley + X1.on.1 + From.Deflection + From.Cross + From.Throughball + Penalty + relativeangle + length, family = binomial(link='logit'), data = DF2)
DF$shotvalue <- predict(shotvaluemodel, DF, type='response')
DF3 <- aggregate(. ~ Shooting.Player, DF, sum, na.action = NULL)
DF3$shot <- as.numeric(table(DF$Shooting.Player))
shotvaluemodelreg <- lm(Goal ~ shotvalue, DF3)
shotmodelreg <- lm(Goal ~ shot, DF3)
png(filename = "Shot Value vs. Shot Plot.png", units = "in", width = 10, height = 8, res = 300)
g1 <- ggplot(DF3, aes(x=shotvalue, y=Goal), factor = Shooting.Player) + geom_point(alpha = 4/5, colour = "orange") +
        geom_smooth(method = "lm") + labs(y = "Observed Goals", x = "Aggregated Shot Values") +
        geom_text(aes(x = 1, y = 4, label = paste('R^2 = ', round(summary(shotvaluemodelreg)[['r.squared']], 3), sep = ''))) +
        ylim(c(-1,5)) + theme(text = element_text(size = 10, family = "Arial"))
g2 <- ggplot(DF3, aes(x=shot, y=Goal), factor = Shooting.Player) + geom_point(alpha = 4/5, colour = "orange") +
        geom_smooth(method = "lm") + labs(y = "Observed Goals", x = "Observed Shots") +
        geom_text(aes(x = 15, y = 4, label = paste('R^2 = ', round(summary(shotmodelreg)[['r.squared']], 3), sep = ''))) +
        theme(text = element_text(size = 10, family = "Arial"))
grid.arrange(g1, g2, ncol = 2, top = textGrob("Relationships between Aggregated Shot Values & Observed Shots with Observed Goals for U18 Leicester City F.C.", gp=gpar(fontsize = 12)))
dev.off()