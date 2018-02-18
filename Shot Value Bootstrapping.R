library(REdaS)
library(psychometric)
library(ggplot2)
library(zoo)
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
set.seed(1234)
DF <- coredata(DF[rep(seq(nrow(DF)), 10000),])
FinishingRating <- sapply(seq(3, 1000, 1), function(x) mean(replicate(10000, {
        r <- sample(3:1000, x, replace = TRUE)
        DF$Goal[r] - DF$shotvalue[r]
})))
lower <- sapply(3:1000, function(i) return(CIr(r = FinishingRating[i], n = seq(3, 1000, 1)[i], level = 0.95)[1]))
upper <- sapply(3:1000, function(i) return(CIr(r = FinishingRating[i], n = seq(3, 1000, 1)[i], level = 0.95)[2]))
DF2 <- data.frame(cbind(FinishingRating, lower, upper, seq(3, 1000, 1)))
DF2 <- read.csv("Bootstrap Data.csv", header = T)
ggplot(DF2, aes(x = V4, y = FinishingRating)) + 
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) +
        labs(x = "Shots", y = "Goal Total - Shot Value Total", title = "Mean and 95% Confidence Intervals of Difference Between Goal Total and Shot Value Total in Increasing Sample Size") +
        geom_hline(yintercept = mean(DF2$FinishingRating), colour = "blue") + theme(text = element_text(size = 10, family = "Arial"), plot.title = element_text(hjust = 0.5))
ggsave("Bootstrap Shot Value Finishing Skill.png", width = 10, height = 8)