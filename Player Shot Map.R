library(REdaS)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
DF <- read.csv("Shot Value Database.csv", header = T, na.strings = c("NA", "#N/A"))
DF1 <- read.csv("Post Coordinates.csv", header = T)
DF2 <- read.csv("/Users/alexwylie/Documents/Football Analytics/Job/Leicester City F.C. Academy/End of Season Report/Performance Analysis/PA Season.csv", header = T, na.strings = "NA")
DF2 <- DF2[DF2$Age.Group == "U18",]
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
DF <- DF[!DF$Penalty == "1",]
DF2 <- merge(DF, DF2[,c("Minutes","Player")], by.x = "Shooting.Player", by.y =  "Player", all.x=T)
shotvaluemelt <- melt(DF2, id.vars = "Shooting.Player", measure.vars = "shotvalue")
minutesmelt <- melt(DF2, id.vars = "Shooting.Player", measure.vars = "Minutes")
shotvaluecast <- dcast(shotvaluemelt, Shooting.Player ~ variable, sum)
minutescast <- dcast(minutesmelt, Shooting.Player ~ variable, mean)
DF2 <- merge(shotvaluecast, minutescast)
DF2$shotvalueP90 <- (DF2$shotvalue/DF2$Minutes)*90
DF1 <- subset(DF, DF$Shooting.Player == "John")
DF2subset <- subset(DF2, DF2$Shooting.Player == "John")
Player <- head(DF1$Shooting.Player, 1)
DF1$Y <- 100-DF1$Y
g.cols <- c("#6D6E72", "#14AFF1")
DF1$GoalColour <- factor(DF1$Goal, levels=sort(c(unique(DF1$Goal))))
headline <- paste(Player, "-", "Minutes:", DF2subset$Minutes, "-", "Goal Total:",  sum(DF1$Goal), "-", "Shot Value Total:", round(sum(DF1$shotvalue), 1), "-", "Shot Total:", nrow(DF1), "\n Goals P90:", round(sum(DF1$Goal)/DF2subset$Minutes*90, 2), "-", "Shot Value P90:", round(DF2subset$shotvalueP90, 2),  "-" , "Shots P90:", round((nrow(DF1)/DF2subset$Minutes)*90, 2), "-", "Shot Value per Shot:", round(sum(DF1$shotvalue)/nrow(DF1), 2))[1]
g <- ggplot(DF1, aes(x = X, y = Y)) + pitch_markings(col_pitch = "white", col_lines = "black") +
        coord_flip() + xlim(head(DF[order(DF$X),"X"], 1) - 2, 101) + ylim(0, 100) +
        theme_pitch() + theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 10, family = "Arial")) +
        geom_point(aes(colour = GoalColour, size = shotvalue), alpha = 2/3) + scale_size_continuous(range = c(1,10), breaks = seq(0,1,by = 0.01), limits = c(head(DF[order(DF$shotvalue),"shotvalue"], 1), tail(DF[order(DF$shotvalue),"shotvalue"], 1))) +
        labs(title = headline) + scale_color_manual(values = g.cols, drop = FALSE)
ggsave("John Shot Value Map.png", height = 8, width = 10, dpi = 300)
secondDF <- read.csv("Shot Value Database.csv", header = T, na.strings = c("NA", "#N/A"))
secondDF1 <- data.frame(table(secondDF$Shooting.Player))
shotvalueDF <- merge(DF2, secondDF1, by.x = "Shooting.Player", by.y = "Var1")
shotvalueDF$shotvaluepershot <- shotvalueDF$shotvalue/shotvalueDF$Freq
colnames(shotvalueDF) <- c("Player", "Shot Value", "Minutes", "Shot Value P90", "Shots", "Shot Value per Shot")
write.csv(shotvalueDF, "Shot Value Comparison.csv")