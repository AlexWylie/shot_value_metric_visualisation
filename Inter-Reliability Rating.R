DF <- read.csv("Inter-rater Reliability Random Sample.csv", header = T, na.strings = c("NA", "#N/A"))
DF1 <- read.csv("Inter-rater Reliability Random Sample Second Rater.csv", header = T, na.strings = c("NA", "#N/A"))
xCoordinatesR2 <- summary(lm(DF$X ~ DF1$X))$r.squared
yCoordinatesR2 <- summary(lm(DF$Y ~ DF1$Y))$r.squared
headerR2 <- summary(lm(DF$Header ~ DF1$Header))$r.squared
volleyR2 <- summary(lm(DF$Header ~ DF1$Header))$r.squared
oneOnoneR2 <- summary(lm(DF$X1.on.1 ~ DF1$X1.on.1))$r.squared
deflectionR2 <- summary(lm(DF$From.Deflection ~ DF1$From.Deflection))$r.squared
fromCrossR2 <- summary(lm(DF$From.Cross ~ DF1$From.Cross))$r.squared
fromThroughballR2 <- summary(lm(DF$From.Throughball ~ DF1$From.Throughball))$r.squared
penaltyR2 <- summary(lm(DF$Penalty ~ DF1$Penalty))$r.squared
goalR2 <- summary(lm(DF$Goal ~ DF1$Goal))$r.squared
DF2 <- as.list(x = c(xCoordinatesR2, yCoordinatesR2, headerR2, volleyR2, oneOnoneR2, deflectionR2, fromCrossR2, fromThroughballR2, penaltyR2, goalR2))
DF2ch <- as.character(c("xCoordinatesR2", "yCoordinatesR2", "headerR2", "volleyR2", "oneOnoneR2", "deflectionR2", "fromCrossR2", "fromThroughballR2", "penaltyR2", "goalR2"))
DF2 <- as.data.frame(DF2, col.names = DF2ch)