library(REdaS)
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
samplesize <- floor(0.8 * nrow(DF))
set.seed(123)
DFsubset <- sample(seq_len(nrow(DF)), size = samplesize)
train <- DF[DFsubset,]
test <- DF[-DFsubset,]
shotvaluemodel <- glm(Goal ~ Header + Volley + X1.on.1 + From.Deflection + From.Cross + From.Throughball + Penalty + relativeangle + length, family = binomial(link='logit'), data = train)
train$shotvalue <- predict(shotvaluemodel, train, type='response')
test$shotvalue <- predict(shotvaluemodel, test, type='response')
rmse <- function(error)
{
        sqrt(mean(error^2))
}
trainerror <- train$Goal - train$shotvalue
testerror <- test$Goal - test$shotvalue
trainrmsevalue <- rmse(trainerror)
testrmsevalue <- rmse(testerror)