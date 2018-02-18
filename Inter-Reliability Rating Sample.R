DF <- read.csv("Shot Value Database.csv", header = T, na.strings = c("NA", "#N/A"))
set.seed(1234)
sample <- sample(seq_len(nrow(DF)), size = 20)
DF <- DF[sample,]
write.csv(DF, file = "Inter-rater Reliability Random Sample.csv", row.names = F)