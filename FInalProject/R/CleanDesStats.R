#RScript that runs functions from AllFUnctions.R. Also cleans data and adds to dataframe. Runs summary function

data <- cleandata(data)
data$finalplacing <- factor(data$POSTSEASON,labels=c("2","1","4","3","6","7","8","5"))
data$teamrating <- teamrating(data$ADJOE, data$ADJDE, data$BARTHAG)
data$winrate <- winrate(data$W, data$G)

summary(data)