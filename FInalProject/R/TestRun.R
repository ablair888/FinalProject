#Test RUn of all R files compiled



#pkgtest function checks to see if a package is install and if not, it installs and loads the package
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    print(paste("installing",x))
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  } else{
    print(paste(x,"found"))
  }
}

packages <- c("aod", "dplyr", "forestmangr", "ggplot2")
lapply(packages, pkgTest)

data <- read.csv("cbb.csv")


#saved as separate R script file
cleandata <- function(x){
  x <- na.omit(x) #remove all unknowns
  x <- round_df(x, digits = 2) #round to nearest tenths
}

#Team Rating; x=Offensive Efficiency, y=Defensive Efficiency, z=Power Rating
teamrating <- function(x, y, z){
  w <- (x + y)/100
  v <- w/z
  return(v)
}

#winrate; x-games won, y=games played
winrate <- function(x,y){
  return(x/y)
}

data <- cleandata(data)
data$finalplacing <- factor(data$POSTSEASON,labels=c("2","1","4","3","6","7","8","5"))
data$teamrating <- teamrating(data$ADJOE, data$ADJDE, data$BARTHAG)
data$winrate <- winrate(data$W, data$G)

summary(data)

logit.df <- data.frame(
  seed <- factor(data$SEED),
  teamrating <- data$teamrating,
  winrate <- data$winrate,
  tourneywinner <- ifelse(as.numeric(data$finalplacing)<2, 1, 0)
)

xtabs(~tourneywinner + seed, data = logit.df)

mylogit <- glm(tourneywinner ~ teamrating + winrate + seed, data = logit.df, family = "binomial")
summary(mylogit)

confint.default(mylogit)

wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms = 4:16)

newdata1 <- with(logit.df, data.frame(teamrating = mean(teamrating), winrate = mean(winrate), seed = factor(1:16)))
newdata1

newdata1$rankP <-predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <-with(logit.df, data.frame(teamrating = rep(seq(from=1, to=3, length.out=100), 4), winrate = mean(winrate), seed = factor(rep(1:16, each=100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",se = TRUE))

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(newdata3, aes(x = teamrating, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,ymax = UL, fill = seed), alpha = 0.2) + geom_line(aes(color = seed),size = 1)
