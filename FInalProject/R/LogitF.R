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