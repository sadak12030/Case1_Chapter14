# Chapter 15, case 2 (NASCAR)
# Set the working directory
setwd("/Users/sadakkhondakar/Desktop/MSBA DUC/6. 5504/L3/Chapter 15")
library(readxl)
nascar <- read_excel("nascar.xlsx")
head(nascar)

#Question- 1:
plot(nascar)
round(cor(nascar[,-1]),2)

# alt method
library(PerformanceAnalytics)
chart.Correlation(nascar[,-1],
                  histogram = TRUE)
#The variable most highly correlated with Winnings ($) is the number of top-ten finishes.

# change col names
colnames(nascar)[5] = "Top5"
colnames(nascar)[6] = "Top10"
colnames(nascar)[7] = "Winnings"

options(scipen = 999)
nascar_model_1<- lm(Winnings ~ Top10, data = nascar)
summary(nascar_model_1)

#From the above output, it can be observed that the P value corresponding to t-test each of the individual variables, it has been found that Top10 finishes (p value 0) making it the best predictor out of four.

#Question- 2:
nascar_model_2 <- lm(Winnings ~ Poles + Wins + Top5 + Top10, data = nascar)
summary(nascar_model_2)

# t-test: The only significant variable is Top 10, with a p-value of .0015.
# Adj. R2 is 0.797, model w/ only Top 10 had adj. R2 of 0.8001

#Question- 3:
nascar$Top2_5 <- nascar$Top5 - nascar$Wins #Top 2-5
nascar$Top6_10 <- nascar$Top10 - nascar$Top5 #Top 6-10
nascar_model_3 <- lm(Winnings ~ Poles + Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_3)

# T test: With a p-value of .9047 Poles is not significant.
# we get better results because we greatly reduced the multicollinearity
# Multicollinearity is reduced by replacing Top 5 with Top 2–5 and replacing Top 10 with Top 6–10.
# Correlation Matrix below provides evidence of this.
round(cor(nascar[, -c(1,5,6)]),2) # excluding driver, top5, and top10

library(PerformanceAnalytics)
chart.Correlation(nascar[, -c(1,5,6)],
                  histogram = TRUE)

# Question- 4:
# Keep Wins, Top2-5, and Top6-10
nascar_model_4 <- lm(Winnings ~ Wins + Top2_5 + Top6_10, data = nascar)
summary(nascar_model_4)

# Wins: one unit increase in wins, all other IV constant, increases winnings by $204,735.
# Top 2–5: one unit increase in Top 2–5 finish, all other IV constant, increases winnings by $186,778.
# Top 6–10: one unit increase in Top 6–10 finish, all other IV constant, increases winnings by $116,189.
