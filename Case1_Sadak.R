rm(list = ls())
setwd("~/Desktop/MSBA DUC/6.5504/L2/Chapter 14")
dir.exists("~/Desktop/MSBA DUC/6.5504/L2/Chapter 14")
my_path<- '/Desktop/MSBA DUC/6.5504/L2/Chapter 14/BuckeyeCreek.xlsx'
my_path <- '/Desktop/MSBA DUC/6.5504/L2/Chapter 14/BuckeyeCreek.xlsx'
library(readxl)
buckeyecreek <- read_excel(my_path)
my_path <- '~/Desktop/MSBA DUC/6.5504/L2/Chapter 14/BuckeyeCreek.xlsx'
library(readxl)
library(readxl)
buckeyecreek <- read_excel("BuckeyeCreek.xlsx")
summary(buckeyecreek)
descriptive_stats(buckeyecreek)
plot(buckeyecreek$Population, buckeyecreek$`Season Pass Holders`,
     xlab = "Population", ylab = "Season Pass Holders",
     main = "Scatter Plot of Season Pass Holders vs. Population")
linear_model <- lm(`Season Pass Holders` ~ Population, data = buckeyecreek)
summary(linear_model)
par(mfrow = c(2, 2))
plot(linear_model)


# Examine Correlation between variables (Multiple R in Excel)
cor(buckeyecreek$Population, buckeyecreek$`Season Pass Holders`)

# Simple linear regression
buckeyecreek_slr <- lm(`Season Pass Holders` ~ Population, data = buckeyecreek)
buckeyecreek_slr$coefficients # coefficients

# Plotting data and regression line
library(tidyverse)
buckeyecreek %>% ggplot(aes(x = Population, y = `Season Pass Holders`)) + # coordinate system
  geom_point() + # scatter plot
  geom_smooth(method = 'lm', se = TRUE) # abline with conf. interval (gray)

#1. Compute descriptive statistics and construct a scatter diagram for the data. Discuss your findings.

#Ans: The following scatter diagram describe the relationship between number of season pass holders, 
#given the total population of a zip code.
#Scatter diagram shows that points move from lower left corner to upper right corner. This implies a positive relationship between the variables. 
#The points lie in and around a straight line, which indicates a possible linear relationship.

#A descriptive study for BuckeyeCreek data reveals that mean size of population is 15738 and mean season pass holder is 128. 
#The population for a given zip code has much higher variability (SD=12639.5436) compared to season pass holder (SD=145.3776).


# Section 2 ----

# R Squared Value
summary(buckeyecreek_slr)$r.squared

# Adjusted R Squared Value
summary(buckeyecreek_slr)$adj.r.squared

#The estimated regression line to predict number of season pass holders is as follows:
#y=16.2584+0.0092x1
#Where, x1 is population.

# Section 3 ----

# Complete regression analysis
summary(buckeyecreek_slr)

# Confidence and Prediction Interval
predict (buckeyecreek_slr, data.frame(Population = 10), interval = "confidence", conf.level = 0.95)
predict (buckeyecreek_slr, data.frame(Population = 10), interval = "prediction", conf.level = 0.95)

#There is significant relationship between the number of seasons pass holders and population, p<0.05. 
#The estimated regression line provides a good fit, p<0.05.

# Section 4 ----

# Residual analysis (method 1)
buckeyecreek$predicted <- fitted(buckeyecreek_slr)
buckeyecreek$residuals <- residuals(buckeyecreek_slr)
buckeyecreek$std_residuals <- rstandard(buckeyecreek_slr)

#The residual plot shows that the residuals are scattered mostly horizontally. This implies that variance of α is same for all values of x. 
#Approximately 95% of standardized residuals fall within -1 and 2. Thus, the error term has normal distribution.

# Residual plot against x
buckeyecreek %>% ggplot(aes(x = Population, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Residual plot against y-hat
buckeyecreek %>% ggplot(aes(x = predicted, y = residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# Standard residual plot against x
buckeyecreek %>% ggplot(aes(x = Population, y =std_residuals)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)  # we observe no pattern

# QQ Plot 
buckeyecreek %>% ggplot(aes(sample = std_residuals)) +
  geom_qq() + 
  geom_qq_line()

# Section 5 ----

# Calculating Leverage (Hat Values)
buckeyecreek$hat <- hatvalues(buckeyecreek_slr) #none greater than 6/10=0.6
qplot (buckeyecreek$Population, buckeyecreek$hat) +
  geom_hline(yintercept=0.6, linetype="dashed", color = "red")

# Residual analysis (method 2)
plot(buckeyecreek_slr, which = 1) # can produce 5 plots
# 1 = residuals versus fitted
# 2 = QQ plot
# 3 = Scale-Location
# 4 = Cook's Distance
# 5 = Williams-like Graph

plot(buckeyecreek_slr, which = 4) #none greater than 0.5
buckeyecreek$cook <- cooks.distance(buckeyecreek_slr)
# Cook's distance is a combination of leverage and residual values
# The higher the leverage and residuals, the higher the Cook’s distance.
# Investigate any point over 0.5, values over 1.0 are influential

plot(buckeyecreek_slr, which = 5) #none greater than 0.5
# in this plot (leverage ~ std) we are looking for values lying outside dashed line

#The marketing team can consider the regression line to predict number of season pass holder 
#provided the population for a certain zip code is within the range of the given population.
