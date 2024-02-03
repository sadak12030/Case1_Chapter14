# Clear the environment
rm(list = ls())

# Set the working directory
setwd("/Users/sadakkhondakar/Desktop/MSBA DUC/6. 5504/L3/Chapter 15")

# Q1 (15-5)-----
library(readxl)
Revenue <- read_excel("Showtime.xlsx")
colnames(Revenue) <- c("Gross Revenue", "TV_Ad", "Paper_Ad")  # rename the column
Revenue_1 <- lm ( `Gross Revenue` ~ TV_Ad, data= Revenue)
summary(Revenue_1)
coefficients(Revenue_1) # a.estimated regression equation with the amount of television advertising as the independent variable.
Revenue_2 <- lm ( `Gross Revenue` ~ TV_Ad+Paper_Ad , data= Revenue)
coefficients(Revenue_2) # b.estimated regression equation with both television advertising and newspaper advertising as the independent variables.
# c. Coefficient of television advertisement is not same for a. & b. For case (a) every 1.6k television advertisement cost increase weekly revenue by
# 1k. Whereas, for case(b) every 2.29k television advertisement cost increase weekly revenue by 1k.

predict(Revenue_2, data.frame(TV_Ad = 3.5, Paper_Ad = 2.3)) # weekly gross revenue for a week when $3500 is spent on television advert- ising and $2300 is spent on newspaper advertising.

# Q2 (15-8)---
library(readxl)
Ships <- read_excel("Ships.xlsx")
colnames(Ships) <- c("Ship name", "Overall", "Shore", "Food")  # rename the column
Ship_1 <- lm ( Overall ~ Shore, data = Ships)
coefficients(Ship_1) # a. estimated regression equation that can be used to predict the overall score given the score for Shore Excursions.
Ship_2 <- lm ( Overall ~ Shore+Food, data = Ships)
coefficients(Ship_2) #b. estimated regression equation that can be used to predict the overall score given the score for Shore Excursions and Food/Dining.
predict(Ship_2, data.frame(Shore = 80, Food = 90)) #c.overall score for a cruise ship with a Shore Excursions score of 80 and a Food/Dining Score of 90.

# Q3 (15-12)---
Data <- data.frame(X1= c(30,47,25,51,40,51,74,36,59,76), X2=c(12,10,17,16,5,19,7,12,13,16), y=c(94,108,112,178,94,175,170,117,142,211))
summary(Data)
Data_lm <- lm(y~X1+X2, data = Data)
summary(Data_lm) # a. Multiple R-squared:  0.9255
# b. Adjusted R-squared:  0.9042
# c. 90% of variability have been explained by this regression equation

# Q4 (15-18)---
library(readxl)
MLB <- read_excel("PitchingMLB.xlsx")
MLB_lm <- lm (`R/IP` ~ `SO/IP`+`HR/IP`, data = MLB)
summary(MLB_lm) #a. Multiple R-squared:  0.5635,	Adjusted R-squared:  0.5121
#b. Estimated regression have explained 51% variability of the results based on Strikeouts and Home runs given.
# we can search for better model which can explain more % of variability of the predicted result.
MLB_lm2 <- lm (ERA ~ `SO/IP`+`HR/IP`, data = MLB)
summary(MLB_lm2) # Multiple R-squared:  0.6251,	Adjusted R-squared:  0.581
#c. This model explained 58% variability compared to 51% of the previuos model
# Thus use of ERA would provide good fit to explain these data.

# Q5 (15-23)---
library(readxl)
Revenue <- read_excel("Showtime.xlsx")
colnames(Revenue) <- c("Gross Revenue", "TV_Ad", "Paper_Ad")  # rename the column
Revenue_2 <- lm ( `Gross Revenue` ~ TV_Ad+Paper_Ad, data= Revenue)
summary(Revenue_2)
predict(Revenue_2, data.frame(TV_Ad = 1, Paper_Ad = 1))
library(corrplot)
corrplot(cor(Revenue), method = "number")
library(GGally)
ggcorr(Revenue, levels=TRUE)
library(PerformanceAnalytics)
chart.Correlation(Revenue, histogram = TRUE)
#a. p-value for F test: 0.001865 is lower than 0.01 and overall model is significant.
#b. p-value for T test of TV_ad is 0.000653, lower than 0.05. So we should not drop it from the model.
#c. p-value for T test of Paper_Ad is 0.009761, lower tha 0.05. So we should not drop it from the model.

#Q6 (15-26)---
library(readxl)
MLB <- read_excel("PitchingMLB.xlsx")
MLB_lm <- lm (`R/IP` ~ `SO/IP`+`HR/IP`, data = MLB)
summary(MLB_lm)# a. p-value for F test is 0.0008 which is lower than 0.05. This indicates that overall relationship is significant.
anova(MLB_lm)# b. Use of T test, p-value SO/IP is 0.0008 and p-value of HR/IP is 0.03.
# p values of both independent variables are lower than 0.05 indicates both of the independent variables are significant for the estimated regression equation.

# Q7 (15-31)---
Car <-read_excel("AutoResale.xlsx")
options(scipen = 999)
Car_lm <- lm(Price ~ Mileage+Age, data = Car)
coef(Car_im)
predict(Car_lm, data.frame(Mileage = 40000, Age=4)) #a. Estimated selling price of a four-year-old Honda Accord with mileage of 40,000 miles.
predict(Car_lm, data.frame(Mileage = 40000, Age=4),interval = "confidence", level = .95) #b. 95% confidence interval for the selling price of a car with the data in part (a).
predict(Car_lm, data.frame(Mileage = 40000, Age=4),interval = "prediction", level = .95) #c. 95% prediction interval for the selling price of a car with the data in part (a).

# Q8 (15-36)---
Repair <- read_excel("Repair.xlsx")
colnames(Repair) <- c("Time", "Month", "Type","Person")  # rename the column
Repair_model <- lm(Time~Month+Type+Person, data = Repair)
summary(Repair_model)
Repair$Person <- relevel(factor(Repair$Person), ref = "Dave Newton")
Repair_model <- lm(Time~Month+Type+Person, data = Repair)
summary(Repair_model)
coef(Repair_model) #a. estimated regression equation
#b. F test p-value is0.002 which lower than 0.05 which indicates overall model has significant relationship among the IVs and DV.
#c. T test of variable x3 shows p-value is 0.16 which is higher than 0.05 which indicates
# the person performing repair has not significant relation with the regression model result.

# Q9 (15-38)---
Stroke <- read_excel("Stroke.xlsx")
Stroke_model <- lm (Risk~Age+Pressure+Smoker, data = Stroke)
coef(Stroke_model) #a. estimated regression equation that relates risk of a stroke to the person's age, blood pressure, and whether the person is a smoker.
summary(Stroke_model) #b. p-value smoking is 0.01 at T test indicates smoking is a significant for stroke at 0.05 alpha level.
predict(Stroke_model, data.frame(Age=68,Pressure=175,Smoker="Yes")) #c. probability of a stroke over the next 10 years is 34.27%.

# Q10 (15-41)---
Revenue <- read_excel("Showtime.xlsx")
colnames(Revenue) <- c("Gross Revenue", "TV_Ad", "Paper_Ad")  # rename the column
Revenue_2 <- lm ( `Gross Revenue` ~ TV_Ad+Paper_Ad , data= Revenue)
coefficients(Revenue_2) # a. estimated regression equation relating weekly gross revenue to television and newspaper advertising.
plot(Revenue_2, which = 1) # 1 = residual versus fitted
plot(Revenue_2, which = 2) # 2 = QQ plot
plot(Revenue_2, which = 3) # 3 = Scale-Location
plot(Revenue_2, which = 4) # 4 = Cook's Distance
plot(Revenue_2, which = 5) # 5 = Williams-like Graph
# b. standardized residual plot does not support the initial assumption about e.
# c. observation 1 is the potential outlier of this data set.
# d. From cook's distance we found observation 1 has significant influence over the data set.

# Q11 (15-42)---
Sportscar <- read_excel("Auto2.xlsx")
colnames(Sportscar) <- c("Carname", "Price", "Weight", "Horsepower", "Speed" )  # rename the column
Sportscar_lm <- lm(Speed ~ Price+Horsepower, data = Sportscar)
coefficients(Sportscar_lm) #a. estimated regression equation that uses price and horsepower to predict 1⁄4-mile speed.
plot(Sportscar_lm, which = 1) # 1 = residual versus fitted
plot(Sportscar_lm, which = 2) # 2 = QQ plot
plot(Sportscar_lm, which = 3) # 3 = Scale-Location
plot(Sportscar_lm, which = 4) # 4 = Cook's Distance
plot(Sportscar_lm, which = 5) # 5 = Williams-like Graph
#b. standardized residual plot does not support the initial assumption about e.
#c. from the QQ plot observation 5 could be an outlier at this data set.
#d. from the Cook's distance we found observation 2 has significant influence over the data set.

# Q12 (15-47)---
College <- read_excel("Lakeland.xlsx")
#library(tidyverse)
#College %>% ggplot(aes (Program, Return))+geom_point(size = 3, alpha = 0.2)+ geom_smooth(method = "lm")
library(stats)
College_model <- glm( Return ~ GPA + Program, data= College, family = 'binomial') #a. logistic regression equation relating x1 and x2 to y.
College_model1 <- glm( Return ~ Program, data = College, family = 'binomial')
predict(College_model1,data.frame(Program=0), type = "response") #b.probability is 0.33 of returing of a student when he/she did not attended the orientation program.
coefficients(College_model) #c. logit for GPA is 2.54 and Program is 1.56
options(scipen = 999)
summary(College_model) 
anova(College_model,test = "Chisq") #d and #e.with aplha level 0.05 both independent variables are significant and overall relation of the model is significant.
exp(College_model$coefficients)
predict(College_model,data.frame(GPA = 2.5, Program = 0), type = "response") #f. probability is 0.37, students with a 2.5 grade point average who did not attend the orientation program. 
predict(College_model,data.frame(GPA = 2.5, Program = 1), type = "response") #f. probability is 0.73, students with a 2.5 grade point average who attended the orientation program.
coefficients(College_model1)
exp(College_model1$coefficients) # g. odd ratio of orientation program is 10.8. it means students participated in the orientation program have 10.8 time higher chances to return the college compare to those who didn't attended.
#h. As we found out orientation program increased the chances of returning students to college is 10.8 times more. we should recommend for the orientation program.







