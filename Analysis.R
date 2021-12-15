rm(list=ls())
library(tidyverse)
library(readxl)
library(ggplot2)

###########
#DataManagement
#contains the lines, their cap hits, and their teams season performance result
team_performance_data<-read_excel("Desktop/lines.xlsx",
                                  sheet = "Team_Stats")
View(team_performance_data)

#contains the lines, their cap hits, and all of the stats for how the specific line performed
line_performance_data<-read_excel("Desktop/lines.xlsx",
                                  sheet = "Line_Stats")

View(line_performance_data)

#joining the data sets to add the columns that do not appear in both
joint_data_set <- merge(line_performance_data,team_performance_data)
View(joint_data_set)

#rounding the Cap Hit variable to three decimals
joint_data_set$`Cap Hit` = round(joint_data_set$`Cap Hit`,digit = 3)

#adding a constant variable to represent the cap for each team
joint_data_set$cap <- c(79.500)

#adding wins and points earned columns for each corresponding lines team. These are team success measurements
joint_data_set$points <- c(43,54,73,37,80,48,55,55,82,60,48,72,79,49,75,59,45,64,71,60,51,58,77,49,63,75,77,50,82,63,77)
joint_data_set$wins <- c(17,24,33,15,36,18,26,24,39,23,19,35,37,21,35,24,19,31,32,27,23,25,37,21,27,36,35,23,40,30,36)

#renaming Team Result to Playoff Result...More descriptive
names(joint_data_set)[names(joint_data_set) == "Team Result"] <- "Playoff_Result"

#average Cap hit of the whole league
average_cap_hit <- mean(joint_data_set$`Cap Hit`)

#playoff data frame
Playoff_Teams <- joint_data_set[joint_data_set$Playoff_Result!=0,]
#league average corsi%
averagee_corsi <- mean(joint_data_set$corsiPercentage)


#cap hit standard deviation for the whole league
cap_hit_sd <- sd(joint_data_set$`Cap Hit`)
###################################
#Regression

#making an indicator variable to indicate whether a team made the playoffs or not
joint_data_set$playoff_indicator <- (0)
joint_data_set$playoff_indicator[joint_data_set$Playoff_Result != 0] <-1

library(sandwich)
library(caret)
options(scipen=999)

#linear
#model1 predicting corsi percent with cap hit and playoff result
corsi1 <- lm(corsiPercentage~`Cap Hit`+ playoff_indicator, data=joint_data_set)
summary(corsi1)

#quantify the residuals
corsi1_residuals<-resid(corsi1)

##residual plot
plot(corsi1_residuals~joint_data_set$`Cap Hit`, xlab="Cap Hit", ylab="e")

#look at the slope and intercept
vcov(corsi1, type="HC1")

#get standard errors
corsi1_SE<-diag(vcov(corsi1, type="HC1"))^0.5
corsi1_SE

#kfold cross validation
corsi1_k_fold <- trainControl(method="cv", number=5)
corsi1_k_model <- train(corsiPercentage~`Cap Hit`+ playoff_indicator, data=joint_data_set, method="lm",trControl = corsi1_k_fold)

corsi1_k_model

#test of significance, make sure parameters are normally distributed
#joint significance
#HO: Cap Hit = playoff_indicator = 0
#HA: At least one of Cap Hit and playoff_indicator != 0
#if one is not zero then can reject null hyp. Meaning one of these predictors is statistically significant
#if all are zero then you fail to reject that null
#larger F statistic is more support to reject null

#this one we would fail to reject the null as our p value us above .05 level of significance and we have a low f-stat

#individual significance
#two tailed
#HO: B1 = B1 = 0
#HA: B1 != B1 != 0
#example for write up
#test for all variables
#explore. Given this info theres a posisbility B1 'Cap Space' could be negatively associated, therefor I did an individual
#test of significance in my regression model. (larger than zero = right tail, < 0 is left tail..doesnt have to be 0) The results are... interpret
#p value for B1 must be less than significance level to reject the null
#p value is the  Pr(>|t)
#fail to reject Cap hit and playoff indicator because p value greater than .05 significance level

#model2 is the same but also using xGoalsPercentage
#looking at how much a team spends on their fourth line and how they perform relates to their fourth line corsi%
corsi2 <- lm(corsiPercentage~`Cap Hit`+ playoff_indicator + xGoalsPercentage, data=joint_data_set)
summary(corsi2)

#quantify the residuals
corsi2_residuals<-resid(corsi2)

#residual plot
plot(corsi2_residuals~joint_data_set$`Cap Hit`, xlab="Cap Hit", ylab="e")

#look at the slope and intercept
vcov(corsi2, type="HC1")

#get standard errors
corsi2_SE<-diag(vcov(corsi2, type="HC1"))^0.5
corsi2_SE


#if variability is small the assumptions of OLS are being met, rarely happens
#this one is U shaped
#may need to do something to regression set to remedy or employ non linear regression
plot(corsi2_residuals~T,xlab="Cap Hit",ylab="e")
abline(h=0)

#model 2 is much more superior, smaller SE and larger R2 use adjusted R2 because we have a different number of predictor variables

#cross validation k fold
#the model with the smallest RMSE will be preferred

corsi2_k_fold <- trainControl(method="cv", number=5)
corsi2_k_model <- train(corsiPercentage~`Cap Hit`+ playoff_indicator + xGoalsPercentage, data=joint_data_set, method="lm",trControl = corsi2_k_fold)

corsi2_k_model
#test of significance, make sure parameters are normally distributed
#joint
#HO: Cap Hit = playoff_indicator = XG% = 0
#HA: At least one of Cap Hit and playoff_indicator and XG% != 0
#reject null because of how low the p value is and because we have a high F-stat. Meaning at least one of those predictors
#is statistically significant at the .05 significance level I specified
#At the .05 percent level I specified for my level of significance, given that information with all four of the predictor variables
#in this model together they are jointly statistically significant in explaining corsi percentage. However we do not know if it is
#one, all, or a combination of all the predictor variables

#individual significance
#fail to reject null Cap Hit and playoff_indicator because their p values are greater than sig level .05,
#reject null hypothesis for xgf% because its p value is less than sig level .05


#interaction
#indicator variables
#to show the type of impact the combonation of two variables has
#one indicator and one numeric
#having a cap of xyz and making the playoffs has a positive impact for example. "the partial effect x on y hat" is b1 when b2 is indicator and is 0
#when indicator is one the effect on yhat is b1 plus b3
#two numerics
#the partial effect on x1 on yhat is b1+b3*x2 this depends on x2
#the partual effect on x2 on yhat is  b2+b3*x1 this depends on x1


#model3
#looking at how much a team spends on their fourth line and how they perform relates to their fourth line corsi% wiht an
#interaction term of playoff indicator and xGoalsPercentage. One is an indicator and one is numeric
corsi3 <- lm(corsiPercentage~`Cap Hit`+ playoff_indicator + xGoalsPercentage + (xGoalsPercentage * playoff_indicator), data=joint_data_set)
summary(corsi3)

#quantify the residuals
corsi3_residuals<-resid(corsi3)

##residual plot
plot(corsi3_residuals~joint_data_set$`Cap Hit`, xlab="Cap Hit", ylab="e")

#look at the slope and intercept
vcov(corsi3, type="HC1")

#get standard errors
corsi3_SE<-diag(vcov(corsi3, type="HC1"))^0.5
corsi3_SE

corsi3_k_fold <- trainControl(method="cv", number=5)
corsi3_k_model <- train(corsiPercentage~`Cap Hit`+ playoff_indicator + xGoalsPercentage + (xGoalsPercentage * playoff_indicator), data=joint_data_set, method="lm",trControl = corsi3_k_fold)

corsi3_k_model

#using logistic regression... Using it because then our phat probability cant be below 0 or above 1. Data is not very non linear so logistic is more applicable
#AKA binary prediction
#playoff predictinon model
#we want to try to predict making playoffs or not because making the playoffs yields a ton of money for the org
#and then you have chance at making more by making it farther
#outcome(phat)
#success y =1
#fail y =0
##########
#model1
#predicts whether you will make the playoffs or not with your cap hit as a predictor variable
playoff_log_model_1 <- glm(playoff_indicator~`Cap Hit`, family = binomial(link = logit),data = joint_data_set)
summary(playoff_log_model_1)

#predicts the probabilities for the given samples in our log model
playoff_log1_pred <- predict(playoff_log_model_1, type= "response")
#confirms it is producing values we would expect it to
summary(playoff_log1_pred)

#round it to zero or 1
playoff_log1_binary <- round(playoff_log1_pred)

#this is the propportion of all values that we accurately classified as 1, orrrr the team did make teh playoffs
100*mean(joint_data_set$playoff_indicator == playoff_log1_binary)

#kfold cross validation
joint_data_set$playoff_indicator_factored <- as.factor(joint_data_set$playoff_indicator)

pi1_k_fold <- trainControl(method = "cv", number = 5)
pi1_k_model <- train(playoff_indicator_factored~`Cap Hit`, data = joint_data_set, trControl = pi1_k_fold, method = "glm",
                     family = binomial(link = logit),
                     metric = "Accuracy")
pi1_k_model

#Joint significance test
#all must be equally distributed
qqnorm(joint_data_set$`Cap Hit`)
qqnorm(joint_data_set$corsiPercentage)
#both look fairly normal

#individual significance test
#HO: Cap Hit = 0
#HA: Cap Hit != 0
#we fail to reject the null hypothesis here because our p value of .511 is greater than our alpha of .05. Cap hit is not statistically
#significant in explaining playoff indication

##########
#model2
#predicts if a team will make the playoffs and now adds corsiPercentage as a predictor
playoff_log_model_2 <- glm(playoff_indicator~`Cap Hit` + corsiPercentage, family = binomial(link = logit),data = joint_data_set)
summary(playoff_log_model_2)

#predicts the probabilities for the given samples in our log model
playoff_log2_pred <- predict(playoff_log_model_2, type= "response")
#confusion matrix, type 1 and type 2 error, I have three teams which I predicted to make the playoffs
#that wont

#confirms it is producing values we would expect it to
summary(playoff_log2_pred)

#round it to zero or 1
playoff_log2_binary <- round(playoff_log2_pred)

#accuracy
100*mean(joint_data_set$playoff_indicator == playoff_log2_binary)
#model 2 is more superior due to a higher accuray of 58%

#for cross validation use accuracy rate since this is a binary predictor

#kfold cross validation
pi2_k_fold <- trainControl(method = "cv", number = 5)
pi2_k_model <- train(playoff_indicator_factored~`Cap Hit` + corsiPercentage, data = joint_data_set, trControl = pi2_k_fold, method = "glm",
                     family = binomial(link = logit),
                     metric = "Accuracy")
pi2_k_model

#joint significance
#H0: Cap hit = corsi % = 0
#HA: at least one of Bj != 0
#since our p values for Cap hit and corsi percentage are both above our significance level of .05 we fail to reject our null
#hypothesis which suggests that cap hit and corsi percentage are not jointly significant in explaining the playoff indication

###########
#model3
#predicts playoff likelihood and now shows interaction through the two numeric variables corsi and caphit
playoff_log_model_3 <- glm(playoff_indicator~`Cap Hit` + corsiPercentage + (`Cap Hit` * corsiPercentage), family = binomial(link = logit),data = joint_data_set)
summary(playoff_log_model_3)

#predicts the probabilities for the given samples in our log model
playoff_log3_pred <- predict(playoff_log_model_3, type= "response")

#confirms it is producing values we would expect it to
summary(playoff_log3_pred)

#round it to zero or 1
playoff_log3_binary <- round(playoff_log3_pred)

#accuracy
100*mean(joint_data_set$playoff_indicator == playoff_log3_binary)

#kfold
pi3_k_fold <- trainControl(method = "cv", number = 5)
pi3_k_model <- train(playoff_indicator_factored~`Cap Hit` + corsiPercentage + (`Cap Hit` * corsiPercentage), 
                     data = joint_data_set, trControl = pi3_k_fold, method = "glm",
                     family = binomial(link = logit),
                     metric = "Accuracy")
pi3_k_model

#joint significance
#H0: Cap hit = corsi % = = Cap hit $ corsi% = 0
#HA: at least one of Bj != 0
#since our p values for Cap hit, corsi percentage and cap hit * corsipercentage are all above our significance level of .05 we fail to reject our null
#hypothesis which suggests that cap hit, corsi percentage and them as indicator variables are not jointly significant in explaining the playoff indication

#########
#analysis on our best model
(-9.309) + (1.551 * 4.5) + (18.061 * 0.52) + (-2.996 * (4.5 * 0.52))
#5% chance of making the playoffs

(-9.309) + (1.551 * 3) + (18.061 * 0.56) + (-2.996 * (3 * 0.56))
#36% chance of mkaing the playoffs. It is apparent that even a small jump in corsi percentage for the 4th line
#yields a lot higher chance of making the playoffs

(-9.309) + (1.551 * 2) + (18.061 * 0.58) + (-2.996 * (2 * 0.58))
#79% of making the playoffs

(-9.309) + (1.551 * 3.75) + (18.061 * 0.58) + (-2.996 * (3.75 * 0.58))
#46% chance of making the playoffs

###########################################################
#probability
#P(x < AVG) = P(z < 4.5 - average_cap_hit/cap_hit_sd) = Z < x
(4.5 - average_cap_hit) / cap_hit_sd
# = Z < -.2815 = .3897
#There is roughly a 39 percent chance that a selected team will have a cap hit below 4.5 million.
#N > 30, can invoke CLT

#what is the probablity that one randomly selected teams has a 4th line cap hit below the 4.5 million
#P(X < 4.5) = P(z < 4.5 - average_cap_hit/cap_hit_sd) = Z < X
(4.5 - average_cap_hit) / cap_hit_sd
# = Z < -.2815 = .3897
#There is roughly a 39 percent chance that a selected team will have a cap hit below 4.5 million.
#N > 30, can invoke CLT

#We know that if you have a corsi percentage of .58(pretty good) and you have a cap hit between 2 and 3.75 million your team will have a quailty chance of makinf the playoffs
#what is the probability that one randomly selected team has a 4th line cap hit between 2 and 3.75 million
#P(X < AVG) = P(z < 3.75 - average_cap_hit/cap_hit_sd) = Z < x
(3.75 - average_cap_hit) / cap_hit_sd
# = z < -.547 = .2966
#There is roughly a 30% chance of having a 4th line cap hit less than 3.75 million
#P(X < 2) = P(z < 2  - average_cap_hit/cap_hit_sd) = Z < X
(2 - average_cap_hit) / cap_hit_sd
# = Z < -1.67 = .0475
#there is about a 5% chance of having a 4th line cap hit below 2 million
# = .2966 - .0475 = .2491
#there is about a 25% chance of having a cap hit between 2 and 3.75 million
#should you have a line with a quality corsi % of say 58%... having a fourth line cap hit in this range should yield you a good chance of making the playoffs
#Because my sample is 31 we can invoke the CLT, meaning that my mean is approx normally distributed

#we have found that the avaerage fourth line corsi is approx 50%
#In a random sample of 31 teams what is the probability that the sample proportion is greater than 51%
#probability the sample proportion is greater tahn 51%
#P(Pbar >= .51) = P(Z >= .51 - average_corsi / sqrt(average_corsi(1 - average_corsi)
(.51 - averagee_corsi) /  (sqrt((averagee_corsi * (1 - averagee_corsi))/ 31))
#= P(z >= .082) = 1 - .5319 = .4681
#Theres a 47 percent chance that your 4th line has a corsi percentage just above the league average

#What is the probability that a randomly selected 4th line from only playoff teams has a cap hit less than 5 million
average_playoff_cap_hit <- mean(Playoff_Teams$`Cap Hit`)
playoff_cap_hit_sd <- sd(Playoff_Teams$`Cap Hit`)
#P(X < 5) = P(z < 5 - average_playoff_cap_hit/playoff_cap_hit_sd) = Z < X
(5 - average_playoff_cap_hit) / playoff_cap_hit_sd
# = Z < -.1858 = .42
#there is about a 42 percent chance that a randomly selected playoff team has a 4th line cap hit below 5 million
#no CLT

##########################################################
#Hypothesis testing

#going to start with a two tailed test because I dont know if those odds will be better or worse depending on if you spend more or less than the mean
#looks like we have an outlier at the top and bottom, fairly normal

#this is the max cap hit from our regression inputs above
t.test(x=joint_data_set$`Cap Hit`, mu= 3.75)

#Our analytics team is curious if playoff teams are spending a significant amount different on their 4th
#line than the non playoff teams

#Null hypothesis: Playoff teams are spending the same amount on their fourth line compared to non playoff teams
#Alternative hypothesis: Playoff teams are spending a different amount on their fourth line compared to non playoff teams

# 95% significance level
#H0: (mean Playoff $) = (mean Non playoff $)
#HA: (mean Playoff $) != (mean Non playoff $)
library(Hmisc)
Non_Playoff_Teams <- joint_data_set[joint_data_set$Playoff_Result==0,]

Non_playoff_mean_CH <- mean(Non_Playoff_Teams$`Cap Hit`)
Non_playoff_mean_sd <- sd(Non_Playoff_Teams$`Cap Hit`)

t.test(Non_Playoff_Teams$`Cap Hit`,Playoff_Teams$`Cap Hit`)
#fail to reject null, pvalue of .52 is greater than our alpha of .05

