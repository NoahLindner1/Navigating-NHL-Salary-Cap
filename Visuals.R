##Marquette University
##COSC 6510
##Noah Lindner
##noah.lindner@marquette.edu
##Visualizations

rm(list=ls())

library(tidyverse)
library(readxl)
library(ggplot2)

#contains the lines, their cap hits, and their teams season performance result
team_performance_data<-read_excel("Desktop/lines.xlsx",
                      sheet = "Team_Stats")
View(team_performance_data)

#contains the lines, their cap hits, and all of the stats for how the specific line performed
line_performance_data<-read_excel("Desktop/lines.xlsx",
                                            sheet = "Line_Stats")

View(line_performance_data)

#################################
#Data Management

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

#makes a variable for the percent of the cap each team has spent on their fourth line
joint_data_set$Proportion_Spent <- (joint_data_set$`Cap Hit` / joint_data_set$cap) * 100
joint_data_set$Proportion_Spent <- round(joint_data_set$Proportion_Spent,digit=1)

#Average 4th line cap hit for all 31 teams
Avg_line_Cap_hit <- mean(joint_data_set$`Cap Hit`)

#makes a frame of just the teams that did not make the playoffs
Non_Playoff_Teams <- joint_data_set[joint_data_set$Playoff_Result== 0,]
Non_Playoff_AVG <- mean(Non_Playoff_Teams$`Cap Hit`)
NP_AVG_Corsi <- mean(Non_Playoff_Teams$corsiPercentage)

#makes a frame of all the teams that made the playoffs
Playoff_Teams <- joint_data_set[joint_data_set$Playoff_Result!=0,]
Playoff_Teams_AVG <- mean(Playoff_Teams$`Cap Hit`)
P_AVG_Corsi <- mean(Playoff_Teams$corsiPercentage)
##MTL is a huge outlier greatly effecting this...consider removing them and trying again

#################################
#visualizations

#the following visual is going to be an exmaple to gaudge my audiece and show them why this
#is important
#I will be using data from this current season to strengthen my argument because this is
#the first season where the cap hasnt rose which further supports my analysis as to why
#how you spend on your fourth line matters

this_year_4th <- read_excel("Desktop/20214ths.xlsx",
                            sheet = "2021")
this_year_4th <- this_year_4th[-c(14:110)]


ggplot(data = this_year_4th, aes(x = team, y = corsiPercentage)) +
  geom_bar(stat = 'identity', fill = "#FF6666") +
  geom_text(aes(label = `Cap Hit`), position=position_dodge(width=0.9), vjust=-0.25) +
  ylim(0,.75) +
  ggtitle("2021 Two Team 4th line comparison")

#scatter plot for team points vs 4th line cost with an abline
plot(joint_data_set$points~joint_data_set$'Cap Hit',
     bty = "n",
     type = "n",
     main = "Team Points vs 4th Line Cost",
     xlab = "4th Line Cap hit (millions)",
     ylab = "Team Points",
     col = "black",
)
abline(joint_data_set$points,joint_data_set$'Cap Hit', col="red",lwd=3)
text(joint_data_set$points~joint_data_set$'Cap Hit', labels = joint_data_set$team, cex = .5)

#creating variables for Line Cap
`Line Cap` <- joint_data_set$`Cap Hit`
#add one to playoff result so the bars appear for the teams that didnt make the playoffs and had an inital value of 0
Playoff_Result <- joint_data_set$Playoff_Result + 1
team <- joint_data_set$team
frame <- data.frame(`Line Cap`,Playoff_Result,team)

#bar chart that shows how far each team made it in the playoffs with the color of each bar representing how 
#much they spent on their 4th line
#contains a red baseline. Everything under it is teams that didnt make the playofss and everythign above is teams that did
ggplot(frame,aes(fill=`Line Cap`, y=Playoff_Result,x=team)) +
  geom_bar(position="stack",stat="identity")+
  labs(y="Post Sesaon Result", x="Team",title = "Playoff Result with 4th Line Cap Hit")+
  geom_abline(intercept= 1, slope=0, col = "red",lty=2)

#scatterplot of Line Cap hit with that lines corsi
plot(joint_data_set$`Cap Hit`~joint_data_set$corsiPercentage,
     main = "Line Corsi vs Line Cost",
     xlab = "Corsi Percentage",
     ylab = "Cap Hit",
     col = "black"
     )

#bar chart for cap hit with cap..Might just delete. Doesnt derive super useful info
ggplot(data = joint_data_set, aes(x = team, y = cap, fill = `Cap Hit`)) +
  geom_bar(stat = "identity")

#bar chart for corsiPercentage 
ggplot(data = Playoff_Teams, aes(x = team, y = corsiPercentage, fill = `Cap Hit`)) +
  geom_bar(stat = "identity") +
  geom_abline(intercept = P_AVG_Corsi, slope = 0, col = "red", lty = 2) +
  geom_text(aes(label = `Cap Hit`), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis_b() +
  ggtitle("Playoff Teams 4th Line Cap Hit and Corsi %")

#making variables to analyze the means and standard deviations 
playoff_sd <- sd(Playoff_Teams$`Cap Hit`)
non_playoff_sd <- sd(Non_Playoff_Teams$`Cap Hit`)
playoff_mean <- mean(Playoff_Teams$`Cap Hit`)
non_playoff_mean <- mean(Non_Playoff_Teams$`Cap Hit`)
playoff_mean_corsi <- mean(Playoff_Teams$corsiPercentage)
playoff_sd_corsi <- sd(Playoff_Teams$corsiPercentage)
nplayoff_mean_corsi <- mean(Non_Playoff_Teams$corsiPercentage)
nplayoff_sd_corsi <- sd(Non_Playoff_Teams$corsiPercentage)

#checking to see if the Montreal Canadiens are an outlier with their outlandish cap hit
boxplot(joint_data_set$`Cap Hit`)
boxplot.stats(Playoff_Teams$`Cap Hit`)$out

#visually appealing box plot to show that their is no outliers
ggplot(joint_data_set) +
  aes(x = "", y = `Cap Hit`) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  ggtitle("NHL Teams 4th Line Cap Hit Boxplot")

#Same bar chart with corsi, team, and cap hit, but for non playoff teams
ggplot(data = Non_Playoff_Teams, aes(x = team, y = corsiPercentage, fill = `Cap Hit`,)) +
  geom_bar(stat = "identity") +
  geom_abline(intercept = NP_AVG_Corsi, slope = 0, col = "red", lty = 2) +
  geom_text(aes(label = `Cap Hit`), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis_b() +
  ggtitle("Non-Playoff Teams 4th Line Cap Hit and Corsi %")

#seeing if their is a any sort of linear or non linear pattern in this scatter plot of cap hit and wins
plot(joint_data_set$wins~joint_data_set$`Cap Hit`)
