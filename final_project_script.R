## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
#QUANT_VAR1
#Calculate views per video
table(data$Views)
mean(data$Views)
sd(data$Views)
summary(data$Views)

#QUANT_VAR2
#Calculate likes per video
table(data$Likes)
mean(data$Likes)
sd(data$Likes)
summary(data$Likes)

#QUAL_VAR1
#Primary age demographic per video
table(data$Age)

#QUAL_VAR2
#Gender breakdown per video
table(data$Gender)

#QUAL_VAR3
#Average numbers of views per like
table(data$Views_per_Like)
mean(data$Views_per_Like)
sd(data$Views_per_Like)
summary(data$Views_per_Like)

#QUANT_VAR4
#Average subscriber gain per video
table(data$Subscribers)
mean(data$Subscribers)
sd(data$Subscribers)
summary(data$Subscribers)

##################################################################################
############### STEP 1: Table 2    ####################   
##################################################################################

#Breaks down the amount of videos that fit in each subsection
#I.E. Type of video, Main age demographic, Gender breakdown
table(data$Type)
table(data$Age)
table(data$Gender)

##videos + age range##
table(data$Type, data$Age)


##video type + gender dispersion##
table(data$Type, data$Gender)
##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

#Can not reject the Null Hypothesis
#Not enough to confirm it either
#Type of video does NOT have an impact on attracting certain audience
chisq.test(table(data$Type, data$Age))

#Can not reject the Null
#Can not confirm it either
#Type of video does NOT have an impact on attracting certain gender demographic
chisq.test(table(data$Type, data$Gender))
##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Views ~ Type, data)

summary(anova_adapted)

##type of video and total views do NOT correlate##
##2.049e7 / 2.049e7 + 2.550e9##
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

#Strong relationship between views and likes
cor(data$Views, data$Likes)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################

#Slope is .02 likes for each new view
#Small P Value indicates a strong predictor 
#48.12% of like variability can be explained by views
#Good F statistic low standard error = high confidence
linear_relationship <- lm(data$Likes ~ data$Views)
summary(linear_relationship)


##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
#slopes#
plot(data$Views, data$Likes)

abline(linear_relationship, col = "red")
#establish y mean#
abline(h=mean(data$Likes),col = "red")
abline(h=y_mean)

#establish x mean#
abline(v=mean(data$Views),col = "red")
abline(v=x_mean)

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Type, residuals(linear_relationship))