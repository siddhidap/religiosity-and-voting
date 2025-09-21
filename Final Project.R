# Student Name: Siddhida Pandya, Haven Biddix, Saurish Reddy
# Section (12:30pm or 3:30pm): 12:30
# POLI281, Fall 2023, Final Project

rm(list=ls()) 

setwd("/Users/siddhidapandya/Desktop/POLI 281/")
getwd()
anes_FTF_2016 <- read.csv("anes_FTF_2016.csv")
library(ggplot2)
library(dplyr)

anes <- data.frame(anes_FTF_2016$V162034a)
names(anes)[1] <- "trump_votes"
anes$religion <- anes_FTF_2016$V161244
# To begin the analysis, we created a new dataset called anes that will house
# only the variables we will need for the analysis. Therefore, we created a dataframe
# with just the trump_votes variable and the religion variable that we recoded from 
# V162034a and V161244 to trump_votes and religion, respectively. We then added them to the 
# dataframe anes to create a smaller, more useful dataframe. 

######## III. Prep work: the dependent variable ########

## Did you vote for trump? 1 is YES, 0 is NO.
anes$trump_votes[anes$trump_votes < 0]  <- NA
anes$trump_votes[anes$trump_votes == 1] <- 0
anes$trump_votes[anes$trump_votes == 3]  <- 0
anes$trump_votes[anes$trump_votes == 4]  <- 0
anes$trump_votes[anes$trump_votes == 5]  <- 0
anes$trump_votes[anes$trump_votes == 7]  <- 0
anes$trump_votes[anes$trump_votes == 9]  <- 0
anes$trump_votes[anes$trump_votes == 2] <- 1
# In order to create a simpler analysis of the dependent variable, we decided to 
# create a binary variable. 1 being the person did vote for Trump and 0 being that they
# did not vote for trump. Thus, we recoded all of the other candidates in the dataset
# to be 0, whereas Trump was 1. we then recoded the missing data as NA by making 
# any number under 0 a NA variable. 

votes_plot <- ggplot(anes, aes(x = trump_votes)) + geom_histogram(bins = 10)
# Then we created a histogram that shows the distribution of the dependent variable
# in the dataset. Essentially, we coded to show how many individuals voted for Trump (1)
# and how many voted for other candidates (0). 

votes_plot <- votes_plot + theme_bw() +
  ggtitle("Trump Votes") +
  xlab("Votes") +
  ylab("Count") +
  theme(title = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 1, by = 1))
# Then, we made the histogram more pretty to create a more readable analysis. So, 
# we created a title called Trump Votes, named the x-axis Votes, the y-axis Count, 
# changed the size of the title, and numbered the x axis to go from 0 to 1. 

######## IV. Prep work: the independent variable ########


anes$religion[anes$religion < 0] <- NA
# Then, I created the bare bones of the independent variable which is religiosity. 
# In order to just produce a chart of the unedited distribution of the religion variable
# I coded out the numbers that are less than 0, which is the irrelevant data to be NA.
# So, we coded out the values that are "Don't know" and "Refused".

religion_plot <- ggplot(anes, aes(x = religion)) + geom_histogram(bins = 10)
religion_plot <- religion_plot + theme_bw() +
  ggtitle("Religious Voters") +
  xlab("Religious?") +
  ylab("Count") +
  theme(title = element_text(size = 10)) 
# These few lines of codes creates the histogram that shows the distribution of the independent variable
# without recoding the variables as binary. Thus, it shows an uneven distribution of variables
# that take place in an undecipherable number that is difficult to interpret. It is quite hard to read and interpret which is why
# it would be best to recode this as a binary variable. 

## Are you religious? 1 is YES, 0 is NO.
anes$religion[anes$religion == 1] <- 1
anes$religion[anes$religion == 2] <- 0
# Thus, we made those that participated in religious events labelled as religious, or 1
# in this variable. Those that did not participate in religious activity we recoded as 0,
# or as no, they are not religious. 

updated_religion_plot <- ggplot(anes, aes(x = religion)) + geom_histogram(bins = 10)
updated_religion_plot <- updated_religion_plot + theme_bw() +
  ggtitle("Religious Voters") +
  xlab("Religious?") +
  ylab("Count") +
  theme(title = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 1, by = 1))
# Thus, after we recoded this as binary and created the differentiation between yes they are religious
# and no they are not religious variables, the distribution of the independent variable is much more
# easy to interpret. We see the 1 and the 0 very clearly and can very clearly also see the count.

######## V. Initial analysis: data visualization ########
grouped_anes <- anes %>% group_by(religion) %>% summarize(avg_trump_votes = mean(trump_votes, na.rm = TRUE)) 
# In order to create a data visualization that shows the relationship between religiosity and Trump votes,
# we grouped anes by religion, and used the summarize function to take the averagae trump votes for 
# each religious option. 

religion_trump_votes_plot <- ggplot(grouped_anes, aes(religion, avg_trump_votes)) + geom_col()
religion_trump_votes_plot <- religion_trump_votes_plot + theme_bw() +
  ggtitle("Relationship between Religiosity and Average Trump Votes") +
  xlab("Religion") +
  ylab("Average Trump Votes") +
  theme(title = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 1, by = 1))
# Then, I created the plot using ggpplot for the grouped_anes variable that we created. We did this
# by creating a bar chart that shows religiosity on the x axis and the average trump votes
# on the y axis. We created a black and white theme and made the two numbers on the x axis only 
# 0 and 1 which shows the religious and non religious variables. 


######## VI. Initial regression analysis########
lm(trump_votes ~ religion, anes)
fit1 <- lm(trump_votes ~ religion, anes)
fit1
summary(fit1)
nobs(fit1)
# Then using the lm() function, we created a linear regression model of religion and trump votes.
# We stored this model in fit 1 and created a summary of what it shows using the summary function
# which will print out the intercept, religion slope, R-squared, and the nobs() function
# which will print out the number of observations. 

######## VIII: Prep work: confounders ########

anes$gender <- anes_FTF_2016$V161002
anes$gender[anes$gender == 2] <- 0
# This is gender: recoding female to be 0, since male is 1 that will create a binary variable. 

anes$age <- anes_FTF_2016$V161267
anes$age[anes$age < 0] <- NA
summary(anes$age)
anes$age <- (anes$age - 18) / (90 - 18)
# This is age rescaled from 0 to 1. 
# We stored age in anes, made the numbers under 0 NA, and then rescaled using the minimum and max age.

anes$economic <- anes_FTF_2016$V161140
anes$economic[anes$economic < 0] <- NA
anes$economic[anes$economic == 2] <- NA
anes$economic[anes$economic == 3] <- 0
# This is the economic opinion question recoded to show whether one thinks it has gotten worse or better
# It was recoded to a binary variable, and the "stays the same" option was coded out to make it more useful. 
# If you think the economy has gotten better, you are 1, and if you think it has gotten worse, you are 1.  

anes$edu <- anes_FTF_2016$V161270
anes$edu[anes$edu > 16] <- NA
anes$edu[anes$edu < 0] <- NA
anes$edu <- (anes$edu - min(anes$edu[!is.na(anes$edu)])) / (max(anes$edu[!is.na(anes$edu)]) - min(anes$edu[!is.na(anes$edu)]))
# This is the levels of education recoded to be a scale from 0-1. 
# I eliminated any education level above 16 and below 0 because they are not applicable to the data. 
# I rescaled using the min and max level of education in the dataset after eliminating NA.

summary(anes$gender)
summary(anes$age)
summary(anes$economic)
summary(anes$edu)
# This is the summary statistic for all of the new variables I created
# The summary function finds the min, median, max that we need.

(cor(anes$religion, anes$gender, use = "complete.obs"))
(cor(anes$religion, anes$age, use = "complete.obs"))
(cor(anes$religion, anes$economic, use = "complete.obs"))
(cor(anes$religion, anes$edu, use = "complete.obs"))
# This finds the correlation between my independent variable and then the other newly created independent variables

(cor(anes$trump_votes, anes$gender, use = "complete.obs"))
(cor(anes$trump_votes, anes$age, use = "complete.obs"))
(cor(anes$trump_votes, anes$economic, use = "complete.obs"))
(cor(anes$trump_votes, anes$edu, use = "complete.obs"))
# This finds the correlation between the dependent variable and my newly created independent variables. 

######## IX. Multivariate regression  ########
lm(trump_votes ~ religion, anes)
fit1 <- lm(trump_votes ~ religion, anes)
fit1
summary(fit1)
# This is reproducing the bivariate regression model between our independent variable
# and our dependent variable.

lm(trump_votes ~ religion + economic, anes)
fit2 <- lm(trump_votes ~ religion + economic, anes)
fit2
summary(fit2)
nobs(fit2)
# This finds the multivariable regression model between religion (our independent variable), economic (the variable that
# seems most likely to be a confounder based on the strong correlation), and our dependent variable.

lm(trump_votes ~ religion + economic + gender + age + edu, anes)
fit3 <- lm(trump_votes ~ religion + economic + gender + age + edu, anes)
fit3
summary(fit3)
nobs(fit3)
# This is the final multivariable regression model that finds the relationship between our independent variable
# (religion), and the four other potential confounding variables (economic, gender, age, edu), and our dependent 
# variable (trump_votes).





