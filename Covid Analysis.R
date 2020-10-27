rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import 

enteries <- read.csv("C:/Users/Impana/Downloads/COVID19_line_list_data (1).csv")
describe(enteries) # Hmisc command

# cleaned up death column
enteries$death_dummy <- as.integer(enteries$death != 0)
# death rate
sum(enteries$death_dummy) / nrow(enteries)

# AGE
# claim: people who die are older
dead = subset(enteries, death_dummy == 1)
survived = subset(enteries, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(survived$age, na.rm = TRUE)
# is this statistically significant?
t.test(survived$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(enteries, gender == "male")
women = subset(enteries, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant