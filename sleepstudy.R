# Loading the library

library(lme4)
library(tidyverse)

#load the sleepstudy dataset

data(sleepstudy)

#check dataset's description

?sleepstudy

#explore the structure of the data

str(sleepstudy)

#Reaction and Days are numerical and subject is a factor

head(sleepstudy)

view(sleepstudy)

(unique(sleepstudy$Subject))

length(unique(sleepstudy$Subject))

#There are 18 unique subjects in the experiment

#There are 10 observations per subject

# Histogram of Reaction times
hist(sleepstudy$Reaction, breaks = 18, 
     main = "Distribution of Reaction Times", xlab = "Reaction Time")

#Boxplot of Reaction times per Subject

ggplot(aes(x=Subject, y = Reaction),data= sleepstudy) + 
  geom_boxplot()+
  labs(title= "Graph of Rection times per subject")

#2
summary(sleepstudy)

summary(sleepstudy$Reaction)
#The minimum average reaction time per subject is 194.3 ms
#The maximum average reaction time per subject is 466.4 ms
#The mean average reaction time per subject is 298.5



ggplot(aes(y= Reaction, x= Days), data= sleepstudy) +geom_point()+
  labs(title = "Reaction time of participants per day")
#From the plot we can see that each subject have different baseline
#reaction time

ggplot(sleepstudy, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill ='Magenta', color = "darkblue") +
  labs(title = "Reaction Times Distribution by Days",
       x = "Days", y = "Reaction Time") +
  theme_minimal()
#The plot does not seem to follow a specific pattern but
#in most cases reaction time increase from one day to the other 
#while in some other cases reaction time reduces


require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")

#From this plot we see that generally for most subjects, 
#the increase in sleep deprivation increases their reaction time.
# However for subject 335, sleep deprivation reduces his/her
#average reaction time

#3
#The observation for reaction time are not independent because 
#we have 10 reaction times for each person, hence we will use
#a mixed effect model

mdl <- lmer(Reaction ~ Days + (1|Subject), data= sleepstudy, subset=Days>=2)
summary(mdl)

#5 Residual Analysis

res <- residuals(mdl)
#Getting the residuals

par(mfrow =c(1,3))
#Setting the 3 plots on one row
hist(res)
#Ploting the histogram of the residuals

qqnorm(res)
qqline(res)
plot(fitted(mdl), res)


