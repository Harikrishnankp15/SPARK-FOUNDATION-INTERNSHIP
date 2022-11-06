#SPARK FOUNDATION TASK1 "PREDICTION USING SUPERVISED ML"
#OBJECTIVES:
#(a) PREDICT THE PERCENTAGE OF STUDENT BASED ON THE NUMBER OF sTUDY HOURS
#(b) WHAT WILL BE THE PREDICTED sCORE IF A STUDENT sTUDIEs FOR 9.5 HOURS


#LOADING THE DATA
task1 <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")


#ARRANGE THE DATA
library(tidyverse)
library(dplyr)
task1new <- arrange (task1,Hours)


#DaTA SUMMARY
summary (task1new)

#NUMBER OF ROWS AND COLOUMNS
nrow(task1new)
ncol(task1new)

#STRUCTURE OF THE DATA
str(task1new)

#CHECK IF MISSING VALUES
is.na("task1new")

#PLOTTING THE DATA USING SCATTERPLOT
Hour <- task1new$Hours
Score <- task1new$Score
plot(Score~Hour)
abline (lm(Score~Hour))

#PLOTTING THE DATA USING GGPOLT
library(ggplot2)
task1new |> ggplot(aes (x= Hour,y = Score)) + geom_point()+geom_smooth(method ="lm")


#BUILDING THE NEW MODEL
r <-lm(Score ~Hour)
summary (r)
#PREDICTING THE PERCENTAGE OF STUDENTS BASED ON THE NUMBER OF HOURS STUDIED
print(predict (r,task1new))


#PREDICTED SCORE IF THE STUDENT STUDIED FOR 9.25 HOURS
predict (r,data.frame(Hour = 9.25))
