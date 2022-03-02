d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
library(ggplot2)

hist(d2$Walc, main="Distribution of walc", xlab="walc")    #The Histogram Reprents that the data is a positively skewed data.
plot(d2$Walc,d2$age, main="Scatterplot Example")
fit0 <- lm(d2$Walc ~ d2$goout)
summary(fit0)
plot(d2$goout, d2$Walc, xlab = "Goout", ylab = "Walc", type = "n")
points(d2$goout[d2$sex == "M"], d2$Walc[d2$sex == "M"], col = "red")
points(d2$goout[d2$sex == "F"], d2$Walc[d2$sex == "F"], col = "blue")
abline(0.93, 0.425, col = "green")   # The Regression line represents the data of all students, which has a Positive Corelation between Goout and alcohol consumption.
fit <- lm(d2$Walc ~ d2$sex*d2$goout)
summary(fit)
abline(1.2, 0.23, col = "Blue")   # The Regression line represents the data of female students, which has a Positive Corelation of 0.23 between Goout and alcohol consumption.
abline(1.2-0.51, 0.23+0.40, col ="red") # The Regression line represents the data of male students, which has a Positive Corelation of 0.63 between Goout and alcohol consumption.

fit00 <- lm(d2$Walc ~ d2$studytime)
summary(fit00)
plot(d2$studytime, d2$Walc, xlab = "Studytime", ylab = "Walc", type = "n")
points(d2$studytime[d2$sex == "M"], d2$Walc[d2$sex == "M"], col = "red")
points(d2$studytime[d2$sex == "F"], d2$Walc[d2$sex == "F"], col = "blue")
abline(2.92, -0.333, col ="green") # The Regression line represents the data of all students, which has a Negative Corelation of -0.333 between Studytime and alcohol consumption. 
fit1 <- lm(d2$Walc ~ d2$sex+d2$studytime)
summary(fit1)
abline(2.44, -0.24, col = "blue" ) # The Regression line represents the data of Female students, which has a Negative Corelation between Studytime and alcohol consumption.
abline(2.44+0.75, -0.24, col = "red" )  # The Regression line represents the data of Male students, which has a Negative Corelation between Studytime and alcohol consumption.

##To Find the interactions between Sex,Goout, Studytime with Walc
fit2 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$studytime+d2$sex:d2$goout+d2$sex:d2$studytime+d2$goout:d2$studytime)
summary(fit2)
fit3 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$studytime+d2$sex:d2$goout+d2$sex:d2$studytime)
summary(fit3)
fit4 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$studytime+d2$sex:d2$goout)
summary(fit4)
p <- 
  ggplot(d2, aes(x = d2$goout, y = d2$Walc)) + 
  geom_point(aes(col = d2$studytime), size = 4) + 
  geom_abline(intercept = 1.71+0.22, slope = -0.22, col = "blue") +
  geom_abline(intercept = 1.71-0.64+0.22, slope = -0.22+0.42, col = "red") +
  xlab("Goout")+
  ylab("Walc")
p
##The 3D plot has a different value of correlation for males than females because it has an interaction with Goout as observed in summary of Fit4.


#To find Interactions between G3, Goout, Studytime, Sex
fit5 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$studytime+d2$G3+d2$sex:d2$goout+d2$sex:d2$studytime+d2$sex:d2$G3+d2$goout:d2$studytime+d2$goout:d2$G3+d2$studytime:d2$G3)
summary(fit5)
fit6 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$G3+d2$sex:d2$goout+d2$sex:d2$studytime+d2$sex:d2$G3+d2$goout:d2$studytime+d2$goout:d2$G3+d2$studytime:d2$G3)
summary(fit6)
fit7 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$G3+d2$sex:d2$goout+d2$sex:d2$G3+d2$goout:d2$studytime+d2$goout:d2$G3+d2$studytime:d2$G3)
summary(fit7)
fit8 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$G3+d2$sex:d2$goout+d2$sex:d2$G3+d2$goout:d2$G3+d2$studytime:d2$G3)
summary(fit8)
fit9 <- lm(d2$Walc ~ d2$sex+d2$goout+d2$G3+d2$sex:d2$goout+d2$sex:d2$G3+d2$studytime:d2$G3)
summary(fit9)
fit10 <- lm(d2$Walc ~ d2$goout+d2$G3+d2$sex:d2$goout+d2$sex:d2$G3+d2$studytime:d2$G3)
summary(fit10)
fit11 <- lm(d2$Walc ~ d2$goout+d2$sex:d2$goout+d2$sex:d2$G3+d2$studytime:d2$G3)
summary(fit11)
fit12 <- lm(d2$Walc ~ d2$goout+d2$sex:d2$goout+d2$studytime:d2$G3)
summary(fit12)
q <-
  ggplot(d2, aes(x = d2$goout, y = d2$Walc)) +
  geom_point(aes(col = d2$G3), size = 4) +
  geom_abline(intercept = 1.38+0.28, slope = -0.0142, col = "blue") +
  geom_abline(intercept = 1.38+0.24+0.28, slope = -0.0142, col = "red") +
  xlab("Gout") +
  ylab("Walc")
q
  
##Pearson's R Correlation of Data
cor.test(d2$goout, d2$Walc, method = c("pearson"), conf.level=0.95)  #There is a Positive Correlation of 0.389 
cor.test(d2$studytime, d2$Walc, method = c("pearson"), conf.level=0.95)  #There is a Negative Correlation of 0.215

##To Find Factors Interacting  
cor.test(d2$health, d2$Walc, method = c("pearson"), conf.level=0.95) #There is a weak positive correlation of 0.11

cor.test(d2$absences, d2$Walc, method = c("pearson"), conf.level=0.95)  #There is a weak positive correlation of 0.16
fit13 <- lm(d2$Walc ~ d2$absences)
summary(fit13)
plot(d2$absences, d2$Walc, xlab = "Absences", ylab = "Walc")   #The Plot represents the above Correlation
abline(2.122, 0.04, col = "green")

cor.test(d2$G3, d2$Walc, method = c("pearson"), conf.level=0.95) #There is a negative correlation of 0.18
fit14 <- lm(d2$Walc ~ d2$G3)
summary(fit14)
plot(d2$G3, d2$Walc, xlab = "Final Grades", ylab = "Walc")   #The Plot represents the above Correlation in which we see that alcohol consumption is effecting the Final Grades of Students in a inverse proportion. 
abline(3.11643, -0.07, col = "green") 
  
cor.test(d2$G3, d2$studytime, method = c("pearson"), conf.level=0.95)  #There is a positive correlation of 0.25
fit15 <- lm(d2$studytime ~ d2$G3)
summary(fit15)
plot(d2$G3, d2$studytime, xlab = "Final Grades", ylab = "Studytime")    #The Plot represents the above Correlation
abline(1.167, 0.062, col = "green")

cor.test(d2$freetime, d2$studytime, method = c("pearson"), conf.level=0.95)  #There is a weakest negative correlation of 0.06 which means studytime doesn't effect final grades 
fit16 <- lm(d2$studytime ~ d2$freetime)
summary(fit16)
plot(d2$freetime, d2$studytime, xlab = "Final Grades", ylab = "Studytime")     #The Plot represents the above Correlation
abline(2.103, -0.054, col = "green")

##Statistical Tests of Data
t.test(d2$Walc, d2$Dalc)
t.test(d2$Walc[d2$sex == "M"], d2$Walc[d2$sex == "F"])   ## The Test Rejects the Null Hypothesis as the sample means are different and it proves median of male's is higher than female's concluding higher alcohol consumption in males than females.

##Data Representation
boxplot(d2$Pstatus, d2$Walc, names = c("Apart","Together"), ylab = "WALC", xlab="Pstatus")  ## The Box Plot represents that the median for Walc in both parents status
boxplot(d2$sex, d2$Walc, col = c("blue","red"), names = c("F","M"), ylab = "WALC", xlab="SEX")  ## The Box Plot Reprents the above performed statistical T-test Data on Male and Female Alcohol Consumption during weekends.
boxplot(d2$sex, d2$Dalc, col = c("blue","red"), names = c("F","M"), ylab = "DALC", xlab="SEX")  ## The Box Plot Reprents the Data on Male and Female Alcohol Consumption during workdays.
    

  