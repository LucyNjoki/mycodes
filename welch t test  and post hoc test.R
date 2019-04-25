#-------------------------------------------------------------------------------
#Purpose : t test and post hoc test
#Date: March 25,2019
#By:Lucy Njoki
#-------------------------------------------------------------------------------

rm(list=ls())

#setting up the working directory
setwd("D:\\NJUKI\\R Training\\Raw Data")


#importing data
mydata<-read.table("D:\\NJUKI\\R Training\\Raw Data\\CO.txt",sep="",header=TRUE)
mydata

#column names
colnames(mydata)

#normality test
shapiro.test(mydata$conc)

#comparing means
#Welch t-test(2 levels in the DV)
t.test(uptake~Treatment,var.equal=FALSE,data=mydata)

#ANOVA test(more than 2 levels in the DV)
fit<-aov(uptake~Plant,data=mydata)
summary(fit)

#post hoc test
#loading the required packages
library(mvtnorm)
library(survival)
library(MASS)
library(multcomp)
library(TH.data)
library(abind)

local({posthoc<-glht(fit)
linfct = mcp(ind = "Tukey")
print(confint(posthoc)) #confidence intervals
print(cld(posthoc)) #compact letter display
})

