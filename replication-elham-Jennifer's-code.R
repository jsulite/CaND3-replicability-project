#replication for Elham's data

#Import the GSS 2017 Family Cycle data and store as a table 
library(haven)
library(psych)
library("tidyverse")
library(lmtest)
library(ggplot2)
options(scipen=999)

dat <- read.csv("/Users/jennifersuliteanu/Desktop/CSDC-CAnD3/gss-12M0025-E-2017-c-31_F1.csv")

#Create a series of Recoded Variables (that will serve as the independent variables for the model) in step 3 to step 9
#Recode FI_105 to a binary variable by replacing the responses “Definitely Yes” and “Probably Yes” 
#with “1” and replacing all other values with “0”

summary(as.factor(dat$FI_105))
dat$FI_105<- plyr::mapvalues(dat$FI_105, from = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                             to = c("1", "1", "0", "0", "0", "0", "0", "0", "0"))
summary(as.factor(dat$FI_105))

#Recode COM_200 to a binary variable by replacing the responses 
#“Joint Account Only” - 2 and “Both Sole and Joint or Mortgage Account” - 3 with “1” and replacing all other values with “0”
summary(as.factor(dat$COM_200))
dat$COM_200<- plyr::mapvalues(dat$COM_200, from = c("1", "2", "3", "6", "7", "8", "9"), 
                             to = c("0", "1", "1", "0", "0", "0", "0"))
summary(as.factor(dat$COM_200))
#Recode STS_410 to a binary variable by replacing the responses “Very Satisfied” and “Satisfied” 
#with “1” and replacing all other values with “0”
summary(as.factor(dat$STS_410))
dat$STS_410<- plyr::mapvalues(dat$STS_410, from = c("1", "2", "3", "4", "6", "7", "8", "9"), 
                              to = c("1", "1", "0", "0", "0", "0", "0", "0"))
summary(as.factor(dat$STS_410))

#Recode REE_02 to a binary variable by replacing the responses 
#“At least Once a Week” and “At Least Once a Month” with “1” and replacing all other values with “0”

summary(as.factor(dat$REE_02))
dat$REE_02<- plyr::mapvalues(dat$REE_02, from = c("1", "2", "3", "4", "5", "7", "8", "9"), 
                             to = c("1", "1", "0", "0", "0", "0", "0", "0"))
summary(as.factor(dat$REE_02))
#Recode REE_03 to a binary variable by replacing the responses 
#“At least Once a Day” and “At Least Once a Week” with “1” and replacing all other values with “0”
summary(as.factor(dat$REE_03))
dat$REE_03<- plyr::mapvalues(dat$REE_03, from = c("1", "2", "3", "4", "5", "6", "97", "98", "99"), 
                             to = c("1", "1", "0", "0", "0", "0", "0", "0", "0"))
summary(as.factor(dat$REE_03))

#Recode RLR_110 to a binary variable by replacing the responses 
#“Very Important” and “Somewhat Important” with “1” and replacing all other values with “0”
summary(as.factor(dat$RLR_110))
dat$RLR_110<- plyr::mapvalues(dat$RLR_110, from = c("1", "2", "3", "4", "7", "8", "9"), 
                             to = c("1", "1", "0", "0", "0", "0", "0"))
summary(as.factor(dat$RLR_110))

#Recode SRH_110 to a binary variable by replacing the responses 
#“Excellent,” “Very Good,” and “Good” with “1” and replacing all other values with “0”

summary(as.factor(dat$SRH_110))
dat$SRH_110<- plyr::mapvalues(dat$SRH_110, from = c("1", "2", "3", "4", "5", "7", "8", "9"), 
                             to = c("1", "1", "1", "0", "0", "0", "0", "0"))
summary(as.factor(dat$SRH_110))
#Run a Multiple Linear Regression with SLM_01 as the dependent variable, 
#and all the above recoded variables as the independent variables

mod <- lm(SLM_01 ~ SRH_110 + RLR_110 + REE_03 + REE_02 + STS_410 + COM_200 + FI_105, data = dat)

#Obtain the summary statistics
summary(mod)

#Run another Multiple Linear Regression with the same dependent variable as stated in step 10, 
#as well as all of the other independent variables except for STS_410. 
#Also, create an interaction terms between REE_02, REE_03, and RLR_110. 

mod2 <- lm(SLM_01 ~ SRH_110 + RLR_110 + REE_03 + REE_02 + COM_200 + FI_105 + REE_02*REE_03*RLR_110, data = dat)
summary(mod2)
