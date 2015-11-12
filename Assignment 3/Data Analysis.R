##########################################################
# Data Analysis                                          #
# Wiebke Weiger & Jasmin Cantzler                        # 
# last updated: 12 November 2015                         #
##########################################################

# Set working directory
#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
setwd("/Users/jasmincantzler/Documents/TaxMorale/Assignment 3/PreparatoryWork")

mydata <- read.csv('Afrobarometer.final.csv')

# CONDUCTS BASIC DESCRIPTIVE STATISTICS 
summary(mydata) 
summary(Afrobarometer.final$TaxMorale)
table(mydata$TaxMorale)

install.packages("pastecs")
library(pastecs)
stat.desc(mydata)

# CONDUCTS INFERENTIAL STATISTICS
Reg1 <- lm(TaxMorale~TrustPresident, data=mydata)