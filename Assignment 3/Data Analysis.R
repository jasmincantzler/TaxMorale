##########################################################
# Data Analysis                                          #
# Wiebke Weiger & Jasmin Cantzler                        # 
# last updated: 12 November 2015                         #
##########################################################

# Set working directory
#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
setwd("/Users/jasmincantzler/Documents/TaxMorale/Assignment 3/PreparatoryWork")

# Load data
mydata <- read.csv('Afrobarometer.final.csv')

# Load packages
library(ggplot2)

# CONDUCTS BASIC DESCRIPTIVE STATISTICS 
summary(mydata) 
summary(mydata$TaxMorale)

barplot(prop.table(table(mydata$TaxMorale)))

#creates a histogram of our dependent variable Tax Morale
qplot(mydata$TaxMorale,
      geom="histogram",
      binwidth=8,
      main="Summary Statistic",
      xlab="The tax department always has the right to make people pay taxes.",
      fill=I("lightblue"))



# CONDUCTS INFERENTIAL STATISTICS

