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
library(knitr)
library(dplyr)

#creates Tax Morale as a numeric variable
TaxMoraleNumeric <- as.numeric(mydata$TaxMorale)

# CONDUCTS BASIC DESCRIPTIVE STATISTICS 
summary(mydata) 
summary(mydata$TaxMorale)

#creates a histogram of our dependent variable Tax Morale
qplot(mydata$TaxMorale,
      geom="histogram",
      binwidth=8,
      main="Summary Statistic",
      xlab="The tax department always has the right to make people pay taxes.",
      fill=I("lightblue"))

#creating summary statistics table


meanage <- mean(mydata$Age, na.rm=TRUE)
meanAge<- data.frame(meanage,mydata$Country)
meantax <-mean(TaxMoraleNumeric, na.rm = TRUE)
meanTax<-data.frame(meantax, mydata$Country)


fitted <- with(mydata,
               data.frame(Mean = mean(Age, na.rm = TRUE),
                          Country = Country))
                  



sum2_table <- merge(obs, Pop09.13,
                    by = c('year'))
percent <- (obs$n / Pop09.13$WorkPop)*100
percent_of_working <- data.frame(year, percent)

sum2_table <- merge(sum2_table, percent_of_working,
                    by = c('year'))
sum2_table <- merge(sum2_table, Tax09.13,
                    by = c('year'))

kable(table)


# CONDUCTS INFERENTIAL STATISTICS
mylogit <- glm(TaxMorale ~ TrustPresident, data=mydata, family=binomial)
summary(mylogit)


