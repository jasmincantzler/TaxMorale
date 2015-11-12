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

# creates a summary per country with mean of Tax Morale
TaxMorale<-data.frame(TaxMoraleNumeric, mydata$Country)
summary_Morale <-TaxMorale %>%
  group_by(mydata.Country) %>%
  summarise(
    mean = mean(TaxMoraleNumeric, na.rm=TRUE)
  )

#renames column header
names(summary_Morale) <- c('Tax Morale', 'Country')

# creates a summary per country with mean of age
summary_df <- mydata %>%
  group_by(Country) %>%
  summarise(
    mean = mean(Age,na.rm=TRUE)
  )

#creating summary statistics table
###not yet sure how to do that
sum2_table <- merge(summary_Morale, summary_df)
sum2_table<- rbind(summary_Morale, summary_df)


                  




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


