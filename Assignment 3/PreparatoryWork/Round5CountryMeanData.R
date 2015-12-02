##############################################
# Create sub-dataframe with mean values      #
# for countries in order to be used for map  # 
# and code for map of Tax Morale             #
# 23 November 2015                           # 
##############################################


#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork") 
r5 <- read.csv('round5.final.csv')

# Load necessary packages

library(doBy)
library(rio)

# first step: make new dataset with relevant variables as numeric:

r5.test <- r5

r5.test$EconomicSituation <- as.numeric(r5.test$EconomicSituation)
r5.test$LivingConditions <- as.numeric(r5.test$LivingConditions)
r5.test$Interest <- as.numeric(r5.test$Interest)
r5.test$Religion <- as.numeric(r5.test$Religion)
r5.test$TaxMorale <- as.numeric(r5.test$TaxMorale)
r5.test$TrustPresident <- as.numeric(r5.test$TrustPresident)
r5.test$TrustParliament <- as.numeric(r5.test$TrustParliament)
r5.test$TrustCourts <- as.numeric(r5.test$TrustCourts)
r5.test$CorruptionPresident <- as.numeric(r5.test$CorruptionPresident)
r5.test$CorruptionParliament <- as.numeric(r5.test$CorruptionParliament)
r5.test$CorruptionOfficials <- as.numeric(r5.test$CorruptionOfficials)
r5.test$CorruptionCouncilors <- as.numeric(r5.test$CorruptionCouncilors)
r5.test$CorruptionTax <- as.numeric(r5.test$CorruptionTax)
r5.test$Gender <- as.numeric(r5.test$Gender)
r5.test$AvoidHowOften <- as.numeric(r5.test$AvoidHowOften)
r5.test$TrustTax <- as.numeric(r5.test$TrustTax)
r5.test$SelfEmployedTax <- as.numeric(r5.test$SelfEmployedTax)
r5.test$AvoidReason <- as.numeric(r5.test$AvoidReason)

# drop that are not needed variables:
r5.test$Year <- NULL
r5.test$EconomicPolicies <- NULL
r5.test$LocalGvtTaxes <- NULL
r5.test$Enforce1 <- NULL
r5.test$Enforce2 <- NULL
r5.test$CorruptionOfficialsLocal <- NULL
r5.test$CorruptionOfficialsNational <- NULL
r5.test$iso2c <- NULL


# Collapse data for the different countries with mean values of the variables:

r5.test <- summaryBy(Age + EconomicSituation + LivingConditions + Interest + Religion + TaxMorale + TrustPresident
                     + TrustParliament + TrustCourts + CorruptionPresident + CorruptionParliament + 
                       CorruptionOfficials + CorruptionCouncilors + CorruptionTax + Gender + AvoidHowOften + 
                       TrustTax + SelfEmployedTax + AvoidReason ~ Country, data=r5.test, FUN=mean, na.rm=TRUE)


# Export and save data:

round5.mean  <- r5.test 

# export data into csv format:
rio::export(round5.mean, file="round5.mean.csv")

# save final data set in Rdata format:
save(round5.mean, file="Round5.mean.RData", list="round5.mean")


#####################################################################

# first attempt at map for average tax morale:

setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork") 
r5.mean <- read.csv('round5.mean.csv')

# make an iso2c-variable:
r5.mean$iso2c <- c('DZ', 'BJ', 'BW', 'BF', 'BI', 'CM', 'CV', 'CI', 'EG', 'GH','GN', 'KE', 'LS', 'LR', 'MG',
                   'MW', 'ML','MU', 'MA', 'MZ', 'NA', 'NE', 'NG', 'SN', 'SL', 'ZA', 'SD', 'SZ', 'TZ', 'TG', 
                   'TN', 'UG', 'ZM', 'ZW')

# Plot:
library(googleVis)
keep_vars = c(7, 21)
taxmorale.mean <- r5.mean[, keep_vars]

afrobarometer_map <- gvisGeoChart(taxmorale.mean, locationvar = 'iso2c',
                                  colorvar = 'TaxMorale.mean',
                                  options = list(
                                    colors = "['#fff7bc', '#d95f0e']",
                                    region= '002'
                                  ))

plot(afrobarometer_map)