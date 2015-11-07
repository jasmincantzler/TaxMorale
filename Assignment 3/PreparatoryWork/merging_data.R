###########################################
# Cleaning and merging Afrobarometer Data #
# Wiebke Weiger                           #
# last updated: 6 November 2015           #
###########################################

# packages needed:
library(plyr)

# set working directory (remember to change to your directory)
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")

# load the data into R from the previously created csv-files:

round5.small <- read.csv("round5.small.csv")
round4.small <- read.csv("round4.small.csv")
round3.small <- read.csv("round3.small.csv")

# alternatively: load data from saved Rdata-files;
# load("Round5.small.RData")
# load("Round4.small.RData")
# load("Round3.small.RData")

# # # # # # # # # # # # 
#  CLEANING ROUND 5:  #
# # # # # # # # # # # #

# sort data according to the "Country"-variable (increasing from 1 to 35):
round5.small <- arrange(round5.small, Country)

# turn "Country" variable into a factor variable with country names:
round5.small$Country <- cut(round5.small$Country,
                            breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 
                                       15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
                                       27, 28, 29, 30, 31, 32, 33, 34, 35),
                            labels = c('Algeria', 'Benin', 'Botswana', 'Burkina Faso',
                                       'Burundi', 'Cameroon', 'Cap Verde', 'Cote dIvoire',
                                       'Egypt', 'Ghana', 'Guinea', 'Kenya', 'Lesotho',
                                       'Liberia', 'Madagascar','Malawi', 'Mali',
                                       'Mauritius', 'Morocco', 'Mozambique', 'Namibia',
                                       'Niger', 'Nigeria', 'Senegal', 'Sierra Leone',
                                       'South Africa', 'Sudan', 'Swaziland', 'Tanzania',
                                       'Togo', 'Tunisia', 'Uganda', 'Zambia', 'Zimbabwe'))


# create unique identifyer by combining "Respondent" and "Year": 
round5.small$RespondentID <- 
  
  
# MISSINGS:
# at the moment R only recognizes missings, when a question was not asked in one
# or several countries
# according to the code books, missings are coded as "-1" which is currently
# recognized by R as a value
  
# Step 1: recode all "-1" as missings:

summary(round5.small) 
### shows that there are "-1" and therefore missings in all of the variables except
### for Country, Year, Respondent and UrbanRural
### The variables EconomicPolicies, LocalGvtTaxes, Enforce1, Enforce2, 
### CorruptionOfficialsLocal and CorruptionOfficialsNational don't contain the value
### "-1" but have been coded as containing only missings (NA)

# a) Age:
age.missing <- round5.small[ which(round5.small$Age==-1), ]# 14 missings
# replace these with NA:
round5.small$Age[round5.small$Age==-1] <- NA 

# b) EconomicSituation:
EconomicSituation.missing <- round5.small[ which(round5.small$EconomicSituation==-1), ]
# 13 missings
# replace these with NA:
round5.small$EconomicSituation[round5.small$EconomicSituation==-1] <- NA

# c) LivingConditions:
LivingConditions.missing <- round5.small[ which(round5.small$LivingConditions==-1), ]
# 58 missings
# replace these with NA:
round5.small$LivingConditions[round5.small$LivingConditions==-1] <- NA

# d) Interest:
Interest.missing <- round5.small[ which(round5.small$Interest==-1), ]
# 14 missings
# replace these with NA:
round5.small$Interest[round5.small$Interest==-1] <- NA

# e) Religion:
Religion.missing <- round5.small[ which(round5.small$Religion==-1), ]
# 28 missings
# replace these with NA:
round5.small$Religion[round5.small$Religion==-1] <- NA

# f) TaxMorale:
TaxMorale.missing <- round5.small[ which(round5.small$TaxMorale==-1), ]
# 28 missings
# replace these with NA:
round5.small$TaxMorale[round5.small$TaxMorale==-1] <- NA

# g) TrustPresident:
TrustPresident.missing <- round5.small[ which(round5.small$TrustPresident==-1), ]
# 16 missings
# replace these with NA:
round5.small$TrustPresident[round5.small$TrustPresident==-1] <- NA

# h) TrustParliament:
TrustParliament.missing <- round5.small[ which(round5.small$TrustParliament==-1), ]
# 23 missings
# replace these with NA:
round5.small$TrustParliament[round5.small$TrustParliament==-1] <- NA

# i) TrustCourts:
TrustCourts.missing <- round5.small[ which(round5.small$TrustCourts==-1), ]
# 46 missings
# replace these with NA:
round5.small$TrustCourts[round5.small$TrustCourts==-1] <- NA

# j) CorruptionPresident:
CorruptionPresident.missing <- round5.small[ which(round5.small$CorruptionPresident==-1), ]
# 11 missings
# replace these with NA:
round5.small$CorruptionPresident[round5.small$CorruptionPresident==-1] <- NA

# k) CorruptionParliament:
CorruptionParliament.missing <- round5.small[ which(round5.small$CorruptionParliament==-1), ]
# 31 missings
# replace these with NA:
round5.small$CorruptionParliament[round5.small$CorruptionParliament==-1] <- NA

# l) CorruptionOfficials:
CorruptionOfficials.missing <- round5.small[ which(round5.small$CorruptionOfficials==-1), ]
# 41 missings
# replace these with NA:
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==-1] <- NA

# m) CorruptionCouncilors:
CorruptionCouncilors.missing <- round5.small[ which(round5.small$CorruptionCouncilors==-1), ]
# 2400 missings
# replace these with NA:
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==-1] <- NA

# n) CorruptionTax:
CorruptionTax.missing <- round5.small[ which(round5.small$CorruptionTax==-1), ]
# 28 missings
# replace these with NA:
round5.small$CorruptionTax[round5.small$CorruptionTax==-1] <- NA

# o) Gender:
Gender.missing <- round5.small[ which(round5.small$Gender==-1), ]
# 0 missings
# replace these with NA:
round5.small$Gender[round5.small$Gender==-1] <- NA

# p) AvoidHowOften:
AvoidHowOften.missing <- round5.small[ which(round5.small$AvoidHowOften==-1), ]
# 33 missings
# replace these with NA:
round5.small$AvoidHowOften[round5.small$AvoidHowOften==-1] <- NA

# q) TrustTax:
TrustTax.missing <- round5.small[ which(round5.small$TrustTax==-1), ]
# 79 missings
# replace these with NA:
round5.small$TrustTax[round5.small$TrustTax==-1] <- NA

# r) SelfEmployedTax:
SelfEmployedTax.missing <- round5.small[ which(round5.small$SelfEmployedTax==-1), ]
# 14 missings
# replace these with NA:
round5.small$SelfEmployedTax[round5.small$SelfEmployedTax==-1] <- NA

# s) AvoidReason:
AvoidReason.missing <- round5.small[ which(round5.small$AvoidReason==-1), ]
# 39 missings
# replace these with NA:
round5.small$AvoidReason[round5.small$AvoidReason==-1] <- NA


  
# Step 2: explore variables for things such as "don't know" "refuse to answer"

# Step 3: recode these as missings as well:



# FACTOR VARIABLES:
# At the moment R recognizes most of the variables as numeric variables even
# though they are factor variables.
# Therefore these will now be coded as factors:

round5.small <- mutate(round5.small,
                       UrbanRural = factor(UrbanRural, levels = 1:2, 
                                           labels = c("urban", "rural")),
                       EconomicSituation = factor(EconomicSituation,levels = 1:5, 
                                                  labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       LivingConditions = factor(LivingConditions, levels = 1:5,
                                                 labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       Interest = factor(Interest, levels = 0:3,
                                         labels = c("Not at all interested", "Not very interested", "Somewhat interested", "Very interested")),
                       Religion = factor(Religion, levels = 0:3,
                                         labels = c("Not a member", "Inactive member", "Active member", "Official Leader")),
                       TaxMorale = factor(TaxMorale, levels = 1:5,
                                          labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
                       TrustPresident = factor(TrustPresident, levels = 0:3, 
                                               labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustParliament = factor(TrustParliament, levels = 0:3, 
                                                labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustCourts = factor(TrustCourts, levels = 0:3, 
                                            labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       CorruptionPresident = factor(CorruptionPresident, levels = 0:3,
                                                    labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionParliament = factor(CorruptionParliament, levels = 0:3,
                                                     labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficials = factor(CorruptionOfficials, levels = 0:3,
                                                    labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionCouncilors = factor(CorruptionCouncilors, levels = 0:3,
                                                     labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionTax = factor(CorruptionTax, levels = 0:3,
                                              labels = c("None", "Some of them", "Most of them", "All of them")),
                       Gender = factor(Gender, levels = 1:2, 
                                       labels = c("male", "female")),
                       AvoidHowOften = factor(AvoidHowOften, levels = 0:3,
                                              labels = c("Never", "Rarely", "Often", "Allways")),
                       TrustTax = factor(TrustTax, levels = 0:3, 
                                         labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       SelfEmployedTax = factor(SelfEmployedTax, levels = 0:1, 
                                                labels = c("Not required to pay", "Required to pay")),
                       AvoidReason = factor(AvoidReason, levels = ,
                                            labels = c("")),## still need to figure out how to label this one
                       EconomicPolicies = factor(EconomicPolicies, levels = ,
                                                 labels = c("")),
                       LocalGvtTaxes = factor(LocalGvtTaxes, levels = ,
                                              labels = c("")),
                       Enforce1 = factor(Enforce1, levels = ,
                                         labels = c("")),
                       Enforce2 = factor(Enforce2, levels = ,
                                         labels = c("")),
                       CorruptionOfficialsLocal = factor(CorruptionOfficialsLocal,
                                                         levels = ,
                                                         labels = c("")),
                       CorruptionOfficialsNational = factor(CorruptionOfficialsNational,
                                                            levels = ,
                                                            labels = c("")),
                       Year = factor(Year))


#EXPORTING AND SAVING DATA:

round5.final <- round5.small 

# export data into csv format:
export(round5.final, file="round5.final.csv")

# save smaller data set in Rdata format:
save(round5.final, file="Round5.final.RData", list="round5.final")


# # # # # # # # # # # # 
#  CLEANING ROUND 4:  #
# # # # # # # # # # # #


# recode "Country" variable so that the numbers match those of round 5:

round4.small$Country[round4.small$Country==20] <- 35
round4.small$Country[round4.small$Country==19] <- 34
round4.small$Country[round4.small$Country==18] <- 33
round4.small$Country[round4.small$Country==17] <- 30
round4.small$Country[round4.small$Country==16] <- 27
round4.small$Country[round4.small$Country==15] <- 25
round4.small$Country[round4.small$Country==14] <- 24
round4.small$Country[round4.small$Country==13] <- 22
round4.small$Country[round4.small$Country==12] <- 21
round4.small$Country[round4.small$Country==11] <- 18
round4.small$Country[round4.small$Country==10] <- 17
round4.small$Country[round4.small$Country==9] <- 16
round4.small$Country[round4.small$Country==8] <- 15
round4.small$Country[round4.small$Country==7] <- 14
round4.small$Country[round4.small$Country==6] <- 13
round4.small$Country[round4.small$Country==5] <- 11
round4.small$Country[round4.small$Country==4] <- 7
round4.small$Country[round4.small$Country==3] <- 4
round4.small$Country[round4.small$Country==2] <- 3
round4.small$Country[round4.small$Country==1] <- 2

# turn "Country" variable into a factor variable with country names:
round4.small$Country <- cut(round4.small$Country,
                            breaks = c(0, 2, 3, 4, 7, 11, 13, 14, 15, 16, 17, 18,
                                       21, 22, 24, 25, 27, 30, 33, 34, 35),
                            labels = c('Benin', 'Botswana', 'Burkina Faso',
                                       'Cap Verde', 'Ghana', 'Kenya', 'Lesotho',
                                       'Liberia', 'Madagascar','Malawi', 'Mali',
                                       'Mozambique', 'Namibia', 'Nigeria', 'Senegal',
                                       'South Africa', 'Tanzania','Uganda', 'Zambia', 'Zimbabwe'))

# create unique identifyer by combining "Respondent" and "Year":


#EXPORTING AND SAVING DATA:

round4.final <- round4.small 

# export data into csv format:
export(round4.final, file="round4.final.csv")

# save smaller data set in Rdata format:
save(round4.final, file="Round4.final.RData", list="round4.final")



# # # # # # # # # # # # 
#  CLEANING ROUND 3:  #
# # # # # # # # # # # #


# recode "Country" variable so that the numbers match those of round 5:

round3.small$Country[round3.small$Country==18] <- 35
round3.small$Country[round3.small$Country==17] <- 34
round3.small$Country[round3.small$Country==16] <- 33
round3.small$Country[round3.small$Country==15] <- 30
round3.small$Country[round3.small$Country==14] <- 27
round3.small$Country[round3.small$Country==13] <- 25
round3.small$Country[round3.small$Country==12] <- 24
round3.small$Country[round3.small$Country==11] <- 22
round3.small$Country[round3.small$Country==10] <- 21
round3.small$Country[round3.small$Country==9] <- 18
round3.small$Country[round3.small$Country==8] <- 17
round3.small$Country[round3.small$Country==7] <- 16
round3.small$Country[round3.small$Country==6] <- 14
round3.small$Country[round3.small$Country==5] <- 13
round3.small$Country[round3.small$Country==4] <- 11
round3.small$Country[round3.small$Country==3] <- 7
round3.small$Country[round3.small$Country==2] <- 3
round3.small$Country[round3.small$Country==1] <- 2

# turn "Country" variable into a factor variable with country names:
round3.small$Country <- cut(round3.small$Country,
                            breaks = c(0, 2, 3, 7, 11, 13, 14, 16, 17, 18,
                                       21, 22, 24, 25, 27, 30, 33, 34, 35),
                            labels = c('Benin', 'Botswana', 'Cap Verde', 'Ghana', 
                                       'Kenya', 'Lesotho', 'Madagascar', 'Malawi', 'Mali',
                                       'Mozambique', 'Namibia', 'Nigeria', 'Senegal',
                                       'South Africa', 'Tanzania', 'Uganda', 'Zambia', 'Zimbabwe'))


# create unique identifyer by combining "Respondent" and "Year":
# before that: the abbreviation of Malawi is MLW in round 4 and 5


#EXPORTING AND SAVING DATA:

round3.final <- round3.small 

# export data into csv format:
export(round3.final, file="round3.final.csv")

# save smaller data set in Rdata format:
save(round3.final, file="Round3.final.RData", list="round3.final")


# # # # # # # #
#   MERGING   #
# # # # # # # #



# for merging use rbind.fill (from plyr-package)

