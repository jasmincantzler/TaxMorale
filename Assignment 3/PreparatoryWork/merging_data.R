###########################################
# Cleaning and merging Afrobarometer Data #
# Wiebke Weiger & Jasmin Cantzler         #
# last updated: 9 November 2015           #
###########################################

# packages needed:
library(plyr)
library(dplyr)
library(rio)

# set working directory (remember to change to your directory)
#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
#setwd("/Users/jasmincantzler/Documents/TaxMorale/Assignment 3/PreparatoryWork")

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
round5.small$Respondent <- paste0(round5.small$Respondent, '_', round5.small$Year)

# turn into string variable:
round5.small$Respondent <- sapply(round5.small$Respondent, as.character)


  
##CLEANING DATA

# MISSINGS:
# at the moment R only recognizes missings, when a question was not asked in one
# or several countries
# according to the code books, missings are coded as "-1" which is currently
# recognized by R as a value

#DON'T KNOWS:

#REFUSE TO ANSWER:
  
# RECODING OF VARIABLES INTO "NA" WHEN APPROPRIATE

summary(round5.small) 
### shows that there are "-1" and therefore missings in all of the variables except
### for Country, Year, Respondent, UrbanRural, and Gender
### The variables EconomicPolicies, LocalGvtTaxes, Enforce1, Enforce2, 
### CorruptionOfficialsLocal and CorruptionOfficialsNational don't contain the value
### "-1" but have been coded as containing only missings (NA)

# a) Age:
# Question Number: Q1
# Question: How old are you?
# Variable Label: Age
# Values: 18-100, 105, 998-999, -1
# Value Labels: 998=Refused to answer, 999=Don‟t know, -1=Missing
#summary(round5.small$Age)
#age.missing <- round5.small[ which(round5.small$Age==-1), ]
# 14 missings
# replace these with NA:
round5.small$Age[round5.small$Age==-1] <- NA 
#age.notknown <- round5.small[ which(round5.small$Age==999), ]
# 400 not knowns
# replace these with NA:
round5.small$Age[round5.small$Age==999] <- NA 
# age.refuse <- round5.small[ which(round5.small$Age==998), ]
# 30 refuse
# replace these with NA:
round5.small$Age[round5.small$Age==998] <- NA

# b) EconomicSituation:
# Question Number: Q3A
# Question: In general, how would you describe: The present economic condition of this country?
# Variable Label: Country‟s present economic condition
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$EconomicSituation)
#EconomicSituation.missing <- round5.small[ which(round5.small$EconomicSituation==-1), ]
# 13 missings
# replace these with NA:
round5.small$EconomicSituation[round5.small$EconomicSituation==-1] <- NA
# EconomicSituation.notknown <- round5.small[ which(round5.small$EconomicSituation==9), ]
# 847 don't know
# replace these with NA:
round5.small$EconomicSituation[round5.small$EconomicSituation==9] <- NA

# c) LivingConditions:
# Question Number: Q3B
# Question: In general, how would you describe: Your own present living conditions?
# Variable Label: Your present living conditions
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$LivingConditions)
#LivingConditions.missing <- round5.small[ which(round5.small$LivingConditions==-1), ]
# 58 missings
# replace these with NA:
round5.small$LivingConditions[round5.small$LivingConditions==-1] <- NA
# LivingConditions.notknown <- round5.small[ which(round5.small$LivingConditions==9), ]
# 142 don't know
# replace these with NA:
round5.small$LivingConditions[round5.small$LivingConditions==9] <- NA

# d) Interest:
# Question Number: Q14
# Question: How interested would you say you are in public affairs?
# Variable Label: Interest in public affairs
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$Interest)
#Interest.missing <- round5.small[ which(round5.small$Interest==-1), ]
# 14 missings
# replace these with NA:
round5.small$Interest[round5.small$Interest==-1] <- NA
# Interest.refused <- round5.small[ which(round5.small$Interest==998), ]
# 1 refuse
# replace these with NA:
round5.small$Interest[round5.small$Interest==998] <- NA
# Interest.notknown <- round5.small[ which(round5.small$Interest==9), ]
# 531 don't know
# replace these with NA:
round5.small$Interest[round5.small$Interest==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$Interest[round5.small$Interest==3] <- 4
round5.small$Interest[round5.small$Interest==2] <- 3
round5.small$Interest[round5.small$Interest==1] <- 2
round5.small$Interest[round5.small$Interest==0] <- 1

# e) Religion:
# Question Number: Q25A
# Question: Let‟s turn to your role in the community. Now I am going to read out a list of groups that people join or attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or not a member: A religious group that meets outside of regular worship services?
# Variable Label: Member of religious group
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$Religion)
# Religion.missing <- round5.small[ which(round5.small$Religion==-1), ]
# 28 missings
# replace these with NA:
round5.small$Religion[round5.small$Religion==-1] <- NA
# Religion.notknown <- round5.small[ which(round5.small$Religion==9), ]
# 354 don't know
# replace these with NA:
round5.small$Religion[round5.small$Religion==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$Religion[round5.small$Religion==3] <- 4
round5.small$Religion[round5.small$Religion==2] <- 3
round5.small$Religion[round5.small$Religion==1] <- 2
round5.small$Religion[round5.small$Religion==0] <- 1

# f) TaxMorale:
# Question Number: Q48C
# Question: For each of the following statements, please tell me whether you disagree or agree: The tax authorities always have the right to make people pay taxes.
# Variable Label: People must pay taxes
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Strongly disagree, 2=Disagree, 3=Neither agree nor disagree, 4=Agree, 5=Strongly agree, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$TaxMorale)
#TaxMorale.missing <- round5.small[ which(round5.small$TaxMorale==-1), ]
# 28 missings
# replace these with NA:
round5.small$TaxMorale[round5.small$TaxMorale==-1] <- NA
# TaxMorale.notknown <- round5.small[ which(round5.small$TaxMorale==9), ]
# 2297 don't know
# replace these with NA:
round5.small$TaxMorale[round5.small$TaxMorale==9] <- NA

# g) TrustPresident:
# Question Number: Q59A
# **Question: How much do you trust each of the following, or haven‟t you heard enough about them to say: The President/Prime Minister?
# Variable Label: Trust key leadership figure (President/Prime Minister)
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don‟t know/Haven‟t heard enough, 997=Not asked, 998=Refused to answer, -1=Missing
# summary(round5.small$TrustPresident)
#TrustPresident.missing <- round5.small[ which(round5.small$TrustPresident==-1), ]
# 16 missings
# replace these with NA:
round5.small$TrustPresident[round5.small$TrustPresident==-1] <- NA
# TrustPresident.notknown <- round5.small[ which(round5.small$TrustPresident==9), ]
# 1779 don't know
# replace these with NA:
round5.small$TrustPresident[round5.small$TrustPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$TrustPresident[round5.small$TrustPresident==3] <- 4
round5.small$TrustPresident[round5.small$TrustPresident==2] <- 3
round5.small$TrustPresident[round5.small$TrustPresident==1] <- 2
round5.small$TrustPresident[round5.small$TrustPresident==0] <- 1

# h) TrustParliament:
# Question Number: Q59B
# **Question: How much do you trust each of the following, or haven‟t you heard enough about them to say: Parliament?
# Variable Label: Trust parliament/national assembly
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don‟t know/Haven‟t heard enough, 997=Not asked, 998=Refused to answer, -1=Missing
# summary(round5.small$TrustParliament)
#TrustParliament.missing <- round5.small[ which(round5.small$TrustParliament==-1), ]
# 23 missings
# replace these with NA:
round5.small$TrustParliament[round5.small$TrustParliament==-1] <- NA
# TrustParliament.notknown <- round5.small[ which(round5.small$TrustParliament==9), ]
# 2568 don't know
# replace these with NA:
round5.small$TrustParliament[round5.small$TrustParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$TrustParliament[round5.small$TrustParliament==3] <- 4
round5.small$TrustParliament[round5.small$TrustParliament==2] <- 3
round5.small$TrustParliament[round5.small$TrustParliament==1] <- 2
round5.small$TrustParliament[round5.small$TrustParliament==0] <- 1

# i) TrustCourts:
# Question Number: Q59J
# Question: How much do you trust each of the following, or haven‟t you heard enough about them to say: Courts of law?
# Variable Label: Trust courts of law
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don‟t know/Haven‟t heard enough, 998=Refused to answer, -1=Missing
# summary(round5.small$TrustCourts)
#TrustCourts.missing <- round5.small[ which(round5.small$TrustCourts==-1), ]
# 46 missings
# replace these with NA:
round5.small$TrustCourts[round5.small$TrustCourts==-1] <- NA
# TrustCourts.notknown <- round5.small[ which(round5.small$TrustCourts==9), ]
# 2452 don't know
# replace these with NA:
round5.small$TrustCourts[round5.small$TrustCourts==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$TrustCourts[round5.small$TrustCourts==3] <- 4
round5.small$TrustCourts[round5.small$TrustCourts==2] <- 3
round5.small$TrustCourts[round5.small$TrustCourts==1] <- 2
round5.small$TrustCourts[round5.small$TrustCourts==0] <- 1

# j) CorruptionPresident:
# Question Number: Q60A
# **Question: How many of the following people do you think are involved in corruption, or haven‟t you heard enough about them to say: The President/Prime Minister and Officials in his Office?
# Variable Label: Corruption: office of the President/Prime Minister
# Values: 0-3, 9, 997, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don‟t know, 997=Not asked, 998=Refused to answer,
# -1=Missing
# summary(round5.small$CorruptionPresident)
#CorruptionPresident.missing <- round5.small[ which(round5.small$CorruptionPresident==-1), ]
# 11 missings
# replace these with NA:
round5.small$CorruptionPresident[round5.small$CorruptionPresident==-1] <- NA
# CorruptionPresident.notknown <- round5.small[ which(round5.small$CorruptionPresident==9), ]
# 8030 don't know
# replace these with NA:
round5.small$CorruptionPresident[round5.small$CorruptionPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$CorruptionPresident[round5.small$CorruptionPresident==3] <- 4
round5.small$CorruptionPresident[round5.small$CorruptionPresident==2] <- 3
round5.small$CorruptionPresident[round5.small$CorruptionPresident==1] <- 2
round5.small$CorruptionPresident[round5.small$CorruptionPresident==0] <- 1

# k) CorruptionParliament:
# Question Number: Q60B
# **Question: How many of the following people do you think are involved in corruption, or haven‟t you heard enough about them to say: Members of Parliament?
# Variable Label: Corruption: Members of Parliament
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don‟t know, 997=Not asked, 998=Refused to answer,
# -1=Missing.
# summary(round5.small$CorruptionParliament)
#CorruptionParliament.missing <- round5.small[ which(round5.small$CorruptionParliament==-1), ]
# 31 missings
# replace these with NA:
round5.small$CorruptionParliament[round5.small$CorruptionParliament==-1] <- NA
# CorruptionParliament.notknown <- round5.small[ which(round5.small$CorruptionParliament==9), ]
# 6826 don't know
# replace these with NA:
round5.small$CorruptionParliament[round5.small$CorruptionParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$CorruptionParliament[round5.small$CorruptionParliament==3] <- 4
round5.small$CorruptionParliament[round5.small$CorruptionParliament==2] <- 3
round5.small$CorruptionParliament[round5.small$CorruptionParliament==1] <- 2
round5.small$CorruptionParliament[round5.small$CorruptionParliament==0] <- 1

# l) CorruptionOfficials:
# Question Number: Q60C
# Question: How many of the following people do you think are involved in corruption, or haven‟t you heard enough about them to say: Government Officials?
# Variable Label: Corruption: government officials
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don‟t know, 998=Refused to answer,
# -1=Missing.
# summary(round5.small$CorruptionOfficials)
#CorruptionOfficials.missing <- round5.small[ which(round5.small$CorruptionOfficials==-1), ]
# 41 missings
# replace these with NA:
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==-1] <- NA
# CorruptionOfficials.notknown <- round5.small[ which(round5.small$CorruptionOfficials==9), ]
# 6072 don't know
# replace these with NA:
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==3] <- 4
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==2] <- 3
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==1] <- 2
round5.small$CorruptionOfficials[round5.small$CorruptionOfficials==0] <- 1

# m) CorruptionCouncilors:
# Question Number: Q60D
# **Question: How many of the following people do you think are involved in corruption, or haven‟t you heard enough about them to say: Local government councilors?
# Variable Label: Corruption: local government councilors
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don‟t know, 997=Not asked, 998=Refused to answer,
# -1=Missing
# summary(round5.small$CorruptionCouncilors)
#CorruptionCouncilors.missing <- round5.small[ which(round5.small$CorruptionCouncilors==-1), ]
# 2400 missings
# replace these with NA:
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==-1] <- NA
# CorruptionCouncilors.notknown <- round5.small[ which(round5.small$CorruptionCouncilors==9), ]
# 5324 don't knows
# replace these with NA:
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==3] <- 4
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==2] <- 3
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==1] <- 2
round5.small$CorruptionCouncilors[round5.small$CorruptionCouncilors==0] <- 1

# n) CorruptionTax:
# Question Number: Q60F
# **Question: How many of the following people do you think are involved in corruption, or haven‟t you heard enough about them to say: Tax Officials (e.g. Ministry of Finance officials or Local Government tax collectors)
# Variable Label: Corruption: tax officials
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don‟t know, 998=Refused to answer,
# -1=Missing
# summary(round5.small$CorruptionTax)
#CorruptionTax.missing <- round5.small[ which(round5.small$CorruptionTax==-1), ]
# 28 missings
# replace these with NA:
round5.small$CorruptionTax[round5.small$CorruptionTax==-1] <- NA
# CorruptionTax.notknown <- round5.small[ which(round5.small$CorruptionTax==9), ]
# 8237 don't knows
# replace these with NA:
round5.small$CorruptionTax[round5.small$CorruptionTax==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$CorruptionTax[round5.small$CorruptionTax==3] <- 4
round5.small$CorruptionTax[round5.small$CorruptionTax==2] <- 3
round5.small$CorruptionTax[round5.small$CorruptionTax==1] <- 2
round5.small$CorruptionTax[round5.small$CorruptionTax==0] <- 1

# o) AvoidHowOften:
# Question Number: Q56I
# Question: In your opinion, how often, in this country: Do people avoid paying the taxes that they owe the government?
# Variable Label: How often avoid paying taxes
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Never, 1=Rarely, 2=Often, 3=Always, 9=Don‟t know, 998=Refused to answer, -1=Missing
# summary(round5.small$AvoidHowOften)
#AvoidHowOften.missing <- round5.small[ which(round5.small$AvoidHowOften==-1), ]
# 33 missings
# replace these with NA:
round5.small$AvoidHowOften[round5.small$AvoidHowOften==-1] <- NA
# AvoidHowOften.notknown <- round5.small[ which(round5.small$AvoidHowOften==9), ]
# 6210 don't knows
# replace these with NA:
round5.small$AvoidHowOften[round5.small$AvoidHowOften==9] <- NA
#AvoidHowOften.refused <- round5.small[ which(round5.small$AvoidHowOften==998), ]
# 0 refused

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$AvoidHowOften[round5.small$AvoidHowOften==3] <- 4
round5.small$AvoidHowOften[round5.small$AvoidHowOften==2] <- 3
round5.small$AvoidHowOften[round5.small$AvoidHowOften==1] <- 2
round5.small$AvoidHowOften[round5.small$AvoidHowOften==0] <- 1

# p) TrustTax:
# Question Number: Q59D
# **Question: How much do you trust each of the following, or haven‟t you heard enough about them to say: The [Tax Department]?
# Variable Label: Trust tax department
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don‟t know/Haven‟t heard enough, 998=Refused to answer, -1=Missing
# summary(round5.small$AvoidHowOften)
#TrustTax.missing <- round5.small[ which(round5.small$TrustTax==-1), ]
# 79 missings
# replace these with NA:
round5.small$TrustTax[round5.small$TrustTax==-1] <- NA
#TrustTax.notknown <- round5.small[ which(round5.small$TrustTax==9), ]
# 5829 don't knows
# replace these with NA:
round5.small$TrustTax[round5.small$TrustTax==-9] <- NA
#TrustTax.refused <- round5.small[ which(round5.small$TrustTax==998), ]
# 0 refused

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round5.small$TrustTax[round5.small$TrustTax==3] <- 4
round5.small$TrustTax[round5.small$TrustTax==2] <- 3
round5.small$TrustTax[round5.small$TrustTax==1] <- 2
round5.small$TrustTax[round5.small$TrustTax==0] <- 1


# q) SelfEmployedTax:
# Question Number: Q73E
# Question: Regardless of whether you are able to pay them, are you required to pay each of the following, or haven‟t you been able to find out about this: If you are self employed, are you required to pay a tax on the earnings from your business or job?
# Variable Label: Payments required: self-employer taxes
# Values: 0, 1, 7, 9, 998, -1
# Value Labels: 0= No, I am not required to pay, 1= Yes, I am required to pay, 7= Not applicable, 9= Don‟t know / Haven‟t had a chance to find out, 997=Not asked, 998=Refused to answer, -1=Missing
# summary(round5.small$SelfEmployedTax)
#SelfEmployedTax.missing <- round5.small[ which(round5.small$SelfEmployedTax==-1), ]
# 14 missings
# replace these with NA:
round5.small$SelfEmployedTax[round5.small$SelfEmployedTax==-1] <- NA
#SelfEmployedTax.notapplicable <- round5.small[ which(round5.small$SelfEmployedTax==7), ]
# 21998 not applicable
# replace these with NA:
round5.small$SelfEmployedTax[round5.small$SelfEmployedTax==7] <- NA
# SelfEmployedTax.notknown <- round5.small[ which(round5.small$SelfEmployedTax==9), ]
# 2024 don't knows
# replace these with NA:
round5.small$SelfEmployedTax[round5.small$SelfEmployedTax==9] <- NA

# r) AvoidReason:
# Question Number: Q77
# Question: What do you think is the main reason that some people avoid paying government the taxes and fees that they owe?
# Variable Label: Why avoid paying taxes
# **Values: 0-10, 661, 780, 1622, 9995, 9997, 9998, 9999, -1
# **Value Labels: 0=People don't avoid paying, 1=The tax system is unfair, 2=The taxes are too high, 3=People cannot afford to pay, 4=The poor services they receive from government, 5=Government does not listen to them, 6=Government wastes tax money, 7=Government officials steal tax money, 8=They know they will not be caught, 9=Greed / selfishness, 10=Ignorance, don't know how to pay or don‟t understand need to pay, 661=Negligence, 780=Government stopped people from paying the tax(s), 1622=Employers don't deduct or don't give to government, 9995=Other, 9997=Not asked, 9998=Refused, 9999=Don't know, -1=Missing
# * Not asked in ALG, EGY, MRC, SUD, TUN
summary(round5.small$AvoidReason)
#AvoidReason.missing <- round5.small[ which(round5.small$AvoidReason==-1), ]
# 39 missings
# replace these with NA:
round5.small$AvoidReason[round5.small$AvoidReason==-1] <- NA
#AvoidReason.notasked <- round5.small[ which(round5.small$AvoidReason==9997), ]
# 0 notasked
#AvoidReason.refused <- round5.small[ which(round5.small$AvoidReason==9998), ]
# 30 notasked
# replace these with NA:
round5.small$AvoidReason[round5.small$AvoidReason==9998] <- NA
#AvoidReason.notknown <- round5.small[ which(round5.small$AvoidReason==9999), ]
# 4425 not known
# replace these with NA:
round5.small$AvoidReason[round5.small$AvoidReason==9999] <- NA

# Recode the following values: 
# 661=Negligence to 11:
round5.small$AvoidReason[round5.small$AvoidReason==661] <- 11
# 780=Government stopped people from paying the tax(s) to 12:
round5.small$AvoidReason[round5.small$AvoidReason==780] <- 12
# 1622=Employers don't deduct or don't give to government
round5.small$AvoidReason[round5.small$AvoidReason==1622] <- 13
# 9995=Other reasons to 14:
round5.small$AvoidReason[round5.small$AvoidReason==9995] <- 14


# s) EconomicPolicies:
# missing for this year

# save version without factor variables:
round5.nofactors <- round5.small 

# export data into csv format:
rio::export(round5.nofactors, file="round5.nofactors.csv")

# save smaller data set in Rdata format:
save(round5.nofactors, file="Round5.nofactors.RData", list="round5.nofactors")

# FACTOR VARIABLES:
# At the moment R recognizes most of the variables as numeric variables even
# though they are factor variables.
# Therefore these will now be coded as factors:

#library(dplyr)
round5.small <- mutate(round5.small,
                       Year = factor(Year),
                       UrbanRural = factor(UrbanRural, levels = 1:2, labels = c("Urban", "Rural")),
                       EconomicSituation = factor(EconomicSituation, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       LivingConditions = factor(LivingConditions, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       Interest = factor(Interest, levels = 1:4, labels = c("Not at all interested", "Not very interested", "Somewhat interested", "Very interested")),
                       Religion = factor(Religion, levels = 1:4, labels = c("Not a member", "Inactive member", "Active member", "Official Leader")),
                       TaxMorale = factor(TaxMorale, levels = 1:5, labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
                       TrustPresident = factor(TrustPresident, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustParliament = factor(TrustParliament, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustCourts = factor(TrustCourts, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       CorruptionPresident = factor(CorruptionPresident, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionParliament = factor(CorruptionParliament, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficials = factor(CorruptionOfficials, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionCouncilors = factor(CorruptionCouncilors, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionTax = factor(CorruptionTax, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       Gender = factor(Gender, levels = 1:2, labels = c("male", "female")),
                       AvoidHowOften = factor(AvoidHowOften, levels = 1:4, labels = c("Never", "Rarely", "Often", "Allways")),
                       TrustTax = factor(TrustTax, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       SelfEmployedTax = factor(SelfEmployedTax, levels = 0:1, labels = c("Not required to pay", "Required to pay")),
                       AvoidReason = factor(AvoidReason, levels = 0:14, labels = c("People don't avoid paying", "The tax system is unfair", "The taxes are too high",
                                                                                   "People cannot afford to pay", "The poor services they receive from government", 
                                                                                   "Government does not listen to them", "Government wastes tax money", "Government officials steal tax money",
                                                                                   "They know they will not be caught", "Greed/selfishness", "Ignorance", "Negligence", 
                                                                                   "Government stopped people from paying", "Employers don't deduct", "Other reasons")),
                       EconomicPolicies = factor(EconomicPolicies, levels = 1:4, labels = c("Agree very strongly with Statement1", "Agree with Statement1", "Agree with Statement2", "Agree very strongly with Statement2")),
                       LocalGvtTaxes = factor(LocalGvtTaxes, levels = 1:4, labels = c("Very Badly","Fairly Badly","Fairly Well","Very Well")),
                       Enforce1 = factor(Enforce1, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       Enforce2 = factor(Enforce2, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       CorruptionOfficialsLocal = factor(CorruptionOfficialsLocal, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficialsNational = factor(CorruptionOfficialsNational, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them"))
)

#EXPORTING AND SAVING DATA:

round5.final <- round5.small 

# export data into csv format:
rio::export(round5.final, file="round5.final.csv")

# save final data set in Rdata format:
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
round4.small$Respondent <- paste0(round4.small$Respondent, '_', round4.small$Year)

# turn into string variable:
round4.small$Respondent <- sapply(round4.small$Respondent, as.character)


# MISSINGS:
# at the moment R only recognizes missings, when a question was not asked in one
# or several countries
# according to the code books, missings are coded as "-1" which is currently
# recognized by R as a value

# RECODING OF VARIABLES INTO "NA" WHEN APPROPRIATE

summary(round4.small) 
### shows that there are "-1" and therefore missings in all of the variables except
### for Country, Year, Respondent, UrbanRural, and Gender
### The variables AvoidHowOften, TrustTax, SelfEmployedTax, AvoidReason, 
### LocalGvtTaxes, Enforce1, Enforce2, CorruptionOfficialsLocal and 
### CorruptionOfficialsNational don't contain the value "-1" but have been coded 
### as containing only missings (NA)

# a) Age:
# Question Number: Q1
# Question: How old are you?
# Variable Label: Age
# Values: 18-110, 998-999, -1
# Value Labels: 998=Refused to answer, 999=Don’t know, -1=Missing data
# summary(round4.small$Age) 
# age.missing <- round4.small[ which(round4.small$Age==-1), ]
# 6 missings
# replace these with NA:
round4.small$Age[round4.small$Age==-1] <- NA 
# age.refuse <- round4.small[ which(round4.small$Age==998), ]
# 8 refuse
# replace these with NA:
round4.small$Age[round4.small$Age==998] <- NA 
# age.notknown <- round4.small[ which(round4.small$Age==999), ]
# 319 not known
# replace these with NA:
round4.small$Age[round4.small$Age==999] <- NA 

# b) EconomicSituation:
# Question Number: Q4A
# Question: In general, how would you describe: The present economic condition of this country?
# Variable Label: Country’s present economic condition
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$EconomicSituation) 
#EconomicSituation.missing <- round4.small[ which(round4.small$EconomicSituation==-1), ]
# 4 missings
# replace these with NA:
round4.small$EconomicSituation[round4.small$EconomicSituation==-1] <- NA
# EconomicSituation.notknown <- round4.small[ which(round4.small$EconomicSituation==9), ]
# 548 don't know
# replace these with NA:
round4.small$EconomicSituation[round4.small$EconomicSituation==9] <- NA

# c) LivingConditions:
# Question Number: Q4B
# Question: In general, how would you describe: Your own present living conditions?
# Variable Label: Your present living conditions
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t know,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$LivingConditions)
#LivingConditions.missing <- round4.small[ which(round4.small$LivingConditions==-1), ]
# 25 missings
# replace these with NA:
round4.small$LivingConditions[round4.small$LivingConditions==-1] <- NA
# LivingConditions.notknown <- round4.small[ which(round4.small$LivingConditions==9), ]
# 125 don't know
# replace these with NA:
round4.small$LivingConditions[round4.small$LivingConditions==9] <- NA

# d) Interest:
# Question Number: Q13
# Question: How interested would you say you are in public affairs?
# Variable Label: Interest in public affairs
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t
# know, 998=Refused to answer, -1=Missing data
# summary(round4.small$Interest)
#Interest.missing <- round4.small[ which(round4.small$Interest==-1), ]
# 7 missings
# replace these with NA:
round4.small$Interest[round4.small$Interest==-1] <- NA
# Interest.notknown <- round4.small[ which(round4.small$Interest==9), ]
# 260 don't know
# replace these with NA:
round4.small$Interest[round4.small$Interest==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$Interest[round4.small$Interest==3] <- 4
round4.small$Interest[round4.small$Interest==2] <- 3
round4.small$Interest[round4.small$Interest==1] <- 2
round4.small$Interest[round4.small$Interest==0] <- 1

# e) Religion:
# Question Number: Q22A
# Question: Let’s turn to your role in the community. Now I am going to read out a list of groups that people join or
# attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or
# not a member: A religious group (e.g., church, mosque)?
# Variable Label: Member of religious group
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not a Member, 1=Inactive member, 2=Active member, 3=Official leader, 9=Don’t know,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$Religion)
#Religion.missing <- round4.small[ which(round4.small$Religion==-1), ]
# 18 missings
# replace these with NA:
round4.small$Religion[round4.small$Religion==-1] <- NA
# Religion.notknown <- round4.small[ which(round4.small$Religion==9), ]
# 94 don't know
# replace these with NA:
round4.small$Religion[round4.small$Religion==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$Religion[round4.small$Religion==3] <- 4
round4.small$Religion[round4.small$Religion==2] <- 3
round4.small$Religion[round4.small$Religion==1] <- 2
round4.small$Religion[round4.small$Religion==0] <- 1


# f) TaxMorale:
# Question Number: Q44C
# Question: For each of the following statements, please tell me whether you disagree or agree: The tax department
# always has the right to make people pay taxes.
# Variable Label: People must pay taxes
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Strongly disagree, 2=Disagree, 3=Neither agree nor disagree, 4=Agree, 5=Strongly agree, 9=Don’t
# know, 998=Refused to answer, -1=Missing data
# summary(round4.small$TaxMorale)
#TaxMorale.missing <- round4.small[ which(round4.small$TaxMorale==-1), ]
# 19 missings
# replace these with NA:
round4.small$TaxMorale[round4.small$TaxMorale==-1] <- NA
# TaxMorale.notknown <- round4.small[ which(round4.small$TaxMorale==9), ]
# 1854 don't know
# replace these with NA:
round4.small$TaxMorale[round4.small$TaxMorale==9] <- NA

# g) TrustPresident:
# Question Number: Q49A
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say: The
# President?
# Variable Label: Trust president
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t know/Haven’t heard enough,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$TrustPresident)
#TrustPresident.missing <- round4.small[ which(round4.small$TrustPresident==-1), ]
# 15 missings
# replace these with NA:
round4.small$TrustPresident[round4.small$TrustPresident==-1] <- NA
# TrustPresident.notknown <- round4.small[ which(round4.small$TrustPresident==9), ]
# 1556 don't know
# replace these with NA:
round4.small$TrustPresident[round4.small$TrustPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$TrustPresident[round4.small$TrustPresident==3] <- 4
round4.small$TrustPresident[round4.small$TrustPresident==2] <- 3
round4.small$TrustPresident[round4.small$TrustPresident==1] <- 2
round4.small$TrustPresident[round4.small$TrustPresident==0] <- 1

# h) TrustParliament:
# Question Number: Q49B
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say:
# Parliament?
# Variable Label: Trust parliament/national assembly
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t know/Haven’t heard enough,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$TrustParliament)
#TrustParliament.missing <- round4.small[ which(round4.small$TrustParliament==-1), ]
# 16 missings
# replace these with NA:
round4.small$TrustParliament[round4.small$TrustParliament==-1] <- NA
# TrustParliament.notknown <- round4.small[ which(round4.small$TrustParliament==9), ]
# 2091 don't know
# replace these with NA:
round4.small$TrustParliament[round4.small$TrustParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$TrustParliament[round4.small$TrustParliament==3] <- 4
round4.small$TrustParliament[round4.small$TrustParliament==2] <- 3
round4.small$TrustParliament[round4.small$TrustParliament==1] <- 2
round4.small$TrustParliament[round4.small$TrustParliament==0] <- 1


# i) TrustCourts:
# Question Number: Q49H
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say: Courts of
# law?
# Variable Label: Trust courts of law
# Values: 0-3, 9, 998, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t know/Haven’t heard enough,
# 998=Refused to answer, -1=Missing data
# summary(round4.small$TrustCourts)
#TrustCourts.missing <- round4.small[ which(round4.small$TrustCourts==-1), ]
# 16 missings
# replace these with NA:
round4.small$TrustCourts[round4.small$TrustCourts==-1] <- NA
# TrustCourts.notknown <- round4.small[ which(round4.small$TrustCourts==9), ]
# 1590 don't know
# replace these with NA:
round4.small$TrustCourts[round4.small$TrustCourts==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$TrustCourts[round4.small$TrustCourts==3] <- 4
round4.small$TrustCourts[round4.small$TrustCourts==2] <- 3
round4.small$TrustCourts[round4.small$TrustCourts==1] <- 2
round4.small$TrustCourts[round4.small$TrustCourts==0] <- 1

# j) CorruptionPresident:
# Question Number: Q50A
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: The President and Officials in his/her Office?
# Variable Label: Corruption: office of the Presidency
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t know, 998=Refused to answer,
# -1=Missing data
# summary(round4.small$CorruptionPresident)
#CorruptionPresident.missing <- round4.small[ which(round4.small$CorruptionPresident==-1), ]
# 10 missings
# replace these with NA:
round4.small$CorruptionPresident[round4.small$CorruptionPresident==-1] <- NA
# CorruptionPresident.notknown <- round4.small[ which(round4.small$CorruptionPresident==9), ]
# 6130 don't know
# replace these with NA:
round4.small$CorruptionPresident[round4.small$CorruptionPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$CorruptionPresident[round4.small$CorruptionPresident==3] <- 4
round4.small$CorruptionPresident[round4.small$CorruptionPresident==2] <- 3
round4.small$CorruptionPresident[round4.small$CorruptionPresident==1] <- 2
round4.small$CorruptionPresident[round4.small$CorruptionPresident==0] <- 1


# k) CorruptionParliament:
# Question Number: Q50B
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Members of Parliament?
# Variable Label: Corruption: Members of Parliament
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t know, 998=Refused to answer,
# -1=Missing data.
# summary(round4.small$CorruptionParliament)
#CorruptionParliament.missing <- round4.small[ which(round4.small$CorruptionParliament==-1), ]
# 13 missings
# replace these with NA:
round4.small$CorruptionParliament[round4.small$CorruptionParliament==-1] <- NA
# CorruptionParliament.notknown <- round4.small[ which(round4.small$CorruptionParliament==9), ]
# 5411 don't know
# replace these with NA:
round4.small$CorruptionParliament[round4.small$CorruptionParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$CorruptionParliament[round4.small$CorruptionParliament==3] <- 4
round4.small$CorruptionParliament[round4.small$CorruptionParliament==2] <- 3
round4.small$CorruptionParliament[round4.small$CorruptionParliament==1] <- 2
round4.small$CorruptionParliament[round4.small$CorruptionParliament==0] <- 1


# l) CorruptionOfficials:
# Question Number: Q50D
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Government Officials?
# summary(round4.small$CorruptionOfficials)
# Variable Label: Corruption: government officials
#CorruptionOfficials.missing <- round4.small[ which(round4.small$CorruptionOfficials==-1), ]
# 23 missings
# replace these with NA:
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==-1] <- NA
# CorruptionOfficials.notknown <- round4.small[ which(round4.small$CorruptionOfficials==9), ]
# 4612 don't know
# replace these with NA:
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==3] <- 4
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==2] <- 3
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==1] <- 2
round4.small$CorruptionOfficials[round4.small$CorruptionOfficials==0] <- 1


# m) CorruptionCouncilors:
# Question Number: Q50C
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Elected Assembly men/women?
# Variable Label: Corruption: local government councilors
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t know, 998=Refused to answer,
# -1=Missing data
# summary(round4.small$CorruptionCouncilors)
#CorruptionCouncilors.missing <- round4.small[ which(round4.small$CorruptionCouncilors==-1), ]
# 22 missings
# replace these with NA:
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==-1] <- NA
# CorruptionCouncilors.notknown <- round4.small[ which(round4.small$CorruptionCouncilors==9), ]
# 4783 don't know
# replace these with NA:
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==3] <- 4
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==2] <- 3
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==1] <- 2
round4.small$CorruptionCouncilors[round4.small$CorruptionCouncilors==0] <- 1


# n) CorruptionTax:
# Question Number: Q50F
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Tax Officials (e.g. Ministry of Finance officials or Local Government tax collectors)
# Variable Label: Corruption: tax officials
# Values: 0-3, 9, 998, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t know, 998=Refused to answer,
# -1=Missing data
# summary(round4.small$CorruptionTax)
#CorruptionTax.missing <- round4.small[ which(round4.small$CorruptionTax==-1), ]
# 14 missings
# replace these with NA:
round4.small$CorruptionTax[round4.small$CorruptionTax==-1] <- NA
# CorruptionTax.notknown <- round4.small[ which(round4.small$CorruptionTax==9), ]
# 5575 don't know
# replace these with NA:
round4.small$CorruptionTax[round4.small$CorruptionTax==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round4.small$CorruptionTax[round4.small$CorruptionTax==3] <- 4
round4.small$CorruptionTax[round4.small$CorruptionTax==2] <- 3
round4.small$CorruptionTax[round4.small$CorruptionTax==1] <- 2
round4.small$CorruptionTax[round4.small$CorruptionTax==0] <- 1


# o) AvoidHowOften: not included in this Round

# p) TrustTax: not included in this Round

# q) SelfEmployedTax: not included in this Round

# r) AvoidReason: not included in this Round

# s) EconomicPolicies:
# Question Number: Q11
# Question: Which of the following statements is closest to your view? Choose Statement 1 or Statement 2.
# Statement 1: The government’s economic policies have helped most people; only a few have suffered.
# Statement 2: The government’s economic policies have hurt most people and only benefited a few.
# Variable Label: Economic policies helped most vs. hurt most
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Agree very strongly with Statement 1, 2=Agree with Statement 1, 3=Agree with Statement 2,
# 4=Agree very strongly with Statement 2, 5=Agree with neither, 9=Don’t know, 998=Refused to answer, -1=Missing
# data
summary(round4.small$EconomicPolicies)
#EconomicPolicies.missing <- round4.small[ which(round4.small$EconomicPolicies==-1), ]
# 6 missings
# replace these with NA:
round4.small$EconomicPolicies[round4.small$EconomicPolicies==-1] <- NA
# EconomicPolicies.notknown <- round4.small[ which(round4.small$EconomicPolicies==9), ]
# 1019 don't know
# replace these with NA:
round4.small$EconomicPolicies[round4.small$EconomicPolicies==9] <- NA
#EconomicPolicies.agreewneither <- round4.small[ which(round4.small$EconomicPolicies==5), ]
# 604 agree with neither
# replace these with NA:
round4.small$EconomicPolicies[round4.small$EconomicPolicies==5] <- NA

# save version without factor variables:
round4.nofactors <- round4.small 

# export data into csv format:
rio::export(round4.nofactors, file="round4.nofactors.csv")

# save smaller data set in Rdata format:
save(round4.nofactors, file="Round4.nofactors.RData", list="round4.nofactors")

# FACTOR VARIABLES:
# At the moment R recognizes most of the variables as numeric variables even
# though they are factor variables.
# Therefore these will now be coded as factors:

#library(dplyr)
round4.small <- mutate(round4.small,
                       Year = factor(Year),
                       UrbanRural = factor(UrbanRural, levels = 1:2, labels = c("Urban", "Rural")),
                       EconomicSituation = factor(EconomicSituation, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       LivingConditions = factor(LivingConditions, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       Interest = factor(Interest, levels = 1:4, labels = c("Not at all interested", "Not very interested", "Somewhat interested", "Very interested")),
                       Religion = factor(Religion, levels = 1:4, labels = c("Not a member", "Inactive member", "Active member", "Official Leader")),
                       TaxMorale = factor(TaxMorale, levels = 1:5, labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
                       TrustPresident = factor(TrustPresident, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustParliament = factor(TrustParliament, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustCourts = factor(TrustCourts, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       CorruptionPresident = factor(CorruptionPresident, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionParliament = factor(CorruptionParliament, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficials = factor(CorruptionOfficials, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionCouncilors = factor(CorruptionCouncilors, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionTax = factor(CorruptionTax, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       Gender = factor(Gender, levels = 1:2, labels = c("male", "female")),
                       AvoidHowOften = factor(AvoidHowOften, levels = 1:4, labels = c("Never", "Rarely", "Often", "Allways")),
                       TrustTax = factor(TrustTax, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       SelfEmployedTax = factor(SelfEmployedTax, levels = 0:1, labels = c("Not required to pay", "Required to pay")),
                       AvoidReason = factor(AvoidReason, levels = 0:14, labels = c("People don't avoid paying", "The tax system is unfair", "The taxes are too high",
                                                                                   "People cannot afford to pay", "The poor services they receive from government", 
                                                                                   "Government does not listen to them", "Government wastes tax money", "Government officials steal tax money",
                                                                                   "They know they will not be caught", "Greed/selfishness", "Ignorance", "Negligence", 
                                                                                   "Government stopped people from paying", "Employers don't deduct", "Other reasons")),
                       EconomicPolicies = factor(EconomicPolicies, levels = 1:4, labels = c("Agree very strongly with Statement1", "Agree with Statement1", "Agree with Statement2", "Agree very strongly with Statement2")),
                       LocalGvtTaxes = factor(LocalGvtTaxes, levels = 1:4, labels = c("Very Badly","Fairly Badly","Fairly Well","Very Well")),
                       Enforce1 = factor(Enforce1, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       Enforce2 = factor(Enforce2, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       CorruptionOfficialsLocal = factor(CorruptionOfficialsLocal, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficialsNational = factor(CorruptionOfficialsNational, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them"))
)


#EXPORTING AND SAVING DATA:

round4.final <- round4.small 

# export data into csv format:
rio::export(round4.small, file="round3.small.csv")

# save final data set in Rdata format:
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
round3.small$Respondent <- paste0(round3.small$Respondent, '_', round3.small$Year)
 
# additionally: the abbreviation of Malawi is MLW in round 4 and 5, but MWI in
# round 3, therefore this has to be changed to MLW for round 3:
round3.small$Respondent <- sapply(round3.small$Respondent, as.character)
MLW <- round3.small$Respondent
round3.small$Respondent <- gsub(pattern = 'MWI', replacement = 'MLW', x = MLW)


# MISSINGS:
# at the moment R only recognizes missings, when a question was not asked in one
# or several countries
# according to the code books, missings are coded as "-1" which is currently
# recognized by R as a value

# Step 1: Recoding of values

summary(round3.small) 
### shows that there are "-1" and therefore missings in all of the variables except
### for Country, Year, Respondent, UrbanRural, Religion and Gender
### The variables CorruptionOfficials, AvoidHowOften, TrustTax, SelfEmployedTax, 
### and AvoidReason don't contain the value "-1" but have been coded as containing 
### only missings (NA)

# a) Age:
# Question Number: Q1
# Question: How old are you?
# Variable Label: Age
# Values: 18-110, 998, 999, -1
# Value Labels: 998=Refused to Answer, 999=Don’t Know, -1=Missing Data
# summary(round3.small$Age)
# age.notknown <- round3.small[ which(round3.small$Age==999), ]
# 272 don't know
# replace these with NA:
round3.small$Age[round3.small$Age==999] <- NA 
# age.refused <- round3.small[ which(round3.small$Age==998), ]
# 1 refused
# replace these with NA
round3.small$Age[round3.small$Age==998] <- NA
#age.missing <- round3.small[ which(round3.small$Age==-1), ]
# 14 missings
# replace these with NA:
round3.small$Age[round3.small$Age==-1] <- NA 

# b) EconomicSituation:
# Question Number: Q4A
# Question: In general, how would you describe: The present economic conditions of this country?
# Variable Label: Country’s present economic condition
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t Know,
# 998=Refused to Answer, -1=Missing Data
# summary(round3.small$EconomicSituation)
#EconomicSituation.missing <- round3.small[ which(round3.small$EconomicSituation==-1), ]
# 16 missings
# replace these with NA:
round3.small$EconomicSituation[round3.small$EconomicSituation==-1] <- NA
# EconomicSituation.notknown <- round3.small[ which(round3.small$EconomicSituation==9), ]
# 272 don't know
# replace these with NA:
round3.small$EconomicSituation[round3.small$EconomicSituation==9] <- NA

# c) LivingConditions:
# Question Number: Q4B
# Question: In general, how would you describe: Your own present living conditions?
# Variable Label: Your present living conditions
# Values: 1-5, 9, 998, -1
# Value Labels: 1=Very bad, 2=Fairly bad, 3=Neither good nor bad, 4=Fairly good, 5=Very good, 9=Don’t Know,
# 998=Refused to Answer, -1=Missing Data
# summary(round3.small$LivingConditions)
#LivingConditions.missing <- round3.small[ which(round3.small$LivingConditions==-1), ]
# 10 missings
# replace these with NA:
round3.small$LivingConditions[round3.small$LivingConditions==-1] <- NA
# LivingConditions.notknown <- round3.small[ which(round3.small$LivingConditions==9), ]
# 79 don't know
# replace these with NA:
round3.small$LivingConditions[round3.small$LivingConditions==9] <- NA


# d) Interest:
# Question Number: Q16
# Question: How interested would you say you are in public affairs?
# Variable Label: Interest in public affairs
# Values: 0-3, 9, 98, -1
# Value Labels: 0=Not at all interested, 1=Not very interested, 2=Somewhat interested, 3=Very interested, 9=Don’t
# Know, 98=Refused to Answer, -1=Missing Data
# summary(round3.small$Interest)
# Interest.missing <- round3.small[ which(round3.small$Interest==-1), ]
# 5 missings
# replace these with NA:
round3.small$Interest[round3.small$Interest==-1] <- NA
# Interest.notknown <- round3.small[ which(round3.small$Interest==9), ]
# 278 don't know
# replace these with NA:
round3.small$Interest[round3.small$Interest==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$Interest[round3.small$Interest==3] <- 4
round3.small$Interest[round3.small$Interest==2] <- 3
round3.small$Interest[round3.small$Interest==1] <- 2
round3.small$Interest[round3.small$Interest==0] <- 1


#e) Religion:
#Question Number: Q28A
# Question: Let’s turn to your role in the community. Now I am going to read out a list of groups that people join or
# attend. For each one, could you tell me whether you are an official leader, an active member, an inactive member, or
# not a member: A religious group (e.g. church, mosque)?
# Variable Label: Member of religious group
# Values: 0-3, 9, 98, -1
# Value Labels: 0=Not a Member, 1=Inactive Member, 2=Active Member, 3=Official Leader, 9=Don’t Know,
# 98=Refused to Answer, -1=Missing Data
# summary(round3.small$Religion)
# Religion.notknown <- round3.small[ which(round3.small$Religion==9), ]
# 59 don't know
# replace these with NA:
round3.small$Religion[round3.small$Interest==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$Religion[round3.small$Religion==3] <- 4
round3.small$Religion[round3.small$Religion==2] <- 3
round3.small$Religion[round3.small$Religion==1] <- 2
round3.small$Religion[round3.small$Religion==0] <- 1


# f) TaxMorale:
# Question Number: Q52D
#Question: For each of the following statements, please tell me whether you disagree or agree: The tax department
#always has the right to make people pay taxes.
#Variable Label: People must pay taxes
#Values: 1-5, 9, 98, -1
#Value Labels: 1=Strongly Disagree, 2=Disagree, 3=Neither Agree Nor Disagree, 4=Agree, 5=Strongly Agree,
#9=Don’t Know, 98=Refused to Answer, -1=Missing Data
#Source: Afrobarometer Round 2
#summary(round3.small$TaxMorale)
# TaxMorale.missing <- round3.small[ which(round3.small$TaxMorale==-1), ]
# 3 missings
# replace these with NA:
round3.small$TaxMorale[round3.small$TaxMorale==-1] <- NA
# TaxMorale.notknown <- round3.small[ which(round3.small$TaxMorale==9), ]
# 2338 don't know
# replace these with NA:
round3.small$TaxMorale[round3.small$TaxMorale==9] <- NA

# g) TrustPresident:
# Question Number: Q55A
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say: The
# President/Prime Minister?
# Variable Label: Trust the President
# Values: 0-3, 9, 98, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t Know/Haven’t Heard Enough,
# 98=Refused to Answer, -1=Missing Data
# summary(round3.small$TrustPresident)
#TrustPresident.missing <- round3.small[ which(round3.small$TrustPresident==-1), ]
# 1 missing
# replace these with NA:
round3.small$TrustPresident[round3.small$TrustPresident==-1] <- NA
# TrustPresident.notknown <- round3.small[ which(round3.small$TrustPresident==9), ]
# 915 don't know
# replace these with NA:
round3.small$TrustPresident[round3.small$TrustPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$TrustPresident[round3.small$TrustPresident==3] <- 4
round3.small$TrustPresident[round3.small$TrustPresident==2] <- 3
round3.small$TrustPresident[round3.small$TrustPresident==1] <- 2
round3.small$TrustPresident[round3.small$TrustPresident==0] <- 1


# h) TrustParliament:
# Question Number: Q55B
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say: The
# Parliament/National Assembly?
# Variable Label: Trust Parliament/National Assembly
# Values: 0-3, 9, 98, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t Know/Haven’t Heard Enough,
# 98=Refused to Answer, -1=Missing Data
# summary(round3.small$TrustParliament)
#TrustParliament.missing <- round3.small[ which(round3.small$TrustParliament==-1), ]
# 2 missings
# replace these with NA:
round3.small$TrustParliament[round3.small$TrustParliament==-1] <- NA
# TrustParliament.notknown <- round3.small[ which(round3.small$TrustParliament==9), ]
# 1894 don't know
# replace these with NA:
round3.small$TrustParliament[round3.small$TrustParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$TrustParliament[round3.small$TrustParliament==3] <- 4
round3.small$TrustParliament[round3.small$TrustParliament==2] <- 3
round3.small$TrustParliament[round3.small$TrustParliament==1] <- 2
round3.small$TrustParliament[round3.small$TrustParliament==0] <- 1


# i) TrustCourts:
# Question Number: Q55I
# Question: How much do you trust each of the following, or haven’t you heard enough about them to say: Courts of
# Law?
# Variable Label: Trust courts of law
# Values: 0-3, 9, 98, -1
# Value Labels: 0=Not at all, 1=Just a little, 2=Somewhat, 3=A lot, 9=Don’t Know/Haven’t Heard Enough,
# 98=Refused to Answer, -1=Missing Data
# summary(round3.small$TrustCourts)
#TrustCourts.missing <- round3.small[ which(round3.small$TrustCourts==-1), ]
# 13 missings
# replace these with NA:
round3.small$TrustCourts[round3.small$TrustCourts==-1] <- NA
# TrustCourts.notknown <- round3.small[ which(round3.small$TrustCourts==9), ]
# 1489 don't know
# replace these with NA:
round3.small$TrustCourts[round3.small$TrustCourts==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$TrustCourts[round3.small$TrustCourts==3] <- 4
round3.small$TrustCourts[round3.small$TrustCourts==2] <- 3
round3.small$TrustCourts[round3.small$TrustCourts==1] <- 2
round3.small$TrustCourts[round3.small$TrustCourts==0] <- 1


# j) CorruptionPresident:
# Question Number: Q56A
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: The President/Prime Minister and Officials in his Office?
# Variable Label: Corruption: Office of the Presidency
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
# -1=Missing Data
# summary(round3.small$CorruptionPresident)
#CorruptionPresident.missing <- round3.small[ which(round3.small$CorruptionPresident==-1), ]
# 2 missings
# replace these with NA:
round3.small$CorruptionPresident[round3.small$CorruptionPresident==-1] <- NA
# CorruptionPresident.notknown <- round3.small[ which(round3.small$CorruptionPresident==9), ]
# 5710 don't know
# replace these with NA:
round3.small$CorruptionPresident[round3.small$CorruptionPresident==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionPresident[round3.small$CorruptionPresident==3] <- 4
round3.small$CorruptionPresident[round3.small$CorruptionPresident==2] <- 3
round3.small$CorruptionPresident[round3.small$CorruptionPresident==1] <- 2
round3.small$CorruptionPresident[round3.small$CorruptionPresident==0] <- 1


# k) CorruptionParliament:
# Question Number: Q56B
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Members of Parliament/National Assembly Representatives?
# Variable Label: Corruption: Members of Parliament
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
# -1=Missing Data
# summary(round3.small$CorruptionParliament)
#CorruptionParliament.missing <- round3.small[ which(round3.small$CorruptionParliament==-1), ]
# 2 missings
# replace these with NA:
round3.small$CorruptionParliament[round3.small$CorruptionParliament==-1] <- NA
# CorruptionParliament.notknown <- round3.small[ which(round3.small$CorruptionParliament==9), ]
# 5435 don't know
# replace these with NA:
round3.small$CorruptionParliament[round3.small$CorruptionParliament==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionParliament[round3.small$CorruptionParliament==3] <- 4
round3.small$CorruptionParliament[round3.small$CorruptionParliament==2] <- 3
round3.small$CorruptionParliament[round3.small$CorruptionParliament==1] <- 2
round3.small$CorruptionParliament[round3.small$CorruptionParliament==0] <- 1


#l) CorruptionOfficials: not included in this Round

# m) CorruptionCouncilors:
# Question Number: Q56C
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: elected local government councilors?
# Variable Label: Corruption: local government councilors
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
#-1=Missing Data
# summary(round3.small$CorruptionCouncilors)
#CorruptionCouncilors.missing <- round3.small[ which(round3.small$CorruptionCouncilors==-1), ]
# 3 missings
# replace these with NA:
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==-1] <- NA
# CorruptionCouncilors.notknown <- round3.small[ which(round3.small$CorruptionCouncilors==9), ]
# 4831 don't know
# replace these with NA:
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==-9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==3] <- 4
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==2] <- 3
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==1] <- 2
round3.small$CorruptionCouncilors[round3.small$CorruptionCouncilors==0] <- 1


# n) CorruptionTax:
# Question Number: Q56G
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Tax Officials (e.g. VATS/IRS officials)
# Variable Label: Corruption: Tax Officials
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
# -1=Missing Data
# summary(round3.small$CorruptionTax)
#CorruptionTax.missing <- round3.small[ which(round3.small$CorruptionTax==-1), ]
# 6 missings
# replace these with NA:
round3.small$CorruptionTax[round3.small$CorruptionTax==-1] <- NA
# CorruptionTax.notknown <- round3.small[ which(round3.small$CorruptionTax==9), ]
# 5698 don't know
# replace these with NA:
round3.small$CorruptionTax[round3.small$CorruptionTax==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionTax[round3.small$CorruptionTax==3] <- 4
round3.small$CorruptionTax[round3.small$CorruptionTax==2] <- 3
round3.small$CorruptionTax[round3.small$CorruptionTax==1] <- 2
round3.small$CorruptionTax[round3.small$CorruptionTax==0] <- 1


# o) AvoidHowOften: not included in this Round

# p) TrustTax: not included in this Round

# q) SelfEmployedTax: not included in this Round

# r) AvoidReason: not included in this Round

# s) EconomicPolicies:
# Question Number: Q13
# Question: Which of the following statements is closest to your view? Choose Statement A or Statement B.
# A: The government’s economic policies have helped most people; only a few have suffered.
# B: The government’s economic policies have hurt most people and only benefited a few.
# Variable Label: Economic policies helped most vs. hurt most
# Values: 1-5, 9, 98, -1
# Value Labels: 1=Agree Very Strongly with A, 2=Agree with A, 3=Agree with B, 4=Agree Very Strongly with B,
# 5=Agree with Neither, 9=Don’t Know, 98=Refused to Answer, -1=Missing Data
# summary(round3.small$EconomicPolicies)
#EconomicPolicies.missing <- round3.small[ which(round3.small$EconomicPolicies==-1), ]
# 2 missings
# replace these with NA:
round3.small$EconomicPolicies[round3.small$EconomicPolicies==-1] <- NA
# EconomicPolicies.notknown <- round3.small[ which(round3.small$EconomicPolicies==9), ]
# 833 don't know
# replace these with NA:
round3.small$EconomicPolicies[round3.small$EconomicPolicies==9] <- NA
# EconomicPolicies.agreewneither <- round3.small[ which(round3.small$EconomicPolicies==5), ]
#735 agree with neither
# replace these with NA:
round3.small$EconomicPolicies[round3.small$EconomicPolicies==5] <- NA

# t) LocalGvtTaxes:
# Question Number: Q67C
# Question: What about local government? How well or badly would you say your local government is handling the
# following matters, or haven’t you heard enough about them to say: Collecting Local Taxes?
# Variable Label: Local govt. handling collecting local taxes
# Values: 1-4, 9, 98, -1
# Value Labels: 1=Very Badly, 2=Fairly Badly, 3=Fairly Well, 4=Very Well, 9=Don’t Know, 98=Refused to
# Answer, -1=Missing Data
# summary(round3.small$LocalGvtTaxes)
#LocalGvtTaxes.missing <- round3.small[ which(round3.small$LocalGvtTaxes==-1), ]
# 9 missings
# replace these with NA:
round3.small$LocalGvtTaxes[round3.small$LocalGvtTaxes==-1] <- NA
# LocalGvtTaxes.notknown <- round3.small[ which(round3.small$LocalGvtTaxes==9), ]
# 5985 don't know
# replace these with NA:
round3.small$LocalGvtTaxes[round3.small$LocalGvtTaxes==-9] <- NA

# u) Enforce1:
# Question Number: Q70C
# Question: How likely do you think it would be that the authorities could enforce the law if a top official did not pay
# a tax on some of the income they earned?
# Variable Label: Enforce law: Top official doesn’t pay tax
# Values: 1-4, 9, 98, -1
# Value Labels: 1=Not at all likely, 2=Not very likely, 3=Likely, 4=Very Likely, 9=Don’t Know, 98=Refused to
# Answer, -1=Missing Data
# summary(round3.small$Enforce1)
# Enforce1.missing <- round3.small[ which(round3.small$Enforce1==-1), ]
# 6 missings
# replace these with NA:
round3.small$Enforce1[round3.small$Enforce1==-1] <- NA
# round3.sm.notknow <- round3.small[ which(round3.small$Enforce1==9), ]
# 1664 don't know
# replace these with NA:
round3.small$Enforce1[round3.small$Enforce1==9] <- NA

# v) Enforce2:
# Question Number: Q70D
# Question: How likely do you think it would be that the authorities could enforce the law if a person like you did not
# pay a tax on some of the income you earned?
# Variable Label: Enforce law: You don’t pay tax
# Values: 1-4, 9, 98, -1
# Value Labels: 1=Not at all likely, 2=Not very likely, 3=Likely, 4=Very Likely, 9=Don’t Know, 98=Refused to
# Answer, -1=Missing Data
#summary(round3.small$Enforce2)
# Enforce2.missing <- round3.small[ which(round3.small$Enforce2==-1), ]
# 7 missings
# replace these with NA:
round3.small$Enforce2[round3.small$Enforce2==-1] <- NA
# Enforce2.notknow <- round3.small[ which(round3.small$Enforce2==9), ]
# 1109 don't know
# replace these with NA:
round3.small$Enforce2[round3.small$Enforce2==-9] <- NA

# w) CorruptionOfficialsLocal:
# Question Number: Q56E
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: Local government officials?
# Variable Label: Corruption: Local government officials
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
# -1=Missing Data
# summary(round3.small$CorruptionOfficialsLocal)
# CorruptionOfficialsLocal.missing <- round3.small[ which(round3.small$CorruptionOfficialsLocal==-1), ]
# 4 missings
# replace these with NA:
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==-1] <- NA
# CorruptionOfficialsLocal.notknown <- round3.small[ which(round3.small$CorruptionOfficialsLocal==9), ]
# 5122 don't know
# replace these with NA:
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==3] <- 4
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==2] <- 3
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==1] <- 2
round3.small$CorruptionOfficialsLocal[round3.small$CorruptionOfficialsLocal==0] <- 1


# x) CorruptionOfficialsNational:
# Question Number: Q56D
# Question: How many of the following people do you think are involved in corruption, or haven’t you heard enough
# about them to say: National government officials?
# Variable Label: Corruption: National government officials
# Values: 0-3, 9, 98, -1
# Value Labels: 0=None, 1=Some of them, 2=Most of them, 3=All of them, 9=Don’t Know, 98=Refused to Answer,
# -1=Missing Data
#CorruptionOfficialsNational.missing <- round3.small[ which(round3.small$CorruptionOfficialsNational==-1), ]
# 5 missings
# replace these with NA:
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==-1] <- NA
# CorruptionOfficialsNational.notknown <- round3.small[ which(round3.small$CorruptionOfficialsNational==9), ]
# 5112 missings
# replace these with NA:
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==9] <- NA

# Recode so that the values range from 1 to 4 instead of 0 to 3:
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==3] <- 4
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==2] <- 3
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==1] <- 2
round3.small$CorruptionOfficialsNational[round3.small$CorruptionOfficialsNational==0] <- 1

# save version without factor variables:
round3.nofactors <- round3.small 

# export data into csv format:
rio::export(round3.nofactors, file="round3.nofactors.csv")

# save smaller data set in Rdata format:
save(round3.nofactors, file="Round3.nofactors.RData", list="round3.nofactors")

# FACTOR VARIABLES:
# At the moment R recognizes most of the variables as numeric variables even
# though they are factor variables.
# Therefore these will now be coded as factors:

#library(dplyr)
round3.small <- mutate(round3.small,
                       Year = factor(Year),
                       UrbanRural = factor(UrbanRural, levels = 1:2, labels = c("Urban", "Rural")),
                       EconomicSituation = factor(EconomicSituation, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       LivingConditions = factor(LivingConditions, levels = 1:5, labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good")),
                       Interest = factor(Interest, levels = 1:4, labels = c("Not at all interested", "Not very interested", "Somewhat interested", "Very interested")),
                       Religion = factor(Religion, levels = 1:4, labels = c("Not a member", "Inactive member", "Active member", "Official Leader")),
                       TaxMorale = factor(TaxMorale, levels = 1:5, labels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")),
                       TrustPresident = factor(TrustPresident, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustParliament = factor(TrustParliament, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       TrustCourts = factor(TrustCourts, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       CorruptionPresident = factor(CorruptionPresident, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionParliament = factor(CorruptionParliament, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficials = factor(CorruptionOfficials, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionCouncilors = factor(CorruptionCouncilors, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionTax = factor(CorruptionTax, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       Gender = factor(Gender, levels = 1:2, labels = c("male", "female")),
                       AvoidHowOften = factor(AvoidHowOften, levels = 1:4, labels = c("Never", "Rarely", "Often", "Allways")),
                       TrustTax = factor(TrustTax, levels = 1:4, labels = c("Not at all", "Just a little", "Somewhat", "A lot")),
                       SelfEmployedTax = factor(SelfEmployedTax, levels = 0:1, labels = c("Not required to pay", "Required to pay")),
                       AvoidReason = factor(AvoidReason, levels = 0:14, labels = c("People don't avoid paying", "The tax system is unfair", "The taxes are too high",
                                                                                   "People cannot afford to pay", "The poor services they receive from government", 
                                                                                   "Government does not listen to them", "Government wastes tax money", "Government officials steal tax money",
                                                                                   "They know they will not be caught", "Greed/selfishness", "Ignorance", "Negligence", 
                                                                                   "Government stopped people from paying", "Employers don't deduct", "Other reasons")),
                       EconomicPolicies = factor(EconomicPolicies, levels = 1:4, labels = c("Agree very strongly with Statement1", "Agree with Statement1", "Agree with Statement2", "Agree very strongly with Statement2")),
                       LocalGvtTaxes = factor(LocalGvtTaxes, levels = 1:4, labels = c("Very Badly","Fairly Badly","Fairly Well","Very Well")),
                       Enforce1 = factor(Enforce1, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       Enforce2 = factor(Enforce2, levels = 1:4, labels = c("Not at all likely", "Not very likely", "Likely", "Very Likely")),
                       CorruptionOfficialsLocal = factor(CorruptionOfficialsLocal, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them")),
                       CorruptionOfficialsNational = factor(CorruptionOfficialsNational, levels = 1:4, labels = c("None", "Some of them", "Most of them", "All of them"))
)

#EXPORTING AND SAVING DATA:

round3.final <- round3.small 

# export data into csv format:
rio::export(round3.final, file="round3.final.csv")

# save smaller data set in Rdata format:
save(round3.final, file="Round3.final.RData", list="round3.final")


# # # # # # # #
#   MERGING   #
# # # # # # # #


# for merging use rbind.fill (from plyr-package)
Afrobarometer.final <- rbind(round3.final, round4.final, round5.final)

# export merged data into csv format:
rio::export(Afrobarometer.final, file="Afrobarometer.final.csv")

# save smaller data set in Rdata format:
save(Afrobarometer.final, file="Afrobarometer.final.RData", list="Afrobarometer.final")



# merge version without factor variables:
Afrobarometer.nofactors <- rbind(round3.nofactors, round4.nofactors, round5.nofactors)

# export merged data into csv format:
rio::export(Afrobarometer.nofactors, file="Afrobarometer.nofactors.csv")

# save smaller data set in Rdata format:
save(Afrobarometer.nofactors, file="Afrobarometer.nofactors.RData", list="Afrobarometer.nofactors")
