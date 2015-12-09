##########################################
# Determinants of Tax Morale in Africa   #
# Models for ordered logistic regression #
# Jasmin Cantzler & Wiebke Weiger        #
# last updated: 7 December 2015          #
##########################################



# Set working directory
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Final Project")
#setwd("/Users/jasmincantzler/Documents/TaxMorale/Final Project")

# packages needed:
library(plyr)
library(dplyr)
library(MASS)
library(corrplot) 
library(nnet)
library(knitr)
library(stargazer)
library(tidyr)
library(foreign)
library(DataCombine)
library(ggplot2)
library(effects)


# data:
data.numeric <- read.csv('Afrobarometer.nofactors.csv')
data.factors <- read.csv('Afrobarometer.final.csv')
round5 <- read.csv('round5.final.csv')
round4 <- read.csv('round4.final.csv')
round3 <- read.csv('round3.final.csv')

##########################################################################################################
## Some preparations:
# First: Sort factor levles
# Second: Turn "Year" variable into factor
# Third: Recode dependent variable "TaxMorale" so that it has 3 instead of 5 levels

# First: Sort factor levels (they are currently in alphabetical order) for output to be more easily
# readable:

# Dependent Variable: tax morale:
levels(data.factors$TaxMorale) # gives the current order of the levels:
# [1] "Agree"                      "Disagree"                   "Neither agree nor disagree"
# [4] "Strongly Agree"             "Strongly Disagree"
# correct order would be: 5, 2, 3, 1, 4
# change order: 
data.factors$TaxMorale = factor(data.factors$TaxMorale,levels(data.factors$TaxMorale)[c(5,2,3,1,4)])
levels(data.factors$TaxMorale) # now order is correct:
# [1] "Strongly Disagree"          "Disagree"                   "Neither agree nor disagree"
# [4] "Agree"                      "Strongly Agree"

# same for round 5 only:
levels(round5$TaxMorale) # gives the current order of the levels:
# [1] "Agree"                      "Disagree"                   "Neither agree nor disagree"
# [4] "Strongly Agree"             "Strongly Disagree"
# correct order would be: 5, 2, 3, 1, 4
# change order: 
round5$TaxMorale = factor(round5$TaxMorale,levels(round5$TaxMorale)[c(5,2,3,1,4)])
levels(round5$TaxMorale) # now order is correct:
# [1] "Strongly Disagree"          "Disagree"                   "Neither agree nor disagree"
# [4] "Agree"                      "Strongly Agree"

# same for round 4 only:
levels(round4$TaxMorale) # gives the current order of the levels:
# [1] "Agree"                      "Disagree"                   "Neither agree nor disagree"
# [4] "Strongly Agree"             "Strongly Disagree"
# correct order would be: 5, 2, 3, 1, 4
# change order: 
round4$TaxMorale = factor(round4$TaxMorale,levels(round4$TaxMorale)[c(5,2,3,1,4)])
levels(round4$TaxMorale) # now order is correct:
# [1] "Strongly Disagree"          "Disagree"                   "Neither agree nor disagree"
# [4] "Agree"                      "Strongly Agree"

# same for round 3 only:
levels(round3$TaxMorale) # gives the current order of the levels:
# [1] "Agree"                      "Disagree"                   "Neither agree nor disagree"
# [4] "Strongly Agree"             "Strongly Disagree"
# correct order would be: 5, 2, 3, 1, 4
# change order: 
round3$TaxMorale = factor(round3$TaxMorale,levels(round3$TaxMorale)[c(5,2,3,1,4)])
levels(round3$TaxMorale) # now order is correct:
# [1] "Strongly Disagree"          "Disagree"                   "Neither agree nor disagree"
# [4] "Agree"                      "Strongly Agree"

# Independent Variables:

# A) TRUST:
# 1) Trust key leadership figure (President/Prime Minister)
levels(data.factors$TrustPresident) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3, 2, 4, 1
# change order: 
data.factors$TrustPresident = factor(data.factors$TrustPresident,levels(data.factors$TrustPresident)[c(3,2,4,1)])
levels(data.factors$TrustPresident) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot" 

# same for round 5 only:
levels(round5$TrustPresident) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round5$TrustPresident = factor(round5$TrustPresident,levels(round5$TrustPresident)[c(3,2,4,1)])
levels(round5$TrustPresident) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 4 only:
levels(round4$TrustPresident) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round4$TrustPresident = factor(round4$TrustPresident,levels(round4$TrustPresident)[c(3,2,4,1)])
levels(round4$TrustPresident) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 3 only:
levels(round3$TrustPresident) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round3$TrustPresident = factor(round3$TrustPresident,levels(round3$TrustPresident)[c(3,2,4,1)])
levels(round3$TrustPresident) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# 2) Trust parliament/national assembly
levels(data.factors$TrustParliament) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3, 2, 4, 1
# change order: 
data.factors$TrustParliament = factor(data.factors$TrustParliament,levels(data.factors$TrustParliament)[c(3,2,4,1)])
levels(data.factors$TrustParliament) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot" 

# same for round 5 only:
levels(round5$TrustParliament) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round5$TrustParliament = factor(round5$TrustParliament,levels(round5$TrustParliament)[c(3,2,4,1)])
levels(round5$TrustParliament) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 4 only:
levels(round4$TrustParliament) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round4$TrustParliament = factor(round4$TrustParliament,levels(round4$TrustParliament)[c(3,2,4,1)])
levels(round4$TrustParliament) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 3 only:
levels(round3$TrustParliament) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round3$TrustParliament = factor(round3$TrustParliament,levels(round3$TrustParliament)[c(3,2,4,1)])
levels(round3$TrustParliament) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# 3) Trust courts of law
levels(data.factors$TrustCourts) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3, 2, 4, 1
# change order: 
data.factors$TrustCourts = factor(data.factors$TrustCourts,levels(data.factors$TrustCourts)[c(3,2,4,1)])
levels(data.factors$TrustCourts) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot" 

# same for round 5 only:
levels(round5$TrustCourts) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round5$TrustCourts = factor(round5$TrustCourts,levels(round5$TrustCourts)[c(3,2,4,1)])
levels(round5$TrustCourts) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 4 only:
levels(round4$TrustCourts) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round4$TrustCourts = factor(round4$TrustCourts,levels(round4$TrustCourts)[c(3,2,4,1)])
levels(round4$TrustCourts) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# same for round 3 only:
levels(round3$TrustCourts) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round3$TrustCourts = factor(round3$TrustCourts,levels(round3$TrustCourts)[c(3,2,4,1)])
levels(round3$TrustCourts) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"

# 4) Trust tax department (round 5 only):
levels(data.factors$TrustTax) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3, 2, 4, 1
# change order: 
data.factors$TrustTax = factor(data.factors$TrustTax,levels(data.factors$TrustTax)[c(3,2,4,1)])
levels(data.factors$TrustTax) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot" 

# same for round 5 only:
levels(round5$TrustTax) # gives the current order of the levels:
# [1] "A lot"         "Just a little" "Not at all"    "Somewhat"
# correct order would be: 3,2,4,1
# change order: 
round5$TrustTax = factor(round5$TrustTax,levels(round5$TrustTax)[c(3,2,4,1)])
levels(round5$TrustTax) # now order is correct:
# [1] "Not at all"    "Just a little" "Somewhat"      "A lot"



# B) CORRUPTION:
# 1) Office of the President/Prime Minister involved in corruptoion
levels(data.factors$CorruptionPresident) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionPresident = factor(data.factors$CorruptionPresident,levels(data.factors$CorruptionPresident)[c(3,4,2,1)])
levels(data.factors$CorruptionPresident) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 5 only:
levels(round5$CorruptionPresident) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round5$CorruptionPresident = factor(round5$CorruptionPresident,levels(round5$CorruptionPresident)[c(3,4,2,1)])
levels(round5$CorruptionPresident) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 4 only:
levels(round4$CorruptionPresident) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round4$CorruptionPresident = factor(round4$CorruptionPresident,levels(round4$CorruptionPresident)[c(3,4,2,1)])
levels(round4$CorruptionPresident) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 3 only:
levels(round3$CorruptionPresident) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round3$CorruptionPresident = factor(round3$CorruptionPresident,levels(round3$CorruptionPresident)[c(3,4,2,1)])
levels(round3$CorruptionPresident) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# 2) Members of Parliament involved in Corruption
levels(data.factors$CorruptionParliament) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionParliament = factor(data.factors$CorruptionParliament,levels(data.factors$CorruptionParliament)[c(3,4,2,1)])
levels(data.factors$CorruptionParliament) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 5 only:
levels(round5$CorruptionParliament) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round5$CorruptionParliament = factor(round5$CorruptionParliament,levels(round5$CorruptionParliament)[c(3,4,2,1)])
levels(round5$CorruptionParliament) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 4 only:
levels(round4$CorruptionParliament) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round4$CorruptionParliament = factor(round4$CorruptionParliament,levels(round4$CorruptionParliament)[c(3,4,2,1)])
levels(round4$CorruptionParliament) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 3 only:
levels(round3$CorruptionParliament) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round3$CorruptionParliament = factor(round3$CorruptionParliament,levels(round3$CorruptionParliament)[c(3,4,2,1)])
levels(round3$CorruptionParliament) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# 3) Government Officials involved in Corruption
levels(data.factors$CorruptionOfficials) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionOfficials = factor(data.factors$CorruptionOfficials,levels(data.factors$CorruptionOfficials)[c(3,4,2,1)])
levels(data.factors$CorruptionOfficials) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 5 only:
levels(round5$CorruptionOfficials) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round5$CorruptionOfficials = factor(round5$CorruptionOfficials,levels(round5$CorruptionOfficials)[c(3,4,2,1)])
levels(round5$CorruptionOfficials) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 4 only:
levels(round4$CorruptionOfficials) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round4$CorruptionOfficials = factor(round4$CorruptionOfficials,levels(round4$CorruptionOfficials)[c(3,4,2,1)])
levels(round4$CorruptionOfficials) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# not available in round 3

# 4) Local Government Councilors involved in Corruption
levels(data.factors$CorruptionCouncilors) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionCouncilors = factor(data.factors$CorruptionCouncilors,levels(data.factors$CorruptionCouncilors)[c(3,4,2,1)])
levels(data.factors$CorruptionCouncilors) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 5 only:
levels(round5$CorruptionCouncilors) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round5$CorruptionCouncilors = factor(round5$CorruptionCouncilors,levels(round5$CorruptionCouncilors)[c(3,4,2,1)])
levels(round5$CorruptionCouncilors) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 4 only:
levels(round4$CorruptionCouncilors) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round4$CorruptionCouncilors = factor(round4$CorruptionCouncilors,levels(round4$CorruptionCouncilors)[c(3,4,2,1)])
levels(round4$CorruptionCouncilors) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 3 only:
levels(round3$CorruptionCouncilors) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round3$CorruptionCouncilors = factor(round3$CorruptionCouncilors,levels(round3$CorruptionCouncilors)[c(3,4,2,1)])
levels(round3$CorruptionCouncilors) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# 5) Tax Officials involved in Corruption
levels(data.factors$CorruptionTax) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionTax = factor(data.factors$CorruptionTax,levels(data.factors$CorruptionTax)[c(3,4,2,1)])
levels(data.factors$CorruptionTax) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 5 only:
levels(round5$CorruptionTax) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round5$CorruptionTax = factor(round5$CorruptionTax,levels(round5$CorruptionTax)[c(3,4,2,1)])
levels(round5$CorruptionTax) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 4 only:
levels(round4$CorruptionTax) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round4$CorruptionTax = factor(round4$CorruptionTax,levels(round4$CorruptionTax)[c(3,4,2,1)])
levels(round4$CorruptionTax) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# same for round 3 only:
levels(round3$CorruptionTax) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
round3$CorruptionTax = factor(round3$CorruptionTax,levels(round3$CorruptionTax)[c(3,4,2,1)])
levels(round3$CorruptionTax) # now order is correct:
# [1] "None"         "Some of them" "Most of them" "All of them"

# C) Living conditions
levels(data.factors$LivingConditions) # gives the current order of the levels:
# [1] "Fairly bad"           "Fairly good"          "Neither good nor bad" "Very bad"            
# [5] "Very good"
# correct order would be: 4,1,3,2,5 
# change order: 
data.factors$LivingConditions = factor(data.factors$LivingConditions,levels(data.factors$LivingConditions)[c(4,1,3,2,5)])
levels(data.factors$LivingConditions) # now order is correct:
# [1] "Very bad"             "Fairly bad"           "Neither good nor bad" "Fairly good"         
# [5] "Very good"

# same for round 5 only:
levels(round5$LivingConditions) # gives the current order of the levels:
# [1] "Fairly bad"           "Fairly good"          "Neither good nor bad" "Very bad"            
# [5] "Very good"
# correct order would be: 4,1,3,2,5 
# change order: 
round5$LivingConditions = factor(round5$LivingConditions,levels(round5$LivingConditions)[c(4,1,3,2,5)])
levels(round5$LivingConditions) # now order is correct:
# [1] "Very bad"             "Fairly bad"           "Neither good nor bad" "Fairly good"         
# [5] "Very good"

# same for round 4 only:
levels(round4$LivingConditions) # gives the current order of the levels:
# [1] "Fairly bad"           "Fairly good"          "Neither good nor bad" "Very bad"            
# [5] "Very good"
# correct order would be: 4,1,3,2,5 
# change order: 
round4$LivingConditions = factor(round4$LivingConditions,levels(round4$LivingConditions)[c(4,1,3,2,5)])
levels(round4$LivingConditions) # now order is correct:
# [1] "Very bad"             "Fairly bad"           "Neither good nor bad" "Fairly good"         
# [5] "Very good"

# same for round 3 only:
levels(round3$LivingConditions) # gives the current order of the levels:
# [1] "Fairly bad"           "Fairly good"          "Neither good nor bad" "Very bad"            
# [5] "Very good"
# correct order would be: 4,1,3,2,5 
# change order: 
round3$LivingConditions = factor(round3$LivingConditions,levels(round3$LivingConditions)[c(4,1,3,2,5)])
levels(round3$LivingConditions) # now order is correct:
# [1] "Very bad"             "Fairly bad"           "Neither good nor bad" "Fairly good"         
# [5] "Very good"

# D) Gender
data.factors$Gender = factor(data.factors$Gender,levels(data.factors$Gender)[c(2,1)])
levels(data.factors$Gender)
# [1] "male"   "female"
round5$Gender = factor(round5$Gender,levels(round5$Gender)[c(2,1)])
levels(round5.Gender)
#[1] "male"   "female"
round4$Gender = factor(round4$Gender,levels(round4$Gender)[c(2,1)])
levels(round4.Gender)
#[1] "male"   "female"
round3$Gender = factor(round3$Gender,levels(round3$Gender)[c(2,1)])
levels(round3.Gender)
#[1] "male"   "female"


# E) Religious
levels(data.factors$Religion) # gives the current order of the levels:
# [1] "Active member"   "Inactive member" "Not a member"    "Official Leader"
# correct order would be: 3,2,1,4 
# change order: 
data.factors$Religion = factor(data.factors$Religion,levels(data.factors$Religion)[c(3,2,1,4)])
levels(data.factors$Religion) # now order is correct:
# [1] "Not a member"    "Inactive member" "Active member"   "Official Leader"

# round 5 only:
levels(round5$Religion) # gives the current order of the levels:
# [1] "Active member"   "Inactive member" "Not a member"    "Official Leader"
# correct order would be: 3,2,1,4 
# change order: 
round5$Religion = factor(round5$Religion,levels(round5$Religion)[c(3,2,1,4)])
levels(round5$Religion) # now order is correct:
# [1] "Not a member"    "Inactive member" "Active member"   "Official Leader"

# round 4 only:
levels(round4$Religion) # gives the current order of the levels:
# [1] "Active member"   "Inactive member" "Not a member"    "Official Leader"
# correct order would be: 3,2,1,4 
# change order: 
round4$Religion = factor(round4$Religion,levels(round4$Religion)[c(3,2,1,4)])
levels(round4$Religion) # now order is correct:
# [1] "Not a member"    "Inactive member" "Active member"   "Official Leader"

# round 3 only:
levels(round3$Religion) # gives the current order of the levels:
# [1] "Active member"   "Inactive member" "Not a member"    "Official Leader"
# correct order would be: 3,2,1,4 
# change order: 
round3$Religion = factor(round3$Religion,levels(round3$Religion)[c(3,2,1,4)])
levels(round3$Religion) # now order is correct:
# [1] "Not a member"    "Inactive member" "Active member"   "Official Leader"

# F) Perceived frequency of others avoiding taxes (only available in round 5)
levels(data.factors$AvoidHowOften) # gives the current order of the levels:
# [1] "Allways" "Never"   "Often"   "Rarely"
# correct order would be: 2,4,3,1 
# change order: 
data.factors$AvoidHowOften = factor(data.factors$AvoidHowOften,levels(data.factors$AvoidHowOften)[c(2,4,3,1)])
levels(data.factors$AvoidHowOften) # now order is correct:
# [1] "Never"   "Rarely"  "Often"   "Allways"

# same for round 5 only:
levels(round5$AvoidHowOften) # gives the current order of the levels:
# [1] "Allways" "Never"   "Often"   "Rarely"
# correct order would be: 2,4,3,1 
# change order: 
round5$AvoidHowOften = factor(round5$AvoidHowOften,levels(round5$AvoidHowOften)[c(2,4,3,1)])
levels(round5$AvoidHowOften) # now order is correct:
# [1] "Never"   "Rarely"  "Often"   "Allways"

# G) Self employed (only available in round 5)
levels(data.factors$SelfEmployedTax) # gives the current order of the levels:
# [1] "Not required to pay" "Required to pay"
# no changes necessary

# round 5:
levels(round5$SelfEmployedTax) # gives the current order of the levels:
# [1] "Not required to pay" "Required to pay"
# no changes necessary

# H) Perceived likelihood of being detected (round 3)
levels(data.factors$Enforce2) # gives the current order of the levels:
# [1] "Likely"            "Not at all likely" "Not very likely"   "Very Likely"
# correct order would be: 2,3,1,4 
# change order: 
data.factors$Enforce2 = factor(data.factors$Enforce2,levels(data.factors$Enforce2)[c(2,3,1,4)])
levels(data.factors$Enforce2) # now order is correct:
# [1] "Not at all likely" "Not very likely"   "Likely"            "Very Likely"

# same for round 3 only:
levels(round3$Enforce2) # gives the current order of the levels:
# [1] "Likely"            "Not at all likely" "Not very likely"   "Very Likely"
# correct order would be: 2,3,1,4 
# change order: 
round3$Enforce2 = factor(round3$Enforce2,levels(round3$Enforce2)[c(2,3,1,4)])
levels(round3$Enforce2) # now order is correct:
# [1] "Not at all likely" "Not very likely"   "Likely"            "Very Likely"


# Second: turn "Year"-variable into factor:

str(data.factors$Year) # shows that Year is currently treated as a numeric variable
data.factors$Year <- cut(data.factors$Year,
                              breaks = c(0, 2005, 2008, 2015),
                              labels = c("2005", "2008", "2015"))
str(data.factors$Year) # shows that Year is now a factor vairable with 3 levels
levels(data.factors$Year) # in correct order: 2005, 2008, 2015

# same for round 5:
str(round5$Year) # shows that Year is currently treated as a numeric variable
round5$Year <- cut(round5$Year,
                         breaks = c(0, 2005, 2008, 2015),
                         labels = c("2005", "2008", "2015"))
str(round5$Year) # shows that Year is now a factor vairable with 3 levels
levels(round5$Year) # in correct order: 2005, 2008, 2015

# same for round 4:
str(round4$Year) # shows that Year is currently treated as a numeric variable
round4$Year <- cut(round4$Year,
                   breaks = c(0, 2005, 2008, 2015),
                   labels = c("2005", "2008", "2015"))
str(round4$Year) # shows that Year is now a factor vairable with 3 levels
levels(round4$Year) # in correct order: 2005, 2008, 2015

# same for round 3:
str(round3$Year) # shows that Year is currently treated as a numeric variable
round3$Year <- cut(round3$Year,
                   breaks = c(0, 2005, 2008, 2015),
                   labels = c("2005", "2008", "2015"))
str(round3$Year) # shows that Year is now a factor vairable with 3 levels
levels(round3$Year) # in correct order: 2005, 2008, 2015


# Third: Recode dependent variable "TaxMorale" so that it has 3 instead of 5 levels
# for data.factors:
data.3l.factors <- data.factors

data.3l.factors$TaxMorale[data.3l.factors$TaxMorale=="Strongly Disagree"] <- "Disagree"
data.3l.factors$TaxMorale[data.3l.factors$TaxMorale=="Strongly Agree"] <- "Agree"

data.3l.factors$TaxMorale <- data.3l.factors$TaxMorale[data.3l.factors$TaxMorale != "Strongly Disagree"]
data.3l.factors$TaxMorale <- data.3l.factors$TaxMorale[data.3l.factors$TaxMorale != "Strongly Agree"]

data.3l.factors$TaxMorale <- factor(data.3l.factors$TaxMorale)

# for data.numeric:
data.3l.numeric <- data.numeric

data.3l.numeric$TaxMorale[data.3l.numeric$TaxMorale==2] <- 1
data.3l.numeric$TaxMorale[data.3l.numeric$TaxMorale==3] <- 2
data.3l.numeric$TaxMorale[data.3l.numeric$TaxMorale==4] <- 3
data.3l.numeric$TaxMorale[data.3l.numeric$TaxMorale==5] <- 3

data.3l.numeric$TaxMorale <- cut(data.3l.numeric$TaxMorale,
                              breaks = c(0, 1, 2, 3),
                              labels = c("Disagree", "Neither agree nor disagre", "Agree"))

levels(data.3l.numeric$TaxMorale) 

# end of preparatory work
########################################################################################################

# MODELS

# 1. Models including data for all three years (only those variables that are available in all rounds)

# Model 1.1: 
# Tax morale as a function of 
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - country fixed effects
#   - year fixed effects

# Step one: get data set that doesn't have any missings on the variables of the model:
data1.1 <- DropNA(data.factors, c("TaxMorale", "TrustPresident", "CorruptionPresident", "Country", "Year"))
# 26845 rows dropped from the data frame (that is 25.6% of all observations)
# take a closer look what that means:
summary(data1.1$TaxMorale)
# distribution of dependent variable TaxMorale after dropping NA's:
# Strongly Disagree:           5607  
# Disagree:                    9381
# Neither agree nor disagree:  7185
# Agree:                      35750
# Strongly Agree:             19929
# Histogram:
taxmoralehist <- qplot(data1.1$TaxMorale,
                        geom="histogram",
                        binwidth=6,
                        main="Distribution of responses",
                        xlab="The tax department always has the right to make people pay taxes.",
                        fill=I("lightblue"))
print(taxmoralehist, tag = 'chart')

summary(data1.1$TrustPresident)
# distribution of TrustPresident after dropping NA's:
# Not at all:    13129  
# Just a little: 16911
# Somewhat:      18071
# A lot:         29740
# Histogram:
trustpreshist <- qplot(data1.1$TrustPresident,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="How much do you trust the President/Prime Minister?",
                       fill=I("lightblue"))
print(trustpreshist, tag = 'chart')

summary(data1.1$CorruptionPresident)
# distribution of CorruptionPresident after dropping NA's:
# None:         15983  
# Some of them: 38430
# Most of them: 15591
# All of them:   7848
# Histogram:
corruptpreshist <- qplot(data1.1$CorruptionPresident,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="How many of the following people do you think are involved in corruption: The President/Prime Minister and Officials in his Office?",
                       fill=I("lightblue"))
print(corruptpreshist, tag = 'chart')

summary(data1.1$Country) # Morocco and Swaziland are not in the dataset anymore and 9 countries have fewer 
                         # than 1000 observations now (Algeria, Burundi, Cameroon, CotedIvoire, Egypt, Niger,
                         # Sudan, Togo, and Tunisia)
countryhist <- qplot(data1.1$Country,
                         geom="histogram",
                         binwidth=3,
                         main="Number of observations per country",
                         xlab="Country of origin",
                         fill=I("lightblue"))
print(countryhist, tag = 'chart')


# Step two: run the model:
reg1.1 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident + Year + Country, method='logistic', 
               data=data1.1, Hess = TRUE)

summary(reg1.1) 

# Store table
(ctable1.1 <- coef(summary(reg1.1)))

# Calculate and store p values
p1.1 <- pnorm(abs(ctable1.1[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.1 <- cbind(ctable1.1, "p value" = p1.1))

# Confidence Interval
ci.1.1 <- confint.default(reg1.1)

# Odds Ratios
exp(coef(reg1.1))

# Combine Odds Ratios and Confidence Interval
ORtable <- exp(cbind(OR = coef(reg1.1), ci.1.1))

kable(ORtable)
ortable1.1 <- kable(ORtable, align = 'c', digits = 2)

# Predicted Probabilities
test <- predict(reg1.1)


#fitted <- with(data.numeric,
#               data.frame(TrustPresident = mean(TrustPresident, na.rm=TRUE),
#                         CorruptionPresident = mean(CorruptionPresident, na.rm=TRUE),
#                          Year = mean(Year),
#                          Country = factor(1:34)))
#fitted

#fitted$predicted <- predict(reg1.1, newdata = fitted,
#                            type = 'response')
#fitted

# try with numeric:
reg11 <- polr(as.factor(TaxMorale) ~ as.factor(TrustPresident) + as.factor(CorruptionPresident) + 
                as.factor(Year) + as.factor(Country), method='logistic', 
              data=data.numeric, Hess = TRUE)

summary(reg11) ## 26845 observations deleted due to missingness (that is 25.6% of all observations)

# Store table
(ctable11 <- coef(summary(reg11)))

# Calculate and store p values
p11 <- pnorm(abs(ctable11[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable11 <- cbind(ctable11, "p value" = p11))

# Confidence Interval
ci.11 <- confint.default(reg11)

# Odds Ratios
exp(coef(reg11))

# Combine Odds Ratios and Confidence Interval
exp(cbind(OR = coef(reg11), ci.11))

# Model 1.1 without the Country Variable to see how important specific country differences are for the model:
reg1.1nocountry <- polr(TaxMorale ~ TrustPresident + CorruptionPresident + Year, method='logistic', 
               data=data1.1, Hess = TRUE)

summary(reg1.1nocountry)

# Store table
(ctable1.1nocountry <- coef(summary(reg1.1nocountry)))

# Calculate and store p values
p1.1nocountry <- pnorm(abs(ctable1.1nocountry[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.1nocountry <- cbind(ctable1.1nocountry, "p value" = p1.1nocountry))

# Confidence Interval
ci.1.1nocountry <- confint.default(reg1.1nocountry)

# Odds Ratios
exp(coef(reg1.1nocountry))

# Combine Odds Ratios and Confidence Interval
ORtable.nocountry <- exp(cbind(OR = coef(reg1.1nocountry), ci.1.1nocountry))
kable(ORtable.nocountry)
ortable1.1nocountry <- kable(ORtable.nocountry, align = 'c', digits = 2)
print(ortable1.1nocountry)

# try a multinomial logit model to compare results:
library(nnet)
multinom1.1 <- multinom(TaxMorale ~ TrustPresident + CorruptionPresident + Country + Year, data=data1.1)
Anova(multinom1.1) # to see if the variables included are statistically significant: they are all significant
#Response: TaxMorale
#LR Chisq  Df Pr(>Chisq)    
#TrustPresident         837.2  12  < 2.2e-16 ***
#CorruptionPresident    313.7  12  < 2.2e-16 ***
#Country               6495.8 132  < 2.2e-16 ***
#Year                   408.4   8  < 2.2e-16 ***
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(multinom1.1) # Warning message: In sqrt(diag(vc)) : NaNs produced
# AIC: 205112 is lower than the AIC for reg 1.1 (209761.52) which suggests that the assumption of proportional 
# odds doesn't hold



# Model 1.2 (including socio-economic controls):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - country fixed effects
#   - year fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious 

# Step one: get data set that doesn't have any missings on the variables of the model:
data1.2 <- DropNA(data.factors, c("TaxMorale", "TrustPresident", "CorruptionPresident", "Country", "Year",
                                  "LivingConditions", "Age", "Gender", "Religion"))
# 27912 rows dropped from the data frame (that is 26.7% of all observations)
# take a closer look what that means:
summary(data1.2$TaxMorale)
# distribution of dependent variable TaxMorale after dropping NA's:
# Strongly Disagree:           5500  
# Disagree:                    9243
# Neither agree nor disagree:  7082
# Agree:                      35251
# Strongly Agree:             19709
# Histogram:
taxmoralehist1.2 <- qplot(data1.2$TaxMorale,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="The tax department always has the right to make people pay taxes.",
                       fill=I("lightblue"))
print(taxmoralehist1.2, tag = 'chart')

summary(data1.2$TrustPresident)
# distribution of TrustPresident after dropping NA's:
# Not at all:    12932  
# Just a little: 16705
# Somewhat:      17886
# A lot:         29262
# Histogram:
trustpreshist1.2 <- qplot(data1.2$TrustPresident,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="How much do you trust the President/Prime Minister?",
                       fill=I("lightblue"))
print(trustpreshist1.2, tag = 'chart')

summary(data1.2$CorruptionPresident)
# distribution of CorruptionPresident after dropping NA's:
# None:         15704  
# Some of them: 37948
# Most of them: 15391
# All of them:   7742
# Histogram:
corruptpreshist1.2 <- qplot(data1.2$CorruptionPresident,
                         geom="histogram",
                         binwidth=6,
                         main="Distribution of responses",
                         xlab="How many of the following people do you think are involved in corruption: The President/Prime Minister and Officials in his Office?",
                         fill=I("lightblue"))
print(corruptpreshist1.2, tag = 'chart')

summary(data1.2$LivingConditions)
# Very bad:             15544
# Fairly bad:           21998
# Neither good nor bad: 15824
# Fairly good:          19951
# Very good:             3468
# Histogram:
livingcond1.2 <- qplot(data1.2$LivingConditions,
                            geom="histogram",
                            binwidth=6,
                            main="Distribution of responses",
                            xlab="In general, how would you describe: Your own present living conditions?",
                            fill=I("lightblue"))
print(livingcond1.2, tag = 'chart')

summary(data1.2$Religion)
# Not a member:    30147
# Inactive member: 15894
# Active member:   26137
# Official Leader:  4607
# Histogram:
religion1.2 <- qplot(data1.2$Religion,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="Are you a member of a religious group that meets outside of regular worship services?",
                       fill=I("lightblue"))
print(religion1.2, tag = 'chart')

summary(data1.2$Gender)
# Male:   40104
# Female: 36681

summary(data1.2$Age)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   18.00   25.00   33.00   36.14   45.00  120.00

# Ovierview over variables used in the model using the ScatterplotMatrix command from car:
# making the matrix takes a lot of time therefore this command should not be run every time.
# a "png" and "pdf" file of the matrix are saved under 
#
#data.numeric1.2 <- DropNA(data.numeric, c("TaxMorale", "TrustPresident", "CorruptionPresident", "Country", "Year",
#                                          "LivingConditions", "Age", "Gender", "Religion"))
#keepvars = c(10, 11, 14, 5, 7, 9, 19)
#data.numeric1.2 <- data.numeric1.2[, keepvars]
#scatterplotMatrix(data.numeric1.2)

# Step two: run the model:
reg1.2 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident +  Year + Country + 
               LivingConditions + Age + Gender + Religion, method='logistic', 
               data=data1.2, Hess = TRUE)

summary(reg1.2) ## 27912 observations deleted due to missingness (that is 26.7% of all observations)

# Store table
(ctable1.2 <- coef(summary(reg1.2)))

# Calculate and store p values
p1.2 <- pnorm(abs(ctable1.2[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.2 <- cbind(ctable1.2, "p value" = p1.2))

# Confidence Interval
ci.1.2 <- confint.default(reg1.2)

# Odds Ratios
exp(coef(reg1.2))

# Combine Odds Ratios and Confidence Interval
exp(cbind(OR = coef(reg1.2), ci.1.2))

# Combine Odds Ratios and Confidence Interval
ORtable1.2 <- exp(cbind(OR = coef(reg1.2), ci.1.2))

kable(ORtable1.2)
ortable1.2 <- kable(ORtable1.2, align = 'c', digits = 2)

# Predicted Probabilities

# Model 1.2 without the Country Variable to see how important specific country differences are for the model:
reg1.2nocountry <- polr(TaxMorale ~ TrustPresident + CorruptionPresident +  Year + 
                 LivingConditions + Age + Gender + Religion, method='logistic', 
               data=data1.2, Hess = TRUE)

summary(reg1.2nocountry)

# Store table
(ctable1.2nocountry <- coef(summary(reg1.2nocountry)))

# Calculate and store p values
p1.2nocountry <- pnorm(abs(ctable1.2nocountry[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.2nocountry <- cbind(ctable1.2nocountry, "p value" = p1.2nocountry))

# Confidence Interval
ci.1.2nocountry <- confint.default(reg1.2nocountry)

# Odds Ratios
exp(coef(reg1.2nocountry))

# Combine Odds Ratios and Confidence Interval
ORtable1.2nocountry <- exp(cbind(OR = coef(reg1.2nocountry), ci.1.2nocountry))
kable(ORtable1.2nocountry)
ortable1.2nocountry <- kable(ORtable1.2nocountry, align = 'c', digits = 2)
print(ortable1.2nocountry)

# try a multinomial logit model to compare results:
library(nnet)
multinom1.2 <- multinom(TaxMorale ~ TrustPresident + CorruptionPresident + Country + Year + 
                          LivingConditions + Age + Gender + Religion, data=data1.2)
Anova(multinom1.2)
# Analysis of Deviance Table (Type II tests)
#Response: TaxMorale
#LR Chisq  Df Pr(>Chisq)    
#TrustPresident         648.6  12  < 2.2e-16 ***
#CorruptionPresident    249.1  12  < 2.2e-16 ***
#Country               5703.1 132  < 2.2e-16 ***
#Year                   369.3   8  < 2.2e-16 ***
#LivingConditions       478.2  16  < 2.2e-16 ***
#Age                     -0.3   4          1    
#Gender                 143.8   4  < 2.2e-16 ***
#Religion                51.3  12  8.234e-07 ***
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
### except for Age all variables are highly statistically significant
summary(multinom1.2) # Warning message: In sqrt(diag(vc)) : NaNs produced
# AIC: 201728.7 is lower than the AIC for reg 1.1 (206619.55) which suggests that the assumption of proportional 
# odds doesn't hold

# rerun model 1.1 , 1.1nocountry and multinom1.1 with the same dataset used in model 1.2 (data1.2) in order to be able to compare AIC values:
reg1.1new <- polr(TaxMorale ~ TrustPresident + CorruptionPresident + Year + Country, method='logistic', 
               data=data1.2, Hess = TRUE)

summary(reg1.1new) 

# Store table
(ctable1.1new <- coef(summary(reg1.1new)))

# Calculate and store p values
p1.1new <- pnorm(abs(ctable1.1new[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.1new <- cbind(ctable1.1new, "p value" = p1.1new))

# Confidence Interval
ci.1.1new <- confint.default(reg1.1new)

# Odds Ratios
exp(coef(reg1.1new))

# Combine Odds Ratios and Confidence Interval
ORtable1.1new <- exp(cbind(OR = coef(reg1.1new), ci.1.1new))
kable(ORtable1.1new)
ortable1.1new <- kable(ORtable1.1new, align = 'c', digits = 2)

# model 1.1 without countries but with same dataset as in model 1.2:
reg1.1newnocountry <- polr(TaxMorale ~ TrustPresident + CorruptionPresident + Year, method='logistic', 
                        data=data1.2, Hess = TRUE)

summary(reg1.1newnocountry)

# Store table
(ctable1.1newnocountry <- coef(summary(reg1.1newnocountry)))

# Calculate and store p values
p1.1newnocountry <- pnorm(abs(ctable1.1newnocountry[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.1newnocountry <- cbind(ctable1.1newnocountry, "p value" = p1.1newnocountry))

# Confidence Interval
ci.1.1newnocountry <- confint.default(reg1.1newnocountry)

# Odds Ratios
exp(coef(reg1.1newnocountry))

# Combine Odds Ratios and Confidence Interval
ORtable.newnocountry <- exp(cbind(OR = coef(reg1.1nocountry), ci.1.1nocountry))
kable(ORtable.newnocountry)
ortable1.1newnocountry <- kable(ORtable.newnocountry, align = 'c', digits = 2)
print(ortable1.1newnocountry)

# multinomial model1.1 but with same dataset as in model 1.2:
library(nnet)
multinom1.1new <- multinom(TaxMorale ~ TrustPresident + CorruptionPresident + Country + Year, data=data1.2)
summary(multinom1.1new) # Warning message: In sqrt(diag(vc)) : NaNs produced
# AIC: 202169.7 is lower than the AIC for reg 1.2 (206619.55) which suggests that the assumption of proportional 
# odds doesn't hold

#####
# Joint table for Models 1.1, 1.2, 1.1new, and the two models without country fixed effects

library(stargazer)
table1 <- stargazer(reg1.1, reg1.2, reg1.1new, reg1.1nocountry, reg1.2nocountry,
          title = 'Ordinal Logistic Regression Results of Tax Morale',
          digits = 2, type = 'html')


# joint table for multinomial models:

table2 <- stargazer(multinom1.1, multinom1.2, multinom1.1new,
                    title = 'Multinomial Logistic Regression Results of Tax Morale',
                    digits = 2, type = 'html')


####

# Model 1.3 (same as 1.1, but with tax morale coded with only 3 levels - disagree, neither nor, agree)

reg1.3 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident + Year + Country, method='logistic', 
               data=data.3l.factors, Hess = TRUE)

summary(reg1.3) ## 26845 observations deleted due to missingness (that is 25.6% of all observations)

# Store table
(ctable1.3 <- coef(summary(reg1.3)))

# Calculate and store p values
p1.3 <- pnorm(abs(ctable1.3[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.3 <- cbind(ctable1.3, "p value" = p1.3))

# Confidence Interval
ci.1.3 <- confint.default(reg1.3)

# Odds Ratios
exp(coef(reg1.3))

# Combine Odds Ratios and Confidence Interval
ORtable1.3 <- exp(cbind(OR = coef(reg1.3), ci.1.3))

kable(ORtable1.3)
ortable1.3 <- kable(ORtable1.3, align = 'c', digits = 2)

# Model 1.4 (same as 1.2, but with tax morale coded with only 3 levels - disagree, neither nor, agree)

reg1.4 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident +  Year + Country + 
               LivingConditions + Age + Gender + Religion, method='logistic', 
               data=data.3l.factors, Hess = TRUE)

summary(reg1.4) ## 27912 observations deleted due to missingness 

# Store table
(ctable1.4 <- coef(summary(reg1.4)))

# Calculate and store p values
p1.4 <- pnorm(abs(ctable1.4[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable1.4 <- cbind(ctable1.4, "p value" = p1.4))

# Confidence Interval
ci.1.4 <- confint.default(reg1.4)

# Odds Ratios
exp(coef(reg1.4))

# Combine Odds Ratios and Confidence Interval
ORtable1.4 <- exp(cbind(OR = coef(reg1.4), ci.1.4))
kable(ORtable1.4)
ortable1.4 <- kable(ORtable1.4, align = 'c', digits = 2)

####
# Combined table models 1.1, 1.2, 1.3,and 1.4
table3 <- stargazer(reg1.1, reg1.2, reg1.3, reg1.4,
                    title = 'Comparison 5 vs. 3 levels of Tax Morale',
                    digits = 2, type = 'html')

# 2. Sub analyses for single years (with variables not available in all rounds)
# Model 2.1 (round 5 only):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - trust in the tax department
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived frequency of others avoiding taxes
#   - self-employed
#   - country fixed effects

# Step one: get data set that doesn't have any missings on the variables of the model:
data2.1 <- DropNA(round5, c("TaxMorale", "TrustPresident", "TrustTax", "CorruptionPresident", 
                                  "AvoidHowOften", "SelfEmployedTax", "Country"))
# 35800 rows dropped from the data frame (that is almost 70% of all observations!!!)
# take a closer look what that means:
summary(data2.1$TaxMorale)
# distribution of dependent variable TaxMorale after dropping NA's:
# Strongly Disagree:           1228  
# Disagree:                    1876
# Neither agree nor disagree:  1004
# Agree:                       7192
# Strongly Agree:              4487
# Histogram:
taxmoralehist2.1 <- qplot(data2.1$TaxMorale,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="The tax department always has the right to make people pay taxes.",
                       fill=I("lightblue"))
print(taxmoralehist2.1, tag = 'chart')

summary(data2.1$TrustPresident)
# distribution of TrustPresident after dropping NA's:
# Not at all:    2490  
# Just a little: 3546
# Somewhat:      3790
# A lot:         5961
# Histogram:
trustpreshist2.1 <- qplot(data2.1$TrustPresident,
                       geom="histogram",
                       binwidth=6,
                       main="Distribution of responses",
                       xlab="How much do you trust the President/Prime Minister?",
                       fill=I("lightblue"))
print(trustpreshist2.1, tag = 'chart')

summary(data2.1$TrustTax)
#distribution of TrustTax after dropping NAs:
# Not at all:    3620  
# Just a little: 4848
# Somewhat:      4181
# A lot:         3138
# Histogram:
trusttax2.1 <- qplot(data2.1$TrustTax,
                          geom="histogram",
                          binwidth=6,
                          main="Distribution of responses",
                          xlab="How much do you trust the tax department?",
                          fill=I("lightblue"))
print(trusttax2.1, tag = 'chart')

summary(data2.1$CorruptionPresident)
# distribution of CorruptionPresident after dropping NA's:
# None:         2708  
# Some of them: 8354
# Most of them: 3187
# All of them:  1538
# Histogram:
corruptpreshist2.1 <- qplot(data2.1$CorruptionPresident,
                         geom="histogram",
                         binwidth=6,
                         main="Distribution of responses",
                         xlab="How many of the following people do you think are involved in corruption: The President/Prime Minister and Officials in his Office?",
                         fill=I("lightblue"))
print(corruptpreshist2.1, tag = 'chart')

summary(data2.1$AvoidHowOften)
# Never:   5256
# Rarely:  5200
# Often:   4068
# Allways: 1263
# Histogram:
avoidhist2.1 <- qplot(data2.1$AvoidHowOften,
                            geom="histogram",
                            binwidth=6,
                            main="Distribution of responses",
                            xlab="How often do people avoid paying the taxes that they owe the government?",
                            fill=I("lightblue"))
print(avoidhist2.1, tag = 'chart')

summary(data2.1$SelfEmployedTax)
# Not required to pay: 5902
# Required to pay:     9885

summary(data2.1$Country) # Algeria, Egypt, Morocco, Sudan, Swaziland, and Tunisia are not in the dataset 
# anymore and 5 countries have fewer than 200 observations now (Botswana, Burkina Faso, Burundi, Mauritius, 
# and Namibia)
countryhist2.1 <- qplot(data2.1$Country,
                     geom="histogram",
                     binwidth=3,
                     main="Number of observations per country",
                     xlab="Country of origin",
                     fill=I("lightblue"))
print(countryhist2.1, tag = 'chart')



# Step two: run the model:
reg2.1 <- polr(TaxMorale ~ TrustPresident + TrustTax + CorruptionPresident +  AvoidHowOften + 
               SelfEmployedTax + Country, 
               method='logistic', 
               data=data2.1, 
               Hess = TRUE)

summary(reg2.1)

# Store table
(ctable2.1 <- coef(summary(reg2.1)))

# Calculate and store p values
p2.1 <- pnorm(abs(ctable2.1[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable2.1 <- cbind(ctable2.1, "p value" = p2.1))

# Confidence Interval
ci.2.1 <- confint.default(reg2.1)

# Odds Ratios
exp(coef(reg2.1))

# Combine Odds Ratios and Confidence Interval
ORtable2.1 <- exp(cbind(OR = coef(reg2.1), ci.2.1))
kable(ORtable2.1)
ortable2.1 <- kable(ORtable2.1, align = 'c', digits = 2)

# Model 2.2 (round 5 only, including socio-economic controls):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - trust in the tax department
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived frequency of others avoiding taxes
#   - self-employed
#   - country fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious

# Step one: get data set that doesn't have any missings on the variables of the model:
data2.2 <- DropNA(round5, c("TaxMorale", "TrustPresident", "TrustTax", "CorruptionPresident", 
                            "AvoidHowOften", "SelfEmployedTax", "Country", "LivingConditions", "Age",
                            "Gender", "Religion"))
# 35955 rows dropped from the data frame (that is almost 70% of all observations!!!)
# take a closer look what that means:
summary(data2.2$Religion)
summary(data2.2$Gender)
summary(data2.2$Age)
summary(data2.2$LivingConditions)
summary(data2.2$Country)
## not so different from 2.1

reg2.2 <- polr(TaxMorale ~ TrustPresident + TrustTax + CorruptionPresident +  AvoidHowOften + 
               SelfEmployedTax + Country + LivingConditions + Age + Gender + Religion,
               method='logistic', 
               data=data2.2, 
               Hess = TRUE)

summary(reg2.2) 

# Store table
(ctable2.2 <- coef(summary(reg2.2)))

# Calculate and store p values
p2.2 <- pnorm(abs(ctable2.2[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable2.2 <- cbind(ctable2.2, "p value" = p2.2))

# Confidence Interval
ci.2.2 <- confint.default(reg2.2)

# Odds Ratios
exp(coef(reg2.2))

# Combine Odds Ratios and Confidence Interval
ORtable2.2 <- exp(cbind(OR = coef(reg2.2), ci.2.2))
kable(ORtable2.2)
ortable2.2 <- kable(ORtable2.2, align = 'c', digits = 2)

# Model 2.3 (round 3 only):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived likelihood of being detected
#   - country fixed effects

# Step one: get data set that doesn't have any missings on the variables of the model:
data2.3 <- DropNA(round3, c("TaxMorale", "TrustPresident", "CorruptionPresident", "Enforce2",
                            "Country"))
# 7656 rows dropped from the data frame (that is 30% of all observations)
# take a closer look what that means:
summary(data2.3$TaxMorale)
summary(data2.3$TrustPresident)
summary(data2.3$CorruptionPresident)
summary(data2.3$Enforce2)
summary(data2.3$Country) # no country entirely dropped out

# Step two: run the model
reg2.3 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident +  Enforce2 + Country,
               method='logistic', 
               data=data2.3, 
               Hess = TRUE)

summary(reg2.3) 

# Store table
(ctable2.3 <- coef(summary(reg2.3)))

# Calculate and store p values
p2.3 <- pnorm(abs(ctable2.3[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable2.3 <- cbind(ctable2.3, "p value" = p2.3))

# Confidence Interval
ci.2.3 <- confint.default(reg2.3)

# Odds Ratios
exp(coef(reg2.3))

# Combine Odds Ratios and Confidence Interval
ORtable2.3 <- exp(cbind(OR = coef(reg2.3), ci.2.3))
kable(ORtable2.3)
ortable2.3 <- kable(ORtable2.3, align = 'c', digits = 2)

# Model 2.4 (round 3 only, including socio-economic controls):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived likelihood of being detected (Enforce2)
#   - country fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious

# Step one: get data set that doesn't have any missings on the variables of the model:
data2.4 <- DropNA(round3, c("TaxMorale", "TrustPresident", "CorruptionPresident", 
                            "Enforce2", "Country", "LivingConditions", "Age","Gender", "Religion"))
# 7845 rows dropped from the data frame (that is 30% of all observations)
# take a closer look what that means:
summary(data2.4$TaxMorale)
summary(data2.4$TrustPresident)
summary(data2.4$CorruptionPresident)
summary(data2.4$Enforce2)
summary(data2.4$Country) # no country dropped entirely
summary(data2.4$LivingConditions)
summary(data2.4$Age)
summary(data2.4$Gender)
summary(data2.4$Religion)


# Step two: run the model:
reg2.4 <- polr(TaxMorale ~ TrustPresident + CorruptionPresident +  Enforce2 + Country + 
               LivingConditions + Age + Gender + Religion,
               method='logistic', 
               data=data2.4, 
               Hess = TRUE)

summary(reg2.4) ## 7845 observations deleted due to missingness (that is 30.9% of all observations)

# Store table
(ctable2.4 <- coef(summary(reg2.4)))

# Calculate and store p values
p2.4 <- pnorm(abs(ctable2.4[, "t value"]), lower.tail = FALSE) * 2

# Combined table
(ctable2.4 <- cbind(ctable2.4, "p value" = p2.4))

# Confidence Interval
ci.2.4 <- confint.default(reg2.4)

# Odds Ratios
exp(coef(reg2.4))

# Combine Odds Ratios and Confidence Interval
ORtable2.4 <- exp(cbind(OR = coef(reg2.4), ci.2.4))
kable(ORtable2.4)
ortable2.4 <- kable(ORtable2.4, align = 'c', digits = 2)