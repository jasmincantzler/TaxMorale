##########################################
# Determinants of Tax Morale in Africa   #
# Models for ordered logistic regression #
# Jasmin Cantzler & Wiebke Weiger        #
# 3 December 2015                        #
##########################################


# Set working directory
#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Final Project")
#setwd("/Users/jasmincantzler/Documents/TaxMorale/Final Project")

# packages needed:
library(plyr)
library(dplyr)
library(MASS)
library(corrplot) 
library(knitr)
library(stargazer)
library(tidyr)

# data:
data.numeric <- read.csv('Afrobarometer.nofactors.csv')
data.factors <- read.csv('Afrobarometer.final.csv')
round5 <- read.csv('round5.final.csv')
round4 <- read.csv('round4.final.csv')
round3 <- read.csv('round3.final.csv')

# First step: Sort factor levels (they are currently in alphabetical order) for output to be more easily
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

# 2) Trust parliament/national assembly

# 3) Trust courts of law

# 4) Trust tax department
# same for round 5 only:

# B) CORRUPTION:
# 1) Office of the President/Prime Minister involved in corruptoion
levels(data.factors$CorruptionPresident) # gives the current order of the levels:
# [1] "All of them"  "Most of them" "None"         "Some of them"
# correct order would be: 3, 4, 2, 1 
# change order: 
data.factors$CorruptionPresident = factor(data.factors$CorruptionPresident,levels(data.factors$CorruptionPresident)[c(3,4,2,1)])
levels(data.factors$CorruptionPresident) # now order is correct:
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

# D) Gender
data.factors$Gender = factor(data.factors$Gender,levels(data.factors$Gender)[c(2,1)])

# E) Religious
levels(data.factors$Religion) # gives the current order of the levels:
# [1] "Active member"   "Inactive member" "Not a member"    "Official Leader"
# correct order would be: 3,2,1,4 
# change order: 
data.factors$Religion = factor(data.factors$Religion,levels(data.factors$Religion)[c(3,2,1,4)])
levels(data.factors$Religion) # now order is correct:
# [1] "Not a member"    "Inactive member" "Active member"   "Official Leader"

# F) Perceived frequency of others avoiding taxes (round 5)
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

# G) Self employed (round 5)
levels(data.factors$SelfEmployedTax) # gives the current order of the levels:
# [1] "Not required to pay" "Required to pay"
# not changes necessary

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





# Models:
# 1. Models including data for all three years (only those variables that are available in all rounds)
# Model 1.1: 
# Tax morale as a function of 
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - country fixed effects
#   - year fixed effects

# Model 1.2 (including socio-economic controls):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - country fixed effects
#   - year fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious 

# Model 1.3 (same as 1.1, but with tax morale coded with only 3 levels - disagree, neither nor, agree)

# Model 1.4 (same as 1.2, but with tax morale coded with only 3 levels - disagree, neither nor, agree)

# 2. Sub analyses for single years (with variables not available in all rounds)
# Model 2.1 (round 5 only):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - trust in the tax department
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived frequency of others avoiding taxes
#   - self-employed
#   - country fixed effects

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

# Model 2.3 (round 3 only):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived likelihood of being detected
#   - country fixed effects

# Model 2.4 (round 3 only, including socio-economic controls):
#   - trust in the government (trying out various versions, e.g. trust in president, trust in parliament, etc)
#   - level of corruption (trying out various versions, e.g. president corrupt, parliament corrupt, etc.)
#   - perceived likelihood of being detected (Enforce2)
#   - country fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious



