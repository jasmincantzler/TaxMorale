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
round5 <- read.csv('round5.final')
round4 <- read.csv('round4.final')
round3 <- read.csv('round3.final')


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
#   - perceived likelihood of being detected
#   - country fixed effects
#   - living conditions
#   - age
#   - gender
#   - religious



