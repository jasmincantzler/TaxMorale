##############################################
#  Loading and exporting Afrobarometer Data  #
#  Wiebke Weiger                             #
#  last updated: 7 November 2015             # 
# ############################################


# set the working directory:
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
# in this case really important, because it specifies where the csv-files are stored
# unless you specify the file-path in the write.cvs command

# packages used for this Rfile:
library(rio) # needed to download data from the Afrobarometer website in SPSS format
library(foreign) # needed to export data from R to other formats
library(dplyr)


# 1) Download Afrobarometer data from website

# Downloading round 5 of the Afrobarometer: 
round5 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-5/merged_r5_data_0.sav")

# Round 5: shrinking data set to relevant variables:
# list of variables to keep and their column numbers:
names(round5)
# [2]   "COUNTRY_ALPHA" - County
# [5]   "RESPNO" - Respondent number
# [6]   "URBRUR" - Urban/Rural
# [39]  "Q1" - Age of respondent
# [42]  "Q3A" - Respondent's perception of economic situation
# [43]  "Q3B" - Respondent's living conditions
# [68]  "Q14" - Respondent's interest in public affairs
# [81]  "Q25A" - Member of religous group
# [139] "Q48C" - Tax morale (dependent variable)
# [162] "Q59A" - Trust the president
# [163] "Q59B" - Trust the parliament/national assembly
# [171] "Q59J" - Trust in courts
# [172] "Q60A" - Corruption: office of the president
# [173] "Q60B" - Corruption: members of parliament
# [174] "Q60C" - Corruption: government officials
# [175] "Q60D" - Corruption: local government councilors
# [177] "Q60F" - Corruption: tax officials
# [307] "Q101" - Respondent's gender

# unique to round 5:
# [159] "Q56I" - How often avoid paying taxes
# [165] "Q59D" - Trust in tax department
# [249] "Q73E" - self-employer taxes
# [256] "Q77" - reasons for avoiding taxes

keep_vars5 = c(2, 5, 6, 39, 42, 43, 68, 81, 139, 162, 
               163, 171, 172, 173, 174, 175, 177, 307,
               159, 165, 249, 256)

round5.small <- round5[, keep_vars5]

# renaming variables (these will be the variable names for all three subsets):
colnames(round5.small) [c(1:22)] <- c("Country", "Respondent", "UrbanRural", "Age",
                                   "EconomicSituation", "LivingConditions", "Interest",
                                   "Religion", "TaxMorale", "TrustPresident", 
                                   "TrustParliament", "TrustCourts", "CorruptionPresident",
                                   "CorruptionParliament", "CorruptionOfficials",
                                   "CorruptionCouncilors", "CorruptionTax", "Gender",
                                   "AvoidHowOften", "TrustTax", "SelfEmployedTax",
                                   "AvoidReason")

# variables that exist in round 3 (and) round 4, but not in round 5:
# "EconomicPolicies" 
# --> this variable exists both in round 3 and 4
# "LocalGvtTaxes"
# "Enforce1"
# "Enforce2" 
# --> all three are only present in round 3 
# "CorruptionOfficialsLocal"
# "CorruptionOfficialsNational" 
# --> this distinction is only made in round 3; in round 4 and 5 there is just 
# one variable for both which is "CorruptionOfficials" 


# create these variables for round 5 with missings
round5.small$EconomicPolicies <- NA
round5.small$LocalGvtTaxes <- NA
round5.small$Enforce1 <- NA
round5.small$Enforce2 <- NA
round5.small$CorruptionOfficialsLocal <- NA
round5.small$CorruptionOfficialsNational <- NA

# create year variable:
round5.small$Year <- 2015

# change order of variables, Year variable as second column:
round5.small <- round5.small[,c(1,29,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                20,21,22,23,24,25,26,27,28)]

# export data into csv format:
export(round5.small, file="round5.small.csv")

# save smaller data set in Rdata format:
save(round5.small, file="Round5.small.RData", list="round5.small")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Downloading round 4 of the Afrobarometer: 
round4 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav")

# Round 4: shrinking data set to relevant variables and renaming variables:
# list of variables to keep and their column numbers:
names(round4)
# [1]   "COUNTRY" - County
# [2]   "RESPNO" - Respondent number
# [3]   "URBRUR" - Urban/Rural
# [32]  "Q1" - Age of respondent
# [36]  "Q4A" - Respondent's perception of economic situation
# [37]  "Q4B" - Respondent's living conditions
# [56]  "Q13" - Respondent's interest in public affairs
# [67]  "Q22A" - Member of religous group
# [111] "Q44C" - Tax morale (dependent variable)
# [121] "Q49A" - Trust the president
# [122] "Q49B" - Trust the parliament/national assembly
# [128] "Q49H" - Trust in courts
# [130] "Q50A" - Corruption: office of the president
# [131] "Q50B" - Corruption: members of parliament
# [132] "Q50C" - Corruption: local government councilors
# [133] "Q50D" - Corruption: government officials
# [135] "Q50F" - Corruption: tax officials
# [267] "Q101" - Respondent's gender


# variables that are only in round 4 and 3:
# [52] "Q11" - Economic policies helped most vs. Hurt most


keep_vars4 =c(1, 2, 3, 32, 36, 37, 56, 67, 111, 121, 
              122, 128, 130, 131, 132, 133, 135, 267, 
              52)

round4.small <- round4[, keep_vars4]

# renaming variables
colnames(round4.small) [c(1:19)] <- c("Country", "Respondent", "UrbanRural", "Age",
                                   "EconomicSituation", "LivingConditions", "Interest",
                                   "Religion", "TaxMorale", "TrustPresident", 
                                   "TrustParliament", "TrustCourts", "CorruptionPresident",
                                   "CorruptionParliament", "CorruptionCouncilors", 
                                   "CorruptionOfficials", "CorruptionTax", "Gender",
                                   "EconomicPolicies")



# variables that exist in round 3 (and) round 5, but not in round 4:
# "AvoidHowOften"
# "TrustTax"
# "SelfEmployedTax"
# "AvoidReason" 
# --> all four are only present in round 5 
# "LocalGvtTaxes"
# "Enforce1"
# "Enforce2" 
# --> all three are only present in round 3 
# "CorruptionOfficialsLocal"
# "CorruptionOfficialsNational" 
# --> this distinction is only made in round 3; in round 4 and 5 there is just 
# one variable for both which is "CorruptionOfficials" 

# create these variables for round 4 with missings:
round4.small$AvoidHowOften <- NA
round4.small$TrustTax <- NA
round4.small$SelfEmployedTax <- NA
round4.small$AvoidReason <- NA
round4.small$LocalGvtTaxes <- NA
round4.small$Enforce1 <- NA
round4.small$Enforce2 <- NA
round4.small$CorruptionOfficialsLocal <- NA
round4.small$CorruptionOfficialsNational <- NA

# create year variable:
round4.small$Year <- 2008

# change order of variables so that they are identical to round 5:
round4.small <- round4.small[,c(1,29,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,20,
                                21,22,23,19,24,25,26,27,28)]




# export data into csv format:
export(round4.small, file="round4.small.csv")

# save smaller data set in Rdata format:
save(round4.small, file="Round4.small.RData", list="round4.small")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Downloading round 3 of the Afrobarometer: 
round3 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav")

# Round 3: shrinking data set to relevant variables and renaming variables:
# list of variables to keep and their column numbers:
names(round3)

# [1]   "country" - County
# [2]   "respno" - Respondent number
# [4]   "urbur" - Urban/Rural
# [19]  "q1" - Age of respondent
# [22]  "q4a" - Respondent's perception of economic situation
# [23]  "q4b" - Respondent's living conditions
# [48]  "q16" - Respondent's interest in public affairs
# [61]  "q28a" - Member of religous group
# [116] "q52d" - Tax morale (dependent variable)
# [128] "q55a" - Trust the president
# [129] "q55b" - Trust the parliament/national assembly
# [136] "q55i" - Trust in courts
# [141] "q56a" - Corruption: office of the president
# [142] "q56b" - Corruption: members of parliament
# [143] "q56c" - Corruption: local government councilors
# [144] "q56d" - Corruption: national government officials # note: seperation 
# [145] "q56e" - Corruption: local government officials # note: seperation
# [147] "q56g" - Corruption: tax officials
# [261] "Q101" - Respondent's gender

# variables that are only in round 4 and 3:
# [41] "q13" - Economic policies helped most vs. Hurt most

# variables unique to round 3: 
# [183] "q67c" - Local govt. handling collecting local taxes
# [192] "q70c" - Enforce law: Top official doesn't pay tax
# [193] "q70d" - Enforce law: You don't pay tax



keep_vars3 = c(1, 2, 4, 19, 22, 23, 48, 61, 116, 128, 
               129, 136, 141, 142, 143, 144, 145, 147,
               261, 41, 183, 192, 193)

round3.small <- round3[, keep_vars3]

# renaming variables
colnames(round3.small) [c(1:23)] <- c("Country", "Respondent", "UrbanRural", "Age",
                                   "EconomicSituation", "LivingConditions", "Interest",
                                   "Religion", "TaxMorale", "TrustPresident", 
                                   "TrustParliament", "TrustCourts", "CorruptionPresident",
                                   "CorruptionParliament", "CorruptionCouncilors", 
                                   "CorruptionOfficialsLocal", "CorruptionOfficialsNational", 
                                   "CorruptionTax", "Gender", "EconomicPolicies",
                                   "LocalGvtTaxes", "Enforce1", "Enforce2")



# variables that exist in round 4 (and) round 5, but not in round 3:
# "AvoidHowOften"
# "TrustTax"
# "SelfEmployedTax"
# "AvoidReason" 
# --> all four are only present in round 5
# "CorruptionOfficials"
# --> instead of the distinction made in round 3, round 4 and 5 only have one
# variable for corruption among government officials

# create these variables for round 4 with missings:
round3.small$CorruptionOfficials <- NA
round3.small$AvoidHowOften <- NA
round3.small$TrustTax <- NA
round3.small$SelfEmployedTax <- NA
round3.small$AvoidReason <- NA

# create year variable:
round3.small$Year <- 2005

# change order of variables so that they are identical to round 5:
round3.small <- round3.small[,c(1,29,2,3,4,5,6,7,8,9,10,11,12,13,14,24,15,18,19,25,
                                26,27,28,20,21,22,23,16,17)]




# export data into csv format:
export(round3.small, file="round3.small.csv")

# save smaller data set in Rdata format:
save(round3.small, file="Round3.small.RData", list="round3.small")

################################################################################
# end of file
################################################################################



