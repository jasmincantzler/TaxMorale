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


# for merging use rbind.fill (from plyr-package)

