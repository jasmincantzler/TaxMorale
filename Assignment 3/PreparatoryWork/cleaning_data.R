###############################
# Cleaning Afrobarometer Data #
# Wiebke Weiger               #
# 31 October 2015             #
###############################

# set working directory (remember to change to your directory)
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")

# load the data into R from the previously created csv-files:

#round2 <- read.csv2("round2.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
#round3 <- read.csv2("round3.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
#round4 <- read.csv2("round4.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
#round5 <- read.csv2("round5.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)

## something went wrong here: when loading the csv-files into R they no longer 
## contain the same number of observations and variables as with the originally
## downloaded data

# instead: load data from saved Rdata-files;

load("Round5.RData")
load("Round4.RData")
load("Round3.RData")
load("Round2.RData")
## this time the number of observations and variables stayed the same

## another option is via "read.table":
round5_table <- read.table("round5_table")
round4_table <- read.table("round4_table")
round3_table <- read.table("round3_table")
round2_table <- read.table("round2_table")