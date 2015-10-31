###############################
# Cleaning Afrobarometer Data #
# Wiebke Weiger               #
# 31 October 2015             #
###############################

# set working directory (remember to change to your directory)
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")

# load the data into R from the previously created csv-files:

round2 <- read.csv2("round2.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
round3 <- read.csv2("round3.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
round4 <- read.csv2("round4.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)
round5 <- read.csv2("round5.csv", header=TRUE, sep = ";", quote = "\"",
                    dec = ",", fill = TRUE)

## something went wrong here: when loading the csv-files into R they no longer 
## contain the same number of observations and variables as with the originally
## downloaded data