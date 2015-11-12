##########################################################
# Makefile Afrobarometer Data for analysis of tax morale #
# Wiebke Weiger & Jasmin Cantzler                        # 
# last updated: 12 November 2015                         #
##########################################################

# Set working directory
#setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
setwd("/Users/jasmincantzler/Documents/TaxMorale/Assignment 3/PreparatoryWork")

# Gather and cleanup raw data files.
source("data_preperation.R")

# Merge cleaned data frames into data frame object CleanedData
source("merging_data.R")

