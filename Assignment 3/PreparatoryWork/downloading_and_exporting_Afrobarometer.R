################################
#  Loading Afrobarometer Data  #
#  Wiebke Weiger               #
#  31 October 2015             #
# ##############################


# set the working directory:
setwd("C:/Users/Wiebke/Documents/RepRes/TaxMorale/Assignment 3/PreparatoryWork")
# in this case really important, because it specifies where the csv-files are stored
# unless you specify the file-path in the write.cvs command

# packages used for this Rfile:
library(rio) # needed to download data from the Afrobarometer website in SPSS format
library(foreign) # needed to export data from R to other formats


# 1) Download Afrobarometer data from website

# round 5 of the Afrobarometer: 
round5 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-5/merged_r5_data_0.sav")

# export data into csv format:
write.csv2(round5, file="round5.csv", quote=FALSE, na="NA")
# we use "write.csv2" here because this R-source file is run on a German computer
# where Excel is programmed to recognize a comma as a decimal point and a 
# semicolon for the seperator which "csv2" accounts for 
# if you want to use it on a different computer with the American setting please
# run:
# write.csv(round5, file="round5.csv", quote=FALSE, na="NA") # which usese "." for
# the decimal point and a comma for the seperator

# export as table:

write.table(round5, file = "round5_table",
            sep = " ", na = "NA", row.names = TRUE, col.names = TRUE)



# round 4 of the Afrobarometer: 
round4 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav")

# export data into csv format:
write.csv2(round4, file="round4.csv", quote=FALSE, na="NA")
# write.csv(round4, file="round4.csv", quote=FALSE, na="NA")

# export as table
write.table(round4, file = "round4_table",
            sep = " ", na = "NA", row.names = TRUE, col.names = TRUE)



# round 3 of the Afrobarometer: 
round3 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav")

# export data into csv format:
write.csv2(round3, file="round3.csv", quote=FALSE, na="NA")
# write.csv(round3, file="round3.csv", quote=FALSE, na="NA")

# export as table
write.table(round3, file = "round3_table",
            sep = " ", na = "NA", row.names = TRUE, col.names = TRUE)


# round 2 of the Afrobarometer:
round2 <- rio::import("http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav")

# export data into csv format:
write.csv2(round2, file="round2.csv", quote=FALSE, na="NA")
# write.csv(round2, file="round2.csv", quote=FALSE, na="NA")

# export as table
write.table(round2, file = "round2_table",
            sep = " ", na = "NA", row.names = TRUE, col.names = TRUE)


# saving data in RData format:
# this way we can load the data in again into R and the number of observations and
# variables stays the same, other than with the "read.csv2"- command

save(round5, file="Round5.RData", list="round5")
save(round4, file="Round4.RData", list="round4")
save(round3, file="Round3.RData", list="round3")
save(round2, file="Round2.RData", list="round2")


