
# Load our functions
source("RDBES_Functions.R")

outputFolder <- "./output/"

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

# Load the RDBES data from the database
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

# Create a CE output file
generateCEFile(yearToUse = 2016, RDBESdata = myRDBESData, outputFileName = paste(outputFolder,"IRL_CE.csv", sep = ""))

# Create a CL output file
generateCLFile(yearToUse = 2016, RDBESdata = myRDBESData, outputFileName = paste(outputFolder,"IRL_CL.csv", sep = ""))

# Create an H5 CS file
generateCSFile_H5(yearToUse = 2016, RDBESdata = myRDBESData, outputFileName = paste(outputFolder,"IRL_H5.csv", sep = ""))

