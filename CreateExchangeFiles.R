
# Load our functions
source("RDBES_Functions.R")
source("ImportReferenceData.R")

# Load the column name mapping file
load(file="./output/List_RDBES_Variables_v1.17.Rdata")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

# Load the allowed values from the XSD files
allowedValues <- loadReferenceDataFromXSD(directoryToSearch = "./referenceData/", recursive = TRUE)

# Load the RDBES data from the database
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

# Create a CE output file
generateCEFile(yearToUse = 2016, country = 'IRL', RDBESdata = myRDBESData)

# Create a CL output file
generateCLFile(yearToUse = 2016, country = 'IRL',RDBESdata = myRDBESData)

# Create an H5 CS file
generateCSFile_H5(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10)
generateCSFile_H5(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
# Save RData files
generateH5RDataFiles(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)


# Create an H1 CS file
generateCSFile_H1(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20)
generateCSFile_H1(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)

# Save RData files
generateH1RDataFiles(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)


# Load RData files
#loadRDataFiles(directoryToSearch="./output/H5")
#loadRDataFiles(directoryToSearch="./output/H1")
