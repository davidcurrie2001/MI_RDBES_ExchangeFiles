
# Load our functions
source("RDBES_Functions.R")
source("ImportReferenceData.R")

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
generateCSFile_H5(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)

# Create an H1 CS file
generateCSFile_H1(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
