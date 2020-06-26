
# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

# Load the validation data from xsd
validationData <- getValidationData(fileLocation = './tableDefs/BaseTypes.xsd')

# Load the reference data: there are 3 ways to do this:

# 1) Load the reference data from the XSD files
#allowedValues <- loadReferenceDataFromXSD(directoryToSearch = "./referenceData/", recursive = TRUE)

# 2) OR if we have already downloaded the reference data from ICES just load that file
load(file="./referenceData/allowedValues.rData")

# 3) OR we can refresh our reference data from ICES
# (we need to remove the first 't' from the list names before we try and download the lists)
#codeListsToRefresh <- unique(validationData[grep('xs:*', validationData$type, invert = TRUE),'type'])
#codeListsToRefresh <- sub('.', '', codeListsToRefresh)
#allowedValues <- refreshReferenceDataFromICES(codeListsToRefresh)

# Load the column name mapping file
#load(file="./output/List_RDBES_Variables_v1.17.Rdata")

# Load the RDBES data from the database - you will need to write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connectio
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

# Example of how to change from DB names to R names and vice versa
#rNames <- changeFieldNames(frameToRename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
#names(myRDBESData[["BV"]]) <- rNames
#dbNames <- changeFieldNames(frameToRsename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "RtoDB")
#names(myRDBESData[["BV"]]) <- dbNames

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE)

#errorsSA <- errors[errors$tableName == 'SA',]


# Create a CE output file
generateCEFile(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCEFile(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)

# Create a CL output file
generateCLFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData, numberOfRows=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCLFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData)

# Create a VD output file
generateVDFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData, numberOfRows=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateVDFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData)

# Create an H5 CS file
generateCSFile_H5(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateCSFile_H5(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)
# Save RData files
generateH5RDataFiles(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)


# Create an H1 CS file
generateCSFile_H1(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateCSFile_H1(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)

# Save RData files
generateH1RDataFiles(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)


# Load RData files
#loadRDataFiles(directoryToSearch="./output/H5")
#loadRDataFiles(directoryToSearch="./output/H1")
