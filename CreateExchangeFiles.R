
# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the RDBES data from the database - you will need to write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connectio
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

# Load the column name mapping file
#load(file="./output/List_RDBES_Variables_v1.17.Rdata")
# Example of how to change from DB names to R names and vice versa
#rNames <- changeFieldNames(frameToRename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
#names(myRDBESData[["BV"]]) <- rNames
#dbNames <- changeFieldNames(frameToRsename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "RtoDB")
#names(myRDBESData[["BV"]]) <- dbNames

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE)

#errorsSS <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("SS"))

#View(errors[errors$tableName == 'DE',])
#View(allowedValues[allowedValues$listName == 'tRS_UnitOfValue',])

# Create a CE output file
generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCEFile(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCEFile(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)

# Create a CL output file
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCLFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData, numberOfRows=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCLFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateVDFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData, numberOfRows=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateVDFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateSLFile(yearToUse = 2017, country = 'IE',RDBESdata = myRDBESData, numberOfRows=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


# Create an H5 CS file
#generateCSFile_H5(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateCSFile_H5(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)
# Save RData files
#generateH5RDataFiles(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)


# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

generateCSFile_H1(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=10,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateCSFile_H1(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


generateCSFile_H1(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)

# Save RData files
generateH1RDataFiles(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)


# Load RData files
#loadRDataFiles(directoryToSearch="./output/H5")
#loadRDataFiles(directoryToSearch="./output/H1")
