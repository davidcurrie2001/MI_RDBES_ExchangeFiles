
# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy
requiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#requiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

# Load the RDBES data from the database - you will need to write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connectio
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))


## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# UGLY FIX 
# Get rid of the errors associates with XXgsaSubarea having a value of NotApplicable - I fix these during the export anyway - can remove this once ICES code list is updated
errors <- errors[!(grepl('gsaSubarea',errors$fieldName) & errors$problemType =='Code list problem'),]

# Can check errros from individual tables using e.g.
#View(errors[errors$tableName == 'BV',])

## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD)

# Create a CE output file
generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)

#UGLY HACKS - these are temporary fixes for my data - they will be removed once correct values are added to reference lists

# Need to change the value of DEsamplingScheme until the correct values are added to the ICES code list
#myRDBESData[['DE']][myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Catch Sampling' | myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Catch Sampling (4S)','DEsamplingScheme'] <- 'National Routine'
  
# Temporarily change the value of FMmeasurementEquipment until ICES add the correct value
#myRDBESData[['FM']][myRDBESData[['FM']]$FMmeasurementEquipment == 'Measured by hand with ruler','FMmeasurementEquipment'] <- 'Measured by hand with caliper'
  
# Truncate any decimal places from SAconversionFactorMesLive - shouldn't be required
#myRDBESData[['SA']]$SAconversionFactorMesLive <- floor(myRDBESData[['SA']]$SAconversionFactorMesLive)
  


# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)



## These functions aren't updated yet

# Load the column name mapping file
#load(file="./output/List_RDBES_Variables_v1.17.Rdata")
# Example of how to change from DB names to R names and vice versa
#rNames <- changeFieldNames(frameToRename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
#names(myRDBESData[["BV"]]) <- rNames
#dbNames <- changeFieldNames(frameToRsename = myRDBESData[["BV"]], fieldNameMap = list_RDBES_Variables, typeOfChange = "RtoDB")
#names(myRDBESData[["BV"]]) <- dbNames

# Save RData files
generateH1RDataFiles(yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData)


# Load RData files
#loadRDataFiles(directoryToSearch="./output/H5")
#loadRDataFiles(directoryToSearch="./output/H1")
