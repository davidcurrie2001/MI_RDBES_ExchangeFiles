
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

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
requiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#requiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

# Load the RDBES data from the database - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were creates using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))


## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

#UGLY HACKS - these are temporary fixes for my data - they will be removed once correct values are added to reference lists
# Temporary fixes
myRDBESData[['DE']][myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Port Sampling',"DEsamplingScheme"] <- "National Routine"
myRDBESData[['FO']][myRDBESData[['FO']]$FOgear == 'OTQ',"FOgear"] <- "OTB"
myRDBESData[['SA']][myRDBESData[['SA']]$sagear == 'OTQ',"SAgear"] <- "OTB"
myRDBESData[['SA']]$SAspeciesCodeFAO <- NA

#myRDBESData[['FT']]$FTencryptedVesselCode <- NA

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Can check errros from individual tables using e.g.
#View(errors[errors$tableName == 'FT',])

## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD)

# Create a CE output file
generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)

# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create an H5 CS file
generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)

## STEP 5) (OPTIONAL) WE CAN SAVE OUR DATA IF WE WANT TO

# Save RData files for H1
saveRDataFilesForCS(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, RequiredTables = requiredTables)
#loadRDataFiles(directoryToSearch="./output/H1")


## STEP 6) (OPTIONAL) If you want to you can switch between using the database field names or the shorter R names for the different columns in our data 

#fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= TRUE, fileLocation = './tableDefs/')
fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= FALSE, fileLocation = './tableDefs/')

myChangedRDBESData <- changeFieldNames(RDBESdata = myRDBESData, fieldNameMap = fieldNameMapping, typeOfChange = "DBtoR")


