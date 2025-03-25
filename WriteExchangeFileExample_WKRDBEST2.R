## Example of how to write exchange files 

# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# TODO
# Load the RDBES data - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were created using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))


## (OPTIONAL) If you want to you can switch between using the database field names or the shorter R names for the different columns in our data 
fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= TRUE, fileLocation = './tableDefs/')
#fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= FALSE, fileLocation = './tableDefs/')
#myChangedRDBESData <- changeFieldNames(RDBESdata = myRDBESData, fieldNameMap = fieldNameMapping, typeOfChange = "DBtoR")


# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies()


## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

#UGLY HACKS - put any ugly temporary fixes e.g.
#myRDBESData[['DE']][myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Port Sampling',"DEsamplingScheme"] <- "National Routine"

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Can check errors from individual tables using e.g.
#View(errors[errors$tableName == 'FT',])

## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD) (change the year and country to what you want, change the value of cleanData to FALSE is you don't want to remove invalid rows)

# Create a CE output file
generateExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
generateExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateExchangeFile(typeOfFile = 'VD', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS and SL)  (change the hierarchy, year and country to what you want, change the value of cleanData to FALSE is you don't want to remove invalid rows)

# Create a SL output file
generateExchangeFile(typeOfFile = 'SL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

# Create an H1 CS file
generateExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)


