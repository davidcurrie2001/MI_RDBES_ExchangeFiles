# This script is used to develp and test functiosn related to describing sources
# of potential bias in the RDBES data.  Linked to the ICES Special Request.

# Load our functions
source("RDBES_Bias_Functions.R")


# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 11/9/2020 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Stratification','type'] <- 'tYesNoFields'

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

# Load the RDBES data from the database - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were created using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))


## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

#UGLY HACKS - these are temporary fixes for my data - they will be removed once correct values are added to reference lists
# Temporary fixes
myRDBESData[['FO']][myRDBESData[['FO']]$FOgear == 'OTQ',"FOgear"] <- "OTB"
myRDBESData[['SA']][myRDBESData[['SA']]$sagear == 'OTQ',"SAgear"] <- "OTB"
myRDBESData[['SA']]$SAspeciesCodeFAO <- NA
myRDBESData[['BV']][!is.na(myRDBESData[['BV']]$BVmeasurementEquipment) &  myRDBESData[['BV']]$BVmeasurementEquipment == '',"BVmeasurementEquipment"] <- "Image Processing"
myRDBESData[['FT']]$FTsequenceNumber <- myRDBESData[['FT']]$FTid
myRDBESData[['SA']]$SAsequenceNumber <- myRDBESData[['SA']]$SAid

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","TE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Run our first function
myResult <- summariseSelectionMethods(hierarchyToCheck = 'H1', yearToUse = 2019, country = 'IE', rdbesData = myRDBESData, requiredTables = allRequiredTables)

# save to csv 
#write.csv(myResult$joinedData, file="summaryOutput.csv")

