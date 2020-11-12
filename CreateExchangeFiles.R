
# Load our functions
source("RDBES_Functions.R")


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
#myRDBESData[['DE']][myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Port Sampling',"DEsamplingScheme"] <- "National Routine"
myRDBESData[['FO']][myRDBESData[['FO']]$FOgear == 'OTQ',"FOgear"] <- "OTB"
myRDBESData[['SA']][myRDBESData[['SA']]$sagear == 'OTQ',"SAgear"] <- "OTB"
myRDBESData[['SA']]$SAspeciesCodeFAO <- NA
myRDBESData[['BV']][!is.na(myRDBESData[['BV']]$BVmeasurementEquipment) &  myRDBESData[['BV']]$BVmeasurementEquipment == '',"BVmeasurementEquipment"] <- "Image Processing"
myRDBESData[['FT']]$FTsequenceNumber <- myRDBESData[['FT']]$FTid
myRDBESData[['SA']]$SAsequenceNumber <- myRDBESData[['SA']]$SAid

# If soem of the values for the total weight are large than the maximum value of xs:int
# we wil have a problem during the upload - lets fix this
# Need to check whether these values are due to mistake e.g. choosing units of kg
# rather than boxes
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAtotalWeightLive) & myRDBESData[['SA']]$SAtotalWeightLive > 2147483647,'SAtotalWeightLive'] <- 2147483647
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAtotalWeightMeasured) & myRDBESData[['SA']]$SAtotalWeightMeasured > 2147483647,'SAtotalWeightMeasured'] <- 2147483647

# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","TE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Can check errros from individual tables using e.g.
#View(errors[errors$tableName == 'SA',])

## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD)

# Create a CE output file
#generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
#generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)


# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=500,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

# Create an H5 CS file
#generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

## STEP 5) (OPTIONAL) WE CAN SAVE OUR DATA IF WE WANT TO

# Save RData files for H1
saveRDataFilesForCS(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, RequiredTables = allRequiredTables)
#loadRDataFiles(directoryToSearch="./output/H1")


## STEP 6) (OPTIONAL) If you want to you can switch between using the database field names or the shorter R names for the different columns in our data 

#fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= TRUE, fileLocation = './tableDefs/')
fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= FALSE, fileLocation = './tableDefs/')

myChangedRDBESData <- changeFieldNames(RDBESdata = myRDBESData, fieldNameMap = fieldNameMapping, typeOfChange = "DBtoR")

## STEP 7) (OPTIONAL) We can also read in exchange files and convert them into RDBES data frames

# Read our exhchange files
myExchangeFileCE <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HCE.csv' )
myExchangeFileCL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HCL.csv' )
myExchangeFileVD <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HVD.csv' )
myExchangeFileSL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HSL.csv' )
myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_H1.csv',RequiredTables = allRequiredTables )
myExchangeFileH5 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_H5.csv',RequiredTables = allRequiredTables )

# We can combine the data we have read in into a single list
myExchangeFileRDBESData <- c(myExchangeFileCE,myExchangeFileCL,myExchangeFileVD,myExchangeFileSL,myExchangeFileH5)

# For interest lets validate the data we just read in - it should be fine if the exchange file was produced using clean, valid data
errorsExchangeFiles <- validateTables(RDBESdata = myExchangeFileRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Now we can compare the CS data we have just imported from the exchange file with the data loaded from the database - if all has gone well they shoudl be the same

# Our RDBES data loaded from the database
dataSet1 <- myRDBESData

# When we created the Exchange file we probably cleaned and filtered the RDBES data - if so, we need to do that again for dataSet1 so that we have a fair comparison 
dataSet1HierarchyToCheck <- 'H1'
dataSet1YearToCheck <- 2019
dataSet1CountryToCheck <- 'IE'

# Clean and filter DataSet1 so that we are comparing like-with-like (this is assuming we cleaned and filtered the data when we generated the exchange file - if not you don't need to do this)
dataSet1 <- filterCSData(RDBESdata = dataSet1 , RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]], YearToFilterBy = dataSet1YearToCheck, CountryToFilterBy = dataSet1CountryToCheck, UpperHierarchyToFilterBy = substr(dataSet1HierarchyToCheck,2,nchar(dataSet1HierarchyToCheck)))
dataSet1 <- cleanCSData(DataToClean = dataSet1,RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]], YearToFilterBy = dataSet1YearToCheck, CountryToFilterBy = dataSet1CountryToCheck,UpperHierarchyToFilterBy = substr(dataSet1HierarchyToCheck,2,nchar(dataSet1HierarchyToCheck)))
dataSet1 <- limitSamplesInCSData(DataToFilter = dataSet1, NumberOfSamples = 500, RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]])

# This is the CS data we have read in from the Exchange file
dataSet2 <- myExchangeFileH1
#dataSet2 <- myExchangeFileH5


# Now we can compare our 2 CS data sets
compareCSData(dataSet1 = dataSet1,dataSet2 = dataSet2, RequiredTables = allRequiredTables)


# Ok, let's compare our CE, CL, VD, and SL exchange files now

# Our RDBES data loaded from the database
dataSet1 <- list(CE = myRDBESData[['CE']],CL = myRDBESData[['CL']],SL = myRDBESData[['SL']],VD = myRDBESData[['VD']])
# When we created the Exchange file we probably cleaned the RDBES data - if so, we need to do that again for dataSet1 so that we have a fair comparison  
myErrors <- validateTables(RDBESdata = dataSet1,RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues,shortOutput = FALSE,framestoValidate = c('CE','CL','SL','VD'))
# Remove any invalid rows 
for (myRequiredTable in c('CE','CL','SL','VD')){
  dataSet1[[myRequiredTable]]<- removeInvalidRows(tableName = myRequiredTable,dataToClean = dataSet1[[myRequiredTable]],errorList = myErrors)
}

# This is the data we have read in from the Exchange file 
dataSet2 <- c(myExchangeFileCE,myExchangeFileCL,myExchangeFileVD,myExchangeFileSL)

# Now run the comparison for our 4 types of data
for (aTable in c('CE','CL','SL','VD')){
  print(paste("Checking ",aTable,sep=""))
  compareSimpleData(dataSet1 = dataSet1,dataSet2 = dataSet2,tableType = aTable)
}



