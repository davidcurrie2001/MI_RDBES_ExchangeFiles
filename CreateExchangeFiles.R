
# Load our functions
source("RDBES_Functions.R")
# Temporary fix required for a function from icesVocab - otherwise the function breaks when it tries to download the EDMO code list (or any list containing carriage returns)
source("tempIcesVocabFix.R")

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
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

# 8/9/21 temp fix - need to check XSD files are up-to-date
allRequiredTables[["H12"]]<- c("DE","SD","LO","TE","LE","SS","SA","FM","BV")
allRequiredTables[["H13"]]<- c("DE","SD","FO","SS","SA","FM","BV")

# Load the RDBES data from the database - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were created using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

# 30/8/2021 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Sex','type'] <- 'tSEXCO'

#DATA FIXES - these are fixes for my data - they probably aren't required for anybody else's data
# - some are just temporary fixes whilst values get requested to be added to code list

# Misc temp fixes
#myRDBESData[['FO']][myRDBESData[['FO']]$FOgear == 'OTQ',"FOgear"] <- "OTB"
#myRDBESData[['SA']][myRDBESData[['SA']]$sagear == 'OTQ',"SAgear"] <- "OTB"

# SA fixes
# Get rid of any FAO species names that aren't in the code list
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  !myRDBESData[['SA']]$SAspeciesCodeFAO %in% allowedValues[allowedValues$listName == 'tSpecASFIS','Key'],'SAspeciesCodeFAO'] <- NA

# CE fixes
# Get rid of NA areas
myRDBESData[['CE']]<-myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEarea),]
# Get rid of 47.1.1, 47.1.3 areas 
myRDBESData[['CE']] <- myRDBESData[['CE']][!myRDBESData[['CE']]$CEarea %in% c('47.1.1','47.1.3'),]
# Get rid of zero fishing hours
myRDBESData[['CE']] <- myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEofficialVesselFishingHour) & myRDBESData[['CE']]$CEofficialVesselFishingHour > 0,]
# Get rid of zero fishing days
myRDBESData[['CE']] <- myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEofficialFishingDays ) & myRDBESData[['CE']]$CEofficialFishingDays > 0,]

# CL fixes
# Get rid of NA areas
myRDBESData[['CL']]<-myRDBESData[['CL']][!is.na(myRDBESData[['CL']]$CLarea),]
# Get rid of 47.1.1, 47.1.3 areas 
myRDBESData[['CL']] <- myRDBESData[['CL']][!myRDBESData[['CL']]$CLarea %in% c('47.1.1','47.1.3'),]
# Get rid of NA landing country
myRDBESData[['CL']]<-myRDBESData[['CL']][!is.na(myRDBESData[['CL']]$CLlandingCountry),]
# Get rid of NA landing location
myRDBESData[['CL']]<-myRDBESData[['CL']][!is.na(myRDBESData[['CL']]$CLlandingLocation),]
# Get rid of NA species
myRDBESData[['CL']]<-myRDBESData[['CL']][!is.na(myRDBESData[['CL']]$CLspeciesCode),]
# Get rid of zero offical weight
myRDBESData[['CL']] <- myRDBESData[['CL']][!is.na(myRDBESData[['CL']]$CLofficialWeight ) & myRDBESData[['CL']]$CLofficialWeight > 0,]
# Set null landing value to them smallest value allowed
myRDBESData[['CL']][is.na(myRDBESData[['CL']]$CLtotalOfficialLandingsValue),'CLtotalOfficialLandingsValue'] <- 0


# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","TE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Can check errors from individual tables using e.g.
View(errors[errors$tableName == 'SA',])

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
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

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
dataSet1HierarchyToCheck <- 'H5'
dataSet1YearToCheck <- 2019
dataSet1CountryToCheck <- 'IE'

# Clean and filter DataSet1 so that we are comparing like-with-like (this is assuming we cleaned and filtered the data when we generated the exchange file - if not you don't need to do this)
dataSet1 <- filterCSData(RDBESdata = dataSet1 , RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]], YearToFilterBy = dataSet1YearToCheck, CountryToFilterBy = dataSet1CountryToCheck, UpperHierarchyToFilterBy = substr(dataSet1HierarchyToCheck,2,nchar(dataSet1HierarchyToCheck)))
dataSet1 <- cleanCSData(DataToClean = dataSet1,RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]], YearToFilterBy = dataSet1YearToCheck, CountryToFilterBy = dataSet1CountryToCheck,UpperHierarchyToFilterBy = substr(dataSet1HierarchyToCheck,2,nchar(dataSet1HierarchyToCheck)))
dataSet1 <- limitSamplesInCSData(DataToFilter = dataSet1, NumberOfSamples = 50, RequiredTables = allRequiredTables[[dataSet1HierarchyToCheck]])

# This is the CS data we have read in from the Exchange file
#dataSet2 <- myExchangeFileH1
dataSet2 <- myExchangeFileH5


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



