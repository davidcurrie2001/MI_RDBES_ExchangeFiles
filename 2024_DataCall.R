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
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')

# Load the RDBES data from the database - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were created using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))

## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS


#DATA FIXES - these are fixes for my data - they probably aren't required for anybody else's data
# - some are just temporary fixes whilst values get requested to be added to code list

# DE fixes
myRDBESData[['DE']]$MI_Created_Date <- NULL


# FO fixes
# Get rid of any values in FOnationalFishingActivity - anything in here woudl need to be added to an ICES code list first
myRDBESData[['FO']]$FOnationalFishingActivity <- NA

# LE fixes
# Get rid of any values in SAnationalFishingActivity - anything in here woudl need to be added to an ICES code list first
myRDBESData[['LE']]$LEnationalFishingActivity <- NA
# PTM_SPF_40-54_0_0 is not allowed in 27.2.a or 27.4.a -> change to PTM_SPF_32-69_0_0
myRDBESData[['LE']][!is.na(myRDBESData[['LE']]$LEmetier6) &  myRDBESData[['LE']]$LEmetier6 == 'PTM_SPF_40-54_0_0' & !is.na(myRDBESData[['LE']]$LEarea) &  myRDBESData[['LE']]$LEarea %in% c('27.2.a','27.4.a'),'LEmetier6'] <- 'PTM_SPF_32-69_0_0'

# SA fixes
# Get rid of any FAO species names that aren't in the code list
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  !myRDBESData[['SA']]$SAspeciesCodeFAO %in% allowedValues[allowedValues$listName == 'tSpecASFIS','Key'],'SAspeciesCodeFAO'] <- NA
# If soem of the values for the total weight are larger than the maximum value of xs:int
# we wil have a problem during the upload - lets fix this
# Need to check whether these values are due to mistake e.g. choosing units of kg
# rather than boxes
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAtotalWeightLive) & myRDBESData[['SA']]$SAtotalWeightLive > 2147483647,'SAtotalWeightLive'] <- 2147483647
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAtotalWeightMeasured) & myRDBESData[['SA']]$SAtotalWeightMeasured > 2147483647,'SAtotalWeightMeasured'] <- 2147483647
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAsampleWeightLive) & myRDBESData[['SA']]$SAsampleWeightLive > 2147483647,'SAsampleWeightLive'] <- 2147483647
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAsampleWeightMeasured) & myRDBESData[['SA']]$SAsampleWeightMeasured > 2147483647,'SAsampleWeightMeasured'] <- 2147483647
# Get rid of any values in SAnationalFishingActivity - anything in here woudl need to be added to an ICES code list first
myRDBESData[['SA']]$SAnationalFishingActivity <- NA
# SAtotalWeightLive should be an int
myRDBESData[['SA']]$SAtotalWeightLive <- as.integer(myRDBESData[['SA']]$SAtotalWeightLive)
# PTM_SPF_40-54_0_0 is not allowed in 27.2.a or 27.4.a -> change to PTM_SPF_32-69_0_0
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAmetier6) & myRDBESData[['SA']]$SAmetier6 == 'PTM_SPF_40-54_0_0' & !is.na(myRDBESData[['SA']]$SAarea) & myRDBESData[['SA']]$SAarea %in% c('27.2.a','27.4.a'),'SAmetier6'] <- 'PTM_SPF_32-69_0_0'
# Soem of our FAO codes aren't being allowed for the species codes they are paired with - remove them for the moment
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 125802 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'MON' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 125743 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'ROL' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 126493 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'GAD' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 127419 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'BOR' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 127262 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'GUX' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 127151 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'LEF' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 105693 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'SCL' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 126473 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'NZA' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 126831 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'CEO' ,'SAspeciesCodeFAO'] <- NA
myRDBESData[['SA']][!is.na(myRDBESData[['SA']]$SAspeciesCode) & !is.na(myRDBESData[['SA']]$SAspeciesCodeFAO) &  myRDBESData[['SA']]$SAspeciesCode == 126751 &  myRDBESData[['SA']]$SAspeciesCodeFAO == 'SAN' ,'SAspeciesCodeFAO'] <- NA



# CE fixes
# Get rid of NA areas
myRDBESData[['CE']]<-myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEarea),]
# Get rid of 47.1.1, 47.1.3 areas 
myRDBESData[['CE']] <- myRDBESData[['CE']][!myRDBESData[['CE']]$CEarea %in% c('47.1.1','47.1.3'),]
# Get rid of zero fishing hours
myRDBESData[['CE']] <- myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEofficialVesselFishingHour) & myRDBESData[['CE']]$CEofficialVesselFishingHour > 0,]
# Get rid of zero fishing days
myRDBESData[['CE']] <- myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEofficialFishingDays ) & myRDBESData[['CE']]$CEofficialFishingDays > 0,]
# The "NA" rows in CEfreshWaterName get changed to actual NAs when read in - let's change them back to "NA"
myRDBESData[['CE']][is.na(myRDBESData[['CE']]$CEfreshWaterName),"CEfreshWaterName"] <- "NA"
# If the values of scientificSoakingMeterHour are too high changes them to the maximum
myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEscientificSoakingMeterHour) & myRDBESData[['CE']]$CEscientificSoakingMeterHour > 1200000, 'CEscientificSoakingMeterHour'] <- 1200000
myRDBESData[['CE']][!is.na(myRDBESData[['CE']]$CEofficialSoakingMeterHour) & myRDBESData[['CE']]$CEofficialSoakingMeterHour  > 1200000, 'CEofficialSoakingMeterHour'] <- 1200000

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
myRDBESData[['CL']][is.na(myRDBESData[['CL']]$CLlandingsValue),'CLlandingsValue'] <- 1
myRDBESData[['CL']][myRDBESData[['CL']]$CLlandingsValue < 1,'CLlandingsValue'] <- 1
# The "NA" rows in CEfreshWaterName get changed to actual NAs when read in - let's change them back to "NA"
myRDBESData[['CL']][is.na(myRDBESData[['CL']]$CLfreshWaterName),"CLfreshWaterName"] <- "NA"


# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","TE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))


# Can check errors from individual tables using e.g.
View(errors[errors$tableName == 'SA',])


## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD)

# Create a CE output file
generateExchangeFile(typeOfFile = 'CE', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'CE', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'CE', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
generateExchangeFile(typeOfFile = 'CL', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'CL', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'CL', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateExchangeFile(typeOfFile = 'VD', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'VD', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'VD', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateExchangeFile(typeOfFile = 'SL', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'SL', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateExchangeFile(typeOfFile = 'SL', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)

# Create an H1 CS file
generateExchangeFile(typeOfFile = 'H1', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateExchangeFile(typeOfFile = 'H1', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateExchangeFile(typeOfFile = 'H1', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

# Create an H5 CS file
generateExchangeFile(typeOfFile = 'H5', yearToUse = 2021, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateExchangeFile(typeOfFile = 'H5', yearToUse = 2022, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
generateExchangeFile(typeOfFile = 'H5', yearToUse = 2023, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)



