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
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

## STEP 2) GENERATE TEST DATA

# Temp working area below!

## Generate fake data

# If no value if given for a table then it is assumed to be unstratified
myStrata <- list(DE = 2, VS = 2)
# if no value is given for a table then it is assumed to be 1
mySampled <- list(VS=5,FO=3,SS=2,SA=2, FM=20,BV=2, VD=10, SL=20)
# if no value is given for a table then it is assumed to be equal to the number sampled + 1
myTotal <- list(VS=30,FO=10,SS=4, FM=20, BV=2)
# if no value is given for a tabel then it is assumed to be simple random sampling SRSWR
myMethods <- list()

myTestData <- createTestData(HierarchyToGenerate = 'H1',LowerHierarchyToGenerate = 'A', RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables, NumberOfStrata = myStrata, NumberSampled = mySampled, NumberTotal = myTotal, SelectionMethods = myMethods)

myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse='IE',YearToUse=2015,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)

# Lets validate our data
#errorsTestData <- validateTables(RDBESdata = myNewTestData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS"))

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2015, country = 'IE', RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2015, country = 'IE', RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a complex exchange file (Hx)
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2015, country = 'IE', RDBESdata = myNewTestData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)





makeTestDataMoreRealistic <- function(DataToUse,CountryToUse,YearToUse,MetierList,SpeciesList,RDBEScodeLists){
  
  # For testing
  #DataToUse <- myTestData
  #RDBEScodeLists <- allowedValues
  #CountryToUse <- 'IE'
  #SpeciesList <- c(126436)
  #MetierList <- c('OTB_DEF_100-119_0_0')
  
  # VESSEL DETAILS
  DataToUse[['VD']][,'VDcountry'] <- CountryToUse
  DataToUse[['VD']][,'VDflagCountry'] <- CountryToUse
  DataToUse[['VD']][,'VDyear'] <- YearToUse
  # Lets pick some random vessel lengths
  myVesselLengths <- RDBEScodeLists[RDBEScodeLists$listName == 'tRS_VesselLengthCategory','Key']
  DataToUse[['VD']][,'VDlengthCategory'] <- sample(myVesselLengths,nrow(DataToUse[['VD']]),replace = TRUE)
  DataToUse[['VD']][,'VDencryptedVesselCode'] <- paste('VDcode_',DataToUse[['VD']][,'VDid'],sep="")
  
  # SPECIES LIST DETAILS
  DataToUse[['SL']][,'SLcountry'] <- CountryToUse
  DataToUse[['SL']][,'SLyear'] <- YearToUse
  DataToUse[['SL']][,'SLspeciesListName'] <- paste(CountryToUse,'_',YearToUse,'_SpeciesList',sep="")
  mySpeciesCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tSpecWoRMS','Key']
  mySpeciesCodes <- c(SpeciesList,mySpeciesCodes)
  mySpeciesCodes <- unique(mySpeciesCodes)
  mySpeciesCodes <- mySpeciesCodes[1:nrow(DataToUse[['SL']])]
  DataToUse[['SL']][,'SLcommercialTaxon'] <- mySpeciesCodes
  DataToUse[['SL']][,'SLspeciesCode'] <- mySpeciesCodes
  
  # CS TABLES
  
  # COUNTRY
  # Sort out the country and location code fields
  DataToUse[['SD']][,'SDcountry'] <- CountryToUse
  if ('LE' %in% names(DataToUse)){
    DataToUse[['LE']][,'LEcountry'] <- CountryToUse
  }
  
  # LOCODE
  # For fields that use locodes we'll pick a random locode from the country we are interested in
  myCountryLocodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tHarbour_LOCODE','Key']
  myCountryLocodes <- myCountryLocodes[grepl(paste('^',CountryToUse,'.*$',sep=''), myCountryLocodes)]
  if ('FT' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['FT']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['FT']][,'FTdepartureLocation'] <- myRandomValues
    DataToUse[['FT']][,'FTarrivalLocation'] <- myRandomValues
  }
  if ('OS' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['OS']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['OS']][,'OSlocode'] <- myRandomValues
  }
  if ('LE' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['LE']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['LE']][,'LElocation'] <- myRandomValues
  }
  if ('LO' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['LO']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['LO']][,'LOlocode'] <- myRandomValues
  }
  
  # YEAR
  DataToUse[['DE']][,'DEyear'] <- YearToUse
  
  # DATES
  # For dates we'll pick a date within the year we are using
  if ('FT' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['FT']]),replace = TRUE)
    myRandomTripLength <- sample(1:30,nrow(DataToUse[['FT']]),replace = TRUE)
    
    DataToUse[['FT']][,'FTdepartureDate'] <- format(myRandomValues,'%Y-%m-%d')
    DataToUse[['FT']][,'FTarrivalDate'] <- format(myRandomValues + myRandomTripLength,'%Y-%m-%d')
  }
  # for FO we'll use the arrival date of the trip for all our fishing acitvity
  if ('FO' %in% names(DataToUse) & 'FT' %in% names(DataToUse)){
    FTDates <- DataToUse[['FT']][,c('FTid','FTdepartureDate','FTarrivalDate')]
    myJoin <- inner_join(DataToUse[['FO']],FTDates, by ='FTid')
    DataToUse[['FO']][,'FOendDate'] <- myJoin[,'FTarrivalDate']
  }
  if ('OS' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['OS']]),replace = TRUE)
    DataToUse[['OS']][,'OSsamplingDate'] <- format(myRandomValues,'%Y/%m/%d')
  }
  if ('LE' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['LE']]),replace = TRUE)
    DataToUse[['LE']][,'LEdate'] <- format(myRandomValues,'%Y/%m/%d')
  }
  
  # SPECIES LIST NAME
  DataToUse[['SS']][,'SSspeciesListName'] <- paste(CountryToUse,'_',YearToUse,'_SpeciesList',sep="")
  
  # VESSELS
  # Ensure we are only referring to vessel that appear in our Vessel Details - this gets a bit complex because we can have the encryptedVesselCode used in a few tables, which can be linked to each other
  myVesselCodes <- DataToUse[['VD']][,'VDencryptedVesselCode']
  
  # If we have VS data - pick a random vessel for each VS row, and then ensure that any FT or LE rows connected to it use the same value
  if ('VS' %in% names(DataToUse)){
    
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['VS']]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    
    DataToUse[['VS']][,'VSencryptedVesselCode'] <- myRandomValues
    DataToUse[['VS']][,'VSunitName'] <- myRandomValues
  }
  
  # IF we have FT data we need to see whether it is connected to an VS record - if so, use the same vessel code, else pick a random one
  if ('FT' %in% names(DataToUse)){
    # Clear out any existing values first
    DataToUse[['FT']][,'FTencryptedVesselCode'] <- NA

    # If we have a VS table then we'll use the vessel code form that record in the FT record
    if ('VS' %in% names(DataToUse)){
      VSvessel <- DataToUse[['VS']][,c('VSid','VSencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['FT']],VSvessel, by ='VSid')
      DataToUse[['FT']][,'FTencryptedVesselCode'] <- myJoin[,'VSencryptedVesselCode']
    } 
    
    # Now use random vessels for any remaining NA vessel codes
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    if (nrow(DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),])>0){
      DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),'FTencryptedVesselCode'] <- myRandomValues
    }
    
  }
  
  # If we have LE data it can be directly linked to either a VS or FT record - need to use the same vessel code if it is - else pick a random one
  if ('LE' %in% names(DataToUse)){
    
    # Clear out any existing values first
    DataToUse[['LE']][,'LEencryptedVesselCode'] <- NA
    
    # If we have a VS table then we'll use the vessel code form that record in the FT record
    if ('VS' %in% names(DataToUse)){
      VSvessel <- DataToUse[['VS']][,c('VSid','VSencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['LE']],VSvessel, by ='VSid')
      DataToUse[['LE']][,'LEencryptedVesselCode'] <- myJoin[,'VSencryptedVesselCode']
    } 
    
    # If we have a FT table then we'll use the vessel code form that record in the FT record
    if ('FT' %in% names(DataToUse)){
      FTvessel <- DataToUse[['FT']][,c('FTid','FTencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['LE']],FTvessel, by ='FTid')
      DataToUse[['LE']][,'LEencryptedVesselCode'] <- myJoin[,'FTencryptedVesselCode']
    } 
    
    # Now use random vessels for any remaining NA vessel codes
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    if (nrow(DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),])>0){
      DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),'LEencryptedVesselCode'] <- myRandomValues
    }
    
  }
  
  # SPECIES
  
  # Ensure we are only sampling species that appear in our Species Details
  mySpeciesCodes <- DataToUse[['SL']][,'SLspeciesCode']
  if (length(mySpeciesCodes) > 1){
    myRandomValues <- sample(mySpeciesCodes,nrow(DataToUse[['SA']]),replace = TRUE)
  } else {
    myRandomValues <- mySpeciesCodes
  }
  DataToUse[['SA']][,'SAspeciesCode'] <- myRandomValues
  
  # METIERS
  
  # If we have been given a list of metiers then we'll just use them 
  if (!is.null(MetierList)){
    myMetiersCodes <- unique(MetierList)
  # Otherwise we'll generate a list of metiers from the code list
  } else {
    myMetiersCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tMetier6_FishingActivity','Key']
    # Pick the maximum number of metiers for our list based on whatever is the biggest number from i) the number of SA records, ii) the number 1 (to cover the very rare case when we don't have any samples)
    numerOfMetiersToUse <- max(nrow(DataToUse[['SA']]), 1 )
    myMetiersCodes <- myMetiersCodes[1:numerOfMetiersToUse]
  }

  # If we have FO data pick a random metier and gear code
  if ('FO' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['FO']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['FO']][,'FOmetier6'] <- myRandomValues
    DataToUse[['FO']][,'FOgear'] <- substring(myRandomValues,1,3)
  }
  
  # If we have LE data pick a random metier and gear code
  if ('LE' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['LE']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['LE']][,'LEmetier6'] <- myRandomValues
    DataToUse[['LE']][,'LEgear'] <- substring(myRandomValues,1,3)
  }
  
  # If we have SA data pick a random metier and gear code
  if ('SA' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['SA']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['SA']][,'SAmetier6'] <- myRandomValues
    DataToUse[['SA']][,'SAgear'] <- substring(myRandomValues,1,3)
  }
  

  # Return our data
  DataToUse
  
}

