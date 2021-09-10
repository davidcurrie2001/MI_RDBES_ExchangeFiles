# Load our functions
source("RDBES_Functions.R")
# Temporary fix required for a function from icesVocab - otherwise the function breaks when it tries to download the EDMO code list (or any list containing carriage returns)
source("tempIcesVocabFix.R")

# This file shows how to generate test data for the RDBES

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 30/8/2021 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Sex','type'] <- 'tSEXCO'

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

# 8/9/21 temp fix - need to check XSD files are up-to-date
allRequiredTables[["H12"]]<- c("DE","SD","LO","TE","LE","SS","SA","FM","BV")
allRequiredTables[["H13"]]<- c("DE","SD","FO","SS","SA","FM","BV")

## STEP 2) GENERATE TEST DATA

# Can use a loop to generate test data for all hierarchies if you want to 
for (i in 1:13){
  myHierarchyToGenerate <- paste('H',i,sep="")
  
  # Define some parameters for our test data
  #myHierarchyToGenerate <- 'H1'

  print(myHierarchyToGenerate)
  myLowerHierarchyToGenerate <- 'A'
  myYear <- 1965
  myCountry <- 'ZW'
  # Number of strata in different tables - if no value if given for a table then it is assumed to be unstratified
  myStrata <- list(DE = 3, VS = 3, FT = 3, OS = 3, LO = 1, TE = 1)
  # Number of things sampled in different tables - if no value is given for a table then it is assumed to be 1
  mySampled <- list(VS=3,FO=3,SS=1,SA=2, FM=10,BV=2, VD=10, SL=20, OS = 3, TE = 3, LO = 3, FT=3, CE=100, CL = 500)
  # Total number of things in different tables - if no value is given for a table then it is assumed to be equal to the number sampled + 1
  myTotal <- list(VS=30,FO=10,SS=4,FM=10,BV=2, OS = 100, LO = 100, TE = 10, FT = 100)
  # Select methods used in different tables - if no value is given for a table then it is assumed to be simple random sampling SRSWR
  myMethods <- list()
  
  # Generate some random data
  myTestData <- createTestData(HierarchyToGenerate = myHierarchyToGenerate, LowerHierarchyToGenerate = myLowerHierarchyToGenerate, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables, NumberOfStrata = myStrata, NumberSampled = mySampled, NumberTotal = myTotal, SelectionMethods = myMethods)
  
  # The data we just generated is too random and won't pass validation or upload check - lets fix that now
  myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  
  # Sampling scheme name for WGRDBES-EST
  myNewTestData[['DE']]$DEsamplingScheme <- 'WGRDBES-EST TEST 1'
  
  # Lets validate our data (Optional)
  #errorsTestData <- validateTables(RDBESdata = myNewTestData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CE","CL"))
  
  # Create a CE output file
  generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a CL output file
  generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a VD output file
  generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a SL output file
  generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a complex exchange file (Hx)
  generateComplexExchangeFile(typeOfFile = myHierarchyToGenerate, yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
  
  # (OPTIONAL) Save the tables as csv files
  write.csv(myNewTestData[['VD']],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_","VD",".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  
  write.csv(myNewTestData[['SL']],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_","SL",".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  
  for (tablesToSave in allRequiredTables[[i]]){
    write.csv(myNewTestData[[tablesToSave]],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_",tablesToSave,".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  }

}
