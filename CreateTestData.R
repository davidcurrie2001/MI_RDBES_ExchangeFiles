# Load our functions
source("RDBES_Functions.R")

# This file shows how to generate test data for the RDBES

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

## STEP 2) GENERATE TEST DATA

# Can use a loop to generate test data for all hierarchies if you want to 
for (i in 1:13){

  #i <- 7
  
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
  #mySampled <- list(VS=20,FO=10,SS=1,SA=2, FM=10,BV=2, VD=10, SL=20, OS = 4, TE = 3, LO = 3, FT=5, CE=100, CL = 500)
  mySampled <- list(VS=3,FO=3,SS=1,SA=1, FM=10,BV=2, VD=10, SL=20, OS = 3, TE = 3, LO = 3, FT=3, CE=100, CL = 500)
  # Total number of things in different tables - if no value is given for a table then it is assumed to be equal to the number sampled + 1
  myTotal <- list(VS=60,FO=20,SS=4,FM=10,BV=2, OS = 100, LO = 100, TE = 10, FT = 100)
  # Select methods used in different tables - if no value is given for a table then it is assumed to be simple random sampling SRSWR
  myMethods <- list()
  
  # Generate some random data
  myTestData <- createTestData(HierarchyToGenerate = myHierarchyToGenerate, LowerHierarchyToGenerate = myLowerHierarchyToGenerate, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables, NumberOfStrata = myStrata, NumberSampled = mySampled, NumberTotal = myTotal, SelectionMethods = myMethods)
  
  metsForCS <- c('DRB_MOL_>0_0_0', 'FPO_CRU_>0_0_0', 'FPO_MOL_>0_0_0', 'LLS_FIF_0_0_0', 'MIS_MIS_0_0_0', 'OTB_DEF_70-99_0_0', 'OTM_LPF_>0_0_0', 'PTM_SPF_>0_0_0')
  
  # The data we just generated is too random and won't pass validation or upload check - lets fix that now
  #myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= metsForCS,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  

  # Lets validate our data (Optional)
  #errorsTestData <- validateTables(RDBESdata = myNewTestData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CE","CL"))
  
  #View(errorsTestData[errorsTestData$tableName == "FO",])
  
  # Create a CE output file
  generateExchangeFile(typeOfFile = 'CE', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a CL output file
  generateExchangeFile(typeOfFile = 'CL', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a VD output file
  generateExchangeFile(typeOfFile = 'VD', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a SL output file
  generateExchangeFile(typeOfFile = 'SL', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a complex exchange file (Hx)
  generateExchangeFile(typeOfFile = myHierarchyToGenerate, yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
  
  # (OPTIONAL) Save the tables as csv files
  #write.csv(myNewTestData[['VD']],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_","VD",".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  
  #write.csv(myNewTestData[['SL']],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_","SL",".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  
  #for (tablesToSave in allRequiredTables[[myHierarchyToGenerate]]){
  #  write.csv(myNewTestData[[tablesToSave]],file=paste("./output/testDataForPackage/",myHierarchyToGenerate,"_",tablesToSave,".csv",sep=""),row.names = FALSE,na="",quote = FALSE)
  #}

}
