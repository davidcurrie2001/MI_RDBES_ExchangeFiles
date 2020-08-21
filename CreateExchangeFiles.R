
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

# Load the RDBES data from the database - you can either write your own database connection string in a format similar to this: 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true' or just manually create a named list of data fames in the correct format
# IMPORTANT - if you are just going to use your own list of data frames make sure you don't have factors in them - my code assumes the data frames were created using stringsAsFactors = FALSE
myRDBESData <- loadRDBESData(readRDS("connectionString.RDS"))


## STEP 2) VALIDATE OUR DATA AND CHECK ERRORS

#UGLY HACKS - these are temporary fixes for my data - they will be removed once correct values are added to reference lists
# Temporary fixes
myRDBESData[['DE']][myRDBESData[['DE']]$DEsamplingScheme == 'Ireland DCF Port Sampling',"DEsamplingScheme"] <- "National Routine"
myRDBESData[['FO']][myRDBESData[['FO']]$FOgear == 'OTQ',"FOgear"] <- "OTB"
myRDBESData[['SA']][myRDBESData[['SA']]$sagear == 'OTQ',"SAgear"] <- "OTB"
myRDBESData[['SA']]$SAspeciesCodeFAO <- NA


# Lets validate our data
errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))

# Can check errros from individual tables using e.g.
#View(errors[errors$tableName == 'FT',])

## STEP 3) GENERATE SIMPLE EXCHANGE FILES (CL,CE,SL,VD)

# Create a CE output file
generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateSimpleExchangeFile(typeOfFile = 'CE', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a CL output file
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
#generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)

# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=500,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

# Create an H5 CS file
generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=100,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
#generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)

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


# Temp working area below!

## Generate fake data

# If no value if given for a table then it is assumed to be unstratified
myStrata <- list(DE = 2, VS = 2)
# if no value is given for a table then it is assumed to be 1
mySampled <- list(VS=5)
# if no value is given for a table then it is assumed to be equal to the number sampled + 1
myTotal <- list(VS=30)
# if no value is given for a tabel then it is assumed to be simple random sampling SRSWR
myMethods <- list()

myTestData <- createTestData(HierarchyToGenerate = 'H1',LowerHierarchyToGenerate = 'A', RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables, NumberOfStrata = myStrata, NumberSampled = mySampled, NumberTotal = myTotal, SelectionMethods = myMethods)




createTestData <- function(HierarchyToGenerate,LowerHierarchyToGenerate, RDBESvalidationdata, RDBEScodeLists, RequiredTables, NumberOfStrata, NumberSampled, NumberTotal, SelectionMethods ){

  myRequiredTables <- RequiredTables[[HierarchyToGenerate]]
  
  # We might need to remove some tables, depending on the lower hierarchy
  if (LowerHierarchyToGenerate == 'A'){
    # No need to do anything
  } else if (LowerHierarchyToGenerate == 'B'){
    # Remove BV
    myRequiredTables <- myRequiredTables[myRequiredTables != 'BV']
  } else if (LowerHierarchyToGenerate == 'C'){
    # Remove FM
    myRequiredTables <- myRequiredTables[myRequiredTables != 'FM']
  } else if (LowerHierarchyToGenerate == 'D'){
    # Remove FM and BV
    myRequiredTables <- myRequiredTables[myRequiredTables != 'FM']
    myRequiredTables <- myRequiredTables[myRequiredTables != 'BV']
  }
  
  # First create a list with all the empty tables we need in it
  myDataList <- list()
  
  for (myTable in myRequiredTables){
    myEmptyDf <- createEmptyDataFrameFromValidationData(nameOfTable = myTable, RDBESvalidationdata = RDBESvalidationdata)
    # Add our empty XXid column to the front of the data frame
    myID <- integer(0)
    myEmptyDf <- cbind(myID, myEmptyDf)
    # Name our new id column correctly
    names(myEmptyDf)[1] <- paste(myTable,'id',sep = "")
    # Add the empty data frame to our list
    myDataList[[myTable]] <- myEmptyDf
    
    # Check if we have been given values for some aspects of the data we want to generate - if not we set defaults
    
    if (!myTable %in% names(NumberOfStrata)){
      NumberOfStrata[[myTable]] <- 1
    }
    
    if (!myTable %in% names(NumberSampled)){
      NumberSampled[[myTable]] <- 1
    }
    
    if (!myTable %in% names(NumberTotal)){
      NumberTotal[[myTable]] <- NumberSampled[[myTable]] +1
    }
    
    if (!myTable %in% names(SelectionMethods)){
      SelectionMethods[[myTable]] <- 'SRSWR'
    }
    
    myDF <- myDataList[[myTable]]
    myNumberOfStrata <- NumberOfStrata[[myTable]]
    myNumberSampled <- NumberSampled[[myTable]]
    myNumberTotal <- NumberTotal[[myTable]]
    myMethod <- SelectionMethods[[myTable]]
    
    # Generate test data for each required table
    myNewDF <- createNewTestDataFrame(HierarchyToGenerate = HierarchyToGenerate,LowerHierarchyToGenerate = LowerHierarchyToGenerate, TableType = myTable, DataFrameToUpdate = myDF, NumberOfStrata = myNumberOfStrata, NumberSampled = myNumberSampled, NumberTotal = myNumberTotal, SelectionMethod = myMethod, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists)
    
    myDataList[[myTable]] <- myNewDF
    
    
  }
  
  #myDataList
  
  # At the moment we have a lot of indepdent test data - we'll mutiple all these records together so that we have our data linked together
  myMultipliedTestData <- list()
  
  currentTable <- NA
  previousTable <- NA
  
  for (myTable in myRequiredTables){
    
    previousTable <- currentTable
    currentTable <- myTable
    #print(currentTable)
    #print(previousTable)
    myCurrentData <-  myDataList[[currentTable]]
    
    # If we're not at the top level table lets start mutiplying data
    if (!is.na(previousTable)){
      myPreviousData <- myMultipliedTestData[[previousTable]]
      
      FKcolumnName <- paste(previousTable,'id',sep="")
      
      # Add our foreign key column to the data
      myCurrentData[,FKcolumnName]<-integer(nrow(myCurrentData))
      # Default the value to NA if we need to
      if (nrow(myCurrentData)>0){
        myCurrentData[,FKcolumnName] <- NA
      }
      
      # For each unique value of the foreign key in the previous table we will mutiply the data in the current table
      for (myFK in unique(myPreviousData[,FKcolumnName])){
        newBlockOfData <- myCurrentData
        newBlockOfData[,FKcolumnName] <- myFK
        myMultipliedTestData[[currentTable]] <- rbind(myMultipliedTestData[[currentTable]],newBlockOfData)
      }
      
      # Re-generate the PK for our mutiplied data because they will have been duplicated
      myMultipliedTestData[[currentTable]][,paste(currentTable,'id',sep="")] <- 1:nrow(myMultipliedTestData[[currentTable]])
      
      # If we're at the first table we'll just take those values without trying to multiply them
    } else {
      myMultipliedTestData[[currentTable]] <- myCurrentData
    }
    
  }
  
  myMultipliedTestData

}

# myTable <- 'VS'
# myDF <- myDataList[[myTable]]
# myNumberOfStrata <- numberOfStrata[[myTable]]
# myNumberSampled <- numberSampled[[myTable]]
# myNumberTotal <- numberTotal[[myTable]]
# 
# myNewDF <- createNewTestDataFrame(TableType = myTable, DataFrameToUpdate = myDF, NumberOfStrata = myNumberOfStrata, NumberSampled = myNumberSampled, NumberTotal = myNumberTotal, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists)



createNewTestDataFrame <- function(HierarchyToGenerate,LowerHierarchyToGenerate, TableType, DataFrameToUpdate, NumberOfStrata, NumberSampled, NumberTotal,SelectionMethod, RDBESvalidationdata, RDBEScodeLists){
  
  # Generate our strata names
  if (NumberOfStrata <= 1){
    myStratumNames <- c('U')
  } else {
    myStratumNames <- sapply(1:NumberOfStrata, function(x) paste(TableType,'_stratum',x,sep=""))
  }
  
  # For each stratum
  for (aStratum in myStratumNames){
    # for each of our sampled units
    for (i in 1:NumberSampled){
      
      # Create a new row id
      newRowID <- nrow(DataFrameToUpdate) +1
      
      # Create a new row
      myNewRowValues <- createNewTestDataRow(HierarchyToGenerate = HierarchyToGenerate,LowerHierarchyToGenerate = LowerHierarchyToGenerate, TableType = TableType, RowID = newRowID, ColumnNames = names(DataFrameToUpdate), StratumName = aStratum, NumberSampled = NumberSampled, NumberTotal = NumberTotal,SelectionMethod = SelectionMethod, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists)
      
      # Add the new row to our data frame
      myNewRow <- as.data.frame(myNewRowValues, stringsAsFactors = FALSE)
      # Slow to use Rbind in a loop :-(
      DataFrameToUpdate <- rbind(DataFrameToUpdate,myNewRow)  
    }
  }
  
  DataFrameToUpdate
  
  
}

  

createNewTestDataRow <- function(HierarchyToGenerate,LowerHierarchyToGenerate, TableType, RowID, ColumnNames, StratumName, NumberSampled, NumberTotal,SelectionMethod, RDBESvalidationdata, RDBEScodeLists){
  
  # Empty list to hold our new row values
  myNewRowValues <- list()
  
  # For each column in our data frame
  for (myColName in ColumnNames){
    
    # Default the new value to NA
    myNewValue <- NA
    
    # We'll deal with the 'special' columns first (most specific first), then deal with all the others
    
    # SPECIAL COLUMN - DEhierarchy
    if (myColName == 'DEhierarchy') {
      myNewValue <- substr(HierarchyToGenerate,2,nchar(HierarchyToGenerate))
    # SPECIAL COLUMN - DEhierarchyCorrect
    } else if (myColName == 'DEhierarchyCorrect') {
      myNewValue <- 'Y'
    # SPECIAL COLUMN - DEhierarchyCorrect
    } else if (myColName == 'SAlowerHierarchy') {
      myNewValue <- LowerHierarchyToGenerate
    # SPECIAL COLUMN - XXid
    } else if (myColName == paste(TableType,'id',sep="")) {
      myNewValue <- RowID
    # SPECIAL COLUMN - XXsequenceNumber
    } else if (grepl('^..sequenceNumber$',myColName)) {
      myNewValue <- RowID
    # SPECIAL COLUMN - XXunitName
    } else if (grepl('^..unitName$',myColName)) {
      myNewValue <- paste(TableType,'_unit_', RowID,sep = "")
    # SPECIAL COLUMN - XXstratification
    } else if (grepl('^..stratification$',myColName)) {
      if (StratumName =='U'){
        myNewValue <- 'No'
      } else {
        myNewValue <- 'Yes'
      }
    # SPECIAL COLUMN - XXstratumName
    } else if (grepl('^..stratumName$',myColName)) {
      myNewValue <- StratumName
    # SPECIAL COLUMN - XXnumberSampled
    } else if (grepl('^..numberSampled$',myColName)) {
      myNewValue <- NumberSampled
    # SPECIAL COLUMN - XXnumberTotal
    } else if (grepl('^..numberTotal$',myColName)) {
      myNewValue <- NumberTotal
    # SPECIAL COLUMN - XXselectionMethod
    } else if (grepl('^..selectionMethod$',myColName)) {
      myNewValue <- SelectionMethod
    # SPECIAL COLUMN - XXsampled
    } else if (grepl('^..sampled$',myColName)) {
      myNewValue <- 'Y'
    # SPECIAL COLUMN - XXclustering
    } else if (grepl('^..clustering$',myColName)) {
      myNewValue <- 'No'
    # SPECIAL COLUMN - XXclusterName
    } else if (grepl('^..clusterName$',myColName)) {
      myNewValue <- 'U'
    # NOT SPECIAL :-(
    } else {
      # Else this column is not special :-( - just put some random data in it....
      
      # Try finding the validation info for this column
      myValidationInfo <- RDBESvalidationdata[RDBESvalidationdata$name == myColName,]
      
      # VALIDATION INFROMATION FOUND
      if (nrow(myValidationInfo)==1){
        #print(myColName)
        # MANDATORY
        if (myValidationInfo$min >0){
          # Mandatory so we need to do soemthing :-)
          # What we do will depden on what type of column it is e.g. code list, int, double etc
          
          # CODE LIST
          if (grepl('^t.*',myValidationInfo$type) & is.na(myValidationInfo$description)){
            # if its a code list we'll get the first entry from the code list
            requiredCodeListName <- myValidationInfo$type
            #print(requiredCodeListName)
            firstEntry <- RDBEScodeLists[RDBEScodeLists$listName == requiredCodeListName,'Key' ][1]
            myNewValue <- firstEntry
            # SIMPLETYPECHECK
          } else if (!is.na(myValidationInfo$description) & myValidationInfo$description == 'simpleTypeCheck'){
            # simpleTypeCheck - string with pattern
            if (!is.na(myValidationInfo$pattern)){
              # string with pattern - I assume this can only be dates or times - might not hold true in the future
              if (myValidationInfo$checkName == 'tDate'){
                myNewValue <- '2019/01/01'
              } else if (myValidationInfo$checkName == 'tDate'){
                myNewValue <- '12:34'
              } else {
                print(paste("There was a pattern check that I didn't deal with in column ", myColName, sep =""))
              }
              # simpleTypeCheck - min value
            } else if (!is.na(myValidationInfo$minValue)){
              myNewValue <- myValidationInfo$minValue
              # simpleTypeCheck - max value (but no min)
            } else if (!is.na(myValidationInfo$maxValue)){
              myNewValue <- myValidationInfo$maxValue
              # simpleTypeCheck - specified max decimal places
            } else if (!is.na(myValidationInfo$fractionDigits)){
              myNewValue <- round(1.12345678912345678912345,as.integer(myValidationInfo$fractionDigits))
            } else {
              print(paste("There was a simpleTypeCheck that I didn't deal with in column ", myColName, sep =""))
            }
            # INT, LONG, OR DECIMAL
          } else if (myValidationInfo$type %in% c('xs:int','xs:long','xs:decimal')){
            myNewValue <- 10
            # STRING
          } else if (myValidationInfo$type == 'xs:string'){
            myNewValue <- 'abc'
            # DIDN'T MATCH - TELL THE USER
          } else {
            print(paste("I didn't deal with the validation information in column ", myColName, sep =""))
          }
          # OPTIONAL
        } else {
          # If the field is optional- don't bother to do anything
        }
        # NO VALIDATION INFORMATION FOUND
      } else {
        print(paste("Could not find information for ",myColName,sep=""))
      }
    }
    # Add our new value to the list of new values
    myNewRowValues[[myColName]] <- myNewValue
  }
  
  myNewRowValues
  
}
