
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

# Create a CL output file
generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a VD output file
generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)

# Create a SL output file
generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)


## STEP 4) GENERATE COMPLEX EXCHANGE FILES (CS)

# Create an H1 CS file
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)
#generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)

# Create an H5 CS file
generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, numberOfSamples=20,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)
#generateComplexExchangeFile(typeOfFile = 'H5', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = requiredTables)

## STEP 5) (OPTIONAL) WE CAN SAVE OUR DATA IF WE WANT TO

# Save RData files for H1
saveRDataFilesForCS(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myRDBESData, RequiredTables = requiredTables)
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

#myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_H1.csv' )

# Put the data frames in a list
myExchangeFileRDBESData <- list(CE=myExchangeFileCE,CL=myExchangeFileCL,VD=myExchangeFileVD,SL=myExchangeFileSL)

# For interest lets validate the data we just red in - it shoudl be fine if the exhcange file was produced using clean, valid data
errorsExchangeFiles <- validateTables(RDBESdata = myExchangeFileRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","VD","VS","CL","CE" ))


readComplexExchangeFile <- function(typeOfFile,RDBESvalidationdata,nameOfFile,RequiredTables){
  
  # For testing
  typeOfFile <- 'H5'
  # RDBESdata<-myRDBESData
  # yearToUse <- 2017
  # country <- 'IE'
  # outputFileName <- ""
  # numberOfSamples <- 10
  # cleanData <- TRUE
  RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  RequiredTables <- requiredTables
  nameOfFile <- 'output/IE_2019_H5.csv'
  
  testedCSfileTypes <- c('H1','H5')
  
  if (!typeOfFile %in% testedCSfileTypes){
    warning(paste("Method not tested for ",typeOfFile, " yet", sep =""))
  }
  
  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  myRequiredTables <- RequiredTables[[typeOfFile]]
  
  # Create a list with all the empty tables we need in it
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
  }
  
  # Read our csv file 
  myLines <- readLines(nameOfFile)
  currentRowType <- NA
  previousRowType <- NA
  myNewRowID <- -1
  myPreviousRowID <- -1
  myForeignKeyID <- -1
  myForeignKeyName <- NA
  
  # Used for tracking and assigning foreign keys 
  foreignKeyTracking <- data.frame(currentRowID = integer(0), currentRowType = character(0),previousRowID = integer(0), previousRowType = character(0),foreignKeyID = integer(0), foreignKeyType = character(0), stringsAsFactors = FALSE)
  
  # store the most recent ids for each tabel type - needed for the foreign key logic
  mostRecentIds <- list()
  
  # For each line in the file
  for (i in 1:length(myLines)){
  #for (i in 1:3){  
    
    # Store the previous row details
    previousRowType <- ifelse(is.na(currentRowType),NA,currentRowType)
    myPreviousRowID <- ifelse(myNewRowID == -1,-1,myNewRowID)
    
    # Read the next line
    myRow <- myLines[[i]]
    myRowValues <- strsplit(myRow,split=",")[[1]]
    currentRowType <- myRowValues[1]
    # Create a new row id - we'll just use sequential primary key - find out how many rows there are already are and add 1
    myNewRowID <- nrow(myDataList[[currentRowType]])+1
    
    # Update the most recent row id for this record type
    mostRecentIds[[currentRowType]] <- myNewRowID
    
    # Work out what the foreign key shoudl be
    if (currentRowType == 'DE'){
      # No foreign key for DE
      myForeignKeyID <- 1
      myForeignKeyName <- NA
    } else if (currentRowType ==  previousRowType){
      # If we are still on the same row type then we'll just keep the same foreign key detaisl that we already have - no need to do anything here then

    } else if (grep(currentRowType,myRequiredTables) > grep(previousRowType,myRequiredTables)) {
      # If we've moved to a row type lower in the hierarchy then we just need to set the new foreign key values
      myForeignKeyID <- myPreviousRowID
      myForeignKeyName <- paste(previousRowType,'id',sep="")
    } else {
      # else we're going back up the hierarchy - trickier...
      
      tableFound <- NA
      
      # Check the tables earlier in the hierarchy and find the lowest one that has an id already recorded - we'll use this for our foreign key
      currentRowTypePositionInVector <- grep(currentRowType,myRequiredTables)
      for (j in (currentRowTypePositionInVector-1):1){
        tableToCheck <- myRequiredTables[j]
        # if this table type exists in the data frame of the most recent keys then we'll use it
        if (!is.null(mostRecentIds[[tableToCheck]])){
          tableFound <- tableToCheck
          break;
        }
      }
      # If we found a mtach we'll use those values for the foreign key
      if (!is.na(tableFound)){
        myForeignKeyID <- mostRecentIds[[tableFound]]
        myForeignKeyName <- paste(tableFound,'id',sep="")
      } else {
        # Soemthing has gone wrong
        # just reset the values for now
        myForeignKeyID <- 1
        myForeignKeyName <- NA
      }
      

    }
    
    # Used for tracking and assigning foreign keys 
    foreignKeyTracking <- rbind(foreignKeyTracking,data.frame(currentRowID = myNewRowID, currentRowType = currentRowType,previousRowID = myPreviousRowID, previousRowType = previousRowType ,foreignKeyID = myForeignKeyID, foreignKeyType = myForeignKeyName, stringsAsFactors = FALSE))
    

    # Our new data
    #myNewData <- c(myNewRowID,myRowValues[2:length(myRowValues)])
    myNewData <- myRowValues[2:length(myRowValues)]
    
    # Save the names because rbind screws them up
    correctNames <- names(myDataList[[currentRowType]])
    #print(correctNames)
    
    # Add the PK to our new data Our new data
    myNewData <- c(myNewRowID,myNewData)
    
    # TODO - need to change this - the second time that this code is run then we'll probably have foreign keys added to the data and this logic doesn't work :-S
    # If the last value in the exchange file row is blank then readLines doesn't pick it up
    # to solve this we'll add a blank value if required
    #if (length(correctNames[!grepl("^..id$",correctNames)])==length(myNewData)+1){
    if (length(correctNames)==length(myNewData)+1){
      #print(correctNames)
      #print(myNewData)
      # Add a blank last value if required
      myNewData <- c(myNewData,"")
    }
    

    # ## If we have a foreign key we might need to add it in to our data
    # if (!is.na(myForeignKeyName)){
    #   # See if we need to add the column to our exisiting data frame
    #   if (!myForeignKeyName %in% names(myDataList[[currentRowType]])){
    #     # Add in the column if we need to, with a default value
    #     myDataList[[currentRowType]][,myForeignKeyName] <- integer(nrow(myDataList[[currentRowType]]))
    #     # Re-Save the names 
    #     correctNames <- names(myDataList[[currentRowType]])
    #   }
    #   ## If we have a foreign key we need to add it in to our new data
    #   myNewData <- c(myNewData,myForeignKeyID)
    # }
    
    ## If we have a foreign key we need to add it in to our new data
    #if (!is.na(myForeignKeyName)){
    #  myNewData <- c(myNewData,myForeignKeyID)
    #}
      
    # Convert the transposed vector to a data frame - otherwise I had problems with factors
    myNewData <- as.data.frame(t(myNewData),stringsAsFactors = FALSE)
    # Ensure the first column (XXid is still an ineteger)
    myNewData[,1] <- as.integer(myNewData[,1])
    
    # for testing
    # if (length(names(myNewData)) != length(correctNames)){
    #   print(correctNames)
    #   print(myNewData)
    # }
    
    # Fix the names
    names(myNewData) <- correctNames
    
    # Add the row to our data
    myDataList[[currentRowType]] <- rbind(myDataList[[currentRowType]],myNewData)

  }

  # TODO append the foreign keys
  
  # See which columns we need to add
  fkColumnsToAdd <- unique(foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType),c("currentRowType","foreignKeyType")])
  
  # Add the FK data
  for (i in 1:nrow(fkColumnsToAdd)){
    
    # first we'll add the new columns required
    myColumnToAdd <- fkColumnsToAdd[i,]
    myDataList[[myColumnToAdd$currentRowType]][,myColumnToAdd$foreignKeyType] <- integer(nrow(myDataList[[myColumnToAdd$currentRowType]]))
    # default FK column to NA
    if (nrow(myDataList[[myColumnToAdd$currentRowType]]) >0) {
      myDataList[[myColumnToAdd$currentRowType]][,myColumnToAdd$foreignKeyType] <- NA
    } 
    
    # Now we need to add the actual values for the FKs
    
    # Get the FK values
    fkValuesToAdd <- foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType) &  foreignKeyTracking$currentRowType == myColumnToAdd$currentRowType & foreignKeyTracking$foreignKeyType == myColumnToAdd$foreignKeyType,]
    
    myDataList[[myColumnToAdd$currentRowType]][,paste(myColumnToAdd$currentRowType,'id',sep="")]
    
    # Left join our data with the FK data - need to use "setNames" to specify the columns to join on because 1 of them is defined using a variable
    mergedFKData <- left_join(myDataList[[myColumnToAdd$currentRowType]],fkValuesToAdd,by=setNames("currentRowID", paste(myColumnToAdd$currentRowType,'id',sep="")))
  
    # Now update the relevent column with the FK value
    mergedFKData[,myColumnToAdd$foreignKeyType] <- mergedFKData$foreignKeyID
    
    # Get rid of the columns we don't need
    colstoRemove <- c("currentRowType","previousRowID","previousRowType","foreignKeyID","foreignKeyType")
    dataWithFK <- select(mergedFKData,-all_of(colstoRemove))
    
    # Update our data list with the modified data
    myDataList[[myColumnToAdd$currentRowType]] <- dataWithFK
  }
  
  # fkTablesToProcess <- unique(foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType),c("currentRowType")])
  # 
  # for (myFKTable in fkTablesToProcess){
  #   
  #   myFKTable <- 'SA'
  #   myIDField <- paste(myFKTable,'id',sep="")
  #   myFKValues <- foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType) & foreignKeyTracking$currentRowType == myFKTable,  ]
  #   myDataList[[myFKTable]][,paste(myFKTable,'id',sep="")]
  #   inner_join(myDataList[[myFKTable]],myFKValues,by=setNames("currentRowID", myIDField))
  #   by = c("a" = "b")
  #   joinBy <- c(myIDField = 'currentRowID')
  #   myIDField <- "currentRowID"
  #   setNames("currentRowID", myIDField)
  # }
  
  
  
  myDataList
}
View(myDataList[['BV']])

 
