library(RODBC)
library(dplyr)
library(data.table)
library(XML)
library(icesVocab)
library(RCurl)
library(httr)

# Location for our output files
outputFolder <- "./output/"

#' loadRDBESData
#' This function loads data that is already in the RDBES format from a relational database.
#' 
#' @param connectionString A string specifying the connection string to the database in a formt that odbcDriverConnect can use e.g. 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true'
#'
#' @return A named list containing the different RDBES tables
#' @export
#'
#' @examples
#' myRDBESData <- loadRDBESData(readRDS('connectionString.RDS'))
loadRDBESData <- function(connectionString){
  
  # Connect to the database
  channel <- odbcDriverConnect(connectionString)
  
  # Run queries to fetch the data
  myCE <- sqlQuery(channel,"select * from dbo.CE", stringsAsFactors = FALSE)
  myCL <- sqlQuery(channel,"select * from dbo.CL", stringsAsFactors = FALSE)
  
  myDE <- sqlQuery(channel,"select * from dbo.Design", stringsAsFactors = FALSE)
  mySD <- sqlQuery(channel,"select * from dbo.SamplingDetails", stringsAsFactors = FALSE)
  myOS <- sqlQuery(channel,"select * from dbo.OnshoreEvent", stringsAsFactors = FALSE)
  myLE <- sqlQuery(channel,"select * from dbo.LandingEvent", stringsAsFactors = FALSE)
  myVS <- sqlQuery(channel,"select * from dbo.VesselSelection", stringsAsFactors = FALSE)
  myFT <- sqlQuery(channel,"select * from dbo.FishingTrip", stringsAsFactors = FALSE)
  myFO <- sqlQuery(channel,"select * from dbo.FishingOperation", stringsAsFactors = FALSE)
  mySS <- sqlQuery(channel,"select * from dbo.SpeciesSelection", stringsAsFactors = FALSE)
  mySA <- sqlQuery(channel,"select * from dbo.Sample", stringsAsFactors = FALSE)
  myFM <- sqlQuery(channel,"select * from dbo.FrequencyMeasure", stringsAsFactors = FALSE)
  myBV <- sqlQuery(channel,"select * from dbo.BiologicalVariable", stringsAsFactors = FALSE)
  
  mySL <- sqlQuery(channel,"select * from dbo.SpeciesListDetails", stringsAsFactors = FALSE)
  myVD <- sqlQuery(channel,"select * from dbo.VesselDetails", stringsAsFactors = FALSE)
  
  #myLocodes <- sqlQuery(channel,"select * from dbo.PortLocodes", stringsAsFactors = FALSE)
  #myAphiaIds <- sqlQuery(channel,"select * from dbo.SpeciesAphiaIDs", stringsAsFactors = FALSE)
  
  # Close the connection
  close(channel)
  
  # Create a named list to return our data
  myRDBESData <- list(  CE = myCE
                       ,CL = myCL
                       ,DE = myDE
                       ,SD = mySD
                       ,OS = myOS
                       ,LE = myLE
                       ,VS = myVS
                       ,FT = myFT
                       ,FO = myFO
                       ,SS = mySS
                       ,SA = mySA
                       ,FM = myFM
                       ,BV = myBV
                       ,SL = mySL
                       ,VD = myVD
                       #,Locodes = myLocodes
                       #,Aphiaids = myAphiaIds
  )
  
  return(myRDBESData)
  
}

#' generateSimpleExchangeFile Generate either a CE, CL, VD, or SL exchange format file for the RDBES
#'
#' @param typeOfFile The file we want to create - allowed values are CL, CE, VD, or SL
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param yearToUse The year we want to generate the CE file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param numberOfRows (Optional) Limit the output to this number of rows (just used for testing)
#' @param cleanData (Optional) if TRUE then remove any invalid rows from the data before generating the upload files - warning data will potentially be lost from your upload file if you do this!
#' @param RDBESvalidationdata (Optional) If you have selected to cleanData then you need to supply validation data (derived from BaseTypes.xsd)
#' @param RDBEScodeLists (Optional) If you have selected to cleanData then you need to supply reference data (derived from ICES vocabulary server)
#' 
#' @return
#' @export
#'
#' @examples generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateSimpleExchangeFile <- function(typeOfFile, outputFileName = "", yearToUse, country, RDBESdata,numberOfRows = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL){
  
  # We only need a 2 letter name for the file/table to use
  if (length(typeOfFile)>2){
    typeOfFile <- substr(typeOfFile,1,2)
  }
  
  # Stop if we don't have a valid file type
  if (!typeOfFile %in% c("CL","CE","VD","SL")){
    stop(paste("Invalid value for 'typeOfFile': ",typeOfFile))
  }
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    fileName <- paste("H",typeOfFile,".csv", sep ="")
    outputFileName <- paste(country,yearToUse,fileName, sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data and write it out
  
  RDBESdataForFile <- list(CE=RDBESdata[['CE']], CL=RDBESdata[['CL']], VD=RDBESdata[['VD']], SL=RDBESdata[['SL']])
  
  myData <- RDBESdataForFile[[typeOfFile]]

  # Filter the data by our input parameters
  if (typeOfFile == 'CE'){
    myDataForFile <- myData[myData$CEyear == yearToUse & myData$CEvesselFlagCountry == country,]
  } else if (typeOfFile == 'CL'){
    myDataForFile <- myData[myData$CLyear == yearToUse & myData$CLvesselFlagCountry == country,]
  } else if (typeOfFile == 'VD') {
    myDataForFile <- myData[myData$VDyear == yearToUse & myData$VDcountry == country,]
  } else if (typeOfFile == 'SL') {
    myDataForFile <- myData[myData$SLyear == yearToUse & myData$SLcountry == country,]
  }
  
  RDBESdataForFile[[typeOfFile]]<-myDataForFile

  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    rowsBefore <- nrow(myDataForFile)
    print(paste(rowsBefore, ' rows before removing invalid data', sep =""))
    
    # Validate
    myErrors <- validateTables(RDBESdata = RDBESdataForFile, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists, shortOutput = FALSE,framestoValidate = c(typeOfFile))
    
    # Remove any invalid rows 
    myDataForFile <- removeInvalidRows(tableName = typeOfFile,dataToClean = myDataForFile,errorList = myErrors)
    
    rowsAfter <- nrow(myDataForFile)
    print(paste(rowsAfter, ' rows after removing invalid data', sep =""))
    
    if (rowsAfter < rowsBefore){
      missingRows <- rowsBefore - rowsAfter
      warning(paste(missingRows,' invalid rows removed before trying to generate output files', sep = ""))
    }
    
  }
  
  # If we only want a certain number of rows we'll subset the data (normally just used during testing)
  if (!is.null(numberOfRows)){
    if (nrow(myDataForFile) > numberOfRows)
    {
      myDataForFile <- myDataForFile[1:numberOfRows,]
      print(paste("File truncated to ",numberOfRows, " rows",sep=""))
    }
  }
  
  # We now write out the data frame with ids and column names included to make debugging easier
  fwrite(myDataForFile, paste(outputFolder, "debug_", outputFileName,sep="") ,row.names=F,col.names=T,quote=F)
  
  # Now we'll get rid of the id values because we won't want them in our final output file
  if (typeOfFile == 'CE'){
    myDataForFile <- select(myDataForFile,-c(CEid))
  } else if (typeOfFile == 'CL'){
    myDataForFile <- select(myDataForFile,-c(CLid))
  } else if (typeOfFile == 'VD') {
    myDataForFile <- select(myDataForFile,-c(VDid))
  } else if (typeOfFile == 'SL') {
    myDataForFile <- select(myDataForFile,-c(SLid))
  }
  
  # Get all the values from ceFile and list them out
  myFinalData <- do.call('paste',c(myDataForFile,sep=','))
  
  # replace NA with blanks
  myFinalData <- gsub('NA','',myFinalData)
  
  # Write out the file
  fwrite(list(myFinalData), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  print(paste("Output file written to ",outputFileName,sep=""))
  
}


#' generateComplexExchangeFile This function creates an RDBES exchange file for CS data
#'
#' @param typeOfFile The hierarchy we want to generate a CS file for e.g. 'H1'
#' @param yearToUse The year we want to generate the CS H5 file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfSamples (Optional) Limit the output to this number of samples (just used for testing)
#' @param cleanData (Optional) if TRUE then remove any invalid rows from the data before generating the upload files - warning data will potentially be lost from your upload file if you do this!
#' @param RDBESvalidationdata (Optional) If you have selected to cleanData then you need to supply validation data (derived from BaseTypes.xsd)
#' @param RDBEScodeLists (Optional) If you have selected to cleanData then you need to supply reference data (derived from ICES vocabulary server)
#' @param RequiredTables A list of the tables required for each hierachy
#'
#' @return
#' @export
#'
#' @examples generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateComplexExchangeFile <- function(typeOfFile, yearToUse, country, RDBESdata, outputFileName="", numberOfSamples = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL, RequiredTables){
  
  # For testing
  # typeOfFile <- 'H1'
  # RDBESdata<-myRDBESData
  # yearToUse <- 2017
  # country <- 'IE'
  # outputFileName <- ""
  # numberOfSamples <- 10
  # cleanData <- TRUE
  # RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  # RequiredTables = requiredTables
  
  ## Step 0 - Check typeOfFile and generate a file name if we need to 
  validCSfileTypes <- c('H1')
  
  if (!typeOfFile %in% c('H1')){
    warning(paste("Method not tested for ",typeOfFile, " yet", sep =""))
  }
  
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,paste(typeOfFile,".csv", sep = ""), sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)

  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  requiredTables <- RequiredTables[[typeOfFile]]
  
  ## Step 1 - Filter the data
  myCSData <- filterCSData(RDBESdata = RDBESdata , RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)
    
  
  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    rowsBefore <- 0
    for (myRequiredTable in requiredTables){
      rowsBefore <- rowsBefore + nrow(myCSData[[myRequiredTable]])
    }
    print(paste(rowsBefore, ' rows before removing invalid data', sep =""))
    
    # Validate 
    myErrors <- validateTables(RDBESdata = myCSData
                               ,RDBESvalidationdata = RDBESvalidationdata
                               ,RDBEScodeLists = RDBEScodeLists
                               ,shortOutput = FALSE
                               ,framestoValidate = requiredTables
    )
    
    # Remove any invalid rows 
    for (myRequiredTable in requiredTables){
      myCSData[[myRequiredTable]]<- removeInvalidRows(tableName = myRequiredTable,dataToClean = myCSData[[myRequiredTable]],errorList = myErrors)
    }
    
    # Filter the data again to ensure we don't have any orphan rows in our output
    myCSData <- filterCSData(RDBESdata = myCSData , RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)
    
    rowsAfter <- 0
    for (myRequiredTable in requiredTables){
      rowsAfter <- rowsAfter + nrow(myCSData[[myRequiredTable]])
    }
    print(paste(rowsAfter, ' rows after removing invalid data', sep =""))
    
    if (rowsAfter < rowsBefore){
      missingRows <- rowsBefore - rowsAfter
      warning(paste(missingRows,' invalid rows removed before trying to generate output files', sep = ""))
    }
    
  }
  
  
  # If required, limit the number of samples we will output (normally just used during testing)
  if (!is.null(numberOfSamples)){
    # Get our samples (not including sub-samples)
    NotSubSamples <- myCSData[['SA']][is.na(myCSData[['SA']]$SAparentSequenceNumber),]
    
    #if (nrow(myCSData[['SA']])>numberOfSamples ) {
    if (nrow(NotSubSamples)>numberOfSamples ) {
      
      #Subset the data to get the SAid values we are interested it
      #SAidsToUse <- myCSData[['SA']][1:numberOfSamples,"SAid"]
      SAidsToUse <- NotSubSamples[1:numberOfSamples,"SAid"]
      
      # Need Sort out SA and FM first, then we'll deal with the other tables
      
      # SA : Top level samples not including sub-samplea
      myCSData[['SA']]<- myCSData[['SA']][myCSData[['SA']]$SAid %in% SAidsToUse,]
      # Now handle any sub-samples
      mySubSampleData <- myCSData[['SA']][!is.na(myCSData[['SA']]$SAparentSequenceNumber),]
      
      # If we have any sub-samples see if we need to include them
      if (nrow(mySubSampleData) > 0){
        # Use a recursive function to fetch the top level sequence number of our sub-samples
        mySubSampleData$topLevelSequenceNumber <- sapply(mySubSampleData$SAsequenceNumber,getTopLevelSequenceNumber,SAdata = mySubSampleData)
        # Only include sub-samples if their top level sequence numebr is in our filtered sample data
        mySubSampleData <- mySubSampleData[mySubSampleData$topLevelSequenceNumber %in% myCSData[['SA']]$SAsequenceNumber,]
        # Remove the column we added
        mySubSampleData$topLevelSequenceNumber <- NULL
        # Combine our samples and sub-samples together
        myCSData[['SA']] <- rbind(myCSData[['SA']],mySubSampleData)
      }
      
      # FM
      myCSData[['FM']]<- myCSData[['FM']][myCSData[['FM']]$SAid %in% myCSData[['SA']]$SAid,]

      # Now deal with all the other tables
      myData <- NULL
      previousRequiredTable <- NULL
      
      # Iterate through the required tables in reverse order and remove any records not assoicated with our selected samples
      for (myRequiredTable in rev(requiredTables)){

        if (myRequiredTable %in% c('SA','FM')){
          # Do nothing - already handled above
        } 
        # Need to check if the BV records are in either the FM or SA tables
        else if (myRequiredTable == 'BV'){
          myData <- myCSData[['BV']][myCSData[['BV']]$FMid %in% myCSData[['FM']]$FMid | myCSData[['BV']]$SAid %in% myCSData[['SA']]$SAid,]
          myCSData[['BV']] <- myData
        } 
        # Other tables can follow a general pattern
        else {
          
          previousHierarchyTable <- myCSData[[previousRequiredTable]]
          ## Assume the primary key is the first field
          currentPrimaryKey <- names(myCSData[[myRequiredTable]])[1]
          myData <- myCSData[[myRequiredTable]][myCSData[[myRequiredTable]][,currentPrimaryKey] %in% previousHierarchyTable[,currentPrimaryKey],]
          myCSData[[myRequiredTable]] = myData
          
        }
        
        previousRequiredTable <- myRequiredTable
        
      }
      
      print(paste("File truncated to data relating to ",numberOfSamples, " samples",sep=""))
    }
  }
  
  
  ## Step 2 - I now add a SortOrder field to each of our fitlered data frames
  ## this will allow me to generate the CS file in the correct row order without needing a slow for-loop
  myCSData <- generateSortOrder(RDBESdataToSort = myCSData, RequiredTables = requiredTables)
  
  # Combine our SortOrder values
  # TODO Need to double-check this works correctly
  for (myRequiredTable in requiredTables){
    if (myRequiredTable == 'DE'){
      FileSortOrder <- myCSData[[myRequiredTable]]$SortOrder
    } else {
      FileSortOrder <-c(FileSortOrder,myCSData[[myRequiredTable]]$SortOrder)
    }
  }
  

  ## STEP 3) Create a version of the output data for debugging
  
  # Here we create a version of the output data with all the ids and sorting columns in so I can check things are correct
  for (myRequiredTable in requiredTables){
    if (myRequiredTable == 'DE'){
      csForChecking <- do.call('paste',c(myCSData[[myRequiredTable]],sep=','))
    } else {
      csForChecking <- c(csForChecking,do.call('paste',c(myCSData[[myRequiredTable]],sep=',')))
    }
  }
  
  # Sort the output into the correct order
  csForCheckingOrdered <- csForChecking[order(FileSortOrder)]
  
  fwrite(list(csForCheckingOrdered), paste(outputFolder,"debug_", outputFileName,sep="") ,row.names=F,col.names=F,quote=F)
  
  ## STEP 4) Create the real version of the output data
  
  # Create the CS data with the sort columns and ids removed - this will then be used to generate the exchange file
  for (myRequiredTable in requiredTables){
    
    # First remove the columns we don't want in the final output (SortOrder and any XXid columns)
    colstoRemove <- c("SortOrder", names(myCSData[[myRequiredTable]])[grepl("^..id$",names(myCSData[[myRequiredTable]]))])
    myData <- select(myCSData[[myRequiredTable]],-all_of(colstoRemove))
    
    # Now stick all the lines from each table together with commas seperating the values
    if (myRequiredTable == 'DE'){
      cs <- do.call('paste',c(myData,sep=','))
    } else {
      cs <- c(cs,do.call('paste',c(myData,sep=',')))
    }
  }
  
  # Sort the output into the correct order
  csOrdered <- cs[order(FileSortOrder)]
  
  # replace NA with blanks
  csOrdered <- gsub('NA','',csOrdered)
  
  fwrite(list(csOrdered), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  print(paste("Output file written to ",outputFileName,sep=""))
  
}


#' filterCSData Filter the CS data to sub-set by year, country, and hierarchy
#'
#' @param RDBESdata 
#' @param RequiredTables 
#' @param YearToFilterBy 
#' @param CountryToFilterBy 
#' @param UpperHierarchyToFilterBy 
#'
#' @return
#' @export
#'
#' @examples
filterCSData <- function(RDBESdata, RequiredTables, YearToFilterBy, CountryToFilterBy, UpperHierarchyToFilterBy){
  
  myCSData <- list()
  myData <- NULL
  previousRequiredTable <- NULL
  
  # Get the data for each required table - filter by year, country, and upper hierarchy
  for (myRequiredTable in RequiredTables){
    myData <- RDBESdata[[myRequiredTable]]
    
    # Need to filter DE by year and upper hieararchy
    if (myRequiredTable == 'DE'){
      myData <- myData[myData$DEyear == YearToFilterBy & myData$DEhierarchy == UpperHierarchyToFilterBy,]
      myCSData[[myRequiredTable]] = myData
    } 
    # Need to filter SD by country
    else if (myRequiredTable == 'SD'){
      myData <- myData[myData$DEid %in% myCSData[[previousRequiredTable]]$DEid & myData$SDcountry == CountryToFilterBy,]
      myCSData[[myRequiredTable]] = myData
    }
    # Need to handle samples and sub-samples for SA
    else if (myRequiredTable == 'SA'){
      
      #Lets deal with Samples first
      previousHierarchyTable <- myCSData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      mySampleData <- myData[myData[,previousPrimaryKey] %in% previousHierarchyTable[,previousPrimaryKey],]
      
      # Now handle any sub-samples
      mySubSampleData <- myData[!is.na(myData$SAparentSequenceNumber),]
      
      # Use a recursive function to fetch the top level sequence number of our sub-samples
      mySubSampleData$topLevelSequenceNumber <- sapply(mySubSampleData$SAsequenceNumber,getTopLevelSequenceNumber,SAdata = mySubSampleData)
      
      # Only include sub-samples if their top level sequence numebr is in our filtered sample data
      mySubSampleData <- mySubSampleData[mySubSampleData$topLevelSequenceNumber %in% mySampleData$SAsequenceNumber,]
      
      # Get rid of the column we added earlier
      mySubSampleData$topLevelSequenceNumber <- NULL
      
      # Stick our samples and sub-samples together
      myData <- rbind(mySampleData, mySubSampleData)
      
      myCSData[[myRequiredTable]] = myData
      
    }
    # BVid can either be in FM or SA
    else if (myRequiredTable == 'BV'){
      myData <- myData[myData$FMid %in% myCSData[['FM']]$FMid | myData$SAid %in% myCSData[['SA']]$SAid,]
      myCSData[[myRequiredTable]] = myData
    } 
    # all other tables can follow a general pattern of matching
    else {
      #previousHierarchyTable <- RDBESdata[[myRequiredTable]]
      previousHierarchyTable <- myCSData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      myData <- myData[myData[,previousPrimaryKey] %in% previousHierarchyTable[,previousPrimaryKey],]
      myCSData[[myRequiredTable]] = myData
    }
    
    previousRequiredTable <- myRequiredTable
  }
  
  
  myCSData
  
}

#' getTopLevelSequenceNumber Recursive function to get the top level SAsequenceNumber of a series of sameples and sub-samples
#'
#' @param SAdata 
#' @param SAsequenceNumber 
#'
#' @return
#' @export
#'
#' @examples
getTopLevelSequenceNumber <- function(SAdata,SAsequenceNumber ){
  #print(SAsequenceNumber)
  dataToCheck <- SAdata[SAdata$SAsequenceNumber == SAsequenceNumber,]
  
  # If we have mutiple matches we probably don't have unique SAsequenceNumber values
  if (nrow(dataToCheck) > 1){
    warning("There is a problem with non-unique SAsequenceNumber values- check your data")
    # Just use the first match
    dataToCheck <- dataToCheck[1,]
  }
  
  if (nrow(dataToCheck) == 0) {
    return (NA)
  } else if (is.na(dataToCheck$SAparentSequenceNumber)) {
    return (SAsequenceNumber)
  } else {
    return (getTopLevelSequenceNumber(SAdata = SAdata,SAsequenceNumber = dataToCheck$SAparentSequenceNumber))
  }
  
}


#' generateSortOrder Adds a SortOrder field to each frame in our list of data frames.  When the data is sorted by this column it shoudl be in the correct order to generate a CS exchange file.  Even if the data frame is empty I add in a blank SortOrder column - that way I can guarantee it exists
#'
#' @param RDBESdataToSort 
#' @param RequiredTables 
#'
#' @return
#' @export
#'
#' @examples
generateSortOrder <- function(RDBESdataToSort, RequiredTables){
  

  # IMPORTANT - I'm using inner_join from dply so we can maintain the ordering of the first data frame in the join
  # if the ordering isn't maintained then the exchange file will be output in the wrong order
  
  # TODO For our data BV only follows SA not FM - need to check that the sort order will work if there is a mix of lower hierarchies
  
  previousRequiredTable <- NULL
  
  for (myRequiredTable in RequiredTables){
    
    # Check if there are any rows in this table
    if(nrow(RDBESdataToSort[[myRequiredTable]])>0) {
      
      # Need to handle DE differently because the SortOrder doesn't just use the primary key
      if (myRequiredTable == 'DE'){
        
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste(RDBESdataToSort[[myRequiredTable]]$DEhierarchy,RDBESdataToSort[[myRequiredTable]]$DEyear,RDBESdataToSort[[myRequiredTable]]$DEstratum,sep="-")
        
      } 
      # Need to handle SA differently because there can be sub-samples
      else if (myRequiredTable == 'SA'){
        
        # We will use SAsequenceNumber in the SortOrder - this shoudl ensure all samples and sub-samples end-up in the correct order
        # TODO this needs checking
        
        previousHierarchyTable <- RDBESdataToSort[[previousRequiredTable]]
        ## Assume the primary key is the first field
        previousPrimaryKey <- names(previousHierarchyTable)[1]
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,"SAsequenceNumber"], sep = "-")
        
      }
      # Need to handle BV differently because it can be linked to from either FM or SA
      else if (myRequiredTable == 'BV') {
        
        # Add the SortOrder field first
        # Bit ugly but we'll call it SortOrder_BV to start with to avoid some issues - we'll name it propery in a minute
        RDBESdataToSort[[myRequiredTable]]$SortOrder_BV <- character(nrow(RDBESdataToSort[[myRequiredTable]]))
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        
        # Add SortOrder where there is a link to FM (rows where FMid is not NA)
        if (nrow( RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),] )>0){
          
          previousHierarchyTable <- myCSData[['FM']]
          previousPrimaryKey <- names(previousHierarchyTable)[1]
          # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
          RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),"SortOrder_BV"] <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,currentPrimaryKey], sep = "-")
        } 
        
        # Add SortOrder where there is a link to SA (rows where SAid is not NA)
        if (nrow( RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),] )>0){
          
          previousHierarchyTable <- RDBESdataToSort[['SA']]
          previousPrimaryKey <- names(previousHierarchyTable)[1]
          # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
          RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),"SortOrder_BV"] <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,currentPrimaryKey], sep = "-")
          
        }
        
        # Rename SortOrder_BV field to SortOrder
        names(RDBESdataToSort[[myRequiredTable]])[names(RDBESdataToSort[[myRequiredTable]]) == "SortOrder_BV"] <- "SortOrder"
        
      }
      # Else follow the general pattern
      else {
        
        previousHierarchyTable <- RDBESdataToSort[[previousRequiredTable]]
        ## Assume the primary key is the first field
        previousPrimaryKey <- names(previousHierarchyTable)[1]
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,currentPrimaryKey], sep = "-")
      }
    }      
    # If there's no rows in the table we'll add an emtpy SortOrder column so it definitely exists
    else  {
      RDBESdataToSort[[myRequiredTable]]$SortOrder <- character(0)
    }
    
    previousRequiredTable <- myRequiredTable
  }
  
  RDBESdataToSort
  
}


#' saveRDataFilesForCS Saves RData files containing the relevent data frames for a CS upper hierarchy.  Each data frame is stored in a seperate RData file.  Data is filtered by country and year 
#'
#' @param typeOfFile 
#' @param yearToUse 
#' @param country 
#' @param RDBESdata 
#' @param RequiredTables 
#'
#' @return
#' @export
#'
#' @examples
saveRDataFilesForCS <- function(typeOfFile, yearToUse, country, RDBESdata, RequiredTables){
  
  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  requiredTables <- RequiredTables[[typeOfFile]]
  
  ## Step 0
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(paste0(outputFolder,typeOfFile))), dir.create(file.path(paste0(outputFolder,typeOfFile))), FALSE)
  
  ## Step 1 - Filter the data
  myCSData <- filterCSData(RDBESdata = RDBESdata , RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)

  ## Step 2 Save the data
  for (myRequiredTable in requiredTables){
    frameToSave <- RDBESdata[[myRequiredTable]]
    save(frameToSave, file = paste0(outputFolder,typeOfFile,"/",myRequiredTable, ".RData"))
  }
  
}




#' changeFieldNames Change the field names of an RDBES data either from database names to R names or vice versa
#'
#' @param frameToRename The RDBES data frame we want to rename
#' @param fieldNameMap The data frame holding our names mappings
#' @param typeOfChange Either RtoDB or DBtoR
#'
#' @return
#' @export
#'
#' @examples changeFieldNames(frameToRename = x, fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
changeFieldNames <- function(frameToRename, fieldNameMap, typeOfChange){
  
  # For testing
  #frameToRename <- myRDBESData[["BV"]]
  #fieldNameMap <- list_RDBES_Variables
  #typeOfChange <- "DBtoR"
  
  if (!typeOfChange %in% c("RtoDB", "DBtoR")) stop("typeOfChange paramter shoudl be either RtoDB or DBtoR")
  # IF change DB to R
  if (typeOfChange == "DBtoR"){
    
    # Get the current column names into a data frame
    myDF <- data.frame(name = names(frameToRename), stringsAsFactors = FALSE)
    
    # Assume the first entry is always the primary key of the table and extract the table name
    myTableName <- substr(myDF[1,"name"],1,2)
    
    # Left join our current names against the replacement names (match to DB names)
    myMapping <- left_join(myDF,list_RDBES_Variables[fieldNameMap$Table==myTableName,c("Field.Name","R.Name")], by=c("name" = "Field.Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping$R.Name <- ifelse(is.na(myMapping$R.Name),myMapping$name,myMapping$R.Name)
    
    # set the new names
    myNewNames <- myMapping$R.Name
    
  } else if (typeOfChange == "RtoDB"){
    
    # Left join our current names against the replacement names (match to R names)
    myMapping <- left_join(myDF,list_RDBES_Variables[fieldNameMap$Table==myTableName,c("Field.Name","R.Name")], by=c("name" = "R.Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping$Field.Name <- ifelse(is.na(myMapping$Field.Name),myMapping$name,myMapping$Field.Name)
    
    # set the new names
    myNewNames <- myMapping$Field.Name
    
  }
  
  # return the new names
  myNewNames
  
}

#' loadRDataFiles Load all the RData files from a directory
#'
#' @param directoryToSearch Directory to load files from
#' @param recursive Should we search for files recursivley? Default = FALSE
#'
#' @return
#' @export
#'
#' @examples
loadRDataFiles <- function(directoryToSearch, recursive = FALSE){
  
  # For testing
  #directoryToSearch = "./output/H5"
  #recursive = FALSE
  
  # Get a list of the RData files
  filesToRead <- list.files(path = directoryToSearch, pattern = "*.RData", recursive = recursive, full.names = TRUE)
  
  # Load each file
  for (i in 1:length(filesToRead)){
    load(file=filesToRead[i])
  }
  

}

#' getValidationData Gets the base types xsd so we know what data type each field should be and what the code list is
#'
#' @param fileLocation Location of the base types xsd file
#'
#' @return
#' @export
#'
#' @examples validationData <- getValidationData(fileLocation = './tableDefs/BaseTypes.xsd')
getValidationData <- function(downloadFromGitHub = TRUE, fileLocation){
  
  # For testing
  #downloadFromGitHub = FALSE
  #fileLocation <- './tableDefs/BaseTypes.xsd'
  
  # STEP 1) Get the BaseTypes file (if required)
  if (downloadFromGitHub){
    # get the latest BaseTypes.xsd file from GitHub
    myBaseTypes <- getURL("https://raw.githubusercontent.com/ices-tools-dev/RDBES/master/XSD-files/BaseTypes.xsd")
    # save the file locally
    writeLines(myBaseTypes, fileLocation)
  }
  
  # STEP 2) Parse the XML
  doc <- xmlTreeParse(fileLocation,useInternal= TRUE)
  myXML <- xmlToList(doc)
  
  # STEP 3) Get all the field names and their types for each table (stored as "complexType" entries)
  
  # Data frame to hold our validation info
  myValidationDF <- NULL
  
  # Get the infromation we want from the xsd file - hard-code to the current structure...
  for (myElement in myXML[names(myXML) == "complexType"]){
    if(names(myElement)[[1]]=="sequence"){
      for (mySubElement in myElement[[1]]){
        myMin <- mySubElement[[1]]
        myMax <- mySubElement[[2]]
        myName <- mySubElement[[3]]
        myType <- mySubElement[[4]]
        myDF <- data.frame(min=myMin, max=myMax, name=myName, type=myType, stringsAsFactors = FALSE)
        if (is.null(myValidationDF)){
          myValidationDF <- myDF
        } else {
          myValidationDF <- rbind(myValidationDF,myDF)
        }
      }
    }
  }
  
  # STEP 4) Get the vaidation information for the thins like decimal ranges (stored as "simpleType")
  
  # Data frame to hold our validation info
  myValidationSimpleTypes <- NULL
  
  # Get the infromation we want from the xsd file - hard-code to the current structure...
  for (myElement in myXML[names(myXML) == "simpleType"]){
    myName <- myElement$.attrs
    myType <- myElement[[1]]$.attrs
    myMin <- myElement[[1]]$minInclusive
    if (is.null(myMin)) {myMin <- NA}
    myMax <- myElement[[1]]$maxInclusive
    if (is.null(myMax)) {myMax <- NA}
    myfractionDigits <- myElement[[1]]$fractionDigits
    if (is.null(myfractionDigits)) {myfractionDigits <- NA}
    myLength <- myElement[[1]]$length
    if (is.null(myLength)) {myLength <- NA}
    myPattern <- myElement[[1]]$pattern
    if (is.null(myPattern)) {myPattern <- NA}
    myDF <- data.frame(name=myName,checkName=myName, description="simpleTypeCheck", minValue=myMin, maxValue=myMax, dataType=myType, fractionDigits = myfractionDigits, length = myLength, pattern = myPattern,  stringsAsFactors = FALSE)
    if (is.null(myValidationSimpleTypes)){
      myValidationSimpleTypes <- myDF
    } else {
      myValidationSimpleTypes <- rbind(myValidationSimpleTypes,myDF)
    }
    
  }
  
  # STEP 5) Join our validation together and return it
  myValidationDF <- left_join(myValidationDF,myValidationSimpleTypes,by=c("type" = "name"))
  

  myValidationDF[!is.na(myValidationDF$dataType),"type"] <- myValidationDF[!is.na(myValidationDF$dataType),"dataType"]
  myValidationDF$dataType <- NULL
  
  myValidationDF
  
  
  
}

#' getTablesInHierarchies Used the H* xsd files to define which tables are required in the different hierachies
#'
#' @param downloadFromGitHub (Optional) Set to TRUE if you want to download the lastest xsd files from GitHub
#' @param fileLocation The folder to read the files from.  If you are downloading from GitHub a copy of the latest files will be saved here.
#'
#' @return
#' @export
#'
#' @examples
getTablesInHierarchies <- function(downloadFromGitHub = TRUE, fileLocation){
  
  # For testing
  #downloadFromGitHub = TRUE
  #fileLocation <- './tableDefs/'
  
  # STEP 1) Get the BaseTypes file (if required)
  if (downloadFromGitHub){
    
    myHierarchyFiles <- NULL
    myResponse <- GET("https://api.github.com/repos/ices-tools-dev/RDBES/contents/XSD-files")
    filesOnGitHub <- content(myResponse)
    print(paste("Downloading ",length(filesOnGitHub), " files from GitHub", sep =""))
    for (myFile in filesOnGitHub){
      myGitHubFile <- data.frame(fileName = myFile$name, downloadURL = myFile$download_url)
      if (is.null(myHierarchyFiles)){
        myHierarchyFiles <- myGitHubFile 
      } else {
        myHierarchyFiles <- rbind(myHierarchyFiles,myGitHubFile)
      }
    }
    # Sub-set to the files we are interested in
    myHierarchyFiles <- myHierarchyFiles[grepl('^H.*xsd$',myHierarchyFiles$fileName),]
    
    # Download our files
    for (i in 1:nrow(myHierarchyFiles)){
      anHierarchyFile <- getURL(myHierarchyFiles[i,'downloadURL'])
      # save the file locally
      writeLines(anHierarchyFile, paste(fileLocation,myHierarchyFiles[i,'fileName'], sep = ""))
    }
    
  }
  
  # Read all the H.*xsd files
  filesToRead <- list.files(path = fileLocation, pattern = "^H.*xsd$", recursive = FALSE, full.names = FALSE)  
  
  
  myHierarchyTables <- list()
  for(fileToParse in filesToRead){
    
    #fileToParse <-"H1.xsd"
    fileToParse <- paste(fileLocation,fileToParse,sep="")
    
    # STEP 2) Parse the XML
    doc <- xmlTreeParse(fileToParse,useInternal= TRUE)
    myXML <- xmlToList(doc)
    
    myResults <- NULL
    hierachyName <- NULL
    
    for (myElement in myXML[names(myXML) == "complexType"]){
      myAttr <- myElement$.attrs
      names(myAttr) <- NULL
      
      if (grepl('^H.*',myAttr)){
        hierachyName <- myAttr
      }
      if (nchar(myAttr)==2 & !grepl('^H.*',myAttr)){
        #print(myElement$.attrs)
        if (is.null(myResults)){
          myResults <- c(myAttr)
        } else {
          myResults <- c(myResults,myAttr)
        }
      }
    }
    
    # Add to our list of results
    myHierarchyTables[[hierachyName]] <- myResults
  }
  
  myHierarchyTables
  
}



#' logValidationError Internal utility function to log errors into an error data frame
#'
#' @param errorList 
#' @param tableName 
#' @param rowID 
#' @param fieldName 
#' @param problemType 
#' @param problemDescription 
#'
#' @return
#'
#' @examples
logValidationError<- function(errorListToAppendTo = NULL,tableName, rowID, fieldName, problemType, problemDescription){
  
  myErrors <- data.frame(tableName = tableName
                         ,rowID = rowID
                         ,fieldName = fieldName
                         ,problemType = problemType
                         ,problemDescription = problemDescription
                         ,stringsAsFactors = FALSE)
  
  if (is.null(errorListToAppendTo)){
    errorListToReturn <- myErrors
  } else {
    errorListToReturn <- rbind(errorListToAppendTo, myErrors)
  }
  
  errorListToReturn
  
}

#' validateTables This function validates your RDBES tables against the ICES RDBES xsd files
#'
#' @param RDBESdata A named list of RDBES tables
#' @param RDBESvalidationdata The validation data derived from BaseTypes.xsd
#' @param RDBEScodeLists The RDBES code lists
#' @param shortOutput Set to TRUE if you want a summarised error output, set to FALSE if you want the full error output
#'
#' @return
#' @export
#'
#' @examples errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE)
validateTables <- function(RDBESdata, RDBESvalidationdata, RDBEScodeLists, shortOutput = FALSE, framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CL","CE" )){
  
  # For testing
  # RDBESdata <- myRDBESData
  # RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  # shortOutput <- TRUE
  # #framestoValidate <- c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CL","CE" )
  # framestoValidate <- c("CE")
  
  # To hold the output
  errorList <- data.frame(tableName=character(0)
                          ,rowID = integer(0)
                          ,fieldName=character(0)
                          ,problemType=character(0)
                          ,problemDescription=character(0), stringsAsFactors = FALSE)
  
  # We'll only validate specific tables
  RDBESdata <- RDBESdata[framestoValidate]
  
  # CHECK 1) are we missing any tables?
  newErrors <- validateMissingTables(RDBESdataToCheck=RDBESdata,framesToCheck=framestoValidate)
  errorList <- rbind(errorList,newErrors)
  
  # Remove any NAs from ths lis of tables (these are the tables we don't have)
  RDBESdata <- RDBESdata[!is.na(names(RDBESdata))]
  
  # for each data frame in our list
  for (dfToCheck in RDBESdata){
    
    #dfToCheck <- RDBESdata[[1]]
    
    # Clear out any errors from any earlier checking
    newErrors <- NULL
    
    # Get the field names as a data frame
    myDF <- data.frame(fieldName = names(dfToCheck), stringsAsFactors = FALSE)
    # Join the field names to the field type data frame
    fieldsAndTypes <- left_join(myDF,RDBESvalidationdata,by=c("fieldName" = "name"))
    # Assume the id is always the first field - might be better to check the field names instead
    myIDField <- names(dfToCheck)[[1]]
    myTableName <- substr(myIDField,1,2)
    print(paste("Validating ",myTableName,sep=""))
    
    # Check 2: Check if we are missing any fields
    newErrors <- validateMissingFields(RDBESdataToCheck=dfToCheck,RDBESvalidationdata=RDBESvalidationdata, tableToCheck=myTableName )
    errorList <- rbind(errorList,newErrors)
    
    # CHECK 3: See if the fields are in the right order
    newErrors <- validateFieldOrder(RDBESdataToCheck=dfToCheck,RDBESvalidationdata=RDBESvalidationdata, tableToCheck=myTableName )
    errorList <- rbind(errorList,newErrors)
    
    

    # Now we'll check each field in our current data frame
    for (i in 1:length(names(dfToCheck))) {
      #i <- 15
      myFieldName <- names(dfToCheck)[[i]]
      myFieldType <- fieldsAndTypes[fieldsAndTypes$fieldName == myFieldName,]
      
      ## Check 3) NA values
      newErrors <- validateNAvalues(fieldToCheck=myFieldName,dataToCheck=dfToCheck,fieldTypeToCheck=myFieldType)
      errorList <- rbind(errorList,newErrors)
      
      # Now that we have passed the NA check point lets get rid of any NA values so 
      # we don't need to worry about them again
      dfToCheckNotNA <- dfToCheck[!is.na(dfToCheck[,myFieldName]),]
      
        # Check 4 Do we know what type this field should be?
        # If not, there's nothing much else we can do so skip to the end and log the error
        
        # Check if we know what types this field should have
        if (!is.na(myFieldType$type)){
          myType <- myFieldType$type
          
          # We only want to carry on with further checks if we actually have some non-NA rows  
          # otherwise we can get validation errors where a field in the data frame is technically the wrong data type 
          # but if there is no data in the field the online RDBES validator won't know or care about that....
          if (nrow(dfToCheckNotNA) >0) {
          
            # IF simple type
            if (length(grep("xs:",myType)) > 0) {
              
              # CHeck 5 For simple data types we'll see if we have the right format data
              newErrors <- validateSimpleTypes(fieldToCheck=myFieldName,dataToCheck=dfToCheckNotNA,fieldTypeToCheck=myFieldType)
              errorList <- rbind(errorList,newErrors)
  
              # ELSE code list
            } else {
              
              # Check 6 If we're dealing with code lists we need to see if we have allowed values
              newErrors <- validateAgainstCodeList(fieldToCheck=myFieldName,dataToCheck=dfToCheckNotNA,fieldTypeToCheck=myFieldType, codeLists = allowedValues)
              errorList <- rbind(errorList,newErrors)
                
            } # ENDIF simple type / code list 
          } # No non-NA rows
          
      # ELSE could not find validation inforation on this field so log an error
     } else {
       
        # id and recordType fields don't have validation information so don't bother recording an error for those types of fields
        if (length(grep("^..id$",myFieldName)) == 0 & length(grep("^..recordType$",myFieldName)) == 0){
          errorList <- logValidationError(errorList = errorList
                                          ,tableName = myTableName
                                          ,rowID = NA
                                          ,fieldName = myFieldName
                                          ,problemType = "Code list missing"
                                          ,problemDescription = paste("Could not find validation information for ", myFieldName, sep = " "))
        }
      } #EndIF validation infromation exists / does not exist
    } # Endfor each field in frame
  } # Endfor each frame in RDBES data list
  
  # If we want shorter output we won't show every error  - just the first of each type
  if (shortOutput){
    
    fieldsToKeep <- names(errorList)
    # Sort the errors
    errorList <- errorList[order(errorList$tableName, errorList$fieldName, errorList$problemType, errorList$rowID),]
    # Get the row number within a group
    numberedErrorList <- errorList %>% group_by(errorList$tableName, errorList$fieldName, errorList$problemType) %>% mutate(rowNum = row_number()) %>% mutate(maxRowNum = max(rowNum))

    # Generate our extra rows that will show there were more errors present
    myExtraRows <- numberedErrorList[numberedErrorList$rowNum == 2,c(fieldsToKeep,'maxRowNum')]
    if(nrow(myExtraRows)>0) {
      countOfOtherRows <- as.numeric(myExtraRows$maxRowNum)
      countOfOtherRows <- countOfOtherRows - 1
      myExtraRows$rowID <- NA
      myExtraRows$problemDescription <- paste(countOfOtherRows," similar error rows removed for clarity", sep = "")
      # Get rid of our maxRowNum field
      myExtraRows <- myExtraRows[,fieldsToKeep]
    }
    
    # Combine the first row within each group with the extra rows
    shortErrorList <- rbind(numberedErrorList[numberedErrorList$rowNum == 1,fieldsToKeep],myExtraRows)
    shortErrorList <- shortErrorList[order(shortErrorList$tableName, shortErrorList$fieldName, shortErrorList$problemType, shortErrorList$rowID),]
    
    errorList <- shortErrorList
    
  }
  
  # Our list of errors
  errorList
  
}


#' validateMissingTables Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param framesToCheck 
#'
#' @return
#'
#' @examples
validateMissingTables <- function(RDBESdataToCheck,framesToCheck){
  
  errorsToReturn <- NULL

  missingTables <-framesToCheck[!framesToCheck %in% names(RDBESdataToCheck)]
  
  if (length(missingTables)>0){
    errorsToReturn <- logValidationError(errorList = NULL
                                    ,tableName = missingTables
                                    ,rowID = NA
                                    ,fieldName = NA
                                    ,problemType = "Missing table check"
                                    ,problemDescription = paste("The following table is missing from your list: ", missingTables, sep = " "))
  } 
  
  errorsToReturn

}


#' validateMissingFields Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param RDBESvalidationdata 
#' @param tableToCheck 
#'
#' @return
#'
#' @examples
validateMissingFields <- function(RDBESdataToCheck,RDBESvalidationdata,tableToCheck){

  errorsToReturn <- NULL
  
  # Get the field names as a data frame
  myDFNames <- data.frame(fieldName = names(RDBESdataToCheck), stringsAsFactors = FALSE)
  
  missingFieldsCheck <- RDBESvalidationdata
  missingFieldsCheck$table <- substr(missingFieldsCheck$name,1,2)
  missingFieldsCheck <- missingFieldsCheck[missingFieldsCheck$table==tableToCheck,]
  missingFields <- missingFieldsCheck[!missingFieldsCheck$name %in% myDFNames$fieldName,]
  # If we have some missing fields lets log them as an error
  if (nrow(missingFields) > 0){
    #Log the error
    errorsToReturn <- logValidationError(errorList = NULL
                                    ,tableName = tableToCheck
                                    ,rowID = NA
                                    ,fieldName = missingFields$name
                                    ,problemType = "Missing field check"
                                    ,problemDescription = paste("The following field is missing from your data frames: ", missingFields$name, sep = " "))
  } 
  
  errorsToReturn

}




#' validateFieldOrder Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param RDBESvalidationdata 
#' @param tableToCheck 
#'
#' @return
#'
#' @examples
validateFieldOrder <- function(RDBESdataToCheck,RDBESvalidationdata,tableToCheck){
  
  errorsToReturn <- NULL
  
  # Get the field names as a data frame
  myDFNames <- data.frame(fieldName = names(RDBESdataToCheck), stringsAsFactors = FALSE)
  
  missingFieldsCheck <- RDBESvalidationdata
  missingFieldsCheck$table <- substr(missingFieldsCheck$name,1,2)
  missingFieldsCheck <- missingFieldsCheck[missingFieldsCheck$table==tableToCheck,]

  # Remove the XXid and XXrecordType fields before checking - these aren't uploaded
  #myDFNamesToCheck <- myDF[!grepl("^..id$",myDF$fieldName) & !grepl("^..recordType$",myDF$fieldName),]
  myDFNamesToCheck <- myDFNames[!grepl("^..id$",myDFNames$fieldName) & !grepl("^..recordType$",myDFNames$fieldName),]
  
  # If the values aren't the same (including the same order) then log an error
  if (!identical(toupper(missingFieldsCheck$name),toupper(myDFNamesToCheck))){
    #Log the error
    errorsToReturn <- logValidationError(errorList = NULL
                                         ,tableName = tableToCheck
                                         ,rowID = NA
                                         ,fieldName = NA
                                         ,problemType = "Field order check"
                                         ,problemDescription = paste("The fields are not in the correct order: ",paste(myDFNamesToCheck,collapse=", "),sep=""))
  } 
  
  errorsToReturn
  
}


#' validateNAvalues Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#'
#' @return
#'
#' @examples
validateNAvalues <- function(fieldToCheck,dataToCheck,fieldTypeToCheck){
  
  errorsToReturn <- NULL
  
  #myFieldType <- fieldsAndTypesData[fieldsAndTypesData$fieldName == fieldToCheck,]
  myValuesNA <- dataToCheck[is.na(dataToCheck[,fieldToCheck]),]
  # If we have NA values check if thats ok
  if (nrow(myValuesNA)>0){
    # if we are not allowed to have empty values here then log an error
    if (ifelse(is.na(fieldTypeToCheck$min),0,fieldTypeToCheck$min) >0){
      #Log the error
      errorsToReturn <- logValidationError(errorList = NULL
                                      ,tableName = substr(fieldToCheck,1,2)
                                      ,rowID = myValuesNA[,1]
                                      ,fieldName = fieldToCheck
                                      ,problemType = "Null value check"
                                      ,problemDescription = paste("Null value problem;",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myValuesNA[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myValuesNA[,fieldToCheck], sep = " "))
    }
  } # Endif NA check
  
  errorsToReturn  
}



#' validateSimpleTypes Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#'
#' @return
#'
#' @examples
validateSimpleTypes <- function(fieldToCheck,dataToCheck,fieldTypeToCheck){
  
  #fieldToCheck<-myFieldName
  #dataToCheck<-dfToCheckNotNA
  #fieldTypeToCheck<-myFieldType

  # TODO - this methd has got a bit long and shoudl be re-factored for clarity
  
  errorsToReturn <- NULL
  
  myTypeToCheck <- fieldTypeToCheck$type
  #print(fieldTypeToCheck)
  
  # Do we have any simpleTypeChecks defined for this field?
  simpleTypeCheckDefined <- FALSE
  if (!is.na(fieldTypeToCheck$description)){
    if (fieldTypeToCheck$description == "simpleTypeCheck"){
      simpleTypeCheckDefined <- TRUE
    } 
  }
  
  # See if we need to do any range checks first
  if (simpleTypeCheckDefined){
    myNumericValues <- dataToCheck[is.numeric(dataToCheck[,fieldToCheck]),]
    myNumericValues <- myNumericValues[!is.na(myNumericValues[,fieldToCheck]),]
    
    # Range checks are only relvent for numeric values
    if (nrow(myNumericValues)>0){
      
      # If min value check
      if(!is.na(fieldTypeToCheck$minValue)){
        minValueNumber <- as.numeric(fieldTypeToCheck$minValue)
        valueTooSmall <- myNumericValues[myNumericValues[,fieldToCheck] < minValueNumber,]
        # If we have soem values that are too small - log an error
        if (nrow(valueTooSmall)){
          #Log the error
          someErrors <- logValidationError(errorList = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = valueTooSmall[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Min value check"
                                           ,problemDescription = paste("Value is below minimum allowed (",minValueNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", valueTooSmall[,1], " ;Column:",fieldToCheck, ";Unallowed value:",valueTooSmall[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      } 
      # Max value check
      if (!is.na(fieldTypeToCheck$maxValue)) {
        maxValueNumber <- as.numeric(fieldTypeToCheck$maxValue)
        valueTooBig <- myNumericValues[myNumericValues[,fieldToCheck] > maxValueNumber,]
        # If we have soem values that are too big - log an error
        if (nrow(valueTooBig)){
          #Log the error
          someErrors <- logValidationError(errorList = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = valueTooBig[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Max value check"
                                           ,problemDescription = paste("Value is above maximum allowed (",maxValueNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", valueTooBig[,1], " ;Column:",fieldToCheck, ";Unallowed value:",valueTooBig[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      }
    }
  }
  
  
  ## Now check the actual data type 
  # Ints
  if (myTypeToCheck == "xs:int"){
    # Check for any non integer values
    #myNonIntValues <- dataToCheck[!is.integer(dataToCheck[,fieldToCheck]),]
    # The above function only checks whether the value is stored as an integer, not whether it is an integer :-S so need to do this instead
    myNonIntValues <- dataToCheck[!as.numeric(dataToCheck[,fieldToCheck])%%1==0,]
    if (nrow(myNonIntValues)>0){
      #Log the error
      someErrors <- logValidationError(errorList = NULL
                                      ,tableName = substr(fieldToCheck,1,2)
                                      ,rowID = myNonIntValues[,1]
                                      ,fieldName = fieldToCheck
                                      ,problemType = "Data type check"
                                      ,problemDescription = paste("Data type problem (int);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myNonIntValues[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myNonIntValues[,fieldToCheck], sep = " "))
      errorsToReturn <- rbind(errorsToReturn,someErrors)
    }
    # Decimal
  } else if (myTypeToCheck == "xs:decimal"){
    # Check for any non numeric values
    myNonDecValues <- dataToCheck[!is.numeric(dataToCheck[,fieldToCheck]),]
    if (nrow(myNonDecValues)>0){
      #Log the error
      someErrors <- logValidationError(errorList = NULL
                                      ,tableName = substr(fieldToCheck,1,2)
                                      ,rowID = myNonDecValues[,1]
                                      ,fieldName = fieldToCheck
                                      ,problemType = "Data type check"
                                      ,problemDescription = paste("Data type problem (decimal);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myNonDecValues[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myNonDecValues[,fieldToCheck], sep = " "))
      errorsToReturn <- rbind(errorsToReturn,someErrors)
    }
    # Now see if we need to do any simpleTypeChecks on our decimal (e.g. precision)
    if (simpleTypeCheckDefined){
      
      # If Precision check
      if(!is.na(fieldTypeToCheck$fractionDigits)){

        # Convert values to strings and see which numbers have decimal places
        myDecValues <- dataToCheck[is.numeric(dataToCheck[,fieldToCheck]),]
        dataWithDPs <- myDecValues[regexpr('.', as.character(myDecValues[,fieldToCheck]), fixed = TRUE) > 0,]
        # See how many characters are found after the decimal place
        if (nrow(dataWithDPs)>0){
          dataWithDPs$numberOfDps <- nchar(substr(as.character(dataWithDPs[,fieldToCheck]), regexpr('.', as.character(dataWithDPs[,fieldToCheck]), fixed = TRUE) + 1, nchar(as.character(dataWithDPs[,fieldToCheck]))))
          dataWithDPs <- dataWithDPs[!is.na(dataWithDPs$numberOfDps),]
          dataWithTooManyDps <- dataWithDPs[dataWithDPs$numberOfDps > as.numeric(fieldTypeToCheck$fractionDigits),]
          # If we have too many numbers after the decimal place
          if (nrow(dataWithTooManyDps)>0){
            #Log the error
            someErrors <- logValidationError(errorList = NULL
                                            ,tableName = substr(fieldToCheck,1,2)
                                            ,rowID = dataWithTooManyDps[,1]
                                            ,fieldName = fieldToCheck
                                            ,problemType = "Precision check"
                                            ,problemDescription = paste("Decimal precision problem (",fieldTypeToCheck$fractionDigits,"decimal places allowed);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", dataWithTooManyDps[,1], " ;Column:",fieldToCheck, ";Unallowed value:",dataWithTooManyDps[,fieldToCheck], sep = " "))
            errorsToReturn <- rbind(errorsToReturn,someErrors)
          }
        }
      }
    }  

      
    # String
  } else if (myTypeToCheck == "xs:string"){

    # We only need to check strings if there simpleTypeChecks defined
    if (simpleTypeCheckDefined){
      # Length check
      if (!is.na(fieldTypeToCheck$length)) {

        maxlengthNumber <- as.numeric(fieldTypeToCheck$length)
        myStringDataToCheck <- dataToCheck[!is.na(dataToCheck[,fieldToCheck]),]
        stringTooLong <- myStringDataToCheck[nchar(myStringDataToCheck[,fieldToCheck]) > maxlengthNumber,]
        # If we have soem strings that are too long 
        if (nrow(stringTooLong)){
          #Log the error
          someErrors <- logValidationError(errorList = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = stringTooLong[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "String length check"
                                           ,problemDescription = paste("String is longer than maximum allowed (",maxlengthNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", stringTooLong[,1], " ;Column:",fieldToCheck, ";Unallowed value:",stringTooLong[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      }
      
      # String Pattern check
      if (!is.na(fieldTypeToCheck$pattern)) {
        
        myStringDataToCheck <- dataToCheck[!is.na(dataToCheck[,fieldToCheck]),]
        # Check the field agaisnt the pattern using grepl
        patternNotMatched <- myStringDataToCheck[!grepl(fieldTypeToCheck$pattern,myStringDataToCheck[,fieldToCheck]),]
        
        # If we have soem strings that don't match the pattern 
        if (nrow(patternNotMatched)){
          #Log the error
          someErrors <- logValidationError(errorList = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = patternNotMatched[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "String pattern check"
                                           ,problemDescription = paste("String does no match required pattern (",fieldTypeToCheck$pattern,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", patternNotMatched[,1], " ;Column:",fieldToCheck, ";Unallowed value:",patternNotMatched[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
        
      }
        
    }
  }
  
  errorsToReturn
  
}


#' validateAgainstCodeList Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#' @param codeLists 
#'
#' @return
#'
#' @examples
validateAgainstCodeList <- function(fieldToCheck,dataToCheck,fieldTypeToCheck,codeLists){

  errorsToReturn <- NULL
  
  myTypeToCheck <- fieldTypeToCheck$type
  
  # see if we can find the correct code list
  myAllowedValues <- codeLists[codeLists$listName == myTypeToCheck,"allowedValues"]
  # If we found which values are allowed then we can check our data against them
  if (length(myAllowedValues)>0){
    # Check if our values are in the allowed list of values
    myResults <- dataToCheck[!dataToCheck[,fieldToCheck] %in% myAllowedValues,]
    # If we have soem values that aren't in the allowed list flag them as errors
    if (nrow(myResults)>0){
      #Log the error
      errorsToReturn <- logValidationError(errorList = NULL
                                      ,tableName = substr(fieldToCheck,1,2)
                                      ,rowID = myResults[,1]
                                      ,fieldName = fieldToCheck
                                      ,problemType = "Code list problem"
                                      ,problemDescription = paste("Code list problem;",paste(substr(fieldToCheck,1,2),"id",sep=""),":", myResults[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myResults[,fieldToCheck], ";Code list name:",myTypeToCheck, sep = " "))
    }
    # ELSE if we didn't find a list of allowed values then log that as an error
  } else {
    #Log the error
    errorsToReturn <- logValidationError(errorList = NULL
                                    ,tableName = substr(fieldToCheck,1,2)
                                    ,rowID = NA
                                    ,fieldName = fieldToCheck
                                    ,problemType = "Missing code list"
                                    ,problemDescription = paste("Could not find code list", myTypeToCheck, " for ", fieldToCheck, sep = " "))
  } # ENDIF find allowed values
  
  errorsToReturn
  
}




#' removeInvalidRows
#'
#' @param tableName The name of the table e.g. 'DE'
#' @param dataToClean The data frame we wish to clean
#' @param errorList The list of errors produced by the validation function
#'
#' @return
#' @export
#'
#' @examples
removeInvalidRows <- function(tableName,dataToClean,errorList ){
  
  # Remove any invalid rows 
  invalidRows <- unique(errorList[errorList$tableName == tableName & !is.na(errorList$rowID),"rowID"])
  # Assume the first column is always the ID field
  dataToClean <- dataToClean[!dataToClean[,1] %in% invalidRows,]
  
  dataToClean
}

#' Load the reference data we need for validation
#'
#' @param downloadFromICES Set to TRUE if you wish to download the latest vocabulary data from ICES, set to FALSE if you just want to use a local file.  When you download the data from ICES it is also saved locally.
#'
#' @return
#' @export
#'
#' @examples
loadReferenceData <- function(downloadFromICES = TRUE, validationData = NULL)
{
  
  if (downloadFromICES){
  
    # Download from the ICES vocabulary server
    codeListsToRefresh <- unique(validationData[grep('xs:*', validationData$type, invert = TRUE),'type'])
    codeListsToRefresh <- sub('.', '', codeListsToRefresh)
    allowedValues <- refreshReferenceDataFromICES(codeListsToRefresh)
    
    # save to file so we don't need to download from ICES every time
    saveRDS(allowedValues, file="referenceData/allowedValues.RDS")
  
  } else {
    # Just load from file
    allowedValues <- readRDS(file="./referenceData/allowedValues.RDS")
  }
  
  allowedValues
}


#' refreshReferenceDataFromICES Downloads the required reference data from ICES
#'
#' @return
#' @export
#'
#' @examples
refreshReferenceDataFromICES <- function(codeListsToRefresh){
  
  #codeListsToRefresh <- unique(validationData[grep('xs:*', validationData$type, invert = TRUE),'type'])
  # We want to remove the first 't' from the list names
  #View(codeListsToRefresh)
  #codeListsToRefresh <- sub('.', '', codeListsToRefresh)
  
  
  # extracts list types from ICES vocabulary server
  codeTypes <- getCodeTypeList()
  
  # pick out the list types that we need
  target_codeTypes <- codeTypes[codeTypes$Key %in% codeListsToRefresh,'Key']
  
  # Create an empty list
  codeLists<-sapply(target_codeTypes,function(x) NULL)
  
  # Get all the reference data we need
  for (i in target_codeTypes){
    print(i)
    codeLists[[i]]<-getCodeList(i)
    codeLists[[i]]$listName <- paste("t", i, sep = "")
    codeLists[[i]]$fileName <- "icesVocab"
    codeLists[[i]]$allowedValues <- codeLists[[i]]$Key
  }
  
  # Put the list entries into a single data frame
  allowedValues <- do.call("rbind", codeLists)
  
  allowedValues
  
}





