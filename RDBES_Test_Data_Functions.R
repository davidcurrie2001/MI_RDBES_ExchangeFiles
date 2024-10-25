

#' createTestData
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#' @param RequiredTables 
#' @param NumberOfStrata 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethods 
#'
#' @return
#' @export
#'
#' @examples
createTestData <- function(HierarchyToGenerate,LowerHierarchyToGenerate, RDBESvalidationdata, RDBEScodeLists, RequiredTables, NumberOfStrata, NumberSampled, NumberTotal, SelectionMethods ){
  
  # FOr testing
  #HierarchyToGenerate <-  'H5'
  #LowerHierarchyToGenerate <- 'A'
  #RDBESvalidationdata <- validationData
  #RDBEScodeLists <- allowedValues
  #RequiredTables <- allRequiredTables
  #NumberOfStrata <- list(DE = 3, VS = 3, FT = 3, OS = 3, LO = 1, TE = 1)
  #NumberSampled <- list(VS=3,FO=3,SS=1,SA=2, FM=10,BV=2, VD=10, SL=20, OS = 3, TE = 3, LO = 3, FT=3, CE=100, CL = 500)
  #NumberTotal <- list(VS=30,FO=10,SS=4,FM=10,BV=2, OS = 100, LO = 100, TE = 10, FT = 100)
  #SelectionMethods <- list()
  
  myRequiredTables <- RequiredTables[[HierarchyToGenerate]]
  myAuxTables <- c('VD','SL')
  myPopTables <- c('CE','CL')
  
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
  
  # First create a list with all the empty tables we need in it (the CS hierarchy tables and VD, SL)
  myDataList <- list()
  
  for (myTable in c(myRequiredTables,myAuxTables,myPopTables)){
    myEmptyDf <- createEmptyDataFrameFromValidationData(nameOfTable = myTable, RDBESvalidationdata = RDBESvalidationdata)
    # Add our empty XXid column to the front of the data frame
    myID <- integer(0)
    # Create the record type column
    myRecType <- character(0)
    myEmptyDf <- cbind(myID,myRecType,myEmptyDf)
    # Name our new columns correctly
    names(myEmptyDf)[1] <- paste(myTable,'id',sep = "")
    names(myEmptyDf)[2] <- paste(myTable,'recordType',sep = "")
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
  
  # Just add the auxillary tables into the output without doing anything to them
  for (myTable in myAuxTables){
    myMultipliedTestData[[myTable]] <- myDataList[[myTable]]
  }
  
  # Just add the population tables into the output without doing anything to them
  for (myTable in myPopTables){
    myMultipliedTestData[[myTable]] <- myDataList[[myTable]]
  }
  
  # Ok we now have soem fairly meaningless random data - lets try and make it a bit more useful
  myMultipliedTestData
  
  
}


#' createNewTestDataFrame
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param TableType 
#' @param DataFrameToUpdate 
#' @param NumberOfStrata 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethod 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
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

#' createNewTestDataRow
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param TableType 
#' @param RowID 
#' @param ColumnNames 
#' @param StratumName 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethod 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
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
      # SPECIAL COLUMN - SAlowerHierarchy
    } else if (myColName == 'SAlowerHierarchy') {
      myNewValue <- LowerHierarchyToGenerate
      # SPECIAL COLUMN - BVfishID
    } else if (myColName == 'BVnationalUniqueFishId') {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXid
    } else if (myColName == paste(TableType,'id',sep="")) {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXrecordType
    } else if (myColName == paste(TableType,'recordType',sep="")) {
      myNewValue <- TableType
      # SPECIAL COLUMN - XXsequenceNumber
    } else if (grepl('^..sequenceNumber$',myColName)) {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXunitName
    } else if (grepl('^..unitName$',myColName)) {
      myNewValue <- paste(TableType,'_unit_', RowID,sep = "")
      # SPECIAL COLUMN - XXstratification
    } else if (grepl('^..stratification$',myColName)) {
      if (StratumName =='U'){
        #myNewValue <-if_else(myColName == 'BVstratification','N','No')
        myNewValue <- 'N'
      } else {
        #myNewValue <-if_else(myColName == 'BVstratification','Y','Yes')
        myNewValue <- 'Y'
      }
      # SPECIAL COLUMN - XXstratumName
    } else if (grepl('^..stratumName$',myColName)) {
      myNewValue <- StratumName
      # Need to modify DEstratumName to stop overwrites
      if (myColName == 'DEstratumName') {
        myNewValue <- paste0(myNewValue,"_",HierarchyToGenerate)
      }
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
      myNewValue <- 'N'
      # SPECIAL COLUMN - XXclusterName
    } else if (grepl('^..clusterName$',myColName)) {
      myNewValue <- 'U'
      # SPECIAL COLUMN - XXgsaSubarea
    } else if (grepl('^..gsaSubarea$',myColName)) {
      myNewValue <- 'NotApplicable'
      # SPECIAL COLUMN - XXArea
    } else if (grepl('^..area$',myColName)) {
      #myNewValue <- '27'
      myAreas <- c('27.7.a', '27.7.b', '27.7.d', '27.7.e', '27.7.f', '27.7.g', '27.7.h', '27.1', '27.2', '27.4', '27.6', '27.7', '27.7.c', '27.7.j', '27.7.k', '27.8', '27.9')
      myNewValue <- sample(myAreas,1)
      
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
            firstEntry <- RDBEScodeLists[RDBEScodeLists$listName == requiredCodeListName & RDBEScodeLists$Deprecated == FALSE  ,'Key' ][1]
            myNewValue <- firstEntry
            # SIMPLETYPECHECK
          } else if (!is.na(myValidationInfo$description) & myValidationInfo$description == 'simpleTypeCheck'){
            # simpleTypeCheck - string with pattern
            if (!is.na(myValidationInfo$pattern)){
              # string with pattern - I assume this can only be dates or times - might not hold true in the future
              if (myValidationInfo$checkName == 'tDate'){
                myNewValue <- '1999-01-01'
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
            } else if (myValidationInfo$checkName == "tStringLength100"){
              myNewValue <- "123456789"
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


#' makeTestDataMoreRealistic
#'
#' @param DataToUse 
#' @param CountryToUse 
#' @param YearToUse 
#' @param MetierList 
#' @param SpeciesList 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
makeTestDataMoreRealistic <- function(DataToUse,CountryToUse,YearToUse,MetierList,SpeciesList,RDBEScodeLists, catchFractionToUse = 'Lan', landingCategoryToUse = 'HuC'){
  
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
  #myVesselLengths <- RDBEScodeLists[RDBEScodeLists$listName == 'tVesselLengthClass' & RDBEScodeLists$Deprecated == FALSE ,'Key']
  #DataToUse[['VD']][,'VDlengthCategory'] <- sample(myVesselLengths,nrow(DataToUse[['VD']]),replace = TRUE)
  DataToUse[['VD']][,'VDlengthCategory'] <- 'VL1518'
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
  DataToUse[['SL']][,'SLcatchFraction'] <- catchFractionToUse
  
  # CS TABLES
  
  # Sampling scheme name 
  DataToUse[['DE']]$DEsamplingScheme <- 'National Routine'
  
  # COUNTRY
  # Sort out the country and location code fields
  DataToUse[['SD']][,'SDcountry'] <- CountryToUse
  if ('LE' %in% names(DataToUse)){
    DataToUse[['LE']][,'LEcountry'] <- CountryToUse
  }
  
  # INSTITUTE
  DataToUse[['SD']][,'SDinstitution'] <- 1051
  DataToUse[['SL']][,'SLinstitute'] <- 1051
  
  
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
  if ('CE' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['CE']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['CE']][,'CElandingLocation'] <- myRandomValues
  }
  if ('CL' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['CL']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['CL']][,'CLlandingLocation'] <- myRandomValues
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
    DataToUse[['OS']][,'OSsamplingDate'] <- format(myRandomValues,'%Y-%m-%d')
    DataToUse[['OS']][,'OSlocationName'] <- paste(DataToUse[['OS']][,'OSlocationName'],DataToUse[['OS']][,'OSid'],sep="")
  }
  if ('LE' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['LE']]),replace = TRUE)
    DataToUse[['LE']][,'LEdate'] <- format(myRandomValues,'%Y-%m-%d')
  }
  
  # SPECIES LIST NAME
  DataToUse[['SS']][,'SSspeciesListName'] <- paste(CountryToUse,'_',YearToUse,'_SpeciesList',sep="")
  DataToUse[['SS']][,'SScatchFraction'] <- catchFractionToUse
  
  
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
    if ('VS' %in% names(DataToUse) & 'VSid' %in% names(DataToUse[['FT']])){
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
    
    DataToUse[['FT']][,'FTsequenceNumber'] <- DataToUse[['FT']][,'FTid']
    
  }
  
  # If we have LE data it can be directly linked to either a VS or FT record - need to use the same vessel code if it is - else pick a random one
  if ('LE' %in% names(DataToUse)){
    
    # Clear out any existing values first
    DataToUse[['LE']][,'LEencryptedVesselCode'] <- NA
    
    # If we have a VS table then we'll use the vessel code form that record in the LE record
    if ('VS' %in% names(DataToUse) & 'VSid' %in% names(DataToUse[['LE']])  ){
      VSvessel <- DataToUse[['VS']][,c('VSid','VSencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['LE']],VSvessel, by ='VSid')
      DataToUse[['LE']][,'LEencryptedVesselCode'] <- myJoin[,'VSencryptedVesselCode']
    } 
    
    # If we have a FT table then we'll use the vessel code form that record in the LE record
    if ('FT' %in% names(DataToUse) & 'FTid' %in% names(DataToUse[['LE']])){
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
    myMetiersCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tMetier6_FishingActivity' & RDBEScodeLists$Deprected == FALSE ,'Key']
    # Pick the maximum number of metiers for our list based on whatever is the biggest number from i) the number of SA records, ii) the number 1 (to cover the very rare case when we don't have any samples)
    numerOfMetiersToUse <- max(nrow(DataToUse[['SA']]), 1 )
    myGearCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tGearType' & RDBEScodeLists$Deprecate == FALSE,'Key']
    # Only consider gear codes that are 3 charcters long - makes life easier
    myGearCodes <- myGearCodes[sapply(myGearCodes,FUN = nchar)==3]
    # Only allow metiers where the first part is also in the allowed gear code list
    myMetiersCodes <- myMetiersCodes[grepl(paste(myGearCodes,collapse="|"),myMetiersCodes)]
    
    #myMetiersCodes <- myMetiersCodes[1:numerOfMetiersToUse]
    # We can have more SA rows than the number of metiers - let's sample it instead
    myMetiersCodes <-sample(myMetiersCodes, size=numerOfMetiersToUse, replace =TRUE)
    
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
    
    DataToUse[['LE']][,'LEsequenceNumber'] <- DataToUse[['LE']][,'LEid']
    
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
    
    # Ensure SAsequence number is unique
    DataToUse[['SA']][,'SAsequenceNumber'] <- DataToUse[['SA']][,'SAid']
    
    # Sort out catch and landing categories
    DataToUse[['SA']][,'SAcatchCategory'] <- catchFractionToUse
    DataToUse[['SA']][,'SAlandingCategory'] <- landingCategoryToUse
    
  }
  
  # Temporal
  if ('TE' %in% names(DataToUse)){
    DataToUse[['TE']][,'TEsequenceNumber'] <- DataToUse[['TE']][,'TEid']
    
  }
  
  # FREQUENCY MEASURE
  
  if ('FM' %in% names(DataToUse)){
    
    # For each species sampled we'll randomly pick a minimum length and then create soem normal length data in the FM data
    speciesSampled <- unique(DataToUse[['SA']][,'SAspeciesCode'])
    minLength <- data.frame( SAspeciesCode = speciesSampled, minLength = sample(10:40,length(speciesSampled), replace = TRUE), stringsAsFactors = FALSE)
    myJoin <- inner_join(DataToUse[['SA']],minLength, by ='SAspeciesCode')
    myJoin2 <- inner_join(DataToUse[['FM']],myJoin,by='SAid')
    # Set all the values of FMclass to be the minium length to start with - we'll change this in a bit
    DataToUse[['FM']][,'FMclassMeasured'] <- myJoin2[,'minLength'] * 10
    DataToUse[['FM']][,'FMtypeMeasured'] <- 'LengthTotal'
    DataToUse[['FM']][,'FMtypeAssessment'] <- 'LengthTotal'
    
    
    # For each sample, create increasing values for FMclass, with a normal distribution of fish counts
    # TODO - shoudl be a better way to do this
    for (mySAid in unique(DataToUse[['FM']][,'SAid'])){
      
      # The FM data associated with this SAid
      myFMDataForASample <- DataToUse[['FM']][DataToUse[['FM']][,'SAid'] == mySAid,]
      
      # First we'll generate soem nromal data that we will use for the number of fish at each length
      myNumberOfLengthClasses <- nrow(myFMDataForASample)
      myMinLengthClass <- min(myFMDataForASample[,'FMclassMeasured'])
      # Set a standard deviation
      mySD <- 5
      # Assume the mean is half way along the number of length classes
      myMean <- myMinLengthClass + (myNumberOfLengthClasses/2)
      # Generate a normal distribution of lengths
      myLengthSequence <- seq(myMinLengthClass, myMinLengthClass + myNumberOfLengthClasses, by = 1)
      myNormalFishCounts <- dnorm(myLengthSequence, mean = myMean, sd = mySD)
      # Let's fix the max number of fish at a length class as 10 and scale everything accordingly
      myMultiplyFactor <- 10/max(myNormalFishCounts)
      myNormalFishCounts <- round(myNormalFishCounts*myMultiplyFactor)
      
      # Now lets generate our lengths and the number of fish at each length
      currentLength <- NA
      FMCount <- 0
      for (myFMid in myFMDataForASample[,'FMid']){
        FMCount <- FMCount + 1
        if (is.na(currentLength)){
          currentLength <- myFMDataForASample[myFMDataForASample[,'FMid']==myFMid,'FMclassMeasured']
        } else {
          # Add 10mm to previous length
          currentLength <- currentLength + 10
        }
        myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMclassMeasured'] <- currentLength
        # Make a random number of fish at that length
        #myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMnumberAtUnit'] <- sample(1:10,1)
        # Use our nomral distribution of fish counts
        myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMnumberAtUnit'] <- myNormalFishCounts[FMCount]
        
      }
      DataToUse[['FM']][DataToUse[['FM']][,'SAid'] == mySAid,] <- myFMDataForASample
    }
  }
  
  # BIOLOGICAL VARIABLES
  if ('BV' %in% names(DataToUse)){
    
    # Sort out BVfishId so that we don't have duplicates
    DataToUse[['BV']][,'BVnationalUniqueFishId'] <- DataToUse[['BV']][,'BVid']
    
    # Ages
    myBVages <- DataToUse[['BV']][DataToUse[['BV']][,'BVtypeMeasured'] == 'Age',]
    
    # If we have soem age data
    if (nrow(myBVages)>0){
      
      # Set the ages to NA to start with 
      DataToUse[['BV']][DataToUse[['BV']][,'BVtypeMeasured'] == 'Age','BVvalueMeasured'] <- NA
      
      # If the BV is linked to a length class use that to generate an age
      if( 'FMid' %in% names(myBVages)){
        myJoin <- inner_join(myBVages,DataToUse[['FM']][,c('FMid','FMclassMeasured')], by ='FMid')
        if (nrow(myJoin)>0){
          # Assume the age is length / 50
          myJoin[,'BVvalueMeasured'] <- round(myJoin[,'FMclassMeasured']/50.0)
          # Update the age values in our data
          # TODO - this is not a good way of doing things in R
          for(myBV in myJoin[,'BVid'] ){
            DataToUse[['BV']][DataToUse[['BV']][,'BVid'] == myBV,'BVvalueMeasured'] <- myJoin[myJoin[,'BVid']==myBV,'BVvalueMeasured']
          }
          
        }
        
      } 
      
      # For any remaining NAs we'll pick a random age
      DataToUse[['BV']][DataToUse[['BV']][,'BVtypeMeasured'] == 'Age' & is.na(DataToUse[['BV']][,'BVvalueMeasured']),'BVvalueMeasured'] <- as.character(sample(1:10, nrow(myBVages),replace = TRUE))
      
      # Set the unit as Year
      DataToUse[['BV']][DataToUse[['BV']][,'BVtypeMeasured'] == 'Age','BVvalueUnitOrScale'] <- 'Ageyear'
    }
  }
  
  
  # CE
  if ('CE' %in% names(DataToUse)){
    DataToUse[['CE']][,'CEyear'] <- YearToUse
    DataToUse[['CE']][,'CEvesselFlagCountry'] <- CountryToUse
    DataToUse[['CE']][,'CEnumberOfUniqueVessels'] <- sample.int(20, size = nrow(DataToUse[['CE']]),  replace =TRUE ) + 3
    DataToUse[['CE']][,'CEnumberOfDominantTrips'] <- sample.int(50, size = nrow(DataToUse[['CE']]),  replace =TRUE )
    DataToUse[['CE']][,'CEnumberOfFractionalTrips'] <-  DataToUse[['CE']][,'CEnumberOfDominantTrips']
    DataToUse[['CE']][,'CEofficialFishingDays'] <- sample.int(50, size = nrow(DataToUse[['CE']]),  replace =TRUE )
    DataToUse[['CE']][,'CEscientificFishingDays'] <- DataToUse[['CE']][,'CEofficialFishingDays']
    DataToUse[['CE']][,'CEofficialDaysAtSea'] <- DataToUse[['CE']][,'CEofficialFishingDays']
    DataToUse[['CE']][,'CEscientificDaysAtSea'] <- DataToUse[['CE']][,'CEofficialFishingDays']
    
    DataToUse[['CE']][,'CEofficialkWDaysAtSea'] <- DataToUse[['CE']][,'CEofficialDaysAtSea'] * 100.0
    DataToUse[['CE']][,'CEscientifickWDaysAtSea'] <-  DataToUse[['CE']][,'CEofficialkWDaysAtSea']
    
    DataToUse[['CE']][,'CEofficialkWFishingDays'] <- DataToUse[['CE']][,'CEofficialFishingDays'] * 100.0
    DataToUse[['CE']][,'CEscientifickWFishingDays'] <-  DataToUse[['CE']][,'CEofficialkWFishingDays']
    
    DataToUse[['CE']][,'CEgTDaysAtSea'] <- DataToUse[['CE']][,'CEofficialDaysAtSea'] * 20.0
    DataToUse[['CE']][,'CEgTFishingDays'] <- DataToUse[['CE']][,'CEofficialFishingDays'] * 20.0
    DataToUse[['CE']][,'CEfishingAreaCategory'] <- 'MO'
    DataToUse[['CE']][,'CEfreshWaterName'] <- 'NA'
    DataToUse[['CE']][,'CEencryptedVesselIds'] <- 'abc;def;ghi'
    
    #myAreas <- RDBEScodeLists[RDBEScodeLists$listName == 'tICES_Area' & grepl('^27.*',RDBEScodeLists$Key) & RDBEScodeLists$Deprecated == FALSE,'Key']
    myAreas <- c('27.7.a', '27.7.b', '27.7.d', '27.7.e', '27.7.f', '27.7.g', '27.7.h', '27.1', '27.2', '27.4', '27.6', '27.7', '27.7.c', '27.7.j', '27.7.k', '27.8', '27.9')
    myRandomValues <- sample(myAreas,nrow(DataToUse[['CE']]),replace = TRUE)
    DataToUse[['CE']][,'CEarea'] <- myRandomValues
    
    # Find which stat rectangles are allowe din the RDBES
    allowedStatRects <- statRects[statRects$ICESNAME %in% RDBEScodeLists[RDBEScodeLists$listName == "tStatRec" & RDBEScodeLists$Deprecated == FALSE, "allowedValues"],]
    
    # Pick a random rectangle from the correct area ( using a for loop is 
    # not a very R way of doing this through :-(  )
    for (myRow in 1:nrow(DataToUse[['CE']])){
      #DataToUse[['CE']][myRow,'CEstatisticalRectangle'] <-   
      #  sample(c('-9',statRects[statRects$Area == DataToUse[['CE']][myRow,'CEarea'],'ICESNAME']),1)
      DataToUse[['CE']][myRow,'CEstatisticalRectangle'] <-   
        sample(c('-9',allowedStatRects[allowedStatRects$Area == DataToUse[['CE']][myRow,'CEarea'],'ICESNAME']),1)
    }
    
    #myMetiersCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tMetier6_FishingActivity' & RDBEScodeLists$Deprecated == FALSE,'Key']
    myMetiersCodes <- c('DRB_MOL_>0_0_0', 'FPO_CRU_>0_0_0', 'FPO_MOL_>0_0_0', 'LHP_DEF_0_0_0', 'LHP_SPF_0_0_0', 'LLS_FIF_0_0_0', 'MIS_MIS_0_0_0', 'OTB_DEF_70-99_0_0', 'OTM_LPF_>0_0_0', 'PTM_SPF_>0_0_0')
    myRandomValues <-sample(myMetiersCodes, nrow(DataToUse[['CE']]), replace =TRUE)
    DataToUse[['CE']][,'CEmetier6'] <- myRandomValues
    
    #myLengthCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tVesselLengthClass' & RDBEScodeLists$Deprecated == FALSE,'Key']
    #myRandomValues <-sample(myLengthCodes, nrow(DataToUse[['CE']]), replace =TRUE)
    #DataToUse[['CE']][,'CEvesselLengthCategory'] <- myRandomValues
    DataToUse[['CE']][,'CEvesselLengthCategory'] <- 'VL1518'
    
    #Dedupe CE 
    DataToUse[['CE']] <- distinct(DataToUse[['CE']], CEdataTypeOfScientificEffort, CEdataSourceOfScientificEffort, CEsamplingScheme, CEvesselFlagCountry, CEyear, CEquarter, CEmonth, CEarea, CEstatisticalRectangle, CEdataSourceOfStatisticalRectangle, CEgsaSubarea, CEjurisdictionArea, CEfishingAreaCategory, CEfreshWaterName, CEexclusiveEconomicZone, CEnationalFishingActivity, CEmetier6, CEincidentalByCatchMitigationDevice, CElandingLocation, CEvesselLengthCategory, CEfishingTechnique, CEdeepSeaRegulation, .keep_all= TRUE)
    
  }
  
  # CL
  if ('CL' %in% names(DataToUse)){
    DataToUse[['CL']][,'CLyear'] <- YearToUse
    DataToUse[['CL']][,'CLvesselFlagCountry'] <- CountryToUse
    DataToUse[['CL']][,'CLlandingCountry'] <- CountryToUse
    DataToUse[['CL']][,'CLnumberOfUniqueVessels'] <- sample.int(20, size = nrow(DataToUse[['CL']]), replace =TRUE ) + 3
    DataToUse[['CL']][,'CLcatchCategory'] <- 'Lan'
    DataToUse[['CL']][,'CLofficialWeight'] <- sample.int(50, size = nrow(DataToUse[['CL']]), replace =TRUE ) * 1000
    DataToUse[['CL']][,'CLscientificWeight'] <- DataToUse[['CL']][,'CLofficialWeight']
    DataToUse[['CL']][,'CLlandingsValue'] <- DataToUse[['CL']][,'CLofficialWeight'] * 2
    DataToUse[['CL']][,'CLexplainDifference'] <- 'NoDiffAssumed'
    DataToUse[['CL']][,'CLfishingAreaCategory'] <- 'MO'
    DataToUse[['CL']][,'CLfreshWaterName'] <- 'NA'
    DataToUse[['CL']][,'CLencryptedVesselIds'] <- 'abc;def;ghi'
    
    
    #myAreas <- RDBEScodeLists[RDBEScodeLists$listName == 'tICES_Area' & grepl('^27.*',RDBEScodeLists$Key) & RDBEScodeLists$Deprecated == FALSE,'Key']
    myAreas <- c('27.7.a', '27.7.b', '27.7.d', '27.7.e', '27.7.f', '27.7.g', '27.7.h', '27.1', '27.2', '27.4', '27.6', '27.7', '27.7.c', '27.7.j', '27.7.k', '27.8', '27.9')
    myRandomValues <- sample(myAreas,nrow(DataToUse[['CL']]),replace = TRUE)
    DataToUse[['CL']][,'CLarea'] <- myRandomValues
    
    # Pick a random rectangle from the correct area ( using a for loop is 
    # not a very R way of doing this through :-(  )
    for (myRow in 1:nrow(DataToUse[['CL']])){
      #DataToUse[['CL']][myRow,'CLstatisticalRectangle'] <-   
      #sample(c('-9',statRects[statRects$Area == DataToUse[['CL']][myRow,'CLarea'],'ICESNAME']),1)
      DataToUse[['CL']][myRow,'CLstatisticalRectangle'] <-   
        sample(c('-9',allowedStatRects[allowedStatRects$Area == DataToUse[['CL']][myRow,'CLarea'],'ICESNAME']),1)
    }
    
    #mySpecies <- RDBEScodeLists[RDBEScodeLists$listName == 'tSpecWoRMS','Key']
    #myRandomValues <- sample(mySpecies,nrow(DataToUse[['CL']]),replace = TRUE)
    #DataToUse[['CL']][,'CLspeciesCode'] <- myRandomValues
    
    mySpeciesCodes <- DataToUse[['SL']][,'SLspeciesCode']
    if (length(mySpeciesCodes) > 1){
      myRandomValues <- sample(mySpeciesCodes,nrow(DataToUse[['CL']]),replace = TRUE)
    } else {
      myRandomValues <- mySpeciesCodes
    }
    DataToUse[['CL']][,'CLspeciesCode'] <- myRandomValues
    
    #myMetiersCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tMetier6_FishingActivity' & RDBEScodeLists$Deprecated == FALSE,'Key']
    myMetiersCodes <- c('DRB_MOL_>0_0_0', 'FPO_CRU_>0_0_0', 'FPO_MOL_>0_0_0', 'LHP_DEF_0_0_0', 'LHP_SPF_0_0_0', 'LLS_FIF_0_0_0', 'MIS_MIS_0_0_0', 'OTB_DEF_70-99_0_0', 'OTM_LPF_>0_0_0', 'PTM_SPF_>0_0_0')
    myRandomValues <-sample(myMetiersCodes, nrow(DataToUse[['CL']]), replace =TRUE)
    DataToUse[['CL']][,'CLmetier6'] <- myRandomValues
    
    #myLengthCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tVesselLengthClass' & RDBEScodeLists$Deprecated == FALSE,'Key']
    #myRandomValues <-sample(myLengthCodes, nrow(DataToUse[['CL']]), replace =TRUE)
    #DataToUse[['CL']][,'CLvesselLengthCategory'] <- myRandomValues
    DataToUse[['CL']][,'CLvesselLengthCategory'] <- 'VL1518'
    
    #Dedupe CL 
    DataToUse[['CL']] <- distinct(DataToUse[['CL']], CLdataTypeOfScientificWeight, CLdataSourceOfScientificWeight, CLsamplingScheme, CLdataSourceLandingsValue, CLlandingCountry, CLvesselFlagCountry, CLyear, CLquarter, CLmonth, CLarea, CLstatisticalRectangle, CLdataSourceOfStatisticalRectangle, CLgsaSubarea, CLjurisdictionArea, CLfishingAreaCategory, CLfreshWaterName, CLexclusiveEconomicZone, CLspeciesCode, CLspeciesFaoCode, CLlandingCategory, CLcatchCategory, CLregDisCategory, CLcommercialSizeCategoryScale, CLcommercialSizeCategory, CLnationalFishingActivity, CLmetier6, CLincidentalByCatchMitigationDevice, CLlandingLocation, CLvesselLengthCategory, CLfishingTechnique, CLdeepSeaRegulation, .keep_all= TRUE)
    
  }
  
  # Return our data
  DataToUse
  
}