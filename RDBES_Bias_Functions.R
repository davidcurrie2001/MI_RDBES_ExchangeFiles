# Functions which highlight sources of potential bias in RDBES data

# Load our general RDBES functions
# - we'll use some of them in the bias functions
source("RDBES_Functions.R")

summariseSelectionMethods <- function(hierarchyToCheck,
                                      yearToUse,
                                      country,
                                      rdbesData,
                                      requiredTables) {
  
  # For testing
  # rdbesData <- myRDBESData
  # requiredTables <- allRequiredTables
  # yearToUse <- 2019
  # country <- 'IE'
  # hierarchyToCheck <- 'H1'
  
  
  warning("Please be aware that this function does not
          consider the clustered sampling variables yet")

  # Find which tables we need for this file type
  upperHierarchy <- substr(hierarchyToCheck, 2, nchar(hierarchyToCheck))
  myRequiredTables <- requiredTables[[hierarchyToCheck]]

  ## Step 1 - Filter the data
  myCSData <- filterCSData(RDBESdata = rdbesData
                           , RequiredTables = myRequiredTables
                           , YearToFilterBy = yearToUse
                           , CountryToFilterBy = country
                           , UpperHierarchyToFilterBy = upperHierarchy)
  
  # If myCSData is empty at this point just stop
  if (length(myCSData)==0){
    print("Error - the data to summarise is empty")
    stop("Could not summarise data -
         please validate your data to identify any problems")
  }
  
  # Lists to hold the outputs
  myData <- list()
  myDataGrouped <- list()
  myJoinedData <- NULL
  previousRequiredTable <- NULL
  
  # STEP 2 Process each table to get the data in the format we need
  # We won't worry about SS,SA,FM,BV at this point
  for (aRequiredTable in myRequiredTables[!myRequiredTables %in% c('SS','SA','FM','BV')]){
    
    #aRequiredTable <- 'DE'
    
    # Get the data we will work with
    myTempData <- myCSData[[aRequiredTable]]
    
    # Process the different tables
    #
    # We need particular fields for DE
    if (aRequiredTable == 'DE'){
      # Create a stratum name for DE
      myTempData$DEfullStratumName <- paste(myTempData[,'DEsamplingScheme'],myTempData[,'DEyear'],myTempData[,'DEstratumName'],sep = '_')

      #Save the data to our list (DE doesn't need to be grouped)
      myData[[aRequiredTable]] <- myTempData[,c('DEid','DEstratumName', 'DEfullStratumName','DEsamplingScheme', 'DEyear', 'DEhierarchyCorrect', 'DEhierarchy')]
      myDataGrouped[[aRequiredTable]] <- myData[[aRequiredTable]]
      
    # We need particualr fields for SD
    } else if (aRequiredTable == 'SD'){

      # Join SD to the previous table (DE in this case)
      previousHierarchyTable <- myData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      currentPrimaryKey <- names(myTempData)[1]
      myTempData <- inner_join(myTempData,previousHierarchyTable, by =previousPrimaryKey)
      
      # Rename the stratum from DE as parent stratum name
      names(myTempData)[names(myTempData) == 'DEfullStratumName'] <- 'SDparentFullStratumName'
      
      # Create a stratum name and full stratum names for SD
      myTempData$SDstratumName <- paste(myTempData[,'SDcountry'],myTempData[,'SDinstitution'],sep = '_')
      myTempData$SDfullStratumName <- paste(myTempData[,'SDparentFullStratumName'],myTempData[,'SDstratumName'],sep = ':')
      
      #Save the data to our list (SD doesn't need to be grouped)
      myData[[aRequiredTable]] <- myTempData[,c(currentPrimaryKey,previousPrimaryKey, 'SDparentFullStratumName','SDstratumName','SDfullStratumName', 'SDcountry', 'SDinstitution')]
      myDataGrouped[[aRequiredTable]] <- myData[[aRequiredTable]]
    
    } else {
      
      # Get the names of the fields we are interested in
      currentStratificationField <- paste(aRequiredTable,'stratification',sep ="")
      currentStratumNameField <- paste(aRequiredTable,'stratumName',sep ="")
      currentNumberSampledField <- paste(aRequiredTable,'numberSampled',sep ="")
      currentNumberTotalField <- paste(aRequiredTable,'numberTotal',sep ="")
      currentSelectionMethodField <- paste(aRequiredTable,'selectionMethod',sep ="")
      currentSampledField <- paste(aRequiredTable,'sampled',sep ="")
      currentFullStratumNameField <- paste(aRequiredTable,'fullStratumName',sep="")
      currentParentFullStratumName <- paste(aRequiredTable,'parentFullStratumName',sep="")
      
      # Join to the previous table 
      previousHierarchyTable <- myData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      currentPrimaryKey <- names(myTempData)[1]
      myTempData <- inner_join(myTempData,previousHierarchyTable, by =previousPrimaryKey)
      
      # Rename the fullStratumName from the previous table as parentFullStratumName
      names(myTempData)[names(myTempData) == paste(previousRequiredTable,'fullStratumName',sep="")] <- currentParentFullStratumName
      
      # Need to modify the stratum name values for unstratified data to include the parent xxID value (otherwise all 'U' records will look liek they are are from the same stratum)
      myTempData[myTempData[,currentStratificationField] == 'N',currentStratumNameField] <- paste(myTempData[myTempData[,currentStratificationField] == 'N',currentStratumNameField],"(",myTempData[myTempData[,currentStratificationField] == 'N',previousPrimaryKey],")",sep="")
      
      # Create the full stratum name field
      myTempData[,currentFullStratumNameField] <- paste(myTempData[,currentParentFullStratumName],myTempData[,currentStratumNameField],sep = ':')
      
      #Save the data to our list 
      myData[[aRequiredTable]] <- myTempData[,c(currentPrimaryKey,previousPrimaryKey, currentParentFullStratumName,currentStratumNameField,currentFullStratumNameField, currentStratificationField,currentNumberSampledField,currentNumberTotalField,currentSelectionMethodField, currentSampledField )]
      
      # Now we will group and summarise the data

      # Get the data we want to group
      myTempToGroup <- myData[[aRequiredTable]][,
                                          c(currentParentFullStratumName,
                                            currentStratumNameField,
                                            currentFullStratumNameField, 
                                            currentStratificationField,
                                            currentSelectionMethodField,
                                            currentSampledField,
                                            currentNumberSampledField,
                                            currentNumberTotalField)]
      
      # CHanges names so we can hard-code them below
      # - dplyr can't seem to cope with variables - grrr
      names(myTempToGroup) <- 
        substr(names(myTempToGroup), 3, nchar(names(myTempToGroup)))
      
      # Add fields to hold the calcualted number of things 
      # we did and didn't sample
      myTempToGroup$numberNotSampledCalc <- NA
      myTempToGroup$numberSampledCalc <- NA
      
      myTempToGroup[myTempToGroup$sampled == 'N' & 
                      !is.na(myTempToGroup$sampled),'numberNotSampledCalc'] <- 1 
      myTempToGroup[myTempToGroup$sampled == 'Y' &
                      !is.na(myTempToGroup$sampled),'numberSampledCalc'] <- 1 
      # Group and summarise
      myDataGrouped[[aRequiredTable]] <- myTempToGroup %>% 
        dplyr::group_by(parentFullStratumName,
                        stratumName,
                        fullStratumName,
                        stratification,
                        selectionMethod) %>% 
        dplyr::summarise(numberOfRows = n(),
                      numberNotSampledCalc = sum(!is.na(numberNotSampledCalc)),
                      numberSampledCalc = sum(!is.na(numberSampledCalc)))
      
      names(myDataGrouped[[aRequiredTable]]) <- paste(aRequiredTable,names(myDataGrouped[[aRequiredTable]]),sep='')
        
      


      
    }
    
    previousRequiredTable <- aRequiredTable
    
  }
  
  
  myJoinedData <- NULL
  previousRequiredTable <- NULL
  
  # Now join our grouped data together
  for (aRequiredTable in myRequiredTables[!myRequiredTables %in% c('SS','SA','FM','BV')]){
    
    if (aRequiredTable == 'DE'){
       myJoinedData <- myDataGrouped[['DE']]
    } else {
      previousTableJoinField <- paste(previousRequiredTable,'fullStratumName',sep="")
      currentTableJoinField <- paste(aRequiredTable,'parentFullStratumName',sep="")
      myJoinedData <- dplyr::inner_join(myJoinedData,myDataGrouped[[aRequiredTable]],by = setNames(currentTableJoinField, previousTableJoinField))
    }
    
    previousRequiredTable <- aRequiredTable
    
  }
  
  
  # Return values
  list(detailedData = myData,
       summaryData = myDataGrouped,
       joinedData = myJoinedData)
  
  
}
