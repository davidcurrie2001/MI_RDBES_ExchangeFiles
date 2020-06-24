library(RODBC)
library(dplyr)
library(data.table)
library(XML)
library(icesVocab)

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
  
  myLocodes <- sqlQuery(channel,"select * from dbo.PortLocodes", stringsAsFactors = FALSE)
  myAphiaIds <- sqlQuery(channel,"select * from dbo.SpeciesAphiaIDs", stringsAsFactors = FALSE)
  
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
                       ,Locodes = myLocodes
                       ,Aphiaids = myAphiaIds
  )
  
  return(myRDBESData)
  
}



#' generateCEFile Generate a CE exchange format file for the RDBS
#'
#' @param yearToUse The year we want to generate the CE file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfRows (Optional) Limit the output to this number of rows (just used for testing)
#'
#' @return
#' @export
#'
#' @examples generateCEFile(yearToUse = 2016, country = 'IRL', RDBESdata = myRDBESData)
generateCEFile <- function(yearToUse, country, RDBESdata, outputFileName = "", numberOfRows = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2017
  #country <- 'IE'
  #outputFileName <- ""
  #numberOfRows <- 50
  #cleanData <- TRUE
  #RDBESvalidationdata <- validationData
  #RDBEScodeLists <- allowedValues
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"HCE.csv", sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data and write it out
  
  CE <- RDBESdata[['CE']]
  
  # Filter the CE data by year
  ceFile <- CE[CE$CEyear == yearToUse & CE$CEvesselFlagCountry == country,]
  
  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    rowsBefore <- nrow(ceFile)
    print(paste(rowsBefore, ' CE rows before removing invalid data', sep =""))
    
    # Validate CL
    myErrors <- validateTables(RDBESdata = RDBESdata, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists, shortOutput = FALSE,framestoValidate = c("CE"))
    
    # UGLY FIX 
    # Get rid of the errors associates with CLgsaSubarea having a value of NotApplicable
    myErrors <- myErrors[!(myErrors$fieldName == 'CEgsaSubarea' & myErrors$problemType =='Code list problem' & grepl('NotApplicable', myErrors$problemDescription)),]
    
    # Remove any invalid rows 
    invalidRows <- unique(myErrors[myErrors$tableName == 'CE' & !is.na(myErrors$rowID),"rowID"])
    ceFile <- ceFile[!ceFile$CEid %in% invalidRows,]
    
    rowsAfter <- nrow(ceFile)
    print(paste(nrow(ceFile), ' CE rows after removing invalid data', sep =""))
    
    if (rowsAfter < rowsBefore){
      missingRows <- rowsBefore - rowsAfter
      warning(paste(missingRows,' invalid rows removed from CE before trying to generate output files', sep = ""))
    }
    
  }
  
  # If we only want a certain number of rows we'll subset the data (normally just used during testing)
  if (!is.null(numberOfRows) & nrow(ceFile) > numberOfRows){
    ceFile <- ceFile[1:numberOfRows,]
  }
  
  # We now write out the data frame with ids and column names included to make debugging easier
  fwrite(ceFile, paste(outputFolder, outputFileName,"_debug",sep="") ,row.names=F,col.names=T,quote=F)
  
  # Now we'll get rid of the CEid values because we won't want them in our final output file
  ceFile <- select(ceFile,-c(CEid))
  
  # Get all the values from ceFile and list them out
  ce <- do.call('paste',c(ceFile,sep=','))
  
  # replace NA with blanks
  ce <- gsub('NA','',ce)
  
  # UGLY FIX 
  # Where there is no GSA sub-area value then we shoudl use the value "NA" - however the previous line will change all "NA"'s into blanks :-S 
  # I needed to do soemthing a bit ugly so that we can submit NA for the actual GSA sub-area value
  ce <- gsub('NotApplicable','NA',ce)
  
  # Write out the file
  fwrite(list(ce), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  
}


#' generateCLFile Generate a CL exchange format file for the RDBS
#'
#' @param yearToUse The year we want to generate the CL file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfRows (Optional) Limit the output to this number of rows (just used for testing)
#' @param cleanData (Optional) if TRUE then remove any invalid rows from the data before generating the upload files - warning data will potentially be lost if you do this!
#'
#' @return
#' @export
#'
#' @examples generateCLFile(yearToUse = 2016, country = 'IRL',RDBESdata = myRDBESData)
generateCLFile <- function(yearToUse, country, RDBESdata, outputFileName = "", numberOfRows = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2017
  #country <- 'IE'
  #outputFileName <- ""
  #numberOfRows <- 50
  #cleanData <- TRUE
  #RDBESvalidationdata <- validationData
  #RDBEScodeLists <- allowedValues
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"HCL.csv", sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data and write it out
  
  CL <- RDBESdata[['CL']]
  
  # Filter the CE data by year
  clFile <- CL[CL$CLyear == yearToUse & CL$CLvesselFlagCountry == country,]
  
  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    rowsBefore <- nrow(clFile)
    print(paste(rowsBefore, ' CL rows before removing invalid data', sep =""))
    
    # Validate CL
    myErrors <- validateTables(RDBESdata = RDBESdata, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists, shortOutput = FALSE,framestoValidate = c("CL"))
    
    # UGLY FIX 
    # Get rid of the errors associates with CLgsaSubarea having a value of NotApplicable
    myErrors <- myErrors[!(myErrors$fieldName == 'CLgsaSubarea' & myErrors$problemType =='Code list problem' & grepl('NotApplicable', myErrors$problemDescription)),]
    
    # Remove any invalid rows 
    invalidRows <- unique(myErrors[myErrors$tableName == 'CL' & !is.na(myErrors$rowID),"rowID"])
    clFile <- clFile[!clFile$CLid %in% invalidRows,]
    
    rowsAfter <- nrow(clFile)
    print(paste(nrow(clFile), ' CL rows after removing invalid data', sep =""))
    
    if (rowsAfter < rowsBefore){
      missingRows <- rowsBefore - rowsAfter
      warning(paste(missingRows,' invalid rows removed from CL before trying to generate output files', sep = ""))
    }
    
  }
  
  # If we only want a certain number of rows we'll subset the data (normally just used during testing)
  if (!is.null(numberOfRows) & nrow(clFile) > numberOfRows){
    clFile <- clFile[1:numberOfRows,]
  }
  
  # We now write out the data frame with ids and column names included to make debugging easier
  fwrite(clFile, paste(outputFolder, outputFileName,"_debug",sep="") ,row.names=F,col.names=T,quote=F)
  
  # Now we'll get rid of the CEid values because we won't want them in our final output file
  clFile <- select(clFile,-c(CLid))
  
  # Get all the values from CL and list them out
  cl <- do.call('paste',c(clFile,sep=','))
  
  # replace NA with blanks
  cl <- gsub('NA','',cl)
  
  # UGLY FIX 
  # Where there is no GSA sub-area value then we shoudl use the value "NA" - however the previous line will change all "NA"'s into blanks :-S 
  # I needed to do soemthing a bit ugly so that we can submit NA for the actual GSA sub-area value
  cl <- gsub('NotApplicable','NA',cl)
  
  fwrite(list(cl), paste(outputFolder, outputFileName, sep = ""),row.names=F,col.names=F,quote=F)
  
}

#' generateVDFile Generate a VD exchange format file for the RDBS
#'
#' @param yearToUse The year we want to generate the VD file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#'
#' @return
#' @export
#'
#' @examples generateVDFile(yearToUse = 2016, country = 'IRL',RDBESdata = myRDBESData)
generateVDFile <- function(yearToUse, country, RDBESdata, outputFileName = ""){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2017
  #country <- 'IE'
  #outputFileName <- ""
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"HVD.csv", sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data and write it out
  
  VD <- RDBESdata[['VD']]
  
  # Filter the CE data by year
  VDFile <- VD[VD$VDyear == yearToUse & VD$VDcountry == country,]
  
  # Remove the IDs from the data frame
  VDFile <- select(VDFile,-c(VDid))
  
  # HACK - the validator currently wants things in a different order to the Data Model spreadsheet
  VDFile$VDtype <- NA
  VDFile <- VDFile[,c('VDrecordType','VDcountry', 'VDencryptedCode' , 'VDyear' ,'VDflagCountry', 'VDhomePort','VDlength','VDlengthCategory' ,'VDpower','VDtonnage','VDtonUnit' ,'VDtype')]

  # Get all the values from VD and list them out
  vd <- do.call('paste',c(VDFile,sep=','))
  
  # replace NA with blanks
  vd <- gsub('NA','',vd)
  
  fwrite(list(vd), paste(outputFolder, outputFileName, sep = ""),row.names=F,col.names=F,quote=F)
  
}

#' generateCSFile_H5 This function creates an RDBES exchange file for Hierarchy 5 CS data
#'
#' @param yearToUse The year we want to generate the CS H5 file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfSamples Limit the output to this number of samples (just used for testing)
#'
#' @return
#' @export
#'
#' @examples generateCSFile_H5(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateCSFile_H5 <- function(yearToUse, country, RDBESdata, outputFileName="", numberOfSamples = NULL){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IE'
  #outputFileName <- ""
  #numberOfSamples <- 5

  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"H5.csv", sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data
  
  # Get the data from our name list
  DE <- RDBESdata[['DE']]
  SD <- RDBESdata[['SD']]
  OS <- RDBESdata[['OS']]
  VD <- RDBESdata[['VD']]
  FT <- RDBESdata[['FT']]
  LE <- RDBESdata[['LE']]
  SL <- RDBESdata[['SL']]
  SS <- RDBESdata[['SS']]
  SA <- RDBESdata[['SA']]
  FM <- RDBESdata[['FM']]
  BV <- RDBESdata[['BV']]

  # Filter by year and country
  DEfile <- DE[DE$DEyear == yearToUse & DE$DEhierarchy == 5,]
  SDfile <- SD[SD$DEid %in% DEfile$DEid & SD$SDcountry == country,]
  OSfile <- OS[OS$SDid %in% SDfile$SDid,]
  FTfile <- FT[FT$OSid %in% OSfile$OSid,]
  LEfile <- LE[LE$FTid %in% FTfile$FTid,]
  SSfile <- SS[SS$LEid %in% LEfile$LEid,]
  SAfile <- SA[SA$SSid %in% SSfile$SSid,]
  FMfile <- FM[FM$SAid %in% SAfile$SAid,]
  BVfile <- BV[BV$FMid %in% FMfile$FMid | BV$SAid %in% SAfile$SAid,]
  
  VDfile <- VD[VD$VDid %in% LEfile$VDid,]
  SLfile <- SL[SL$SLlistName %in% SSfile$SSspeciesListName,]
  

  # If required, limit the number of samples we will output (normally just used during testing)
  if (!is.null(numberOfSamples)){
    # Take a sample of the samples :-)
    SAidsToUse <- sample(SAfile$SAid, size = numberOfSamples, replace = FALSE)
    SAfile <- SAfile[SAfile$SAid %in% SAidsToUse,]
    FMfile <- FM[FM$SAid %in% SAfile$SAid,]
    BVfile <- BV[BV$FMid %in% FMfile$FMid | BV$SAid %in% SAfile$SAid,]
    SSfile <- SS[SS$SSid %in% SAfile$SSid,]
    LEfile <- LE[LE$LEid %in% SSfile$LEid,]
    FTfile <- FT[FT$FTid %in% LEfile$FTid,]
    OSfile <- OS[OS$OSid %in% FTfile$OSid,]
    SDfile <- SD[SD$SDid %in% OSfile$SDid,]
    DEfile <- DE[DE$DEid %in% SDfile$DEid,]
    VDfile <- VD[VD$VDid %in% LEfile$VDid,]
    SLfile <- SLfile[SLfile$SLlistName %in% SSfile$SSspeciesListName,]
  
  }
  
  ## TODO - hacks due to mistakes in the validator
  
  ## DE - this is hack because the validator isn't correctly checking for duplicates
  # so I have had to alter the sampling scheme name
  DEfile$DEsamplingScheme <- paste(DEfile$DEsamplingScheme, DEfile$DEstratum,sep="-")
  
  ## SL - this is a hack because the validator will only allow 1 SL line
  SLfile <- head(SLfile,1)
   
  ## LE currently requires stat rectangle
  LEfile$LErectangle <- "-9"
  # Not allowed MIS or a blank target species
  LEfile[is.na(LEfile$LEtargetSpecies),"LEtargetSpecies"] <- "MPD"
  # Validation fails without a value for this
  LEfile$LEreasonNotSampled <- "Other"
  
  # The only currently allowed value of sub-polygon is "NA"
  LEfile$LEsubpolygon <- "ChangeMe"
  LEfile$LEnationalCategory <- "ChangeMe"

  # SA 
  # Some values were too large
  SAfile[SAfile$SAtotalWeightLive > 2000000000,"SAtotalWeightLive"] <- 2000000000
  # not allowed blank unit types
  SAfile[SAfile$SAunitType =="","SAunitType"] <- "Tray"
   
  ## BV hacks for mistakes in the validator
  if (nrow(BVfile)>0){
    BVfile$BVstratification <- 1
    BVfile[BVfile$BVunitValue %in% c("year","MaturityScale","Sex"),"BVunitValue"] <- "mm"
    BVfile$BVmeasurementEquipment <- "Measured by hand with caliper"
    BVfile$BVunitScaleList <- "A"
  }
   
  ## Step 2 - I now add a SortOrder field to each of our fitlered data frames
  ## this will allow me to generate the CS file in the correct row order without needing a slow for-loop
  
  # IMPORTANT - I'm using inner_join from dply so we can maintain the ordering of the first data frame in the join
  # if the ordering isn't maintained then the exchange file will be output in the wrong order
  DEfile$SortOrder <- paste(DEfile$DEhierarchy,DEfile$DEyear,DEfile$DEstratum,sep="-")
  SDfile$SortOrder <- paste( inner_join(SDfile,DEfile, by ="DEid")[,c("SortOrder")], SDfile$SDid, sep = "-")
  OSfile$SortOrder <- paste( inner_join(OSfile,SDfile, by ="SDid")[,c("SortOrder")], OSfile$OSid, sep = "-")
  FTfile$SortOrder <- paste( inner_join(FTfile,OSfile, by ="OSid")[,c("SortOrder")], FTfile$FTid, "b", sep = "-")
  LEfile$SortOrder <- paste( inner_join(LEfile,FTfile, by ="FTid")[,c("SortOrder")], LEfile$LEid, sep = "-")
  
  # Need to have VD appear before LE in the output file so I need to do something different with the SortOrder value
  # Also can't use the VD frame directly becasue it will hve the wrong number of rows (not every FT/LE has a unique VD)
  VDfile2 <- inner_join(VDfile,LEfile, by ="VDid")
  # Assuming there are no other 'a's or 'b's in the string we'll change 'b' to 'a' for the VD sort order
  # this will make it appear before the FT line in the sorted output
  VDfile2$SortOrder <- gsub('b','a',VDfile2$SortOrder)
  
  SSfile$SortOrder <- paste( inner_join(SSfile,LEfile, by ="LEid")[,c("SortOrder")], SSfile$SSid, sep = "-")
  

  SAfile$SortOrder <- paste( inner_join(SAfile,SSfile, by ="SSid")[,c("SortOrder")], SAfile$SAid, "d", sep = "-")
  
  # I now need SL to appear after SS so I need to do soemthing similar to VD here...
  SLfile2 <- inner_join(SLfile,SSfile, by = c("SLlistName" = "SSspeciesListName"))
  # Multiple columns called Sortorder so I need to change the name
  names(SLfile2)[names(SLfile2)=="SortOrder"]<-"SortOrder_SS"
  SLfile3 <- inner_join(SLfile2,SAfile,by="SSid")
  # Assuming there are no other 'c's or 'd's in the string we'll change 'c' to 'd' for the SL sort order
  # this will make it appear after the SS line in the sorted output
  SLfile3$SortOrder <- gsub('d','c',SLfile3$SortOrder)
  
  
  FMfile$SortOrder <- paste( inner_join(FMfile,SAfile, by ="SAid")[,c("SortOrder")], FMfile$FMid, sep = "-")
  BVfile$SortOrder <- paste( inner_join(BVfile,FMfile, by ="FMid")[,c("SortOrder")], BVfile$BVid, sep = "-")

  # Combine our SortOrder values
  FileSortOrder <- c(
    DEfile$SortOrder,
    SDfile$SortOrder,
    OSfile$SortOrder,
    VDfile2$SortOrder,
    FTfile$SortOrder,
    LEfile$SortOrder,
    SLfile3$SortOrder,
    SSfile$SortOrder,
    SAfile$SortOrder,
    FMfile$SortOrder,
    BVfile$SortOrder
  )

  ## STEP 3) Create a version of the output data for debugging
  
  # Here we create a version of the output data with all the ids and sorting columns in so I can check things are correct
  csForChecking <- c(
    do.call('paste',c(DEfile,sep=','))
    ,do.call('paste',c(SDfile,sep=','))
    ,do.call('paste',c(OSfile,sep=','))
    ,do.call('paste',c(VDfile2,sep=','))
    ,do.call('paste',c(FTfile,sep=','))
    ,do.call('paste',c(LEfile,sep=','))
    ,do.call('paste',c(SLfile3,sep=','))
    ,do.call('paste',c(SSfile,sep=','))
    ,do.call('paste',c(SAfile,sep=','))
    ,do.call('paste',c(FMfile,sep=','))
    ,do.call('paste',c(BVfile,sep=','))
  )

  # Sort the output into the correct order
  csForCheckingOrdered <- csForChecking[order(FileSortOrder)]
  
  fwrite(list(csForCheckingOrdered), paste(outputFolder,outputFileName,"_debug", sep = ""),row.names=F,col.names=F,quote=F)

  ## STEP 4) Create the real version of the output data
  
  
  # Create the CS data with the sort columns and ids removed - this will then be used to generate the exchange file
  cs <- c(
    do.call('paste',c(select(DEfile,-c(DEid,SortOrder)),sep=','))
    ,do.call('paste',c(select(SDfile,-c(DEid,SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(OSfile,-c(SDid,OSid,SortOrder)),sep=','))
    # Different pattern for VD
    ,do.call('paste',c(select(VDfile2,c(VDrecordType,VDencryptedCode,VDhomePort,VDflagCountry,VDlength,VDlengthCategory,VDpower,VDsize,VDsizeUnit,VDtype)),sep=','))
    ,do.call('paste',c(select(FTfile,-c(FTid, OSid, VSid, VDid, SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(LEfile,-c(OSid,FTid,VSid,VDid,SAid,LEid,SortOrder)),sep=','))
    # Different patern for SL
    ,do.call('paste',c(select(SLfile3,c(SLrecordType, SLlistName,SLyear, SLspeciesCode, SLcommercialSpecies, SLcatchFraction)),sep=','))
    ,do.call('paste',c(select(SSfile,-c(LEid,FOid,SSid,SSspeciesListID,SortOrder)),sep=','))
    ,do.call('paste',c(select(SAfile,-c(SAparentID,SSid,SAid,SortOrder)),sep=','))
    ,do.call('paste',c(select(FMfile,-c(SAid,FMid,SortOrder)),sep=','))
    ,do.call('paste',c(select(BVfile,-c(SAid,FMid,BVid,SortOrder)),sep=','))
  )

  # Sort the output into the correct order
  csOrdered <- cs[order(FileSortOrder)]

  # replace NA with blanks
  csOrdered <- gsub('NA','',csOrdered)

  # TODO - one of the code lists only has an allowed value of NA - we don't want this replaced with blanks so we do this hack...
  # replace ChangeMe with NA
  csOrdered <- gsub('ChangeMe','NA',csOrdered)

  fwrite(list(csOrdered), paste(outputFolder,outputFileName, sep = ""),row.names=F,col.names=F,quote=F)
  
}

#' generateCSFile_H1 This function creates an RDBES exchange file for Hierarchy 1 CS data
#'
#' @param yearToUse The year we want to generate the CS H5 file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfSamples Limit the output to this number of samples (just used for testing)
#'
#' @return
#' @export
#'
#' @examples generateCSFile_H1(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateCSFile_H1 <- function(yearToUse, country, RDBESdata, outputFileName="", numberOfSamples = NULL){
  
  # For testing
  # RDBESdata<-myRDBESData
  # yearToUse <- 2017
  # country <- 'IE'
  # outputFileName <- ""
  # numberOfSamples <- 10
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"H1.csv", sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data
  
  # Get the data from our name list
  DE <- RDBESdata[['DE']]
  SD <- RDBESdata[['SD']]
  VS <- RDBESdata[['VS']]
  VD <- RDBESdata[['VD']]
  FT <- RDBESdata[['FT']]
  FO <- RDBESdata[['FO']]
  SL <- RDBESdata[['SL']]
  SS <- RDBESdata[['SS']]
  SA <- RDBESdata[['SA']]
  FM <- RDBESdata[['FM']]
  BV <- RDBESdata[['BV']]
  
  # Filter by year and country
  DEfile <- DE[DE$DEyear == yearToUse & DE$DEhierarchy == 1,]
  SDfile <- SD[SD$DEid %in% DEfile$DEid & SD$SDcountry == country,]
  VSfile <- VS[VS$SDid %in% SDfile$SDid,]
  FTfile <- FT[FT$VSid %in% VSfile$VSid,]
  FOfile <- FO[FO$FTid %in% FTfile$FTid,]
  SSfile <- SS[SS$FOid %in% FOfile$FOid,]
  SAfile <- SA[SA$SSid %in% SSfile$SSid,]
  FMfile <- FM[FM$SAid %in% SAfile$SAid,]
  BVfile <- BV[BV$FMid %in% FMfile$FMid | BV$SAid %in% SAfile$SAid,]
  VDfile <- VD[VD$VDid %in% VSfile$VDid,]
  SLfile <- SL[SL$SLlistName %in% SSfile$SSspeciesListName,]
  
  # If required, limit the number of samples we will output (normally just used during testing)
  if (!is.null(numberOfSamples)){
    # Take a sample of the samples :-)
    SAidsToUse <- sample(SAfile$SAid, size = numberOfSamples, replace = FALSE)
    SAfile <- SAfile[SAfile$SAid %in% SAidsToUse,]
    FMfile <- FM[FM$SAid %in% SAfile$SAid,]
    BVfile <- BV[BV$FMid %in% FMfile$FMid | BV$SAid %in% SAfile$SAid,]
    SSfile <- SS[SS$SSid %in% SAfile$SSid,]
    FOfile <- FO[FO$FOid %in% SSfile$FOid,]
    FTfile <- FT[FT$FTid %in% FOfile$FTid,]
    VSfile <- VS[VS$VSid %in% FTfile$VSid,]
    SDfile <- SD[SD$SDid %in% VSfile$SDid,]
    DEfile <- DE[DE$DEid %in% SDfile$DEid,]
    SLfile <- SLfile[SLfile$SLlistName %in% SSfile$SSspeciesListName,]
    
  }
  
  
  ## TODO - hacks due to mistakes in the validator
  
  ## DE - this is hack because the validator isn't correctly checking for duplicates
  # so I have had to alter the sampling scheme name
  #DEfile$DEsamplingScheme <- paste(DEfile$DEsamplingScheme, DEfile$DEstratum,sep="-")
  
  ## SL - this is a hack because the validator will only allow 1 SL line per SS line
  # We'll just get the first row of each unique list name
  #SLfile <- head(SLfile,1)
  #SLfile <- SLfile %>% group_by(SLlistName) %>% filter(row_number()==1)
  
  ## VS - need to provide values for soem fields that shoudl be blank really
  #VSfile$VSsampProb  <- -1
  #VSfile$VSselectionMethodCluster <- "SYSS"
  #VSfile$VStotalClusters <- -1
  #VSfile$VSsampledClusters <- -1
  #VSfile$VSclustersProb <- -1
  
  ## FT
  #FTfile$FTsampProb <- -1
  
  ## FO
  #FOfile$FOobservationCode <- "None"
  # Chaneg OTQ gears to OTB
  #FOfile[FOfile$FOgear=="OTQ","FOgear"] <- "OTB"
  #FOfile[FOfile$FOcatchReg=="Non","FOcatchReg"] <- "None"
  
  ## SL
  #SLfile[SLfile$SLspeciesCode == 125743,"SLspeciesCode"] <- 126458
  #SLfile[SLfile$SLspeciesCode == 10331,"SLspeciesCode"] <- 127126
  
  ## SA
  #SAfile$SAcommercialSpecies <- ''
  #SAfile[SAfile$SAspeciesCode == 125743,"SAspeciesCode"] <- 126458
  #SAfile[SAfile$SAspeciesCode == 10331,"SAspeciesCode"] <- 127126
  
  ## BV hacks for mistakes in the validator
  #if (nrow(BVfile)>0){
  #  BVfile$BVstratification <- 1
  #  BVfile[BVfile$BVunitValue %in% c("year","MaturityScale","Sex"),"BVunitValue"] <- "mm"
  #  BVfile$BVmeasurementEquipment <- "Measured by hand with caliper"
  #  BVfile$BVunitScaleList <- "A"
  #}
  
  ## Step 2 - I now add a SortOrder field to each of our fitlered data frames
  ## this will allow me to generate the CS file in the correct row order without needing a slow for-loop
  
  ## I need the rows in the following order:
  # DE, SD, VS, FT, FO, SS, SA, FM, BV
  

  # IMPORTANT - I'm using inner_join from dply so we can maintain the ordering of the first data frame in the join
  # if the ordering isn't maintained then the exchange file will be output in the wrong order
  DEfile$SortOrder <- paste(DEfile$DEhierarchy,DEfile$DEyear,DEfile$DEstratum,sep="-")
  SDfile$SortOrder <- paste( inner_join(SDfile,DEfile, by ="DEid")[,c("SortOrder")], SDfile$SDid, sep = "-")
  VSfile$SortOrder <- paste( inner_join(VSfile,SDfile, by ="SDid")[,c("SortOrder")], VSfile$VSid, sep = "-")
  FTfile$SortOrder <- paste( inner_join(FTfile,VSfile, by ="VSid")[,c("SortOrder")],FTfile$FTid, FTfile$VSid,"a", sep = "-")
  FOfile$SortOrder <- paste( inner_join(FOfile,FTfile, by ="FTid")[,c("SortOrder")], FOfile$FOid, sep = "-")
  SSfile$SortOrder <- paste( inner_join(SSfile,FOfile, by ="FOid")[,c("SortOrder")], SSfile$SSid, sep = "-")
  SAfile$SortOrder <- paste( inner_join(SAfile,SSfile, by ="SSid")[,c("SortOrder")], SAfile$SAid, sep = "-")
  FMfile$SortOrder <- paste( inner_join(FMfile,SAfile, by ="SAid")[,c("SortOrder")], FMfile$FMid, sep = "-")
  # TODO For our data BV only follows SA not FM - need to check that the sort order will work if there is a mix of lower hierarchies
  BVfile$SortOrder <- paste( inner_join(BVfile,SAfile, by ="SAid")[,c("SortOrder")], BVfile$BVid, sep = "-")
  
  # Combine our SortOrder values
  FileSortOrder <- c(
    DEfile$SortOrder,
    SDfile$SortOrder,
    VSfile$SortOrder,
    FTfile$SortOrder,
    FOfile$SortOrder,
    SSfile$SortOrder,
    SAfile$SortOrder,
    FMfile$SortOrder,
    BVfile$SortOrder
  )
  
  ## STEP 3) Create a version of the output data for debugging
  
  # Here we create a version of the output data with all the ids and sorting columns in so I can check things are correct
  csForChecking <- c(
     do.call('paste',c(DEfile,sep=','))
    ,do.call('paste',c(SDfile,sep=','))
    ,do.call('paste',c(VSfile,sep=','))
    ,do.call('paste',c(FTfile,sep=','))
    ,do.call('paste',c(FOfile,sep=','))
    ,do.call('paste',c(SSfile,sep=','))
    ,do.call('paste',c(SAfile,sep=','))
    ,do.call('paste',c(FMfile,sep=','))
    ,do.call('paste',c(BVfile,sep=','))
  )
  
  # Sort the output into the correct order
  csForCheckingOrdered <- csForChecking[order(FileSortOrder)]
  
  fwrite(list(csForCheckingOrdered), paste(outputFolder, outputFileName,"_debug",sep="") ,row.names=F,col.names=F,quote=F)
  
  ## STEP 4) Create the real version of the output data
  
  #names(VSfile)
  # Create the CS data with the sort columns and ids removed - this will then be used to generate the exchange file
  cs <- c(
    do.call('paste',c(select(DEfile,-c(DEid,SortOrder)),sep=','))
    ,do.call('paste',c(select(SDfile,-c(DEid,SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(VSfile,-c(VSid,SDid,VDid,TEid,SortOrder)),sep=','))
    ,do.call('paste',c(select(FTfile,-c(FTid, OSid, VSid, VDid, SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(FOfile,-c(FOid,FTid,SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(SSfile,-c(LEid,FOid,SSid,SSspeciesListID,SortOrder)),sep=','))
    ,do.call('paste',c(select(SAfile,-c(SSid,SAid,SortOrder)),sep=','))
    ,do.call('paste',c(select(FMfile,-c(SAid,FMid,SortOrder)),sep=','))
    ,do.call('paste',c(select(BVfile,-c(SAid,FMid,BVid,SortOrder)),sep=','))
  )
  
  # Sort the output into the correct order
  csOrdered <- cs[order(FileSortOrder)]
  
  # replace NA with blanks
  csOrdered <- gsub('NA','',csOrdered)
  
  # TODO - one of the code lists only has an allowed value of NA - we don't want this replaced with blanks so we do this hack...
  # replace ChangeMe with NA
  #csOrdered <- gsub('ChangeMe','NA',csOrdered)
  
  fwrite(list(csOrdered), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  
}

#' generateH5RDataFiles Generates RData files containing the relevent data frames for hierarchy 5.  Each data frame is stored in a seperate RData file.  Data is filtered by country and year (optional)
#'
#' @param yearToUse (Optional) Year to extract the data for
#' @param country Country code to extract the data for (2 letter ISO code)
#' @param RDBESdata A list containing the RDBES data frames
#'
#' @return
#' @export
#'
#' @examples generateH5RDataFiles(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateH5RDataFiles <- function(yearToUse = NULL, country, RDBESdata){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IE'
  
  subfolder <- 'H5/'
  
  ## Step 0
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(paste0(outputFolder,subfolder))), dir.create(file.path(paste0(outputFolder,subfolder))), FALSE)
  
  ## Step 1 - Filter the data
  
  # Get the data from our name list
  DE <- RDBESdata[['DE']]
  SD <- RDBESdata[['SD']]
  OS <- RDBESdata[['OS']]
  VD <- RDBESdata[['VD']]
  FT <- RDBESdata[['FT']]
  LE <- RDBESdata[['LE']]
  SL <- RDBESdata[['SL']]
  SS <- RDBESdata[['SS']]
  SA <- RDBESdata[['SA']]
  FM <- RDBESdata[['FM']]
  BV <- RDBESdata[['BV']]
  
  # Filter by year and country
  DE <- DE[DE$DEyear == yearToUse & DE$DEhierarchy == 5,]
  SD <- SD[SD$DEid %in% DE$DEid & SD$SDcountry == country,]
  OS <- OS[OS$SDid %in% SD$SDid,]
  FT <- FT[FT$OSid %in% OS$OSid,]
  LE <- LE[LE$FTid %in% FT$FTid,]
  SS <- SS[SS$LEid %in% LE$LEid,]
  SA <- SA[SA$SSid %in% SS$SSid,]
  FM <- FM[FM$SAid %in% SA$SAid,]
  BV <- BV[BV$FMid %in% FM$FMid | BV$SAid %in% SA$SAid,]
  
  VD <- VD[VD$VDid %in% LE$VDid,]
  SL <- SL[SL$SLlistName %in% SS$SSspeciesListName,]
  
  # Anonymise the vessels
  VD$VDpower <- NA
  VD$VDsize <-NA
  
  listOfFrames <- list("DE"=DE, "SD"=SD, "OS"=OS, "FT"=FT, "LE"=LE, "SS"=SS, "SA"=SA, "FM"=FM, "BV"=BV, "VD"=VD, "SL"=SL)
  
  newNames <- lapply(listOfFrames, function(x)  changeFieldNames(frameToRename = x, fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR"))
  
  # Change the field names of the entries in our list of data frames
  for (myFrame in  names(listOfFrames)){
    names(listOfFrames[[myFrame]]) <- newNames[[myFrame]]
  }
  
  #names(listOfFrames[["BV"]])
  #newNames[['BV']]
  #names(BV)
  
  # The stupid save function can only save whole objects so we need to convert the members in a list
  # back into single objects
  DE <- listOfFrames[['DE']]
  SD <- listOfFrames[['SD']]
  OS <- listOfFrames[['OS']]
  VD <- listOfFrames[['VD']]
  FT <- listOfFrames[['FT']]
  LE <- listOfFrames[['LE']]
  SL <- listOfFrames[['SL']]
  SS <- listOfFrames[['SS']]
  SA <- listOfFrames[['SA']]
  FM <- listOfFrames[['FM']]
  BV <- listOfFrames[['BV']]
  
  
  save(DE, file = paste0(outputFolder,subfolder,"DE", ".RData"))
  save(SD, file = paste0(outputFolder,subfolder,"SD", ".RData"))
  save(OS, file = paste0(outputFolder,subfolder,"OS", ".RData"))
  save(FT, file = paste0(outputFolder,subfolder,"FT", ".RData"))
  save(LE, file = paste0(outputFolder,subfolder,"LE", ".RData"))
  save(SS, file = paste0(outputFolder,subfolder,"SS", ".RData"))
  save(SA, file = paste0(outputFolder,subfolder,"SA", ".RData"))
  save(FM, file = paste0(outputFolder,subfolder,"FM", ".RData"))
  save(BV, file = paste0(outputFolder,subfolder,"BV", ".RData"))
  save(VD, file = paste0(outputFolder,subfolder,"VD", ".RData"))
  save(SL, file = paste0(outputFolder,subfolder,"SL", ".RData"))
  

}

#' generateH1RDataFiles Generates RData files containing the relevent data frames for hierarchy 1.  Each data frame is stored in a seperate RData file.  Data is filtered by country and year (optional)
#'
#' @param yearToUse (Optional) Year to extract the data for
#' @param country Country code to extract the data for (2 letter ISO code)
#' @param RDBESdata A list containing the RDBES data frames
#'
#' @return
#' @export
#'
#' @examples generateH1RDataFiles(yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateH1RDataFiles <- function(yearToUse = NULL, country, RDBESdata){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IE'
  
  subfolder <- 'H1/'
  
  ## Step 0
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(paste0(outputFolder,subfolder))), dir.create(file.path(paste0(outputFolder,subfolder))), FALSE)
  
  ## Step 1 - Filter the data
  
  DE <- RDBESdata[['DE']]
  SD <- RDBESdata[['SD']]
  VS <- RDBESdata[['VS']]
  VD <- RDBESdata[['VD']]
  FT <- RDBESdata[['FT']]
  FO <- RDBESdata[['FO']]
  SL <- RDBESdata[['SL']]
  SS <- RDBESdata[['SS']]
  SA <- RDBESdata[['SA']]
  FM <- RDBESdata[['FM']]
  BV <- RDBESdata[['BV']]
  
  # Filter by year and country
  DE <- DE[DE$DEyear == yearToUse & DE$DEhierarchy == 1,]
  SD <- SD[SD$DEid %in% DE$DEid & SD$SDcountry == country,]
  VS <- VS[VS$SDid %in% SD$SDid,]
  FT <- FT[FT$VSid %in% VS$VSid,]
  FO <- FO[FO$FTid %in% FT$FTid,]
  SS <- SS[SS$FOid %in% FO$FOid,]
  SA <- SA[SA$SSid %in% SS$SSid,]
  FM <- FM[FM$SAid %in% SA$SAid,]
  BV <- BV[BV$FMid %in% FM$FMid | BV$SAid %in% SA$SAid,]
  VD <- VD[VD$VDid %in% VS$VDid,]
  SL <- SL[SL$SLlistName %in% SS$SSspeciesListName,]
  
  # Anonymise the vessels
  #VD$VDpower <- NA
  #VD$VDsize <-NA
  
  listOfFrames <- list("DE"=DE, "SD"=SD, "VS"=VS, "FT"=FT, "FO"=FO, "SS"=SS, "SA"=SA, "FM"=FM, "BV"=BV, "VD"=VD, "SL"=SL)
  
  newNames <- lapply(listOfFrames, function(x)  changeFieldNames(frameToRename = x, fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR"))
  
  # Change the field names of the entries in our list of data frames
  for (myFrame in  names(listOfFrames)){
    names(listOfFrames[[myFrame]]) <- newNames[[myFrame]]
  }

  #names(listOfFrames[["BV"]])
  #newNames[['BV']]
  #names(BV)
  
  # The stupid save function can only save whole objects so we need to convert the members in a list
  # back into single objects
  DE <- listOfFrames[['DE']]
  SD <- listOfFrames[['SD']]
  VS <- listOfFrames[['VS']]
  VD <- listOfFrames[['VD']]
  FT <- listOfFrames[['FT']]
  FO <- listOfFrames[['FO']]
  SL <- listOfFrames[['SL']]
  SS <- listOfFrames[['SS']]
  SA <- listOfFrames[['SA']]
  FM <- listOfFrames[['FM']]
  BV <- listOfFrames[['BV']]
  

  save(DE, file = paste0(outputFolder,subfolder,"DE", ".RData"))
  save(SD, file = paste0(outputFolder,subfolder,"SD", ".RData"))
  save(VS, file = paste0(outputFolder,subfolder,"VS", ".RData"))
  save(FT, file = paste0(outputFolder,subfolder,"FT", ".RData"))
  save(FO, file = paste0(outputFolder,subfolder,"FO", ".RData"))
  save(SS, file = paste0(outputFolder,subfolder,"SS", ".RData"))
  save(SA, file = paste0(outputFolder,subfolder,"SA", ".RData"))
  save(FM, file = paste0(outputFolder,subfolder,"FM", ".RData"))
  save(BV, file = paste0(outputFolder,subfolder,"BV", ".RData"))
  save(VD, file = paste0(outputFolder,subfolder,"VD", ".RData"))
  save(SL, file = paste0(outputFolder,subfolder,"SL", ".RData"))
  
  
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
getValidationData <- function(fileLocation){
  
  # For testing
  #fileLocation <- './tableDefs/BaseTypes.xsd'
  
  # Parse the XML
  doc <- xmlTreeParse(fileLocation,useInternal= TRUE)
  myXML <- xmlToList(doc)
  
  # Data frame to hold out validation info
  myValidationDF <- NULL
  
  # Get the infromation we want from the xsd file - hard-code to the current structure...
  for (myElement in myXML){
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
  
  myValidationDF
  
  
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
logValidationError<- function(errorList,tableName, rowID, fieldName, problemType, problemDescription){
  
  
  myErrors <- data.frame(tableName = tableName
                         ,rowID = rowID
                         ,fieldName = fieldName
                         ,problemType = problemType
                         ,problemDescription = problemDescription
                         ,stringsAsFactors = FALSE)
  errorList <- rbind(errorList, myErrors)
  
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
  #RDBESdata <- myRDBESData
  #RDBESvalidationdata <- validationData
  #RDBEScodeLists <- allowedValues
  #shortOutput <- TRUE
  
  # To hold the output
  errorList <- data.frame(tableName=character(0)
                          ,rowID = integer(0)
                          ,fieldName=character(0)
                          ,problemType=character(0)
                          ,problemDescription=character(0), stringsAsFactors = FALSE)
  
  # We'll only validate specifc tables
  RDBESdata <- RDBESdata[framestoValidate]
  
  # Remove any NAs (these are tables we don't have)
  RDBESdata <- RDBESdata[!is.na(names(RDBESdata))]
  
  # for each data frame in our list
  for (dfToCheck in RDBESdata){
    
    # Get the field names as a data frame
    myDF <- data.frame(fieldName = names(dfToCheck), stringsAsFactors = FALSE)
    # Join the field names to the field type data frame
    fieldsAndTypes <- left_join(myDF,RDBESvalidationdata,by=c("fieldName" = "name"))
    ## Assume the id is always the first field - might be better to check the field names instead
    myIDField <- names(dfToCheck)[[1]]
    myTableName <- substr(myIDField,1,2)
    #print(myTableName)
    
    ## Check 0: Check if we are missing any fields
    missingFieldsCheck <- RDBESvalidationdata
    missingFieldsCheck$table <- substr(missingFieldsCheck$name,1,2)
    missingFieldsCheck <- missingFieldsCheck[missingFieldsCheck$table==myTableName,]
    missingFields <- missingFieldsCheck[!missingFieldsCheck$name %in% myDF$fieldName,]
    # If we have some missing fields lets log them as an error
    if (nrow(missingFields) > 0){
      #Log the error
      errorList <- logValidationError(errorList = errorList
                                      ,tableName = myTableName
                                      ,rowID = NA
                                      ,fieldName = missingFields$name
                                      ,problemType = "Missing field check"
                                      ,problemDescription = paste("The following field is missing from your data frames: ", missingFields$name, sep = " "))
    }
    
    
    # Now we'll check each field in our current data frame
    for (i in 1:length(names(dfToCheck))) {
      
      myFieldName <- names(dfToCheck)[[i]]
      myFT <- fieldsAndTypes[fieldsAndTypes$fieldName == names(dfToCheck)[[i]],]
      
      ## Check 1) NA values
      myValuesNA <- dfToCheck[is.na(dfToCheck[,myFieldName]),]
      # If we have NA values check if thats ok
      if (nrow(myValuesNA)>0){
        # if we are not allowed to have empty values here then log an error
        if (ifelse(is.na(myFT$min),0,myFT$min) >0){
          #Log the error
          errorList <- logValidationError(errorList = errorList
                                          ,tableName = myTableName
                                          ,rowID = myValuesNA[,1]
                                          ,fieldName = myFieldName
                                          ,problemType = "Null value check"
                                          ,problemDescription = paste("Null value problem;",myIDField,":", myValuesNA[,1], " ;Column:",myFieldName, ";Unallowed value:",myValuesNA[,myFieldName], sep = " "))
        }
      } # Endif NA check
      
      # Now that we have passed the NA check point lets get rid of any NA values so 
      # we don't need to worry about them again
      #dfToCheck <- dfToCheck[!is.na(dfToCheck[,myFieldName]),]
      dfToCheckNotNA <- dfToCheck[!is.na(dfToCheck[,myFieldName]),]
      
      # Check 2 Do we know what type this field should be?
      # If not, there's nothing much else we can do so skip to the end and log the error
      
      #Check if we know what types this field should have
      if (!is.na(myFT$type)){
        myType <- myFT$type
        
        # CHeck 3 For simple data types we'll see if we have the right format data
        
        # IF simple type
        if (length(grep("xs:",myType)) > 0) {
          ## simple type, so check data type 
          # Ints
          if (myType == "xs:int"){
            # Check for any non integer values
            myNonIntValues <- dfToCheckNotNA[!is.integer(dfToCheckNotNA[,myFieldName]),]
            if (nrow(myNonIntValues)>0){
              #Log the error
              errorList <- logValidationError(errorList = errorList
                                              ,tableName = myTableName
                                              ,rowID = myNonIntValues[,1]
                                              ,fieldName = myFieldName
                                              ,problemType = "Data type check"
                                              ,problemDescription = paste("Data type problem (int);",myIDField,":", myNonIntValues[,1], " ;Column:",myFieldName, ";Unallowed value:",myNonIntValues[,myFieldName], sep = " "))
            }
            # Decimal
          } else if (myType == "xs:decimal"){
            # Check for any non numeric values
            myNonDecValues <- dfToCheckNotNA[!is.numeric(dfToCheckNotNA[,myFieldName]),]
            if (nrow(myNonDecValues)>0){
              #Log the error
              errorList <- logValidationError(errorList = errorList
                                              ,tableName = myTableName
                                              ,rowID = myNonDecValues[,1]
                                              ,fieldName = myFieldName
                                              ,problemType = "Data type check"
                                              ,problemDescription = paste("Data type problem (decimal);",myIDField,":", myNonDecValues[,1], " ;Column:",myFieldName, ";Unallowed value:",myNonDecValues[,myFieldName], sep = " "))
            }
            # String
          } else if (myType == "xs:string"){
            # Everythign can be converted to a string so no problems here!
          }
          
          # Check 4 If we're dealing with code lists we need to see if we have allowed values  
          
          # ELSE code list
        } else {
          
          # if we have some non-NA values lets check them
          if (nrow(dfToCheckNotNA)){
            # see if we can find the correct code list
            myAllowedValues <- RDBEScodeLists[RDBEScodeLists$listName == myType,"allowedValues"]
            # If we found which values are allowed then we can check our data against them
            if (length(myAllowedValues)>0){
              # Check if our values are in the allowed list of values
              myResults <- dfToCheckNotNA[!dfToCheckNotNA[,myFieldName] %in% myAllowedValues,]
              # If we have soem values that aren't in the allowed list flag them as errors
              if (nrow(myResults)>0){
                #Log the error
                errorList <- logValidationError(errorList = errorList
                                                ,tableName = myTableName
                                                ,rowID = myResults[,1]
                                                ,fieldName = myFieldName
                                                ,problemType = "Code list problem"
                                                ,problemDescription = paste("Code list problem;",myIDField,":", myResults[,1], " ;Column:",myFieldName, ";Unallowed value:",myResults[,myFieldName], ";Code list name:",myType, sep = " "))
              }
              # ELSE if we didn't find a list of allowed values then log that as an error
            } else {
              #Log the error
              errorList <- logValidationError(errorList = errorList
                                              ,tableName = myTableName
                                              ,rowID = NA
                                              ,fieldName = myFieldName
                                              ,problemType = "Missing code list"
                                              ,problemDescription = paste("Could not find code list", myType, " for ", myFieldName, sep = " "))
            } # ENDIF find allowed values
          } # Endif NA/non na values
        } # ENDIF simple type 
        # Could not find validation inforation on this field
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
      }
    } # Endfor each field in frame
    
  } # Endfor each frame in RDBES data list
  
  # If we want shorter output we won't show every error  - just the first of each type
  if (shortOutput){
    
    fieldsToKeep <- names(errorList)
    # Sort the errors
    errorList <- errorList[order(errorList$tableName, errorList$fieldName, errorList$problemType, errorList$rowID),]
    # Get the row number within a group
    numberedErrorList <- errorList %>% group_by(errorList$tableName, errorList$fieldName, errorList$problemType) %>% mutate(rowNum = row_number())
    
    # Generate our extra rows that will show there were more errors present
    myExtraRows <- numberedErrorList[numberedErrorList$rowNum == 2,fieldsToKeep]
    if(nrow(myExtraRows)>0) {
      myExtraRows$rowID <- NA
      myExtraRows$problemDescription <- "Multiple similar error error rows removed for clarity"
    }
    
    # Combine the first row within each group with the extra rows
    shortErrorList <- rbind(numberedErrorList[numberedErrorList$rowNum == 1,fieldsToKeep],myExtraRows)
    shortErrorList <- shortErrorList[order(shortErrorList$tableName, shortErrorList$fieldName, shortErrorList$problemType, shortErrorList$rowID),]
    
    errorList <- shortErrorList
    
  }
  
  # Our list of errors
  errorList
  
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
  
  # save to file so we don't need to download from ICES every time
  save(allowedValues, file="referenceData/allowedValues.rData")
  
  allowedValues
  
}


# installs access to ices vocabulary https://github.com/ices-tools-prod/icesVocab
#install.packages("icesVocab")
library(icesVocab)
#?icesVocab



#' loadReferenceDataFromXSD Searches a directory (recursively if desired) and then attempts to extract the allowed values from any .xsd files it finds there. The results are returned as a large data frame.
#'
#' @param directoryToSearch The directory to search
#' @param recursive TRUE to search recursively
#'
#' @return
#' @export
#'
#' @examples allowedValues <- loadReferenceDataFromXSD(directoryToSearch = "./referenceData/", recursive = TRUE)
loadReferenceDataFromXSD <- function(directoryToSearch, recursive){
  
  # For testing
  #directoryToSearch <- "./referenceData/"
  #recursive <- FALSE
  
  filesToRead <- list.files(path = directoryToSearch, pattern = "*.xsd", recursive = recursive, full.names = TRUE)
  
  # This returns a list of data frames - 1 data frame for each file
  myResults <- lapply(filesToRead, function(x) getAllowedValues(x) )
  
  # rbind all the data frames into a single data frame
  myResults <- do.call("rbind", myResults)
  
}

#' getAllowedValues Reads an XSD file from the ICES RDBES and extracts the list name and the allwoed values into a data frame.  NOTE: This doesn't currently do any error checking and assumes the structure of the XSD files is always the same - this might not be the case!
#'
#' @param fileName - the name of the file to read (including path)
#'
#' @return
#' @export
#'
#' @examples getAllowedValues("./referenceData/RS_BiologicalMeasurementType.xsd")
getAllowedValues<-function(fileName){
  
  # For testing
  #fileName <- "./referenceData/EDMO.xsd"
  
  # Parse the XML
  doc <- xmlTreeParse(fileName,useInternal= TRUE)
  myXML <- xmlToList(doc)
  
  # Get the list name (assumes this is always defined within a simpleType)
  listName <- as.character(myXML$simpleType$.attrs)
  
  # get the allowed values (assumes these are always defined as restictions within a simpleType)
  myValues <- lapply(myXML$simpleType$restriction, function(x) { as.character(x[[1]]) })
  # Don't want the attrs included in our allowed values so lets remove them
  myValues[[".attrs"]]<-NULL
  
  # Put the results in a data frame
  myDF <- data.frame(listName = listName, fileName = fileName, allowedValues = unlist(myValues), stringsAsFactors=FALSE)
  
  myDF
  
}

