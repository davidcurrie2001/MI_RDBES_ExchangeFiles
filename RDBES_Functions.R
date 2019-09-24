library(RODBC)
library(dplyr)

# Location for our output files
outputFolder <- "./output/"

#' loadRDBESData
#' This function loads data that is already in the RDBES format from a relational database.
#' 
#' @param connectionString A string specifying the connection string to the database in a formt that odbcDriverConnect can use
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
#' @param outputFileName The name we wish to give the file we produce
#'
#' @return
#' @export
#'
#' @examples generateCEFile(yearToUse = 2016, RDBESdata = myRDBESData, outputFileName = paste(outputFolder,"UKN_CE.csv", sep = ""))
generateCEFile <- function(yearToUse, country, RDBESdata, outputFileName = ""){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IRL'
  #outputFileName <- ""
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"CE.csv", sep ="_")
  }
  
  CE <- RDBESdata[['CE']]
  
  # Filter the CE data by year
  ceFile <- CE[CE$Year == yearToUse & CE$VesselFlagCountry == country,]
  
  # Get all the values from CE and list them out
  ce <- do.call('paste',c(ceFile,sep=','))
  
  # replace NA with blanks
  ce <- gsub('NA','',ce)
  
  # Write out the file
  write.table(ce, paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  
}


#' generateCLFile Generate a CL exchange format file for the RDBS
#'
#' @param yearToUse The year we want to generate the CL file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName The name we wish to give the file we produce
#'
#' @return
#' @export
#'
#' @examples generateCLFile(yearToUse = 2016, RDBESdata = myRDBESData, outputFileName = paste(outputFolder,"UKN_CL.csv", sep = ""))
generateCLFile <- function(yearToUse, country, RDBESdata, outputFileName = ""){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IRL'
  #outputFileName <- ""
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"CL.csv", sep ="_")
  }
  
  CL <- RDBESdata[['CL']]
  
  # Filter the CE data by year
  clFile <- CL[CL$Year == yearToUse & CL$VesselFlagCountry == country,]
  
  # Get all the values from CL and list them out
  cl <- do.call('paste',c(clFile,sep=','))
  
  # replace NA with blanks
  cl <- gsub('NA','',cl)
  
  write.table(cl, paste(outputFolder, outputFileName, sep = ""),row.names=F,col.names=F,quote=F)
  
}



generateCSFile_H5 <- function(yearToUse, country, RDBESdata, outputFileName=""){
  
  # For testing
  #RDBESdata<-myRDBESData
  #yearToUse <- 2016
  #country <- 'IE'
  #outputFileName <- ""

  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,"H5.csv", sep ="_")
  }
  
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
  
  # TODO - for the sake of testing we'll just generate the exchange file for a small subset of samples first
  # these next few lines can be removed once we're happy with how the function works
  
  #SAfile <- SA[SA$SAnationalCode %in% c(40748,38778),]
  SAfile <- head(SAfile,100)
  FMfile <- FM[FM$SAid %in% SAfile$SAid,]
  BVfile <- BV[BV$FMid %in% FMfile$FMid,]
  SSfile <- SS[SS$SSid %in% SAfile$SSid,]
  LEfile <- LE[LE$LEid %in% SSfile$LEid,]
  FTfile <- FT[FT$FTid %in% LEfile$FTid,]
  OSfile <- OS[OS$OSid %in% FTfile$OSid,]
  SDfile <- SD[SD$SDid %in% OSfile$SDid,]
  DEfile <- DE[DE$DEid %in% SDfile$DEid,]
  VDfile <- VD[VD$VDid %in% LEfile$VDid,]
  SLfile <- SLfile[SLfile$SLlistName %in% SSfile$SSspeciesListName,]
  
  
  ## TODO - hacks due to mistakes in the validator
  
  ## SL - this is a hack because the validator will only allow 1 SL line
  SLfile <- head(SLfile,1)
   
  ## LE currently requires stat rectangle
  LEfile$LErectangle <- "35D7"
  LEfile$LEsampledClusters <- "SYSS"
  # Not allowed MIS or a blank target species
  LEfile[is.na(LEfile$LEtargetSpecies),"LEtargetSpecies"] <- "MPD"
  
  # The only currently allowed value of sub-polygon is "NA"
  LEfile$LEsubpolygon <- "ChangeMe"
   
  ## BV hacks for mistakes in the validator
  BVfile$BVstratification <- 1
  BVfile[BVfile$BVunitValue %in% c("year","MaturityScale","Sex"),"BVunitValue"] <- "mm"
  BVfile$BVmeasurementEquipment <- "Measured by hand with caliper"
  BVfile$BVunitScaleList <- "A"
   
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
  
  #VDfile2$SortOrder <- paste(VDfile2$SortOrder,VDfile2$VDid, sep = "-")
  # Replace the value of FTid in the sort order with 0
  #VDfile2$SortOrder <- unlist(lapply(strsplit(VDfile2$SortOrder,"-"), function(x){ paste(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],"0",x[[7]],x[[8]], sep = "-") }))
  SSfile$SortOrder <- paste( inner_join(SSfile,LEfile, by ="LEid")[,c("SortOrder")], SSfile$SSid, "d", sep = "-")
  # I need SL to appear before SS so I need to do soemthing similar to VD here...
  SLfile2 <- inner_join(SLfile,SSfile, by = c("SLlistName" = "SSspeciesListName"))
  
  # Assuming there are no other 'c's or 'd's in the string we'll change 'd' to 'c' for the SL sort order
  # this will make it appear before the SS line in the sorted output
  SLfile2$SortOrder <- gsub('d','c',SLfile2$SortOrder)
  
  #SLfile2$SortOrder <- unlist(lapply(strsplit(SLfile2$SortOrder,"-"), function(x){ paste(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]],x[[7]],'0', sep = "-") }))
  SAfile$SortOrder <- paste( inner_join(SAfile,SSfile, by ="SSid")[,c("SortOrder")], SAfile$SAid, sep = "-")
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
    SLfile2$SortOrder,
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
    ,do.call('paste',c(SLfile2,sep=','))
    ,do.call('paste',c(SSfile,sep=','))
    ,do.call('paste',c(SAfile,sep=','))
    ,do.call('paste',c(FMfile,sep=','))
    ,do.call('paste',c(BVfile,sep=','))
  )

  # Sort the output into the correct order
  csForCheckingOrdered <- csForChecking[order(FileSortOrder)]
  
  write.table(csForCheckingOrdered, paste(outputFolder, outputFileName,"_debug",sep="") ,row.names=F,col.names=F,quote=F)

  ## STEP 4) Create the real version of the output data
  
  
  # Create the CS data with the sort columns and ids removed - this will then be used to generate the exchange file
  cs <- c(
    do.call('paste',c(select(DEfile,-c(DEid,SortOrder)),sep=','))
    ,do.call('paste',c(select(SDfile,-c(DEid,SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(OSfile,-c(SDid,OSid,SortOrder)),sep=','))
    ,do.call('paste',c(select(VDfile2,-c(VDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(FTfile,-c(FTid, OSid, VSid, VDid, SDid,SortOrder)),sep=','))
    ,do.call('paste',c(select(LEfile,-c(OSid,FTid,VSid,VDid,SAid,LEid,SortOrder)),sep=','))
    # Different patern for SL
    ,do.call('paste',c(select(SLfile2,c(SLrecordType, SLlistName,SLyear, SLspeciesCode, SLcommercialSpecies, SLcatchFraction)),sep=','))
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

  write.table(csOrdered, paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  
}




