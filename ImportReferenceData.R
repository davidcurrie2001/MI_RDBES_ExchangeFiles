library(XML)


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
