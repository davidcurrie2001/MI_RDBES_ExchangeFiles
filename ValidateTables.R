

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