## Example of how to import exchange files - assumes exchange files are saved in
## the folder ./output/uploaded

# Load our functions
source("RDBES_Functions.R")
# Temporary fix required for a function from icesVocab - otherwise the function breaks when it tries to download the EDMO code list (or any list containing carriage returns)
source("tempIcesVocabFix.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 30/8/2021 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Sex','type'] <- 'tSEXCO'

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

myExchangeFileVD <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HVD.csv' )
myExchangeFileSL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_HSL.csv' )
myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'output/IE_2019_tst_H1.csv',RequiredTables = allRequiredTables )

# Check that we can generate a valid file from the data we just read in
generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2019, country = 'IE', RDBESdata = myExchangeFileH1, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
