#' Import of SILVAngs OTU table(s) in csv format
#' 
#' Import of 16S OTU table(s) predicted by SILVAngs
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://tax4fun.gobics.de/}
#' @param inputFiles (required): a character vector with one or more character string(s) indicating the file location(s) of the csv formatted file(s).
#' @return A list containing the imported SILVAngs OTU table(s) and sample name(s) which can be used as input for the \code{\link{Tax4Fun}} prediction.
#' @export
#' 
importSilvaNgsData <- function(inputFiles){
    # Import SILVAngs data sets and merge them
    if(length(inputFiles)>1){
      for(i in 1:length(inputFiles)){
        tmpSilvaNgsInput <- importSilvaNgsDataFile(inputFiles[i])
        if(i > 1){
          silvaNgsOtuTable <- merge(silvaNgsOtuTable, tmpSilvaNgsInput$otuTable, by=0, all=TRUE)
          rownames(silvaNgsOtuTable) <- silvaNgsOtuTable$Row.names
          silvaNgsOtuTable <- subset(silvaNgsOtuTable, select = -c(Row.names))
          colnames(silvaNgsOtuTable) <- paste("V",1:length(silvaNgsOtuTable),sep="")
          silvaNgsSampleNames <-  c(silvaNgsSampleNames,tmpSilvaNgsInput$sampleNames)
        }else{
          silvaNgsOtuTable <-  tmpSilvaNgsInput$otuTable
          silvaNgsSampleNames <-  tmpSilvaNgsInput$sampleNames
        }
      }
      silvaNgsOtuTable[is.na(silvaNgsOtuTable)] <- 0 
      TaxFunInput <- list(sampleNames=silvaNgsSampleNames,otuTable=silvaNgsOtuTable)
    }else{
      TaxFunInput <- importSilvaNgsDataFile(inputFiles)
    }
  return(TaxFunInput)
}

#' Import of single SILVAngs file in csv format
#' 
#' Import of single SILVAngs file in csv format.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://tax4fun.gobics.de/}
#' @param file (required): a character vector with one character string indicating the file location of the csv formatted file.
#' @return A list containing the imported and modified QIIME OTU table and sample names which can be used as input for the \code{\link{Tax4Fun}} prediction.

importSilvaNgsDataFile <- function(file){
  sampleNames <- unlist(strsplit(readLines(file,n=1),split="\t"))
  otuTable <- read.csv(file, header=FALSE, sep="\t",quote="",dec=".", skip=1,row.names=length(sampleNames)+1)
  inputData <- list(sampleNames=sampleNames, otuTable=otuTable)
  return(inputData)
}