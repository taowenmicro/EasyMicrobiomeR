#' Import of Taxy-Pro profile(s)
#' 
#' Import of taxonomic profile(s) predicted by Taxy-Pro based on the KEGG bacterial and archaeal organisms.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/TaxyPro/}
#' @param inputFiles (required):  a character vector with one or more character string(s) indicating the file location(s) of the Taxy-Pro csv formatted file(s).
#' @return A list containing the imported Taxy-Pro profile(s) and sample name which can be used as input for the \code{\link{MoPPro}} metabolic prediction  (MoP-Pro approach).
#' @export
#' 
importTaxyProData <- function(inputFiles){
  # Import Taxy-Pro data sets and merge them
  if(length(inputFiles)>1){
    for(i in 1:length(inputFiles)){
      tmpTaxyProInput <- importTaxyProDataFile(inputFiles[i])
      if(i > 1){
        TaxyProProfile <- merge(TaxyProProfile, tmpTaxyProInput$taxProfile, by=0, all=TRUE)
        rownames(TaxyProProfile) <- TaxyProProfile$Row.names
        TaxyProProfile <- subset(TaxyProProfile, select = -c(Row.names))
        colnames(TaxyProProfile) <- paste("V",1:length(TaxyProProfile),sep="")
        TaxyProSampleNames <-  c(TaxyProSampleNames,tmpTaxyProInput$sampleNames)
      }else{
        TaxyProProfile <-  tmpTaxyProInput$taxProfile
        TaxyProSampleNames <-  tmpTaxyProInput$sampleNames
      }
    }
    TaxyProProfile[is.na(TaxyProProfile)] <- 0 
    TaxFunInput <- list(sampleNames=TaxyProSampleNames,taxProfile=TaxyProProfile)
  }else{
    TaxFunInput <- importTaxyProDataFile(inputFiles)
  }
  return(TaxFunInput)
}

#' Import of single Taxy-Pro profile
#' 
#' Import of single taxonomic profile predicted by Taxy-Pro based on the KEGG bacterial and archaeal organisms.
#' @references 
#' \url{http://gobics.de/TaxyPro/}
#' @param file (required):  a character vector with one character string indicating the file location of the Taxy-Pro csv formatted file.
#' @return The imported taxonomic profile(s) which can be used as input for the \code{\link{MoPPro}} metabolic prediction (MoP-Pro approach).
#'
importTaxyProDataFile <- function(file){
  TaxyProOutput <- read.table(file,header=1,sep=",",row.names=1)
  inputData <- list(sampleNames=basename(file), taxProfile=TaxyProOutput)
  return(inputData)
}