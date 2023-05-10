#' Import of QIIME 16S OTU table(s) in txt format
#' 
#' Import of 16S OTU table(s) predicted by QIIME based on the SILVA database and removal of suffixes from taxonomic tags containing uncultured archaeon/organisms/bacterium/crenarchaeote/euryarchaeote.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://tax4fun.gobics.de/}
#' @param inputFiles (required): a character vector with one or more character string(s) indicating the file location(s) of the txt formatted file(s).
#' @return A list containing the imported and modified QIIME OTU table(s) and sample name(s) which can be used as input for the \code{\link{Tax4Fun}} prediction.
#' @export
#' 
importQIIMEData <- function(inputFiles){
#require(qiimer)
  # Import QIIME data sets and merge them
  if(length(inputFiles)>1){
    for(i in 1:length(inputFiles)){
      tmpQIIMEInput <- importQIIMEDataFile(inputFiles[i])
      if(i > 1){
        qiimeOtuTable <- merge(qiimeOtuTable, tmpQIIMEInput$otuTable, by=0, all=TRUE)
        rownames(qiimeOtuTable) <- qiimeOtuTable$Row.names
        qiimeOtuTable <- subset(qiimeOtuTable, select = -c(Row.names))
        colnames(qiimeOtuTable) <- paste("V",1:length(qiimeOtuTable),sep="")
        qiimeSampleNames <-  c(qiimeSampleNames,tmpQIIMEInput$sampleNames)
      }else{
        qiimeOtuTable <-  tmpQIIMEInput$otuTable
        qiimeSampleNames <-  tmpQIIMEInput$sampleNames
      }
    }
    qiimeOtuTable[is.na(qiimeOtuTable)] <- 0 
    TaxFunInput <- list(sampleNames=qiimeSampleNames,otuTable=qiimeOtuTable)
  }else{
    TaxFunInput <- importQIIMEDataFile(inputFiles)
  }
return(TaxFunInput)
}

#' Import of single QIIME 16S OTU table in txt format
#' 
#' Import of 16S OTU table predicted by QIIME based on the SILVA database and removal of suffixes from taxonomic tags containing uncultured archaeon/organisms/bacterium/crenarchaeote/euryarchaeote.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://tax4fun.gobics.de/}
#' @param file (required): a character vector with one character string indicating the file location of the txt formatted file.
#' @return A list containing the imported and modified QIIME OTU table and sample name which can be used as input for the \code{\link{Tax4Fun}} prediction.

importQIIMEDataFile <- function(file){
  #require(qiimer)
  qiimeOtuTable <- read_qiime_otu_table(file)
  
  #ModSilvaIds <- gsub("_[0-9]+.[0-9]+_[0-9]+_[0-9]+","",ArchaeaOtuTable$metadata)
  ModSilvaIds <- gsub("uncultured archaeon","",qiimeOtuTable$metadata)
  ModSilvaIds <- gsub("uncultured organism","",ModSilvaIds)
  ModSilvaIds <- gsub("uncultured bacterium","",ModSilvaIds)
  ModSilvaIds <- gsub("uncultured crenarchaeote","",ModSilvaIds)
  ModSilvaIds <- gsub("uncultured euryarchaeote","",ModSilvaIds)
  ModSilvaIds <- gsub("; ",";",ModSilvaIds)
  
  otuTable <- rowsum(data.frame(qiimeOtuTable$counts),ModSilvaIds)
  inputData <- list(sampleNames=qiimeOtuTable$sample_ids, otuTable=otuTable)
  return(inputData)
}