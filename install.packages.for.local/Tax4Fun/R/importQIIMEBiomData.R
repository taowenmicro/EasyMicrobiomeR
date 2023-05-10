#' Import of QIIME 16S OTU table(s) in BIOM format
#' 
#' Import of 16S OTU table(s) predicted by QIIME based on the SILVA database and removal of suffixes from taxonomic tags containing uncultured archaeon/organisms/bacterium/crenarchaeote/euryarchaeote.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/kathrin/Tax4Fun/Tax4Fun.html}
#' @param inputFiles (required): a character vector with one or more character string(s) indicating the file location(s) of the BIOM formatted file(s).
#' @return A list containing the imported and modified QIIME OTU table(s) and sample name(s) which can be used as input for the \code{\link{Tax4Fun}} prediction.
#' @export
#' 
importQIIMEBiomData <- function(inputFiles){
  #require(biom)
  # Import QIIME data sets and merge them
  if(length(inputFiles)>1){
    for(i in 1:length(inputFiles)){
      tmpQIIMEInput <- importQIIMEBiomDataFile(inputFiles[i])
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
    tmpQIIMEInput <- importQIIMEBiomDataFile(inputFiles)
    TaxFunInput <- list(sampleNames=tmpQIIMEInput$sampleNames,otuTable=as.matrix(tmpQIIMEInput$otuTable))
    #TaxFunInput <- list(sampleNames=inputFiles,otuTable=as.matrix(tmpQIIMEInput$otuTable))
    #TaxFunInput <- importQIIMEBiomDataFile(inputFiles)
  }
  return(TaxFunInput)
}

#' Import of single QIIME 16S OTU table in BIOM format
#' 
#' Import of 16S OTU table predicted by QIIME based on the SILVA database and removal of suffixes from taxonomic tags containing uncultured archaeon/organisms/bacterium/crenarchaeote/euryarchaeote.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/kathrin/Tax4Fun/Tax4Fun.html}
#' @param file (required): a character vector with one character string indicating the file location of the BIOM formatted file.
#' @return A list containing the imported and modified QIIME OTU table and sample name which can be used as input for the \code{\link{Tax4Fun}} prediction.

importQIIMEBiomDataFile <- function(file){
  #require(biom)
  qiimeBiomData = read_biom(file)
  taxProfile <- biom_data(qiimeBiomData)
  taxSeqIds <- observation_metadata(qiimeBiomData)
  SilvaIds <- vector(mode = "character", length=length(taxSeqIds))
  for(i in 1:length(taxSeqIds)){
    seqId <- taxSeqIds[i]
    SilvaIds[i] <- paste(unlist(seqId),collapse=";")
    SilvaIds[i] <- paste(SilvaIds[i],";",sep="")
    
  }

  ModSilvaIds <- gsub(";uncultured archaeon","",SilvaIds)
  ModSilvaIds <- gsub(";uncultured organism","",ModSilvaIds)
  ModSilvaIds <- gsub(";uncultured bacterium","",ModSilvaIds)
  ModSilvaIds <- gsub(";uncultured crenarchaeote","",ModSilvaIds)
  ModSilvaIds <- gsub(";uncultured euryarchaeote","",ModSilvaIds)
  
  otuTable <- rowsum(as.matrix(taxProfile),ModSilvaIds)
  if(ncol(otuTable)==1){
    inputData <- list(sampleNames=file, otuTable=otuTable)
  }else{
    inputData <- list(sampleNames=colnames(otuTable), otuTable=otuTable)
  }
  return(inputData)
}