#' Prediction of functional and metabolic profiles from amplicon-data using the Tax4Fun approach
#' 
#' Tax4Fun predicts the functional and metabolic capabilities of 
#' microbial communities based on 16S data samples. Tax4Fun provides a good
#' functional approximation to functional profiles obtained through metagenome
#' sequencing. Tax4Fun can be used as a first
#' instance functional profiling tool for an estimate of the functional
#' capabilities of microbial communities based on amplicon data. 
#' Tax4Fun is applicable to output as obtained through the
#' SILVAngs web server or the application of QIIME against the SILVA database.
#' @name Tax4Fun
#' @docType package
#' @title Prediction of functional profiles from amplicon-data
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/kathrin/Tax4Fun/Tax4Fun.html}
#' \url{http://bioinformatics.oxfordjournals.org/cgi/content/abstract/btv287?ijkey=lkv1hczUZURLzHl&keytype=ref }
#' @param Tax4FunInput (required): list containing the OTU table and sample names, e.g. imported with the functions \code{\link{importQIIMEBiomData}},  \code{\link{importQIIMEData}}, or  \code{\link{importSilvaNgsData}} 
#' @param folderReferenceData (required): a character vector with one character string indicating the folder location of the unzipped reference data.
#' @param fctProfiling (optional): logical; if TRUE (default) the functional capabilities of microbial communities based on 16S data samples are computed using the pre-computed KEGG Ortholog reference profiles, and if FALSE the metabolic capabilities using the pre-computed KEGG Pathway reference profiles according to the MoP approach are computed.
#' @param refProfile (optional): an optional character string giving the method for pre-computing the functional reference profiles. This must be either "UProC" (default) or "PAUDA".
#' @param shortReadMode (optional): logical; if TRUE (default) the functional reference profiles are computed based on 100 bp reads, and if FALSE the reference profiles are computed based on 400 bp reads.
#' @param normCopyNo (optional): logical; if TRUE (default) the taxonomic profiles are normalized by the 16S rRNA gene copy number.
#' @return A list containing the predicted functional or metabolic profiles and the FTU statistics
#' @export

Tax4Fun <- function(Tax4FunInput,folderReferenceData, fctProfiling=TRUE,refProfile="UProC",shortReadMode=TRUE,normCopyNo=TRUE){
  Tax4FunReferenceData <- importTax4FunReferenceData(folderReferenceData)
  
  #Intersect Mapping SILVA to KEGG and user OTU table
  commonOTUs <- intersect(Tax4FunReferenceData$SilvaIDs$V1,row.names(Tax4FunInput$otuTable))
  indexInput <- match(commonOTUs,row.names(Tax4FunInput$otuTable))
  indexSILVAToKEGG <- match(commonOTUs,Tax4FunReferenceData$SilvaIDs$V1)
  subsetOTUTables <- as.matrix(Tax4FunInput$otuTable[indexInput,])
  subsetSILVAToKEGG <- Tax4FunReferenceData$SilvaToKEGGMappingMat[indexSILVAToKEGG,]
  subsetSILVAToKEGG <- as.data.frame(as.matrix(subsetSILVAToKEGG))
  
  #Calculate taxonomic profile for KEGG organisms
  if(fctProfiling){
    FctCat <- Tax4FunReferenceData$KEGGKOInformation
    if(refProfile=="UProC"){
      if(shortReadMode){
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchUProCShort
      }else{
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchUProCLong
      }
    }else if(refProfile=="PAUDA"){
      if(shortReadMode){
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchPAUDAShort
      }else{
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchPAUDALong
      }
    }else{
      print("Invalid functional profiling method. Using default functional profiling method.")
      if(shortReadMode){
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchUProCShort
      }else{
        RefProfile <- Tax4FunReferenceData$FctAbundancesKEGGBacArchUProCLong
      }
    }
    
  }else{
    RefProfile <- Tax4FunReferenceData$PathwayAbundancesKEGGBacArch
    FctCat <- Tax4FunReferenceData$PathwayInformationKEGGBacArch
  }
  
  Tax4FunProfile <- matrix(data=0,nrow=ncol(subsetOTUTables),ncol=nrow(RefProfile))
  for(i in 1:ncol(subsetOTUTables)){
    KEGGTaxProfile <- t(subsetSILVAToKEGG) %*% subsetOTUTables[,i]
    #Normalize by KEGG copy number
    if(normCopyNo){
      NormKEGGTaxProfile <- KEGGTaxProfile/Tax4FunReferenceData$KEGGBacArchCopyNumbers
    }else{
      NormKEGGTaxProfile <- as.data.frame(KEGGTaxProfile)
    }
    NormKEGGTaxProfile <- NormKEGGTaxProfile/sum(NormKEGGTaxProfile)
    #Calculate TaxFun profile
    Tax4FunProfile[i,] <- as.matrix(RefProfile) %*% NormKEGGTaxProfile$V1
  }
  
  if(fctProfiling){
  ###functional profiling
  FctCat <- FctCat[which(!colSums(Tax4FunProfile)==0),c(1,2)]
  Tax4FunProfile <- Tax4FunProfile[,which(!colSums(Tax4FunProfile)==0)]
  Tax4FunProfile <- as.matrix(Tax4FunProfile)
  
  if(ncol(Tax4FunProfile)>1){
    colnames(Tax4FunProfile) <- paste(FctCat$V2,FctCat$V1,sep="; ")
    rownames(Tax4FunProfile) <- Tax4FunInput$sampleNames
  }else{
    Tax4FunProfile <- as.matrix(t(Tax4FunProfile))
    colnames(Tax4FunProfile) <- paste(FctCat$V2,FctCat$V1,sep="; ")
    rownames(Tax4FunProfile) <- Tax4FunInput$sampleNames
  }
  }else{
  ###metabolic profiling
  FctCat <- FctCat[which(!colSums(Tax4FunProfile)==0),c(1,2)]
  Tax4FunProfile <- Tax4FunProfile[,which(!colSums(Tax4FunProfile)==0)]
  Tax4FunProfile <- as.matrix(Tax4FunProfile)
  
  if(ncol(Tax4FunProfile)>1){
    colnames(Tax4FunProfile) <- paste(FctCat$V3,FctCat$V4,sep="; ")
    rownames(Tax4FunProfile) <- Tax4FunInput$sampleNames
  }else{
    Tax4FunProfile <- as.matrix(t(Tax4FunProfile))
    colnames(Tax4FunProfile) <- paste(FctCat$V3,FctCat$V4,sep="; ")
    rownames(Tax4FunProfile) <- Tax4FunInput$sampleNames
  }
  }
  
  
  #colnames(Tax4FunProfile) <- paste(FctCat$V2,FctCat$V1,sep="; ")
  #rownames(Tax4FunProfile) <- Tax4FunInput$sampleNames
  FTU <- 1-colSums(subsetOTUTables)/colSums(Tax4FunInput$otuTable)
  names(FTU) <- Tax4FunInput$sampleNames
  Tax4FunProfile <- list(Tax4FunProfile=Tax4FunProfile, FTU=FTU,fctProfiling=fctProfiling,refProfile=refProfile,shortReadMode=shortReadMode)
  class(Tax4FunProfile) <- "Tax4Fun"
  return(Tax4FunProfile)
} 

# List of packages for session
.packages = c("Matrix", "biom", "qiimer")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)


#' @S3method print Tax4Fun
print.Tax4Fun <- function(x,...){
  cat("Tax4Fun\n")
  cat(c("Data files: ",rownames(x$Tax4FunProfile),"\n"))
  if(x$fctProfiling){
    cat("Performing functional profiling using ")
    if(x$refProfile=="UProC"){
      cat("UProC KEGG Ortholog reference profiles")
    }else{
      cat("PAUDA KEGG Ortholog reference profiles")
    }
    if(x$shortReadMode){
      cat(" in short read mode.\n")
    }else{
      cat(" in long read mode.\n")
    }
  }else{
    cat("Performing metabolic profiling with KEGG Pathway reference profiles.")}
  cat("FTUs:\n")
  print(round(x$FTU,4))
}

#' Import of Tax4Fun and MoP-Pro reference data
#' 
#' Import of Tax4Fun and MoP-Pro reference data.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/kathrin/Tax4Fun/Tax4Fun.html}
#' @param folder (required): a character vector with one character string indicating the folder location of the reference data.
#' @return A list containing the imported reference data for \code{\link{Tax4Fun}} and \code{\link{MoPPro}} prediction.

importTax4FunReferenceData <- function(folder){
  
  if(substr(folder,nchar(folder),nchar(folder))=="/"){
    pathReferenceData <- folder
  }else{
    pathReferenceData <- paste(folder,"/",sep="")
  }
    
  referenceData <- list()
  
  tmpReferenceData <- readRDS(paste(pathReferenceData,"KEGGBacArchTaxInformationMoPPro.RData",sep=""))
  referenceData$KEGGBacArchTaxInformationMoPPro <- tmpReferenceData
  
  tmpReferenceData <- readRDS(paste(pathReferenceData,"PathwayAbundancesKEGGBacArchMoPPro.RData",sep=""))
  referenceData$PathwayAbundancesKEGGBacArchMoPPro <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"PathwayInformationKEGGBacArchMoPPro.RData",sep=""))
  referenceData$PathwayInformationKEGGBacArchMoPPro <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"PathwayAbundancesKEGGBacArch.RData",sep=""))
  referenceData$PathwayAbundancesKEGGBacArch <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"PathwayInformationKEGGBacArch.RData",sep=""))
  referenceData$PathwayInformationKEGGBacArch <- tmpReferenceData
  
  tmpReferenceData <- readRDS(paste(pathReferenceData,"KEGGKOInformation.RData",sep=""))
  referenceData$KEGGKOInformation <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"KEGGBacArchTaxInformation.RData",sep=""))
  referenceData$KEGGBacArchTaxInformation <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"KEGGBacArchCopyNumbers.RData",sep=""))
  referenceData$KEGGBacArchCopyNumbers <- tmpReferenceData
  
  tmpReferenceData <- readRDS(paste(pathReferenceData,"FctAbundancesKEGGBacArchPAUDALong.RData",sep=""))
  referenceData$FctAbundancesKEGGBacArchPAUDALong <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"FctAbundancesKEGGBacArchPAUDAShort.RData",sep=""))
  referenceData$FctAbundancesKEGGBacArchPAUDAShort <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"FctAbundancesKEGGBacArchUProCLong.RData",sep=""))
  referenceData$FctAbundancesKEGGBacArchUProCLong <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"FctAbundancesKEGGBacArchUProCShort.RData",sep=""))
  referenceData$FctAbundancesKEGGBacArchUProCShort <- tmpReferenceData
  
  
  tmpReferenceData <- readRDS(paste(pathReferenceData,"SilvaToKEGGMappingMat.RData",sep=""))
  referenceData$SilvaToKEGGMappingMat <- tmpReferenceData
  tmpReferenceData <- readRDS(paste(pathReferenceData,"SilvaIDs.RData",sep=""))
  referenceData$SilvaIDs <- tmpReferenceData
  return(referenceData)
}
