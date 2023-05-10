#' Estimation of metabolic profile(s) based on Taxy-Pro predictions from whole metagenome shotgun data
#' 
#' MoP-Pro, a mixture model-based approach for the estimation of pathway abundances, provides a basis for statistical interpretation and fast computation of metabolic profiles of metagenomes.
#' To overcome computationally intense homology searches, MoP-Pro implements a shortcut to estimate the metabolic profile of a metagenome. 
#' The taxonomic profile of the metagenome is linked to a set of pre-computed metabolic reference profiles.
#' The combination of the taxonomic abundance estimates, obtained through the fast method Taxy-Pro, and the
#' metabolic reference profiles, based on the KEGG database, achieves an unrivaled speed of the metabolic profiling approach.
#' @title Estimation of metabolic profile(s) based on Taxy-Pro predictions from whole metagenome shotgun data
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://drops.dagstuhl.de/opus/volltexte/2013/4238/pdf/p001-asshauer.pdf}
#' @param MoPProInput (required): list containing the taxonomic profile and sample name(s), e.g. imported with the function \code{\link{importTaxyProData}}.
#' @param folderReferenceData (required): a character vector with one character string indicating the folder location of the unzipped reference data.
#' @return A vector/matrix containing the estimated metabolic profile(s)
#' @export MoPPro

MoPPro <- function(MoPProInput,folderReferenceData){
  Tax4FunReferenceData <- importTax4FunReferenceData(folderReferenceData)
  FctCat <-  Tax4FunReferenceData$PathwayInformationKEGGBacArchMoPPro

  commonKEGGOrgs <- intersect(colnames(Tax4FunReferenceData$PathwayAbundancesKEGGBacArchMoPPro),row.names(MoPProInput$taxProfile))
  indexInput <- match(commonKEGGOrgs,row.names(MoPProInput$taxProfile))
  indexSILVAToKEGG <- match(commonKEGGOrgs,colnames(Tax4FunReferenceData$PathwayAbundancesKEGGBacArchMoPPro))
  subsetTaxProfile <- as.matrix(MoPProInput$taxProfile[indexInput,])
  subsetPathwayAbundancesKEGGBacArchMoPPro <- as.matrix(Tax4FunReferenceData$PathwayAbundancesKEGGBacArchMoPPro[,indexSILVAToKEGG])

  MoPProProfile <- matrix(data=0,ncol=ncol(MoPProInput$taxProfile),nrow=nrow(subsetPathwayAbundancesKEGGBacArchMoPPro))
  
  
  for(i in 1:ncol(subsetTaxProfile)){
    #Calculate metabolic profile
    MoPProProfile[,i] <- subsetPathwayAbundancesKEGGBacArchMoPPro %*% subsetTaxProfile[,i]
  }
  
  MoPProProfile <- t(MoPProProfile)
  
  FctCat <- FctCat[which(!colSums(MoPProProfile)==0),2]
  MoPProProfile <- MoPProProfile[,which(!colSums(MoPProProfile)==0)]
  if(ncol(subsetTaxProfile)>1){
    colnames(MoPProProfile) <- FctCat
    rownames(MoPProProfile) <- MoPProInput$sampleNames
  }else{
    MoPProProfile <- as.matrix(t(MoPProProfile))
    colnames(MoPProProfile) <- FctCat
    rownames(MoPProProfile) <- MoPProInput$sampleNames
  }

MoPProProfile <- list(MoPProProfile=MoPProProfile)

  
  class(MoPProProfile) <- "MoPPro"
  return(MoPProProfile)
}

#' @S3method print MoPPro
print.MoPPro <- function(x,...){
  cat("MoP-Pro\n")
  cat("Performing metabolic profiling with KEGG Pathway reference profiles.\n")
  cat(c("Data files:\n",paste(rownames(x$MoPProProfile),"\n"),"\n"))
}

#' Import of Tax4Fun and MoP-Pro reference data
#' 
#' Import of Tax4Fun and MoP-Pro reference data.
#' @author Kathrin P. Asshauer \email{kathrin@@gobics.de}
#' @references 
#' \url{http://gobics.de/kathrin/Tax4Fun/Tax4Fun.html}
#' @param folder (required): a character vector with one character string indicating the folder location of the reference data.
#' @return A list containing the imported reference data for \code{\link{Tax4Fun}} and \code{\link{MoPPro}} prediction.

importMoPProReferenceData <- function(folder){
  
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
  tmpReferenceData <- readRDS(paste(pathReferenceData,"PathwayInformationBacArchMoPPro.RData",sep=""))
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