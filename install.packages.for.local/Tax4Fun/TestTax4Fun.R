###### Tax4Fun ######

#setwd("C:/Users//Kathrin/Documents/Promotion/Paper/BioinformaticsMoP16S_2014/RPackageWindows/TestData/")
#setwd("/home/kathrin/Downloads/")
#packages<-dir()
#install.packages("TaxFun", repos=NULL)
#library(TaxFun)
setwd("/home/kathrin/Dokumente/Paper/BioinformaticsMoP16S_2014/RPackageWindows/TestData/")

#pathReferenceData <- "C:/Users/Kathrin/Documents/Promotion/Paper/BioinformaticsMoP16S_2014/RPackageWindows/Tax4Fun_Data_Sept2014/SILVA115/"
#pathReferenceData <- "/home/kathrin/Dokumente/Paper/BioinformaticsMoP16S_2014/RPackageWindows/Tax4Fun_Data_Sept2014/SILVA119"
pathReferenceData <- "/home/kathrin/Dokumente/Paper/BioinformaticsMoP16S_2014/RPackageWindows/Tax4Fun_Data_Sept2014/SILVA119_Bernd"

##### QIIME txt import #####

## single file ##
file <- "GNM_0.97_table.txt"
QIIMESingleData <- importQIIMEData(file)

## multiple files ##
files <- c("GNM_0.97_table.txt","HMP_0.97_table.txt","MammalianGordon_0.97_table.txt","Soil_0.97_table.txt")
QIIMEMultipleData <- importQIIMEData(files)

##### QIIME biom import #####

## single file ##
file <- "GNM_0.97_table.biom"
QIIMESingleBiomData <- importQIIMEBiomData(file)

## multiple files ##
files <- c("GNM_0.97_table.biom","HMP_0.97_table.biom","MammalianGordon_0.97_table.biom","Soil_0.97_table.biom")
QIIMEMultipleBiomData <- importQIIMEBiomData(files)

##### SILVAngs import #####

## single file ##
file <- "GN/gn_microbial_mat_sanger---ssu---fingerprint----Total---sim_93---tax_silva---td_20.csv"
SILVAngsSingleData <- importSilvaNgsData(file)

## GN mat files ##
files <- c("GN/gn_microbial_mat_sanger---ssu---fingerprint----Total---sim_93---tax_silva---td_20.csv","GN/gn_microbial_mat_2_sang---ssu---fingerprint----Total---sim_93---tax_silva---td_20.csv")
SILVAngsMultipleData <- importSilvaNgsData(files)

##### Taxy-Pro import #####

## single file ##
file <- "../../TaxyProForTax4Fun/TestInputData/4440963.3.299.screen.passed.fna_upro2_Taxy/4440963.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv"
TaxyProSingleData <- importTaxyProData(file)

file <- "TaxyProForTax4Fun/TestInputData/4440963.3.299.screen.passed.fna_upro2_Taxy/4440963.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv"
TaxyProSingleData <- importTaxyProData(file)

## GN mat files ##
files <- c("TaxyProForTax4Fun/TestInputData/4440963.3.299.screen.passed.fna_upro2_Taxy/4440963.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440964.3.299.screen.passed.fna_upro2_Taxy/4440964.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440965.3.299.screen.passed.fna_upro2_Taxy/4440965.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440966.3.299.screen.passed.fna_upro2_Taxy/4440966.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440967.3.299.screen.passed.fna_upro2_Taxy/4440967.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440968.3.299.screen.passed.fna_upro2_Taxy/4440968.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440969.3.299.screen.passed.fna_upro2_Taxy/4440969.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440970.3.299.screen.passed.fna_upro2_Taxy/4440970.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440971.3.299.screen.passed.fna_upro2_Taxy/4440971.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv",
           "TaxyProForTax4Fun/TestInputData/4440972.3.299.screen.passed.fna_upro2_Taxy/4440972.3.299.screen.passed.fna_upro2_speciesID_MoPPro.csv")
TaxyProMultipleData <- importTaxyProData(files)

##### Tax4Fun functional profiling #####

#### Tax4Fun UProC short read mode ####
## ##
Tax4FunOutput1 <- Tax4Fun(QIIMESingleBiomData,pathReferenceData,TRUE,"UProC",TRUE)
print(Tax4FunOutput1)
Tax4FunOutput2 <- Tax4Fun(QIIMEMultipleBiomData,pathReferenceData,TRUE,"UProC",TRUE)
print(Tax4FunOutput2)

Tax4FunOutput3 <- Tax4Fun(QIIMESingleData,pathReferenceData,TRUE,"UProC",TRUE)
print(Tax4FunOutput3)
Tax4FunOutput4 <- Tax4Fun(QIIMEMultipleData,pathReferenceData,TRUE,"UProC",TRUE)
print(Tax4FunOutput4)

Tax4FunOutput5 <- Tax4Fun(SILVAngsSingleData,pathReferenceData,TRUE,"UProC",TRUE)
Tax4FunOutput5
Tax4FunOutput6 <- Tax4Fun(SILVAngsMultipleData,pathReferenceData,TRUE,"UProC",TRUE)
Tax4FunOutput6

#### Tax4Fun UProC long read mode ####
Tax4FunOutput7 <- Tax4Fun(QIIMESingleBiomData,pathReferenceData,TRUE,"UProC",FALSE)
Tax4FunOutput7
Tax4FunOutput8 <- Tax4Fun(QIIMEMultipleBiomData,pathReferenceData,TRUE,"UProC",FALSE)
Tax4FunOutput8

#### Tax4Fun PAUDA short read mode ####
Tax4FunOutput9 <- Tax4Fun(QIIMESingleBiomData,pathReferenceData,TRUE,"PAUDA",TRUE)
Tax4FunOutput9
Tax4FunOutput10 <- Tax4Fun(QIIMEMultipleBiomData,pathReferenceData,TRUE,"PAUDA",TRUE)
Tax4FunOutput10

#### Tax4Fun PAUDA long read mode ####
Tax4FunOutput11 <- Tax4Fun(QIIMESingleBiomData,pathReferenceData,TRUE,"PAUDA",FALSE)
Tax4FunOutput11
Tax4FunOutput12 <- Tax4Fun(QIIMEMultipleBiomData,pathReferenceData,TRUE,"PAUDA",FALSE)
Tax4FunOutput12

##### Tax4Fun metabolic profiling #####
Tax4FunOutput13 <- Tax4Fun(QIIMESingleBiomData,pathReferenceData,FALSE)
Tax4FunOutput13
Tax4FunOutput14 <- Tax4Fun(QIIMEMultipleBiomData,pathReferenceData,FALSE)
Tax4FunOutput14

##### MoP-Pro metabolic profiling #####
MoPProOutput1 <- MoPPro(TaxyProSingleData,pathReferenceData)
head(MoPProOutput1)
MoPProOutput2 <- MoPPro(TaxyProMultipleData,pathReferenceData)
head(MoPProOutput2)

# ##### SILVA 119 #####
# file <- "HE361_otuTable_0.99_rDNA_Silva119.biom"
# QIIMESingle119BiomData <- importQIIMEBiomData(file)
# Tax4FunOutput14 <- Tax4Fun(QIIMESingle119BiomData,pathReferenceData,FALSE)
# Tax4FunOutput14
# 
# file <- "HE361_otuTable_0.99_rRNA_Silva119.biom"
# QIIMESingle119BiomData <- importQIIMEBiomData(file)
# Tax4FunOutput14 <- Tax4Fun(QIIMESingle119BiomData,pathReferenceData,FALSE)
# Tax4FunOutput14
