# Example Tax4Fun:
#
# Prediction of the functional profile for the 
# Guerrero negro hypersaline microbial mat data samples (Harris et al., 2013; Kunin et al., 2008)
# based on amplicon data with the Tax4Fun approach using UProC based functional reference profiles
data(GN16SData)
GN16SData$sampleNames
#Tax4FunOutput <- Tax4Fun(GN16SData,fctProfiling = TRUE, refProfile = "UProC",
#  shortReadMode = FALSE)
#print(Tax4FunOutput)
  
# Prediction of the metabolic profile for the 
# Guerrero negro hypersaline microbial mat data samples (Harris et al., 2013; Kunin et al., 2008)
# based on amplicon data with the Tax4Fun approach usning MoP-Pro based metabolic reference profiles
#Tax4FunOutput <- Tax4Fun(GN16SData,fctProfiling = FALSE)
#print(Tax4FunOutput)