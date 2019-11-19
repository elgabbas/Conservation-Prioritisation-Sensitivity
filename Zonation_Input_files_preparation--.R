#### |----------------------------------------| ####
# 0 introduction -----------------------------------
#### |----------------------------------------| ####

### This code aimed at the preparation of zonation files similar to those used in El-Gabbas et al. paper: Spatial conservation prioritisation in data-poor countries: a quantitative sensitivity analysis.
### Manuscript: El-Gabbas, Gilbert, and Dormann. Spatial conservation prioritisation in data-poor countries: a quantitative sensitivity analysis. BMC Ecology
### Appendix S2: R script for automatizing the creation of input files to run the sensitivity analyses of Zonation 
# written by Ahmed El-Gabbas (elgabbas[@]outlook.com - https://elgabbas.github.io/), March 2018; updated October 2019

###||||||||||||||||||||||||||||||||||||||||||||||||

# In this reproducible code, I will use only 5 species for each of the surrogate groups used (butterflies, mammals, reptiles); 15 species in total (all species analyses)
# In addition to this file, there are:
	# 4 '.csv' files for an example weights used
	# 'ZigInputFiles' folder contains 4 .dat files to be used in the analyses
	# 'ZigInputFiles\Maps' contains
		# example predicted distribution maps for each combination of species distribution models and sampling bias correction
		# CostLayer.tif: urban area to be given low priority 
		# Mask_PAs.tif: protected areas to be given high priority in some analyses
		# BQPcurves.txt: Data for different BQP curves used (see below)

###||||||||||||||||||||||||||||||||||||||||||||||||
#### . ####
###||||||||||||||||||||||||||||||||||||||||||||||||

#### |----------------------------------------| ####
# 1 Abbreviations used -----------------------------
#### |----------------------------------------| ####

# Weights
  # WTWO - without weights 
  # WTLoc - with national red-list status weighting
  # NoUncert - without predictive uncertainty
  # Uncert - with predictive uncertainty
# BQP - Boundary Quality Penalty
  # BQP-WO - no connectivity analyses
  # BQP-Low / BQP-Med / BQP-Strng - low/medium/strong BQP curves
# Modelling algorithms -- Mxnt: Maxent; EN: elastic net
# Sampling bias
	# BiasWO - environment-only models; no bias correction
	# Bias0 - bias-free predictions
# Surrogate groups
	# MMls: mammals; Rep: reptiles; Butr: butterflies; AllSP: all three groups together

###||||||||||||||||||||||||||||||||||||||||||||||||
#### . ####
###||||||||||||||||||||||||||||||||||||||||||||||||

#### |----------------------------------------| ####
# 2. Preparing .spp files -----------------------------------------
#### |----------------------------------------| ####

# 768 ".spp" files to be created using this code
# 4 weights X 12 BQP combinations X 2 Modelling algorithms X 2 Sampling bias scenarios X 4 surrogate options

# 4 weighting options
# WTWO-NoUncert: no weighting; without predictive uncertainty
# WTLoc-NoUncert: weighting with national red-list status weighting; without predictive uncertainty
# WTWO-Uncert: : no weighting; with predictive uncertainty
# WTLoc-Uncert: : weighting with national red-list status weighting; with predictive uncertainty
Weights <- c("WTWO-NoUncert", "WTLoc-NoUncert", "WTWO-Uncert", "WTLoc-Uncert")

# 4 BQP curves and 3 radii
BQP_Curve <- expand.grid(c("BQP-WO", "BQP-Low", "BQP-Med", "BQP-Strng"), 1:3)
BQP_Curve <- paste0(BQP_Curve[,1], "-", BQP_Curve[,2])

# 2 Modelling algorithms X 2 Sampling bias scenarios X 4 surrogate options
SpeciesMaps <- expand.grid(c("Mxnt", "EN"),
                           c("BiasWO", "Bias0"),
                           c("MMls", "Rep", "Butr", "AllSP")) 
SpeciesMaps <- paste0(SpeciesMaps[,1], "-",
                      SpeciesMaps[,2], "-",
                      SpeciesMaps[,3])

AllSppFiles <- expand.grid(Weights = Weights,
                           BQP_Curve = BQP_Curve,
                           SpeciesMaps = SpeciesMaps)
AllSppFiles <- paste0(AllSppFiles[,1], "__",
                      AllSppFiles[,2], "__",
                      AllSppFiles[,3], ".spp")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

AllSppData <- vector(mode = "list", length = length(AllSppFiles))
names(AllSppData) <- AllSppFiles

# Different spp files has different number of rows based on the number of species to be studied
# 5 species for analyses of each of butterflies, reptiles, and mammals; 15 species for all species analyses
Index_AllSP <- grep(pattern = "AllSP", x = names(AllSppData))
Index_MMls <- grep(pattern = "MMls", x = names(AllSppData))
Index_Rep <- grep(pattern = "Rep", x = names(AllSppData))
Index_Butr <- grep(pattern = "Butr", x = names(AllSppData))

for(i in Index_AllSP){
  AllSppData[[i]] <- data.frame(matrix(ncol = 6, nrow = 15))
}

for(i in c(Index_MMls, Index_Rep, Index_Butr)){
  AllSppData[[i]] <- data.frame(matrix(ncol = 6, nrow=5))
}

AllSppData <- lapply(AllSppData, FUN = function(x){
  names(x) <- c("Weight", "Alpha", "BQP_CurveNum", "BQP_Buff", "ABF_Rate", "SpeciesMaps")
  x$Alpha <- 1
  x$ABF_Rate <- 1
  x
  })

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# >> 2.1 Adding relevant BQP radius -----------------------------------------

Index_BQP_Buff1 <- grep(pattern = "__BQP-.*-1__", x = names(AllSppData))
Index_BQP_Buff2 <- grep(pattern = "__BQP-.*-2__", x = names(AllSppData))
Index_BQP_Buff3 <- grep(pattern = "__BQP-.*-3__", x = names(AllSppData))
for(i in Index_BQP_Buff1){ AllSppData[[i]]$BQP_Buff <- 1 }
for(i in Index_BQP_Buff2){ AllSppData[[i]]$BQP_Buff <- 2 }
for(i in Index_BQP_Buff3){ AllSppData[[i]]$BQP_Buff <- 3 }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# >> 2.2 Adding relevant BQP curve number -----------------------------------------

BQP_CurveNum1 <- grep(pattern = "BQP-WO", x = names(AllSppData))
BQP_CurveNum2 <- grep(pattern = "BQP-Low", x = names(AllSppData))
BQP_CurveNum3 <- grep(pattern = "BQP-Med", x = names(AllSppData))
BQP_CurveNum4 <- grep(pattern = "BQP-Strng", x = names(AllSppData))
for(i in BQP_CurveNum1){ AllSppData[[i]]$BQP_CurveNum <- 1 }
for(i in BQP_CurveNum2){ AllSppData[[i]]$BQP_CurveNum <- 2 }
for(i in BQP_CurveNum3){ AllSppData[[i]]$BQP_CurveNum <- 3 }
for(i in BQP_CurveNum4){ AllSppData[[i]]$BQP_CurveNum <- 4 }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# >> 2.3 Adding relevant species data -----------------------------------------

# >>>> 2.3.1 All species -----------------------------------------

SpNums <- 1:15
FileNames_AllSp_Mxnt_NoBias <- paste0("ZigInputFiles/Maps/Mxnt_NoBias/Mxnt_NoBias_Sp", SpNums, ".tif")
FileNames_AllSp_Mxnt_Bias0 <- paste0("ZigInputFiles/Maps/Mxnt_Bias0/Mxnt_Bias0_Sp", SpNums, ".tif")
FileNames_AllSp_En_NoBias <- paste0("ZigInputFiles/Maps/En_NoBias/En_NoBias_Sp", SpNums, ".tif")
FileNames_AllSp_En_Bias0 <- paste0("ZigInputFiles/Maps/En_Bias0/En_Bias0_Sp", SpNums, ".tif")
Index_AllSP_Mxnt_NoBias <- grep(pattern = "Mxnt-BiasWO-AllSP", x = names(AllSppData))
Index_AllSP_Mxnt_Bias0 <- grep(pattern = "Mxnt-Bias0-AllSP", x = names(AllSppData))
Index_AllSP_En_NoBias <- grep(pattern = "EN-BiasWO-AllSP", x = names(AllSppData))
Index_AllSP_En_Bias0 <- grep(pattern = "EN-Bias0-AllSP", x = names(AllSppData))
for (i in Index_AllSP_Mxnt_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_AllSp_Mxnt_NoBias }
for (i in Index_AllSP_Mxnt_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_AllSp_Mxnt_Bias0 }
for (i in Index_AllSP_En_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_AllSp_En_NoBias }
for (i in Index_AllSP_En_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_AllSp_En_Bias0 }

# >>>> 2.3.2 Reptiles -----------------------------------------

SpNums <- 1:5
FileNames_REPT_Mxnt_NoBias <- paste0("ZigInputFiles/Maps/Mxnt_NoBias/Mxnt_NoBias_Sp", SpNums, ".tif")
FileNames_REPT_Mxnt_Bias0 <- paste0("ZigInputFiles/Maps/Mxnt_Bias0/Mxnt_Bias0_Sp", SpNums, ".tif")
FileNames_REPT_En_NoBias <- paste0("ZigInputFiles/Maps/En_NoBias/En_NoBias_Sp", SpNums, ".tif")
FileNames_REPT_En_Bias0 <- paste0("ZigInputFiles/Maps/En_Bias0/En_Bias0_Sp", SpNums, ".tif")
Index_REPT_Mxnt_NoBias <- grep(pattern = "Mxnt-BiasWO-Rep", x = names(AllSppData))
Index_REPT_Mxnt_Bias0 <- grep(pattern = "Mxnt-Bias0-Rep", x = names(AllSppData))
Index_REPT_En_NoBias <- grep(pattern = "EN-BiasWO-Rep", x = names(AllSppData))
Index_REPT_En_Bias0 <- grep(pattern = "EN-Bias0-Rep", x = names(AllSppData))
for (i in Index_REPT_Mxnt_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_REPT_Mxnt_NoBias }
for (i in Index_REPT_Mxnt_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_REPT_Mxnt_Bias0 }
for (i in Index_REPT_En_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_REPT_En_NoBias }
for (i in Index_REPT_En_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_REPT_En_Bias0 }

# >>>> 2.3.3 Butterflies -----------------------------------------

SpNums <- 6:10
FileNames_BTTR_Mxnt_NoBias <- paste0("ZigInputFiles/Maps/Mxnt_NoBias/Mxnt_NoBias_Sp", SpNums, ".tif")
FileNames_BTTR_Mxnt_Bias0 <- paste0("ZigInputFiles/Maps/Mxnt_Bias0/Mxnt_Bias0_Sp", SpNums, ".tif")
FileNames_BTTR_En_NoBias <- paste0("ZigInputFiles/Maps/En_NoBias/En_NoBias_Sp", SpNums, ".tif")
FileNames_BTTR_En_Bias0 <- paste0("ZigInputFiles/Maps/En_Bias0/En_Bias0_Sp", SpNums, ".tif")
Index_BTTR_Mxnt_NoBias <- grep(pattern = "Mxnt-BiasWO-Butr", x = names(AllSppData))
Index_BTTR_Mxnt_Bias0 <- grep(pattern = "Mxnt-Bias0-Butr", x = names(AllSppData))
Index_BTTR_En_NoBias <- grep(pattern = "EN-BiasWO-Butr", x = names(AllSppData))
Index_BTTR_En_Bias0 <- grep(pattern = "EN-Bias0-Butr", x = names(AllSppData))
for (i in Index_BTTR_Mxnt_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_BTTR_Mxnt_NoBias }
for (i in Index_BTTR_Mxnt_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_BTTR_Mxnt_Bias0 }
for (i in Index_BTTR_En_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_BTTR_En_NoBias }
for (i in Index_BTTR_En_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_BTTR_En_Bias0 }

# >>>> 2.3.4 Mammals -----------------------------------------

SpNums <- 11:15
FileNames_MMLS_Mxnt_NoBias <- paste0("ZigInputFiles/Maps/Mxnt_NoBias/Mxnt_NoBias_Sp", SpNums, ".tif")
FileNames_MMLS_Mxnt_Bias0 <- paste0("ZigInputFiles/Maps/Mxnt_Bias0/Mxnt_Bias0_Sp", SpNums, ".tif")
FileNames_MMLS_En_NoBias <- paste0("ZigInputFiles/Maps/En_NoBias/En_NoBias_Sp", SpNums, ".tif")
FileNames_MMLS_En_Bias0 <- paste0("ZigInputFiles/Maps/En_Bias0/En_Bias0_Sp", SpNums, ".tif")
Index_MMLS_Mxnt_NoBias <- grep(pattern = "Mxnt-BiasWO-MMls", x = names(AllSppData))
Index_MMLS_Mxnt_Bias0 <- grep(pattern = "Mxnt-Bias0-MMls", x = names(AllSppData))
Index_MMLS_En_NoBias <- grep(pattern = "EN-BiasWO-MMls", x = names(AllSppData))
Index_MMLS_En_Bias0 <- grep(pattern = "EN-Bias0-MMls", x = names(AllSppData))
for (i in Index_MMLS_Mxnt_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_MMLS_Mxnt_NoBias }
for (i in Index_MMLS_Mxnt_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_MMLS_Mxnt_Bias0 }
for (i in Index_MMLS_En_NoBias){ AllSppData[[i]]$SpeciesMaps <- FileNames_MMLS_En_NoBias }
for (i in Index_MMLS_En_Bias0){ AllSppData[[i]]$SpeciesMaps <- FileNames_MMLS_En_Bias0 }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# >> 2.4 Adding relevant weights -----------------------------------------

# these files contain the different values of per-species weight
WeightsData_EN_Bias0 <- read.csv("ZigInputFiles/Data/WeightsData_EN_Bias0.csv", sep = ",", header = T)
WeightsData_EN_WOBias <- read.csv("ZigInputFiles/Data/WeightsData_EN_WOBias.csv", sep = ",", header = T)
WeightsData_Mxnt_Bias0 <- read.csv("ZigInputFiles/Data/WeightsData_Mxnt_Bias0.csv", sep = ",", header = T)
WeightsData_Mxnt_WOBias <- read.csv("ZigInputFiles/Data/WeightsData_Mxnt_WOBias.csv", sep = ",", header = T)

# >>>> 2.4.1 No predictive uncertainty -----------------------------------------

# >>>>>> 2.4.1.1. Equal weights for all species (without Red-List weight) -----------------------------------------
Index_WTWO_NoUncert <- grep(pattern = "WTWO-NoUncert", x = names(AllSppData))
for (i in Index_WTWO_NoUncert){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO[11:15],
                              print("ERROR")))))
}

# >>>>>> 2.4.1.2. with Red-List weight -----------------------------------------
Index_WTLoc_NoUncert <- grep(pattern = "WTLoc-NoUncert", x = names(AllSppData))
for (i in Index_WTLoc_NoUncert){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local[11:15],
                              print("ERROR")))))
}

# >>>> 2.4.2 With predictive uncertainty -----------------------------------------

# >>>>>> 2.4.2.1. without Red-List weight -----------------------------------------
Index_WTWO_Uncert_Mxnt_BiasWO <- grep(pattern = "WTWO-Uncert.*Mxnt-BiasWO", x = names(AllSppData))
for (i in Index_WTWO_Uncert_Mxnt_BiasWO){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_WO_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTWO_Uncert_EN_BiasWO <- grep(pattern = "WTWO-Uncert.*EN-BiasWO", x = names(AllSppData))
for (i in Index_WTWO_Uncert_EN_BiasWO){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_WO_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_WO_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_WO_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_WO_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTWO_Uncert_Mxnt_Bias0 <- grep(pattern = "WTWO-Uncert.*Mxnt-Bias0", x = names(AllSppData))
for (i in Index_WTWO_Uncert_Mxnt_Bias0){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_WO_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_WO_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_WO_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_WO_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTWO_Uncert_EN_Bias0 <- grep(pattern = "WTWO-Uncert.*EN-Bias0", x = names(AllSppData))
for (i in Index_WTWO_Uncert_EN_Bias0){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_WO_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_WO_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_WO_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_WO_Uncert[11:15],
                              print("ERROR")))))
}

# >>>>>> 2.4.2.2. with Red-List weight -----------------------------------------
Index_WTLoc_Uncert_Mxnt_BiasWO <- grep(pattern = "WTLoc-Uncert.*Mxnt-BiasWO", x = names(AllSppData))
for (i in Index_WTLoc_Uncert_Mxnt_BiasWO){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_WOBias$WT_Local_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTLoc_Uncert_EN_BiasWO <- grep(pattern = "WTLoc-Uncert.*EN-BiasWO", x = names(AllSppData))
for (i in Index_WTLoc_Uncert_EN_BiasWO){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_Local_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_Local_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_Local_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_EN_WOBias$WT_Local_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTLoc_Uncert_Mxnt_Bias0 <- grep(pattern = "WTLoc-Uncert.*Mxnt-Bias0", x = names(AllSppData))
for (i in Index_WTLoc_Uncert_Mxnt_Bias0){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_Local_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_Local_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_Local_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_Mxnt_Bias0$WT_Local_Uncert[11:15],
                              print("ERROR")))))
}

Index_WTLoc_Uncert_EN_Bias0 <- grep(pattern = "WTLoc-Uncert.*EN-Bias0", x = names(AllSppData))
for (i in Index_WTLoc_Uncert_EN_Bias0){
  ifelse(grepl(pattern = "AllSP", x = names(AllSppData[i])),
         AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_Local_Uncert,
         ifelse(grepl(pattern = "Rep", x = names(AllSppData[i])),
                AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_Local_Uncert[1:5],
                ifelse(grepl(pattern = "Butr", x = names(AllSppData[i])),
                       AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_Local_Uncert[6:10],
                       ifelse(grepl(pattern = "MMls", x = names(AllSppData[i])),
                              AllSppData[[i]]$Weight <- WeightsData_EN_Bias0$WT_Local_Uncert[11:15],
                              print("ERROR")))))
}

###||||||||||||||||||||||||||||||||||||||||||||||||

# >> 2.5 Exporting all .spp files -----------------------------------------

for (i in 1:length(AllSppData)){
  write.table(x = AllSppData[[i]],
              file = paste0("output_spp/", names(AllSppData[i])),
              sep="\t", row.names = FALSE, col.names = FALSE)
  # print(i)
}


###||||||||||||||||||||||||||||||||||||||||||||||||
#### . ####
###||||||||||||||||||||||||||||||||||||||||||||||||

#### |----------------------------------------| ####
# 3 .dat files -------------------------------------
#### |----------------------------------------| ####

# 8 files are available in 'Maps' folder (they were written outside R)



###||||||||||||||||||||||||||||||||||||||||||||||||
#### . ####
###||||||||||||||||||||||||||||||||||||||||||||||||

#### |----------------------------------------| ####
# 4 Preparing .sh files ----------------------------
#### |----------------------------------------| ####

# These files were prepared to work in a Centos Linux cluster servers; using Slurm Workload Manager
# For windows version, .bat files are needed to be (slight differently) prepared instead of .sh files
# an .sh file for each combination of spp and dat file will be prepared 

SppFiles <- list.files(path = "output_spp\\", pattern=".*.spp", full.names=FALSE)
SppFilesInit <- gsub(pattern = ".spp", replacement = "", x = SppFiles)
datFiles <- list.files(path = "ZigInputFiles\\Dat\\",
                       pattern=".*.dat", full.names=FALSE)
datFilesInit <- gsub(pattern = ".dat", replacement = "", x = datFiles)

AllShFiles <- expand.grid(dat = datFilesInit,
                          spp = SppFilesInit,
                          stringsAsFactors = FALSE)

AllShFiles$sh <- paste0(AllShFiles$dat, "__", AllShFiles$spp, ".sh")
AllShFiles$OutFile <- paste0("output/", AllShFiles$dat, "__",
                             AllShFiles$spp, ".txt")
AllShFiles$dat <- paste0(AllShFiles$dat, ".dat")
AllShFiles$spp <- paste0(AllShFiles$spp, ".spp")

for (i in 1:nrow(AllShFiles)){
  Currdat <- AllShFiles[i,"dat"]
  CurrSpp <- AllShFiles[i,"spp"]
  CurrOutSh <- AllShFiles[i,"sh"]
  CurrOutTxt <- AllShFiles[i,"OutFile"]
  cat(paste0('#!/bin/bash\nzig4 -r "',
             Currdat, '" "', CurrSpp, '" "',
             CurrOutTxt, '" ', '0 1 1 1 --use-threads=16'),
      file = paste0("output_sh\\", CurrOutSh))
  rm(Currdat, CurrSpp, CurrOutSh, CurrOutTxt)
  # cat(i)
  # cat("_")
}

# When no BQP connectivity analyses were performed, the use of BQP radius field become redundant - these files needed to be removed
ToOmitFiles <- c(list.files(path = "output_sh\\",
                            pattern=".*BQP-WO-2.*", full.names=T),
                 list.files(path = "output_sh\\",
                            pattern=".*BQP-WO-3.*", full.names=T))
invisible(file.remove(ToOmitFiles))

# Similar code to produce .bat files to work under windows environment
AllShFiles$bat <- paste0(AllShFiles$dat, "__", AllShFiles$spp, ".bat")
for (i in 1:nrow(AllShFiles)){
  Currdat <- AllShFiles[i,"dat"]
  CurrSpp <- AllShFiles[i,"spp"]
  CurrOutBat <- AllShFiles[i,"bat"]
  CurrOutTxt <- AllShFiles[i,"OutFile"]
  cat(paste0('call zig4.exe -r "', Currdat, '" "', CurrSpp,
             '" "', CurrOutTxt, '" ', '0 1 1 1 --use-threads=16'),
      file = paste0("output_bat\\", CurrOutBat))
  rm(Currdat, CurrSpp, CurrOutBat, CurrOutTxt)
  # cat(i)
  # cat("_")
}

ToOmitFiles <- c(list.files(path = "output_bat\\",
                            pattern=".*BQP-WO-2.*", full.names=T),
                 list.files(path = "output_bat\\",
                            pattern=".*BQP-WO-3.*", full.names=T))
file.remove(ToOmitFiles)


# There should be 2,560 .sh files: 4 surrogate taxa (butterflies vs reptiles vs mammals vs the three groups together) X 2 modelling algorithms (Maxent vs elastic net) X 2 biases (without vs with sampling-bias correction) X  2 cell-removal rules (ABF vs CAZ) × 4 weighting schemes (with vs without Red-List or predictive-uncertainty weights) X 10 connectivity options X 2 PA-integration options (without vs with PA masking)
# and There should be 640 .spp files



###||||||||||||||||||||||||||||||||||||||||||||||||
###||||||||||||||||||||||||||||||||||||||||||||||||

#### . ####

#### |----------------------------------------| ####
# 5 Running on the server: creating .moab files----------------------------
#### |----------------------------------------| ####

# This code creates .moab files to facilitate the parallel run of 2,560 Zonation runs

# >> 5.1 Analyses without PAs masking -----------------------------------------

# 1280 Zonation runs to be split into 64 files, each run 20 Zonation run in parallel
FirstRuns <- list.files(path = "output_sh\\", pattern=".*.sh")
FirstRuns <- FirstRuns[grep(pattern = ".*_MaskNo__", x = FirstRuns)]
FirstRuns1 <- split(FirstRuns,
                    ceiling(seq_along(1:length(FirstRuns))/20))
names(FirstRuns1) <- paste0("Task_", 1:64, ".moab")

for (ii in 1:length(FirstRuns1)){
  CurrFile <- paste0("output_moab\\", names(FirstRuns1)[ii])
  cat("#!/bin/sh\n", file = CurrFile, append=FALSE)
  cat(paste0("########### Begin MOAB/Slurm header ##########\n#\n# Give job a reasonable name\n#MOAB -N Task_", 0+ii), file = CurrFile, append=TRUE)
  cat(paste0('\n#\n# Request number of nodes and CPU cores per node for job\n#MOAB -l nodes=1:ppn=16, pmem=48gb\n#\n# Estimated wallclock time for job\n#MOAB -l walltime=00:12:00:00\n#\n# Write standard output and errors in same file\n#MOAB -j oe\n#\n# Send mail when job begins, aborts and ends\n#MOAB -m bae\n#\n########### End MOAB header ##########\n\necho "Working Directory:                    $PWD"\necho "Running on host                       $HOSTNAME"\necho "Job id:                               $MOAB_JOBID"\necho "Job name:                             $MOAB_JOBNAME"\necho "Number of nodes allocated to job:     $MOAB_NODECOUNT"\necho "Number of cores allocated to job:     $MOAB_PROCCOUNT"\n\nmodule load geo/zonation/4.0.0\ncd ZonationEgypt/\n\n'), file = CurrFile, append=TRUE)
  for(i in 1:length(FirstRuns1[[ii]])){
    cat(paste0('bash "', FirstRuns1[[ii]][i], '" &\n'), file = CurrFile, append=TRUE)
  }
  cat(paste0('wait\necho "All are complete"'), file = CurrFile, append=TRUE)
}
# Tasks <- paste0("msub Task_", 1:128, ".moab")
# cat(Tasks, file = "output_tasks\\Tasks1_128.txt", sep = "\n")



# >> 5.2 Analyses with PAs masking -----------------------------------------

FirstRuns <- list.files(path = "output_sh\\", pattern=".*.sh")
FirstRuns <- FirstRuns[grep(pattern = ".*_Mask__", x = FirstRuns)]
FirstRuns1 <- split(FirstRuns, ceiling(seq_along(1:length(FirstRuns))/20))
names(FirstRuns1) <- paste0("Task_", 65:128, ".moab")

for (ii in 1:length(FirstRuns1)){
  CurrFile <- paste0("output_moab\\", names(FirstRuns1)[ii])
  cat("#!/bin/sh\n", file = CurrFile, append=FALSE)
  cat(paste0("########### Begin MOAB/Slurm header ##########\n#\n# Give job a reasonable name\n#MOAB -N Task_", 64+ii), file = CurrFile, append=TRUE)
  cat(paste0('\n#\n# Request number of nodes and CPU cores per node for job\n#MOAB -l nodes=1:ppn=16, pmem=48gb\n#\n# Estimated wallclock time for job\n#MOAB -l walltime=00:12:00:00\n#\n# Write standard output and errors in same file\n#MOAB -j oe\n#\n# Send mail when job begins, aborts and ends\n#MOAB -m bae\n#\n########### End MOAB header ##########\n\necho "Working Directory:                    $PWD"\necho "Running on host                       $HOSTNAME"\necho "Job id:                               $MOAB_JOBID"\necho "Job name:                             $MOAB_JOBNAME"\necho "Number of nodes allocated to job:     $MOAB_NODECOUNT"\necho "Number of cores allocated to job:     $MOAB_PROCCOUNT"\n\nmodule load geo/zonation/4.0.0\ncd ZonationEgypt/\n\n'), file = CurrFile, append=TRUE)
  for(i in 1:length(FirstRuns1[[ii]])){
    cat(paste0('bash "', FirstRuns1[[ii]][i], '" &\n'), file = CurrFile, append=TRUE)
  }
  cat(paste0('wait\necho "All are complete"'), file = CurrFile, append=TRUE)
}
# Tasks <- paste0("msub Task_", 65:128, ".moab")
# cat(Tasks, file = "output_tasks\\Tasks65_128.txt", sep = "\n")

###||||||||||||||||||||||||||||||||||||||||||||||||
###||||||||||||||||||||||||||||||||||||||||||||||||
