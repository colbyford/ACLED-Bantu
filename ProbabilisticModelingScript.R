###################################
#     Probabilistic Modeling      #
#        Bantu Migration          #
#        By: Colby Ford           #
###################################
library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(psych)
library(MASS)
library(dimRed)

####################################################################
## Input as Table
CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin.csv") #Inner Join Data
#CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_OuterJoin.csv") #Outer Join Data
colnames(CombinedData)[1] <- "TaxaID" #Fix <U+FEFF> Issues
mtDNA_cols <- paste0("mtDNA.pos", seq(1:nchar(CombinedData$mtDNA[1])))
Ychr_cols <- paste0("Ychr.pos", seq(1:nchar(CombinedData$Ychr_STR[1])))
Cultural_cols <- paste0("Cultural.pos", seq(1:nchar(CombinedData$Cultural_EthnogAtlas[1])))

## Separate Strings into Individual Columns
SepCombinedData <- CombinedData %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:nchar(CombinedData$mtDNA[1])), remove = TRUE) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:nchar(CombinedData$Ychr_STR[1])), remove = TRUE) %>% 
  separate(., Cultural_EthnogAtlas, Cultural_cols, sep = seq(1:nchar(CombinedData$Cultural_EthnogAtlas[1])), remove = TRUE)

## Remove NA Columns
SepCombinedData <- SepCombinedData[!is.na(names(SepCombinedData))]

## Clean Up Memory to Avoid Error
rm("CombinedData", "Cultural_cols", "mtDNA_cols", "Ychr_cols")
gc()
memory.limit(size=99999)

## Dummy Code Each Column and Write to Disk
DummyCodedCombinedData <- as.data.frame(lapply(SepCombinedData, dummy.code)) #This takes forever
DummyCodedCombinedData_orig <- DummyCodedCombinedData #Save for testing purposes
DummyStartLoc <- which(colnames(DummyCodedCombinedData)=="Cultural.pos1.1")

DummyCodedCombinedData <- cbind(SepCombinedData[,1:5],
                                DummyCodedCombinedData[,DummyStartLoc:ncol(DummyCodedCombinedData)]) #Append Data
DummyCodedCombinedData <- DummyCodedCombinedData %>% 
  dplyr::select(-starts_with("X")) #Remove Variable With Only 1 Value (Starts with X)
write_csv(DummyCodedCombinedData, "datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded.csv", append = FALSE)
#DummyCodedCombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded.csv")

## Clean Up Memory
rm(SepCombinedData)
gc()

## Collapse Data By TaxaID
CollapsedDummyCodedCombinedData <- DummyCodedCombinedData %>% 
  dplyr::select(-GuthrieZone, -CulturalTaxa, -mtDNATaxa, -YchrTaxa) %>% 
  group_by(TaxaID) %>% 
  summarise_all(mean)

write_csv(CollapsedDummyCodedCombinedData, "datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded_TaxaID_Collapsed.csv", append = FALSE)
#CollapsedDummyCodedCombinedData <- as.data.frame(read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded_GuthrieZone_Collapsed.csv"))
rownames(CollapsedDummyCodedCombinedData) <- CollapsedDummyCodedCombinedData$GuthrieZone
CollapsedDummyCodedCombinedData$TaxaID <- NULL
CollapsedDummyCodedCombinedData$GuthrieZone <- NULL
CollapsedDummyCodedCombinedData$CulturalTaxa <- NULL
CollapsedDummyCodedCombinedData$mtDNATaxa <- NULL
CollapsedDummyCodedCombinedData$YchrTaxa <- NULL

## Clean Up Memory
rm(DummyCodedCombinedData)
gc()

