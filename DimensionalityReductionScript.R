###################################
# Dimensionality Reduction Script #
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
mtDNA_cols <- paste0("mtDNA.pos", seq(1:16590))
Ychr_cols <- paste0("Ychr.pos", seq(1:12))
Cultural_cols <- paste0("Cultural.pos", seq(1:92))

## Separate Strings into Individual Columns
SepCombinedData <- CombinedData %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:12), remove = TRUE) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:16590), remove = TRUE) %>% 
  separate(., Cultural_EthnogAtlas, Cultural_cols, sep = seq(1:92), remove = TRUE)

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

####################################################################
## Multidimensional Scaling
## Non-Metric MDS
MDS_NonMetric_Combined <- isoMDS(dist(CollapsedDummyCodedCombinedData),
                                   y = cmdscale(dist(CollapsedDummyCodedCombinedData),
                                                k = 2),
                                   k = 2,
                                   maxit = 500,
                                   trace = TRUE,
                                   tol = 1e-3,
                                   p = 2)
MDS_NonMetric_Combined_Points <- as.data.frame(MDS_NonMetric_Combined$points) #Extract Points
MDS_NonMetric_Combined_Points$TaxaID <- rownames(CollapsedDummyCodedCombinedData) #Add Column and Rename
colnames(MDS_NonMetric_Combined_Points) <- c("x","y","TaxaID")

write_csv(as.data.frame(MDS_NonMetric_Combined_Points),
          "datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded_TaxaID_Collapsed_Non-MetricMDS-2.csv",
          append = FLASE)

#Shepard Functions for MDS
MDS_NonMetric_Combined_2_Shep <- Shepard(dist(CollapsedDummyCodedCombinedData), MDS_NonMetric_Combined_2$points)
plot(MDS_NonMetric_Combined_2_Shep)

####################################################################
##Laplacian Eigenmaps

#Find optimal number of k in k-nn
knn_accuracy <- data.frame(k = 0,
                           accuracy = 0)
for (k in 1:nrow(CollapsedDummyCodedCombinedData)){
  knn <- class::knn(CollapsedDummyCodedCombinedData,
                    CollapsedDummyCodedCombinedData,
                    rownames(CollapsedDummyCodedCombinedData),
                    k=2)
  knn_prop <- prop.table(table(knn, rownames(CollapsedDummyCodedCombinedData)))
  accuracy <- sum(diag(knn_prop))/sum(knn_prop)
  iter_accuracy <- data.frame(k = k,
                   accuracy = accuracy)
  knn_accuracy <- rbind(iter_accuracy, knn_accuracy)
}

most_accurate_k <- knn_accuracy[which.max(knn_accuracy$accuracy),]$k

CollapsedDummyCodedCombinedData_dimRed <- dimRedData(CollapsedDummyCodedCombinedData[,1:ncol(CollapsedDummyCodedCombinedData)]) #Convert to dimRed class
leim <- LaplacianEigenmaps() #S4 Object for LE
LEpars <- list(ndim = 2, #Define Parameter Set for LE
               sparse = "knn",
               knn = most_accurate_k,
               #eps = 0.1,
               t = Inf,
               norm = FALSE)

LE_Combined <- leim@fun(CollapsedDummyCodedCombinedData_dimRed, LEpars)
LE_Combined_Points <- as.data.frame(LE_Combined@data@data) #Extract Points
LE_Combined_Points$TaxaID <- rownames(CollapsedDummyCodedCombinedData) #Add Column and Rename
#colnames(MDS_NonMetric_Combined_Points) <- c("x","y","z","TaxaID")

write_csv(LE_Combined_Points,
          "datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_InnerJoin_DummyCoded_GuthrieZone_Collapsed_LE-2.csv",
          append = FALSE)


