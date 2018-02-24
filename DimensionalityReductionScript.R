library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(psych)

## Input as Table
CombinedData <- read_csv("DimensionalityReduction_CombinedData_InnerJoin.csv")
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
  select(-starts_with("X")) #Remove Variable With Only 1 Value (Starts with X)
write_csv(DummyCodedCombinedData, "DimensionalityReduction_CombinedData_InnerJoin_DummyCoded.csv", append = FALSE)
#DummyCodedCombinedData <- read_csv("DimensionalityReduction_CombinedData_InnerJoin_DummyCoded.csv")

## Clean Up Memory
rm(SepCombinedData)
gc()

## Collapse Data By Guthrie Zones
CollapsedDummyCodedCombinedData <- DummyCodedCombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa) %>% 
  group_by(GuthrieZone) %>% 
  summarise_all(mean)

write_csv(CollapsedDummyCodedCombinedData, "DimensionalityReduction_CombinedData_InnerJoin_DummyCoded_Collapsed.csv", append = FALSE)

## Clean Up Memory
rm(DummyCodedCombinedData)
gc()


## Multidimensional Scaling
MDS_Combined_2 <- cmdscale(dist(DummyCodedCombinedData[1:100,1:25]), eig=TRUE, k=2) # Dimensions
MDS_Combined_2 # view results



# plot solution 
x <- MDS_Combined_2$points[,1]
y <- MDS_Combined_2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS",	type="n")
text(x, y, labels = row.names(DummyCodedCombinedData), cex=.7)

