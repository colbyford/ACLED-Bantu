library(tidyr)
library(dplyr)
library(psych)

mtDNA <- c("AGCGTCGTCGATCGAGAGACCATGCTATGA","GCGTCGATCGATCGAGAGACCATGCTATGA", "GCGTCGTACGATCGAGAGACCATGCTATGA")
mtDNA_cols <- paste0("mtDNA.pos", seq(1:nchar(mtDNA[1])))

Ychr <- c("CHIAGGLEDPYW","GLEDPYWQLIAG","GLEPAYWQLIAG")
Ychr_cols <- paste0("Ychr.pos", seq(1:nchar(Ychr[1])))

Cultural <- c("0A129BCCCD90234","1B139BCCD90234A","21B139BCD90234A")
Cultural_cols <- paste0("Cultural.pos", seq(1:nchar(Cultural[1])))

CombinedData <- data.frame(mtDNA, Ychr, Cultural)

SepCombinedData <- CombinedData %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:nchar(mtDNA[1])), remove = TRUE) %>% 
  separate(., Ychr, Ychr_cols, sep = seq(1:nchar(Ychr[1])), remove = TRUE) %>% 
  separate(., Cultural, Cultural_cols, sep = seq(1:nchar(Cultural[1])), remove = TRUE)

DummyCodedCombinedData <- as.data.frame(lapply(SepCombinedData, dummy.code))

#### MDS
MDS_Combined_2 <- cmdscale(dist(DummyCodedCombinedData), eig=TRUE, k=2) # Dimensions
MDS_Combined_2 # view results



# plot solution 
x <- MDS_Combined_2$points[,1]
y <- MDS_Combined_2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS",	type="n")
text(x, y, labels = row.names(DummyCodedCombinedData), cex=.7)

