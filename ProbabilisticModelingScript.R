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
library(caret)
library(lime)
library(randomForest)

## Set up Parallelization
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

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

rfSepCombinedData <- SepCombinedData %>% 
  dplyr::select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa)

## Clean Up Memory to Avoid Error
rm("CombinedData", "Cultural_cols", "mtDNA_cols", "Ychr_cols")
gc()
memory.limit(size=99999)

## Split up the data set
trainIndex <- createDataPartition(rfSepCombinedData$GuthrieZone,
                                  p = 0.80,
                                  list = FALSE)
data_train <- rfSepCombinedData[ trainIndex,]
data_test <- rfSepCombinedData[-trainIndex,]



## Create Random Forest model and 10-fold CV
model <- caret::train(GuthrieZone ~ .,
               data_train,
               method = 'rf',
               trControl = trainControl(method = "cv", 
                                        number = 10, 
                                        #repeats = 5, 
                                        verboseIter = TRUE,
                                        savePredictions = TRUE))

# Create an explainer object
explainer <- lime(data_train, model)

# Explain new observation
explanation <- explain(iris_test, explainer, n_labels = 1, n_features = 2)

plot_features(explanation)

