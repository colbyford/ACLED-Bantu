###################################
#     Probabilistic Modeling      #
#        Bantu Migration          #
#        By: Colby Ford           #
###################################
library(tidyr)
library(readr)
library(dplyr)
library(caret)
library(lime)
library(randomForest)

## Set up Parallelization
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

############
## Load Full Data
# Create Separated Data
CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_OuterJoin.csv") #Outer Join Data
colnames(CombinedData)[1] <- "TaxaID" #Fix <U+FEFF> Issues
mtDNA_cols <- paste0("mtDNA.pos", seq(1:16590))
Ychr_cols <- paste0("Ychr.pos", seq(1:12))
Cultural_cols <- paste0("Cultural.pos", seq(1:92))

## Separate Strings into Individual Columns
CombinedData <- CombinedData %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:16590), remove = TRUE) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:12), remove = TRUE) %>% 
  separate(., Cultural_EthnogAtlas, Cultural_cols, sep = seq(1:92), remove = TRUE)

## Remove NA Columns
CombinedData <- CombinedData[!is.na(names(CombinedData))]

CombinedData <- CombinedData %>% 
  dplyr::select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa)

CombinedData$GuthrieZone <- as.factor(CombinedData$GuthrieZone)

############
## Load Dummy Coded TaxaID Summarized Data
CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed.csv")

CombinedData <- CombinedData %>% 
  separate(., TaxaID, c("GuthrieZone","other"), sep = 1, remove = TRUE) %>% 
  dplyr::select(-other)
  
CombinedData$GuthrieZone <- as.factor(CombinedData$GuthrieZone)
############

## Derive Subsets
mtDNAData <- CombinedData %>% 
  select(GuthrieZone, starts_with("mtDNA"))

YchrData <- CombinedData %>% 
  select(GuthrieZone, starts_with("Ychr"))

CulturalData <- CombinedData %>% 
  select(GuthrieZone, starts_with("Cultural"))

## Split up the datasets
CombinedData_trainIndex <- createDataPartition(CombinedData$GuthrieZone, p = 0.50, list = FALSE)
CombinedData_train <- CombinedData[ CombinedData_trainIndex,]
CombinedData_test <- CombinedData[-CombinedData_trainIndex,]

YchrData_trainIndex <- createDataPartition(YchrData$GuthrieZone, p = 0.50, list = FALSE)
YchrData_train <- YchrData[ YchrData_trainIndex,]
YchrData_test <- YchrData[-YchrData_trainIndex,]

mtDNAData_trainIndex <- createDataPartition(mtDNAData$GuthrieZone, p = 0.50, list = FALSE)
mtDNAData_train <- mtDNAData[ mtDNAData_trainIndex,]
mtDNAData_test <- mtDNAData[-mtDNAData_trainIndex,]

CulturalData_trainIndex <- createDataPartition(CulturalData$GuthrieZone, p = 0.50, list = FALSE)
CulturalData_train <- CulturalData[ CulturalData_trainIndex,]
CulturalData_test <- CulturalData[-CulturalData_trainIndex,]

## Create Random Forest model and 10-fold CV
Combined_model_cv <- caret::train(CombinedData_train,
                      CombinedData_train$GuthrieZone,
                      method = 'rf',
                      trControl = trainControl(method = "cv", 
                                               number = 10, 
                                               verboseIter = TRUE,
                                               savePredictions = TRUE))

mtDNA_model_cv <- caret::train(mtDNAData_train,
                                  mtDNAData_train$GuthrieZone,
                                  method = 'rf',
                                  trControl = trainControl(method = "cv", 
                                                           number = 10, 
                                                           verboseIter = TRUE,
                                                           savePredictions = TRUE))

Ychr_model_cv <- caret::train(YchrData_train,
                                  YchrData_train$GuthrieZone,
                                  method = 'rf',
                                  trControl = trainControl(method = "cv", 
                                                           number = 10, 
                                                           verboseIter = TRUE,
                                                           savePredictions = TRUE))

Cultural_model_cv <- caret::train(CulturalData_train,
                                  CulturalData_train$GuthrieZone,
                                  method = 'rf',
                                  trControl = trainControl(method = "cv", 
                                                           number = 10, 
                                                           verboseIter = TRUE,
                                                           savePredictions = TRUE))

#saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_CombinedRandomForestCVModel.RDS")
#saveRDS(mtDNA_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_mtDNARandomForestCVModel.RDS")
#saveRDS(Ychr_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_YchrRandomForestCVModel.RDS")
#saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_CulturalRandomForestCVModel.RDS")

saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CombinedRandomForestCVModel.RDS")
saveRDS(mtDNA_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_mtDNARandomForestCVModel.RDS")
saveRDS(Ychr_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_YchrRandomForestCVModel.RDS")
saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CulturalRandomForestCVModel.RDS")


## Lime Stuff
# Create an explainer object
explainer <- lime(data_train, model)

# Explain new observation
explanation <- explain(iris_test, explainer, n_labels = 1, n_features = 2)

plot_features(explanation)