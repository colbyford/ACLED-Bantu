###################################
#     Probabilistic Modeling      #
#        Bantu Migration          #
#        By: Colby Ford           #
###################################
library(tidyr)
library(readr)
library(dplyr)
library(caret)
#library(lime)
#library(randomForest)
library(ranger)

memory.limit(size = NA)
options(expressions = 5e5)

## Set up Parallelization
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

############
## Load Full Data
CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_OuterJoin.csv") #Outer Join Data
colnames(CombinedData)[1] <- "TaxaID" #Fix <U+FEFF> Issues
CombinedData[ CombinedData == "NULL" ] <- NA


## Create mtDNA Datasets
mtDNA_cols <- paste0("mtDNA.pos", seq(1:16590))

mtDNAData <- CombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa, -Cultural_EthnogAtlas, -Ychr_STR) %>% 
  filter(complete.cases(.)) %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:16590), remove = TRUE)
# Remove NA Columns
mtDNAData <- mtDNAData[!is.na(names(mtDNAData))]
mtDNAData$GuthrieZone <- as.factor(mtDNAData$GuthrieZone)
# Split for ML
mtDNAData_trainIndex <- createDataPartition(mtDNAData$GuthrieZone, p = 0.50, list = FALSE)
mtDNAData_train <- mtDNAData[ mtDNAData_trainIndex,]
mtDNAData_test <- mtDNAData[-mtDNAData_trainIndex,]

## Create Ychr Datasets
Ychr_cols <- paste0("Ychr.pos", seq(1:12))

YchrData <- CombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa, -Cultural_EthnogAtlas, -mtDNA) %>% 
  filter(complete.cases(.)) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:12), remove = TRUE)
# Remove NA Columns
YchrData <- YchrData[!is.na(names(YchrData))]
YchrData$GuthrieZone <- as.factor(YchrData$GuthrieZone)
# Split for ML
YchrData_trainIndex <- createDataPartition(YchrData$GuthrieZone, p = 0.50, list = FALSE)
YchrData_train <- YchrData[ YchrData_trainIndex,]
YchrData_test <- YchrData[-YchrData_trainIndex,]

## Create Genetic Only Datasets
GeneticData <- CombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa, -Cultural_EthnogAtlas) %>% 
  filter(complete.cases(.)) %>%
  separate(., mtDNA, mtDNA_cols, sep = seq(1:16590), remove = TRUE) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:12), remove = TRUE)
# Remove NA Columns
GeneticData <- GeneticData[!is.na(names(GeneticData))]
GeneticData$GuthrieZone <- as.factor(GeneticData$GuthrieZone)
# Split for ML
GeneticData_trainIndex <- createDataPartition(GeneticData$GuthrieZone, p = 0.50, list = FALSE)
GeneticData_train <- GeneticData[ GeneticData_trainIndex,]
GeneticData_test <- GeneticData[-GeneticData_trainIndex,]

## Create Cultural Datasets
Cultural_cols <- paste0("Cultural.pos", seq(1:92))

CulturalData <- CombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa, -mtDNA, -Ychr_STR) %>% 
  filter(complete.cases(.)) %>% 
  separate(., Cultural_EthnogAtlas, Cultural_cols, sep = seq(1:92), remove = TRUE)
# Remove NA Columns
CulturalData <- CulturalData[!is.na(names(CulturalData))]
CulturalData$GuthrieZone <- as.factor(CulturalData$GuthrieZone)
# Split for ML
CulturalData_trainIndex <- createDataPartition(CulturalData$GuthrieZone, p = 0.50, list = FALSE)
CulturalData_train <- CulturalData[ CulturalData_trainIndex,]
CulturalData_test <- CulturalData[-CulturalData_trainIndex,]

## Create Combined Datasets
CombinedData <- CombinedData %>% 
  select(-TaxaID, -CulturalTaxa, -mtDNATaxa, -YchrTaxa) %>% 
  #filter(complete.cases(.)) %>% 
  separate(., mtDNA, mtDNA_cols, sep = seq(1:16590), remove = TRUE) %>% 
  separate(., Ychr_STR, Ychr_cols, sep = seq(1:12), remove = TRUE) %>% 
  separate(., Cultural_EthnogAtlas, Cultural_cols, sep = seq(1:92), remove = TRUE)
# Remove NA Columns
CombinedData <- CombinedData[!is.na(names(CombinedData))]
CombinedData$GuthrieZone <- as.factor(CombinedData$GuthrieZone)
# Split for ML
CombinedData_trainIndex <- createDataPartition(CombinedData$GuthrieZone, p = 0.50, list = FALSE)
CombinedData_train <- CombinedData[ CombinedData_trainIndex,]
CombinedData_test <- CombinedData[-CombinedData_trainIndex,]


############
## Load Dummy Coded TaxaID Summarized Data
#CombinedData <- read_csv("datasets/DimensionalityReduction/DimensionalityReduction_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed.csv")

#CombinedData <- CombinedData %>% 
#  separate(., TaxaID, c("GuthrieZone","other"), sep = 1, remove = TRUE) %>% 
#  dplyr::select(-other)
  
#CombinedData$GuthrieZone <- as.factor(CombinedData$GuthrieZone)
############

## Create Random Forest model and 10-fold CV
Combined_model_cv <- caret::train(CombinedData_train,
                                  CombinedData_train$GuthrieZone,
                                  method = 'ranger',
                                  metric = 'Accuracy',
                                  trControl = trainControl(method = "repeatedcv",
                                                           number = 10,
                                                           repeats = 3,
                                                           verboseIter = TRUE,
                                                           savePredictions = TRUE))

mtDNA_model_cv <- caret::train(mtDNAData_train,
                               mtDNAData_train$GuthrieZone,
                               method = 'ranger',
                               metric = 'Accuracy',
                               trControl = trainControl(method = "repeatedcv",
                                                        number = 10,
                                                        repeats = 3,
                                                        verboseIter = TRUE,
                                                        savePredictions = TRUE))

Ychr_model_cv <- caret::train(YchrData_train,
                              YchrData_train$GuthrieZone,
                              method = 'ranger',
                              metric = 'Accuracy',
                              trControl = trainControl(method = "repeatedcv",
                                                       number = 10,
                                                       repeats = 3,
                                                       verboseIter = TRUE,
                                                       savePredictions = TRUE))

Genetic_model_cv <- caret::train(GeneticData_train,
                              GeneticData_train$GuthrieZone,
                              method = 'ranger',
                              metric = 'Accuracy',
                              trControl = trainControl(method = "repeatedcv",
                                                       number = 10,
                                                       repeats = 3,
                                                       verboseIter = TRUE,
                                                       savePredictions = TRUE))

Cultural_model_cv <- caret::train(CulturalData_train,
                                  CulturalData_train$GuthrieZone,
                                  method = 'ranger',
                                  metric = 'Accuracy',
                                  trControl = trainControl(method = "repeatedcv",
                                                           number = 10,
                                                           repeats = 3,
                                                           verboseIter = TRUE,
                                                           savePredictions = TRUE))

## Save Model Objects

#saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_CombinedRandomForestCVModel.RDS")
#saveRDS(mtDNA_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_mtDNARandomForestCVModel.RDS")
#saveRDS(Ychr_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_YchrRandomForestCVModel.RDS")
#saveRDS(Cultural_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_DummyCoded_TaxaID_Collapsed_CulturalRandomForestCVModel.RDS")

saveRDS(Combined_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CombinedRandomForestCVModel.RDS")
saveRDS(mtDNA_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_mtDNARandomForestCVModel.RDS")
saveRDS(Ychr_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_YchrRandomForestCVModel.RDS")
saveRDS(Genetic_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_GeneticRandomForestCVModel.RDS")
saveRDS(Cultural_model_cv, "datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CulturalRandomForestCVModel.RDS")

## Reload Model Objects

Combined_model_cv <- readRDS("datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CombinedRandomForestCVModel.RDS")
mtDNA_model_cv <- readRDS("datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_mtDNARandomForestCVModel.RDS")
Ychr_model_cv <- readRDS("datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_YchrRandomForestCVModel.RDS")
Genetic_model_cv <- readRDS("datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_GeneticRandomForestCVModel.RDS")
Cultural_model_cv <- readRDS("datasets/MachineLearning/MachineLearning_CombinedData_OuterJoin_CulturalRandomForestCVModel.RDS")

## Lime Stuff
# Create an explainer object
explainer <- lime(data_train, model)

# Explain new observation
explanation <- explain(iris_test, explainer, n_labels = 1, n_features = 2)

plot_features(explanation)