# Loading necessary libraries
library(dplyr) # For data manipulation
library(tidyverse) # For data manipulation and visualization
library(corrplot) # using for corrplot
library(caTools) # using for spliting the data frame
library(car) # For regression model diagnostics
library(randomForest) # using for Random Forest Model
library(shapper) # using for shap value
library(DALEX) # For model explainability

# Installing shapper package
shapper::install_shap() 

# Setting working directory
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")

# Reading data for Bananas production and preprocessing
Bananas <- read.csv("Bananas ProdSD4 SpilloverSD4 G28.csv", header = T)

Bananas <- Bananas %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(BananasProd, BananasProdLogDiff, BananasProdSD2, BananasProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(BananasProdSD4 ~ ., data = Bananas)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")
summary(StepBack)

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/BananaStepBackwardCoeff.csv", row.names = T)


model1 <- lm(BananasProdSD4~1,data=Bananas)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + CattleMeatProdSD2 + 
                      CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + MaizeProd + 
                      MaizeProdLogDiff + MaizeProdSD2 + MaizeProdSD3 + MaizeProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + PotatoesProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/BananaStepForwardCoeff.csv", row.names = T)



Bananas <- Bananas %>%
  subset(select = c(BananasProdSD4, IrrigationEquippedLand, tnn, MaizeProd, AgriLandShare, rx5day, 
                    AgriLand, CattleRawMilkProdSD4, pr, txx, RuralPopulGrowth, Area))

### Make Correlation Matrix
NumeriCols <- sapply(Bananas, is.numeric)
CorBananas <- cor(Bananas[NumeriCols])
corrplot(CorBananas, method = "number", type = "lower", diag = F, number.cex = 0.7)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Bananas$BananasProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainBananas <- subset(Bananas, split == TRUE)
TestBananas <- subset(Bananas, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(BananasProdSD4 ~ ., data = TrainBananas)
PredBananas <- predict(RandForModel, TestBananas)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/Banana3CorlessImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredBananas - TestBananas$BananasProdSD4))
MSE <- mean((PredBananas - TestBananas$BananasProdSD4)^2)
RMSE <- sqrt(mean((PredBananas - TestBananas$BananasProdSD4)^2))
R2 <- 1 - (sum((TestBananas$BananasProdSD4 - PredBananas)^2) / sum((TestBananas$BananasProdSD4 - mean(TestBananas$BananasProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainBananas[, -1], 
                      y = TrainBananas$BananasProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestBananas[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/Banana3CorlessShapVals.csv", row.names = T)


# Meat of Cattle
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
CattleMeat <- read.csv("CattleMeat ProdSD4 SpilloverSD4 G44.csv", header = T)

## Make Meat of Cattle data frame
CattleMeat <- CattleMeat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(CattleMeatProd, CattleMeatProdSD3, CattleMeatProdSD2, CattleMeatProdLogDiff, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(CattleMeatProdSD4 ~ ., data = CattleMeat)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")
summary(StepBack)

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/CattleMeatStepBackwardCoeff.csv", row.names = T)


model1 <- lm(CattleMeatProdSD4~1,data=CattleMeat)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleRawMilkProd + CattleRawMilkProdLogDiff + 
                      CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + CattleRawMilkProdSD4 + 
                      ChickensMeatProd + ChickensMeatProdLogDiff + ChickensMeatProdSD2 + 
                      ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + PotatoesProdLogDiff + 
                      PotatoesProdSD2 + PotatoesProdSD3 + PotatoesProdSD4, method='forward')
summary(StepForward)

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/CattleMeatStepForwardCoeff.csv", row.names = T)



CattleMeat <- CattleMeat %>%
  subset(select = c(CattleMeatProdSD4, AgriLand, ChickensMeatProd, CattleRawMilkProd, RuralPopulLogDiff, 
                    Area, CattleRawMilkProdSD4, IrrigationEquippedLand, tr, PotatoesProd, 
                    AgriLandShare, PopulGrowth, ChickensMeatProdSD4))

### Make Correlation Matrix
NumeriCols <- sapply(CattleMeat, is.numeric)
CorCattleMeat <- cor(CattleMeat[NumeriCols])
corrplot(CorCattleMeat, method = "number", type = "lower", diag = F, number.cex = 0.7)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(CattleMeat$CattleMeatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainCattleMeat <- subset(CattleMeat, split == TRUE)
TestCattleMeat <- subset(CattleMeat, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(CattleMeatProdSD4 ~ ., data = TrainCattleMeat)
PredCattleMeat <- predict(RandForModel, TestCattleMeat)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/CattleMeat2Corless4ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredCattleMeat - TestCattleMeat$CattleMeatProdSD4))
MSE <- mean((PredCattleMeat - TestCattleMeat$CattleMeatProdSD4)^2)
RMSE <- sqrt(mean((PredCattleMeat - TestCattleMeat$CattleMeatProdSD4)^2))
R2 <- 1 - (sum((TestCattleMeat$CattleMeatProdSD4 - PredCattleMeat)^2) / sum((TestCattleMeat$CattleMeatProdSD4 - mean(TestCattleMeat$CattleMeatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainCattleMeat[, -1], 
                      y = TrainCattleMeat$CattleMeatProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestCattleMeat[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/CattleMeatCorless4ShapVals.csv", row.names = T)


# Raw Milk of Cattle
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
CattleRawMilk <- read.csv("CattleRawMilk ProdSD4 SpilloverSD4 G44.csv", header = T)
## Make Raw Milk of Cattle data frame
CattleRawMilk <- CattleRawMilk %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(CattleRawMilkProd, CattleRawMilkProdLogDiff, CattleRawMilkProdSD2, 
                     CattleRawMilkProdSD3, Year))


### Check significance of all variables of the data frame
AllVarModel <- lm(CattleRawMilkProdSD4 ~ ., data = CattleRawMilk)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")
CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/CattleRawMilkStepBackwardCoeff.csv", row.names = T)

model1 <- lm(CattleRawMilkProdSD4~1,data=CattleRawMilk)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + ChickensMeatProd + 
                      ChickensMeatProdLogDiff + ChickensMeatProdSD2 + ChickensMeatProdSD3 + 
                      ChickensMeatProdSD4 + PotatoesProd + PotatoesProdLogDiff + PotatoesProdSD2 + 
                      PotatoesProdSD3 + PotatoesProdSD4, method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/CattleRawMilkStepForwardCoeff.csv", row.names = T)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(CattleRawMilk$CattleRawMilkProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainCattleRawMilk <- subset(CattleRawMilk, split == TRUE)
TestCattleRawMilk <- subset(CattleRawMilk, split == FALSE)

CattleRawMilk <- CattleRawMilk %>%
  subset(select = c(CattleRawMilkProdSD4, IrrigationEquippedLand, CattleMeatProd, PotatoesProd, 
                    AgriLand, AgriLandShare, CattleMeatProdSD4, tr, PotatoesProdSD4, txx, 
                    ChickensMeatProdSD4))

### Make Correlation Matrix
NumeriCols <- sapply(CattleRawMilk, is.numeric)
CorCattleRawMilk <- cor(CattleRawMilk[NumeriCols])
corrplot(CorCattleRawMilk, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(CattleRawMilk$CattleRawMilkProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainCattleRawMilk <- subset(CattleRawMilk, split == TRUE)
TestCattleRawMilk <- subset(CattleRawMilk, split == FALSE)


# Make Random Forest Model
RandForModel <- randomForest(CattleRawMilkProdSD4 ~ ., data = TrainCattleRawMilk)
PredCattleRawMilk <- predict(RandForModel, TestCattleRawMilk)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/CattleRawMilkCorless4ImportanceScore.csv", row.names = T)

# Calculate evaluation metrics
MAE <- mean(abs(PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4))
MSE <- mean((PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4)^2)
RMSE <- sqrt(mean((PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4)^2))
R2 <- 1 - (sum((TestCattleRawMilk$CattleRawMilkProdSD4 - PredCattleRawMilk)^2) / sum((TestCattleRawMilk$CattleRawMilkProdSD4 - mean(TestCattleRawMilk$CattleRawMilkProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainCattleRawMilk[, -1], 
                      y = TrainCattleRawMilk$CattleRawMilkProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestCattleRawMilk[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/CattleRawMilkCorless4ShapVals.csv", row.names = T)



# Meat of Chickens
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
ChickensMeat <- read.csv("ChickensMeat ProdSD4 SpilloverSD4 G44.csv", header = T)
## Make Meat of Chickens data frame
ChickensMeat <- ChickensMeat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(ChickensMeatProd, ChickensMeatProdLogDiff, ChickensMeatProdSD2, ChickensMeatProdSD3, 
                     Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(ChickensMeatProdSD4 ~ ., data = ChickensMeat)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/ChickensMeatStepBackwardCoeff.csv", row.names = T)


model1 <- lm(ChickensMeatProdSD4~1,data=ChickensMeat)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + PotatoesProd + PotatoesProdLogDiff + PotatoesProdSD2 + 
                      PotatoesProdSD3 + PotatoesProdSD4, method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/ChickensMeatStepForwardCoeff.csv", row.names = T)


ChickensMeat <- ChickensMeat %>%
  subset(select = c(ChickensMeatProdSD4, CattleMeatProd, Population, AgriLand, PopulGrowth, pr, 
                    CattleMeatProdSD4, AgriLandShare, tasmin, RuralPopulGrowth, 
                    CattleRawMilkProdSD4, PotatoesProd, PotatoesProdSD4))

### Make Correlation Matrix
NumeriCols <- sapply(ChickensMeat, is.numeric)
CorChickensMeat <- cor(ChickensMeat[NumeriCols])
corrplot(CorChickensMeat, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(ChickensMeat$ChickensMeatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainChickensMeat <- subset(ChickensMeat, split == TRUE)
TestChickensMeat <- subset(ChickensMeat, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(ChickensMeatProdSD4 ~ ., data = TrainChickensMeat)
PredChickensMeat <- predict(RandForModel, TestChickensMeat)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/ChickensMeatCorless2ImportanceScore.csv", row.names = T)



# Calculate evaluation metrics
MAE <- mean(abs(PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4))
MSE <- mean((PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4)^2)
RMSE <- sqrt(mean((PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4)^2))
R2 <- 1 - (sum((TestChickensMeat$ChickensMeatProdSD4 - PredChickensMeat)^2) / sum((TestChickensMeat$ChickensMeatProdSD4 - mean(TestChickensMeat$ChickensMeatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainChickensMeat[, -1], 
                      y = TrainChickensMeat$ChickensMeatProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestChickensMeat[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/ChickensMeatCorless2ShapVals.csv", row.names = T)





# Hen Eggs
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
HenEggs <- read.csv("HenEggs ProdSD4 SpilloverSD4 G42.csv", header = T)
## Make Hen Eggs data frame
HenEggs <- HenEggs %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(HenEggsProd, HenEggsProdLogDiff, HenEggsProdSD2, HenEggsProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(HenEggsProdSD4 ~ ., data = HenEggs)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/HenEggStepBackwardCoeff.csv", row.names = T)

model1 <- lm(HenEggsProdSD4~1,data=HenEggs)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + PotatoesProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/HenEggStepForwardCoeff.csv", row.names = T)

HenEggs <- HenEggs %>%
  subset(select = c(HenEggsProdSD4, ChickensMeatProdSD4, Population, PotatoesProd, tasmin, AgriLandShare, 
                    AgriLand, CattleRawMilkProd, ChickensMeatProd, CattleMeatProdSD4))

### Make Correlation Matrix
NumeriCols <- sapply(HenEggs, is.numeric)
CorHenEggs <- cor(HenEggs[NumeriCols])
corrplot(CorHenEggs, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(HenEggs$HenEggsProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainHenEggs <- subset(HenEggs, split == TRUE)
TestHenEggs <- subset(HenEggs, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(HenEggsProdSD4 ~ ., data = TrainHenEggs)
PredHenEggs <- predict(RandForModel, TestHenEggs)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/HenEggs2CorlessImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredHenEggs - TestHenEggs$HenEggsProdSD4))
MSE <- mean((PredHenEggs - TestHenEggs$HenEggsProdSD4)^2)
RMSE <- sqrt(mean((PredHenEggs - TestHenEggs$HenEggsProdSD4)^2))
R2 <- 1 - (sum((TestHenEggs$HenEggsProdSD4 - PredHenEggs)^2) / sum((TestHenEggs$HenEggsProdSD4 - mean(TestHenEggs$HenEggsProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainHenEggs[, -1], 
                      y = TrainHenEggs$HenEggsProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestHenEggs[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/HenEggsCorless2ShapVals.csv", row.names = T)

# Maize
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Maize <- read.csv("Maize ProdSD4 SpilloverSD4 G43.csv", header = T)
## Make Maize data frame
Maize <- Maize %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(MaizeProd, MaizeProdLogDiff, MaizeProdSD2, MaizeProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(MaizeProdSD4 ~ ., data = Maize)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/MaizeStepBackwardCoeff.csv", row.names = T)


model1 <- lm(MaizeProdSD4~1,data=Maize)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + PotatoesProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/MaizeStepForwardCoeff.csv", row.names = T)

Maize <- Maize %>%
  subset(select = c(MaizeProdSD4, CattleMeatProd, Area, AgriLand, IrrigationEquippedLand, 
                    ChickensMeatProdSD3, AgriLandShare, PotatoesProd, tasmin))

### Make Correlation Matrix
NumeriCols <- sapply(Maize, is.numeric)
CorMaize <- cor(Maize[NumeriCols])
corrplot(CorMaize, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Maize$MaizeProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainMaize <- subset(Maize, split == TRUE)
TestMaize <- subset(Maize, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(MaizeProdSD4 ~ ., data = TrainMaize)
PredMaize <- predict(RandForModel, TestMaize)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/MaizeCorless2ImportanceScore.csv", row.names = T)

# Calculate evaluation metrics
MAE <- mean(abs(PredMaize - TestMaize$MaizeProdSD4))
MSE <- mean((PredMaize - TestMaize$MaizeProdSD4)^2)
RMSE <- sqrt(mean((PredMaize - TestMaize$MaizeProdSD4)^2))
R2 <- 1 - (sum((TestMaize$MaizeProdSD4 - PredMaize)^2) / sum((TestMaize$MaizeProdSD4 - mean(TestMaize$MaizeProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainMaize[, -1], 
                      y = TrainMaize$MaizeProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestMaize[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/MaizeCorless2ShapVals.csv", row.names = T)

# Potatoes
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Potatoes <- read.csv("Potatoes ProdSD4 SpilloverSD3 G44.csv", header = T)
## Make Potatoes data frame
Potatoes <- Potatoes %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(PotatoesProd, PotatoesProdLogDiff, PotatoesProdSD2, PotatoesProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(PotatoesProdSD4 ~ ., data = Potatoes)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/PotatoeStepBackwardCoeff.csv", row.names = T)


model1 <- lm(PotatoesProdSD4~1,data=Potatoes)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4, method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/PotatoeStepForwardCoeff.csv", row.names = T)

Potatoes <- Potatoes %>%
  subset(select = c(PotatoesProdSD4, RuralPopul, tas, AgriLand, CattleRawMilkProdSD4, 
                    CattleRawMilkProd, AgriLandShare, ChickensMeatProdSD4, pr, 
                    rx1day, TempChange))

### Make Correlation Matrix
NumeriCols <- sapply(Potatoes, is.numeric)
CorPotatoes <- cor(Potatoes[NumeriCols])
corrplot(CorPotatoes, method = "number", type = "lower", diag = F, number.cex = 0.7)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Potatoes$PotatoesProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainPotatoes <- subset(Potatoes, split == TRUE)
TestPotatoes <- subset(Potatoes, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(PotatoesProdSD4 ~ ., data = TrainPotatoes)
PredPotatoes <- predict(RandForModel, TestPotatoes)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/Potatoes2Corless2ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredPotatoes - TestPotatoes$PotatoesProdSD4))
MSE <- mean((PredPotatoes - TestPotatoes$PotatoesProdSD4)^2)
RMSE <- sqrt(mean((PredPotatoes - TestPotatoes$PotatoesProdSD4)^2))
R2 <- 1 - (sum((TestPotatoes$PotatoesProdSD4 - PredPotatoes)^2) / sum((TestPotatoes$PotatoesProdSD4 - mean(TestPotatoes$PotatoesProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainPotatoes[, -1], 
                      y = TrainPotatoes$PotatoesProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestPotatoes[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/PotatoesCorless2ShapVals.csv", row.names = T)




# Rice
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Rice <- read.csv("Rice ProdSD4 SpilloverSD4 G37.csv", header = T)
## Make Rice data frame
Rice <- Rice %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(RiceProd, RiceProdLogDiff, RiceProdSD2, RiceProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(RiceProdSD4 ~ ., data = Rice)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/RiceStepBackwardCoeff.csv", row.names = T)


model1 <- lm(RiceProdSD4~1,data=Rice)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + MaizeProd + 
                      MaizeProdLogDiff + MaizeProdSD2 + MaizeProdSD3 + MaizeProdSD4, method='forward')


CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/RiceStepForwardCoeff.csv", row.names = T)

Rice <- Rice %>%
  subset(select = c(RiceProdSD4, PotatoesProd, rx5day, MaizeProd, tasmin, pr, RuralPopul, 
                    PotatoesProdSD2, CattleRawMilkProdLogDiff, CattleRawMilkProd, 
                    MaizeProdLogDiff, AgriLandShare, CattleRawMilkProdSD4, 
                    RuralPopulGrowth))

### Make Correlation Matrix
NumeriCols <- sapply(Rice, is.numeric)
CorRice <- cor(Rice[NumeriCols])
corrplot(CorRice, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Rice$RiceProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainRice <- subset(Rice, split == TRUE)
TestRice <- subset(Rice, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(RiceProdSD4 ~ ., data = TrainRice)
PredRice <- predict(RandForModel, TestRice)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/Rice3Corless3ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredRice - TestRice$RiceProdSD4))
MSE <- mean((PredRice - TestRice$RiceProdSD4)^2)
RMSE <- sqrt(mean((PredRice - TestRice$RiceProdSD4)^2))
R2 <- 1 - (sum((TestRice$RiceProdSD4 - PredRice)^2) / sum((TestRice$RiceProdSD4 - mean(TestRice$RiceProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainRice[, -1], 
                      y = TrainRice$RiceProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestRice[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/Rice3Corless2ShapVals.csv", row.names = T)


# Soya Beans
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
SoyaBeans <- read.csv("SoyaBeans ProdSD4 SpilloverSD4 G26.csv", header = T)

## Make Soya Beans data frame
SoyaBeans <- SoyaBeans %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(SoyaBeansProd, SoyaBeansProdLogDiff, SoyaBeansProdSD2, SoyaBeansProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(SoyaBeansProdSD4 ~ ., data = SoyaBeans)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/SoyaBeanStepBackwardCoeff.csv", row.names = T)

model1 <- lm(SoyaBeansProdSD4~1,data=SoyaBeans)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + MaizeProd + 
                      MaizeProdLogDiff + MaizeProdSD2 + MaizeProdSD3 + MaizeProdSD4 + HenEggsProd + 
                      HenEggsProdLogDiff + HenEggsProdSD2 + HenEggsProdSD3 + HenEggsProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/SoyaBeanStepForwardCoeff.csv", row.names = T)

SoyaBeans <- SoyaBeans %>%
  subset(select = c(SoyaBeansProdSD4, CattleMeatProd, MaizeProdSD2, PopulLogDiff, PotatoesProd, 
                    rx1day, AgriLand, CattleMeatProdSD4, Area, ChickensMeatProdSD4))

### Make Correlation Matrix
NumeriCols <- sapply(SoyaBeans, is.numeric)
CorSoyaBeans <- cor(SoyaBeans[NumeriCols])
corrplot(CorSoyaBeans, method = "number", type = "lower", diag = F, number.cex = 0.7)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(SoyaBeans$SoyaBeansProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainSoyaBeans <- subset(SoyaBeans, split == TRUE)
TestSoyaBeans <- subset(SoyaBeans, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(SoyaBeansProdSD4 ~ ., data = TrainSoyaBeans)
PredSoyaBeans <- predict(RandForModel, TestSoyaBeans)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/SoyaBeans3Corless2ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4))
MSE <- mean((PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4)^2)
RMSE <- sqrt(mean((PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4)^2))
R2 <- 1 - (sum((TestSoyaBeans$SoyaBeansProdSD4 - PredSoyaBeans)^2) / sum((TestSoyaBeans$SoyaBeansProdSD4 - mean(TestSoyaBeans$SoyaBeansProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainSoyaBeans[, -1], 
                      y = TrainSoyaBeans$SoyaBeansProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestSoyaBeans[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/SoyaBeans3Coreless2ShapVals.csv", row.names = T)



# Sugar Cane
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
SugarCane <- read.csv("SugarCane ProdSD4 SpilloverSD4 G30.csv", header = T)
## Make Sugar Cane data frame
SugarCane <- SugarCane %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(SugarCaneProd, SugarCaneProdLogDiff, SugarCaneProdSD2, 
                     SugarCaneProdSD3, Year))


### Check significance of all variables of the data frame
AllVarModel <- lm(SugarCaneProdSD4 ~ ., data = SugarCane)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/SugarCaneStepBackwardCoeff.csv", row.names = T)


model1 <- lm(SugarCaneProdSD4~1,data=SugarCane)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + MaizeProd + 
                      MaizeProdLogDiff + MaizeProdSD2 + MaizeProdSD3 + MaizeProdSD4 + HenEggsProd + 
                      HenEggsProdLogDiff + HenEggsProdSD2 + HenEggsProdSD3 + HenEggsProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/SugarCaneStepForwardCoeff.csv", row.names = T)


SugarCane <- SugarCane %>%
  subset(select = c(SugarCaneProdSD4, CattleRawMilkProd, ChickensMeatProdSD4, CattleRawMilkProdSD4, 
                    CattleMeatProdSD2, AgriLand, tas, tr, rx5day, pr, HenEggsProd, AgriLandShare, 
                    RuralPopulGrowth, MalePopulSD2, MaizeProdSD2))

### Make Correlation Matrix
NumeriCols <- sapply(SugarCane, is.numeric)
CorSugarCane <- cor(SugarCane[NumeriCols])
corrplot(CorSugarCane, method = "number", type = "lower", diag = F, number.cex = 0.7)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(SugarCane$SugarCaneProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainSugarCane <- subset(SugarCane, split == TRUE)
TestSugarCane <- subset(SugarCane, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(SugarCaneProdSD4 ~ ., data = TrainSugarCane)
PredSugarCane <- predict(RandForModel, TestSugarCane)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/SugarCane3Corless2ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredSugarCane - TestSugarCane$SugarCaneProdSD4))
MSE <- mean((PredSugarCane - TestSugarCane$SugarCaneProdSD4)^2)
RMSE <- sqrt(mean((PredSugarCane - TestSugarCane$SugarCaneProdSD4)^2))
R2 <- 1 - (sum((TestSugarCane$SugarCaneProdSD4 - PredSugarCane)^2) / sum((TestSugarCane$SugarCaneProdSD4 - mean(TestSugarCane$SugarCaneProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainSugarCane[, -1], 
                      y = TrainSugarCane$SugarCaneProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestSugarCane[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/SugarCane3Corless2ShapVals.csv", row.names = T)


# Wheat
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Wheat <- read.csv("Wheat ProdSD4 SpilloverSD4 G40.csv", header = T)
## Make Wheat data frame
Wheat <- Wheat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(WheatProd, WheatProdLogDiff, WheatProdSD2, WheatProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(WheatProdSD4 ~ ., data = Wheat)

### Check necessary variables with backward steps
StepBack <- step(AllVarModel,method="backward")

CoeffDB <- summary(StepBack)[4]

write.csv(CoeffDB, file = "FeatureSelection/WheatStepBackwardCoeff.csv", row.names = T)

model1 <- lm(WheatProdSD4~1,data=Wheat)
StepForward <- step(model1,scope=~Area + Population + PopulGrowth + PopulationSD2 + PopulLogDiff + 
                      MalePopul + MalePopulGrowth + MalePopulSD2 + MalePopulLogDiff + RuralPopul + 
                      RuralPopulGrowth + RuralPopulSD2 + RuralPopulLogDiff + AgriLandShare + AgriLand + 
                      IrrigationEquippedLand + TempChange + tas + tasmax + tasmin + tr + tnn + txx + pr + 
                      rx1day + rx5day + fd + CattleMeatProd + CattleMeatProdLogDiff + 
                      CattleMeatProdSD2 + CattleMeatProdSD3 + CattleMeatProdSD4 + CattleRawMilkProd + 
                      CattleRawMilkProdLogDiff + CattleRawMilkProdSD2 + CattleRawMilkProdSD3 + 
                      CattleRawMilkProdSD4 + ChickensMeatProd + ChickensMeatProdLogDiff + 
                      ChickensMeatProdSD2 + ChickensMeatProdSD3 + ChickensMeatProdSD4 + PotatoesProd + 
                      PotatoesProdLogDiff + PotatoesProdSD2 + PotatoesProdSD3 + PotatoesProdSD4, 
                    method='forward')

CoeffDF <- summary(StepForward)[4]

write.csv(CoeffDF, file = "FeatureSelection/WheatStepForwardCoeff.csv", row.names = T)

Wheat <- Wheat %>%
  subset(select = c(WheatProdSD4, AgriLand, IrrigationEquippedLand, CattleMeatProd, 
                    fd, AgriLandShare, PotatoesProd, CattleRawMilkProdSD4, Area, PotatoesProdSD3, 
                    pr, tr, CattleMeatProdSD4, RuralPopulLogDiff, CattleMeatProdLogDiff))

### Make Correlation Matrix
NumeriCols <- sapply(Wheat, is.numeric)
CorWheat <- cor(Wheat[NumeriCols])
corrplot(CorWheat, method = "number", type = "lower", diag = F, number.cex = 0.7)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Wheat$WheatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainWheat <- subset(Wheat, split == TRUE)
TestWheat <- subset(Wheat, split == FALSE)

# Make Random Forest Model
RandForModel <- randomForest(WheatProdSD4 ~ ., data = TrainWheat)
PredWheat <- predict(RandForModel, TestWheat)

# Get feature importance scores
ImportanceScores <- importance(RandForModel)

# Sort features by importance
SortedFeatures <- ImportanceScores[order(-ImportanceScores[, 1]), , drop = FALSE]

write.csv(SortedFeatures, file = "FeatureSelection/Wheat3Corless2ImportanceScore.csv", row.names = T)


# Calculate evaluation metrics
MAE <- mean(abs(PredWheat - TestWheat$WheatProdSD4))
MSE <- mean((PredWheat - TestWheat$WheatProdSD4)^2)
RMSE <- sqrt(mean((PredWheat - TestWheat$WheatProdSD4)^2))
R2 <- 1 - (sum((TestWheat$WheatProdSD4 - PredWheat)^2) / sum((TestWheat$WheatProdSD4 - mean(TestWheat$WheatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


# Create an explainer object using DALEX package
ExpRandFor <- explain(model = RandForModel, data = TrainWheat[, -1], 
                      y = TrainWheat$WheatProdSD4, label = "Random Forest Model")

# Define a new observation for explanation
NewObservation <- TestWheat[1, -1]

# Generate SHAP attributions
ShapeValues <- shap(ExpRandFor, new_observation = NewObservation)

# Assuming 'output_data' is your data frame or object
write.csv(ShapeValues, file = "ShapValues/Wheat3Corless2ShapVals.csv", row.names = T)
