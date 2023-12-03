
library(dplyr)
library(tidyverse)
library(corrplot) # using for corrplot
library(caTools) # using for spliting the data frame
library(car)
library(rpart) # using for Regression Tree Model
library(randomForest) # using for Random Forest Model
library(xgboost) # using for Gradient Boosting Model



setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Bananas <- read.csv("Bananas ProdSD4 SpilloverSD4 G28.csv", header = T)

## Make Bananas data frame
Bananas <- Bananas %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(BananasProd, BananasProdLogDiff, BananasProdSD2, BananasProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(BananasProdSD4 ~ ., data = Bananas)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Bananas$BananasProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainBananas <- subset(Bananas, split == TRUE)
TestBananas <- subset(Bananas, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(BananasProdSD4 ~ ., data = TrainBananas)
summary(LinRegModel)
# Make predictions on the test dataset
PredBananas <- predict(LinRegModel, newdata = TestBananas)

# Make a Regression Tree model
RegTreeModel <- rpart(BananasProdSD4 ~ ., data = TrainBananas)
PredBananas <- predict(RegTreeModel, TestBananas)


# Make Random Forest Model
RandForModel <- randomForest(BananasProdSD4 ~ ., data = TrainBananas)
PredBananas <- predict(RandForModel, TestBananas)


# Make Gradient Boosting Model
GradBoostModel <- xgboost(data = as.matrix(TrainBananas[, -c(3)]), label = TrainBananas$BananasProdSD3, 
                          nrounds = 10)
PredBananas <- predict(GradBoostModel, as.matrix(TestBananas[, -c(3)]))

# Calculate evaluation metrics
MAE <- mean(abs(PredBananas - TestBananas$BananasProdSD4))
MSE <- mean((PredBananas - TestBananas$BananasProdSD4)^2)
RMSE <- sqrt(mean((PredBananas - TestBananas$BananasProdSD4)^2))
R2 <- 1 - (sum((TestBananas$BananasProdSD4 - PredBananas)^2) / sum((TestBananas$BananasProdSD4 - mean(TestBananas$BananasProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")



# Meat of Cattle
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
CattleMeat <- read.csv("CattleMeat ProdSD4 SpilloverSD3 G44.csv", header = T)

## Make Meat of Cattle data frame
CattleMeat <- CattleMeat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(CattleMeatProd, CattleMeatProdSD3, CattleMeatProdSD2, CattleMeatProdLogDiff, Year))


### Check significance of all variables of the data frame
AllVarModel <- lm(CattleMeatProdSD4 ~ ., data = CattleMeat)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(CattleMeat$CattleMeatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainCattleMeat <- subset(CattleMeat, split == TRUE)
TestCattleMeat <- subset(CattleMeat, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(CattleMeatProdSD4 ~ ., data = TrainCattleMeat)
summary(LinRegModel)
# Make predictions on the test dataset
PredCattleMeat <- predict(LinRegModel, newdata = TestCattleMeat)

# Make a Regression Tree model
RegTreeModel <- rpart(CattleMeatProdSD4 ~ ., data = TrainCattleMeat)
PredCattleMeat <- predict(RegTreeModel, TestCattleMeat)


# Make Random Forest Model
RandForModel <- randomForest(CattleMeatProdSD4 ~ ., data = TrainCattleMeat)
PredCattleMeat <- predict(RandForModel, TestCattleMeat)

# Make Gradient Boosting Model
GradBoostModel <- xgboost(data = as.matrix(TrainCattleMeat[, -c(3)]), 
                          label = TrainCattleMeat$CattleMeatProdSD3, nrounds = 1000)
PredCattleMeat <- predict(GradBoostModel, as.matrix(TestCattleMeat[, -c(3)]))

# Calculate evaluation metrics
MAE <- mean(abs(PredCattleMeat - TestCattleMeat$CattleMeatProdSD4))
MSE <- mean((PredCattleMeat - TestCattleMeat$CattleMeatProdSD4)^2)
RMSE <- sqrt(mean((PredCattleMeat - TestCattleMeat$CattleMeatProdSD4)^2))
R2 <- 1 - (sum((TestCattleMeat$CattleMeatProdSD4 - PredCattleMeat)^2) / sum((TestCattleMeat$CattleMeatProdSD4 - mean(TestCattleMeat$CattleMeatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")



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


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(SugarCane$SugarCaneProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainSugarCane <- subset(SugarCane, split == TRUE)
TestSugarCane <- subset(SugarCane, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(SugarCaneProdSD4 ~ ., data = TrainSugarCane)
summary(LinRegModel)
# Make predictions on the test dataset
PredSugarCane <- predict(LinRegModel, newdata = TestSugarCane)

# Make a Regression Tree model
RegTreeModel <- rpart(SugarCaneProdSD4 ~ ., data = TrainSugarCane)
PredSugarCane <- predict(RegTreeModel, TestSugarCane)

# Make Random Forest Model
RandForModel <- randomForest(SugarCaneProdSD4 ~ ., data = TrainSugarCane)
PredSugarCane <- predict(RandForModel, TestSugarCane)


# Calculate evaluation metrics
MAE <- mean(abs(PredSugarCane - TestSugarCane$SugarCaneProdSD4))
MSE <- mean((PredSugarCane - TestSugarCane$SugarCaneProdSD4)^2)
RMSE <- sqrt(mean((PredSugarCane - TestSugarCane$SugarCaneProdSD4)^2))
R2 <- 1 - (sum((TestSugarCane$SugarCaneProdSD4 - PredSugarCane)^2) / sum((TestSugarCane$SugarCaneProdSD4 - mean(TestSugarCane$SugarCaneProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


# Raw Milk of Cattle
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
CattleRawMilk <- read.csv("CattleRawMilk ProdSD4 SpilloverSD3 G44.csv", header = T)

## Make Raw Milk of Cattle data frame
CattleRawMilk <- CattleRawMilk %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(CattleRawMilkProd, CattleRawMilkProdLogDiff, CattleRawMilkProdSD2, CattleRawMilkProdSD3, 
                     Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(CattleRawMilkProdSD4 ~ ., data = CattleRawMilk)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(CattleRawMilk$CattleRawMilkProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainCattleRawMilk <- subset(CattleRawMilk, split == TRUE)
TestCattleRawMilk <- subset(CattleRawMilk, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(CattleRawMilkProdSD4 ~ ., data = TrainCattleRawMilk)
summary(LinRegModel)
# Make predictions on the test dataset
PredCattleRawMilk <- predict(LinRegModel, newdata = TestCattleRawMilk)

# Make a Regression Tree model
RegTreeModel <- rpart(CattleRawMilkProdSD4 ~ ., data = TrainCattleRawMilk)
PredCattleRawMilk <- predict(RegTreeModel, TestCattleRawMilk)

# Make Random Forest Model
RandForModel <- randomForest(CattleRawMilkProdSD4 ~ ., data = TrainCattleRawMilk)
PredCattleRawMilk <- predict(RandForModel, TestCattleRawMilk)

# Calculate evaluation metrics
MAE <- mean(abs(PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4))
MSE <- mean((PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4)^2)
RMSE <- sqrt(mean((PredCattleRawMilk - TestCattleRawMilk$CattleRawMilkProdSD4)^2))
R2 <- 1 - (sum((TestCattleRawMilk$CattleRawMilkProdSD4 - PredCattleRawMilk)^2) / sum((TestCattleRawMilk$CattleRawMilkProdSD4 - mean(TestCattleRawMilk$CattleRawMilkProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")



# Meat of Chickens
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
ChickensMeat <- read.csv("ChickensMeat ProdSD4 SpilloverSD3 G44.csv", header = T)

## Make Meat of Chickens data frame
ChickensMeat <- ChickensMeat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(ChickensMeatProd, ChickensMeatProdLogDiff, ChickensMeatProdSD2, ChickensMeatProdSD3, 
                     Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(ChickensMeatProdSD4 ~ ., data = ChickensMeat)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(ChickensMeat$ChickensMeatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainChickensMeat <- subset(ChickensMeat, split == TRUE)
TestChickensMeat <- subset(ChickensMeat, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(ChickensMeatProdSD4 ~ ., data = TrainChickensMeat)
summary(LinRegModel)
# Make predictions on the test dataset
PredChickensMeat <- predict(LinRegModel, newdata = TestChickensMeat)

# Make a Regression Tree model
RegTreeModel <- rpart(ChickensMeatProdSD4 ~ ., data = TrainChickensMeat)
PredChickensMeat <- predict(RegTreeModel, TestChickensMeat)

# Make Random Forest Model
RandForModel <- randomForest(ChickensMeatProdSD4 ~ ., data = TrainChickensMeat)
PredChickensMeat <- predict(RandForModel, TestChickensMeat)

# Calculate evaluation metrics
MAE <- mean(abs(PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4))
MSE <- mean((PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4)^2)
RMSE <- sqrt(mean((PredChickensMeat - TestChickensMeat$ChickensMeatProdSD4)^2))
R2 <- 1 - (sum((TestChickensMeat$ChickensMeatProdSD4 - PredChickensMeat)^2) / sum((TestChickensMeat$ChickensMeatProdSD4 - mean(TestChickensMeat$ChickensMeatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")




# Hen Eggs
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
HenEggs <- read.csv("HenEggs ProdSD4 SpilloverSD3 G42.csv", header = T)

## Make Hen Eggs data frame
HenEggs <- HenEggs %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(HenEggsProd, HenEggsProdLogDiff, HenEggsProdSD2, HenEggsProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(HenEggsProdSD4 ~ ., data = HenEggs)

# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(HenEggs$HenEggsProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainHenEggs <- subset(HenEggs, split == TRUE)
TestHenEggs <- subset(HenEggs, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(HenEggsProdSD4 ~ ., data = TrainHenEggs)
summary(LinRegModel)
# Make predictions on the test dataset
PredHenEggs <- predict(LinRegModel, newdata = TestHenEggs)

# Make a Regression Tree model
RegTreeModel <- rpart(HenEggsProdSD4 ~ ., data = TrainHenEggs)
PredHenEggs <- predict(RegTreeModel, TestHenEggs)

# Make Random Forest Model
RandForModel <- randomForest(HenEggsProdSD4 ~ ., data = TrainHenEggs)
PredHenEggs <- predict(RandForModel, TestHenEggs)

# Calculate evaluation metrics
MAE <- mean(abs(PredHenEggs - TestHenEggs$HenEggsProdSD4))
MSE <- mean((PredHenEggs - TestHenEggs$HenEggsProdSD4)^2)
RMSE <- sqrt(mean((PredHenEggs - TestHenEggs$HenEggsProdSD4)^2))
R2 <- 1 - (sum((TestHenEggs$HenEggsProdSD4 - PredHenEggs)^2) / sum((TestHenEggs$HenEggsProdSD4 - mean(TestHenEggs$HenEggsProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")




# Maize
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Maize <- read.csv("Maize ProdSD4 SpilloverSD3 G43.csv", header = T)

## Make Maize data frame
Maize <- Maize %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(MaizeProd, MaizeProdLogDiff, MaizeProdSD2, MaizeProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(MaizeProdSD4 ~ ., data = Maize)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Maize$MaizeProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainMaize <- subset(Maize, split == TRUE)
TestMaize <- subset(Maize, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(MaizeProdSD4 ~ ., data = TrainMaize)
summary(LinRegModel)
# Make predictions on the test dataset
PredMaize <- predict(LinRegModel, newdata = TestMaize)

# Make a Regression Tree model
RegTreeModel <- rpart(MaizeProdSD4 ~ ., data = TrainMaize)
PredMaize <- predict(RegTreeModel, TestMaize)

# Make Random Forest Model
RandForModel <- randomForest(MaizeProdSD4 ~ ., data = TrainMaize)
PredMaize <- predict(RandForModel, TestMaize)

# Calculate evaluation metrics
MAE <- mean(abs(PredMaize - TestMaize$MaizeProdSD4))
MSE <- mean((PredMaize - TestMaize$MaizeProdSD4)^2)
RMSE <- sqrt(mean((PredMaize - TestMaize$MaizeProdSD4)^2))
R2 <- 1 - (sum((TestMaize$MaizeProdSD4 - PredMaize)^2) / sum((TestMaize$MaizeProdSD4 - mean(TestMaize$MaizeProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")




# Potatoes
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Potatoes <- read.csv("Potatoes ProdSD4 SpilloverSD3 G44.csv", header = T)

## Make Potatoes data frame
Potatoes <- Potatoes %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(PotatoesProd, PotatoesProdLogDiff, PotatoesProdSD2, PotatoesProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(PotatoesProdSD4 ~ ., data = Potatoes)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Potatoes$PotatoesProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainPotatoes <- subset(Potatoes, split == TRUE)
TestPotatoes <- subset(Potatoes, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(PotatoesProdSD4 ~ ., data = TrainPotatoes)
summary(LinRegModel)
# Make predictions on the test dataset
PredPotatoes <- predict(LinRegModel, newdata = TestPotatoes)

# Make a Regression Tree model
RegTreeModel <- rpart(PotatoesProdSD4 ~ ., data = TrainPotatoes)
PredPotatoes <- predict(RegTreeModel, TestPotatoes)

# Make Random Forest Model
RandForModel <- randomForest(PotatoesProdSD4 ~ ., data = TrainPotatoes)
PredPotatoes <- predict(RandForModel, TestPotatoes)

# Calculate evaluation metrics
MAE <- mean(abs(PredPotatoes - TestPotatoes$PotatoesProdSD4))
MSE <- mean((PredPotatoes - TestPotatoes$PotatoesProdSD4)^2)
RMSE <- sqrt(mean((PredPotatoes - TestPotatoes$PotatoesProdSD4)^2))
R2 <- 1 - (sum((TestPotatoes$PotatoesProdSD4 - PredPotatoes)^2) / sum((TestPotatoes$PotatoesProdSD4 - mean(TestPotatoes$PotatoesProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")



# Rice
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Rice <- read.csv("Rice ProdSD4 SpilloverSD4 G37.csv", header = T)

## Make Rice data frame
Rice <- Rice %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(RiceProd, RiceProdLogDiff, RiceProdSD2, RiceProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(RiceProdSD4 ~ ., data = Rice)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Rice$RiceProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainRice <- subset(Rice, split == TRUE)
TestRice <- subset(Rice, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(RiceProdSD4 ~ ., data = TrainRice)
summary(LinRegModel)
# Make predictions on the test dataset
PredRice <- predict(LinRegModel, newdata = TestRice)

# Make a Regression Tree model
RegTreeModel <- rpart(RiceProdSD4 ~ ., data = TrainRice)
PredRice <- predict(RegTreeModel, TestRice)

# Make Random Forest Model
RandForModel <- randomForest(RiceProdSD4 ~ ., data = TrainRice)
PredRice <- predict(RandForModel, TestRice)

# Calculate evaluation metrics
MAE <- mean(abs(PredRice - TestRice$RiceProdSD4))
MSE <- mean((PredRice - TestRice$RiceProdSD4)^2)
RMSE <- sqrt(mean((PredRice - TestRice$RiceProdSD4)^2))
R2 <- 1 - (sum((TestRice$RiceProdSD4 - PredRice)^2) / sum((TestRice$RiceProdSD4 - mean(TestRice$RiceProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")



# Soya Beans
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
SoyaBeans <- read.csv("SoyaBeans ProdSD4 SpilloverSD4 G26.csv", header = T)

## Make Soya Beans data frame
SoyaBeans <- SoyaBeans %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(SoyaBeansProd, SoyaBeansProdLogDiff, SoyaBeansProdSD2, SoyaBeansProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(SoyaBeansProdSD4 ~ ., data = SoyaBeans)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(SoyaBeans$SoyaBeansProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainSoyaBeans <- subset(SoyaBeans, split == TRUE)
TestSoyaBeans <- subset(SoyaBeans, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(SoyaBeansProdSD4 ~ ., data = TrainSoyaBeans)
summary(LinRegModel)
# Make predictions on the test dataset
PredSoyaBeans <- predict(LinRegModel, newdata = TestSoyaBeans)

# Make a Regression Tree model
RegTreeModel <- rpart(SoyaBeansProdSD4 ~ ., data = TrainSoyaBeans)
PredSoyaBeans <- predict(RegTreeModel, TestSoyaBeans)

# Make Random Forest Model
RandForModel <- randomForest(SoyaBeansProdSD4 ~ ., data = TrainSoyaBeans)
PredSoyaBeans <- predict(RandForModel, TestSoyaBeans)

# Calculate evaluation metrics
MAE <- mean(abs(PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4))
MSE <- mean((PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4)^2)
RMSE <- sqrt(mean((PredSoyaBeans - TestSoyaBeans$SoyaBeansProdSD4)^2))
R2 <- 1 - (sum((TestSoyaBeans$SoyaBeansProdSD4 - PredSoyaBeans)^2) / sum((TestSoyaBeans$SoyaBeansProdSD4 - mean(TestSoyaBeans$SoyaBeansProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


# Wheat
setwd("C:/Users/Saeed/Desktop/Dissertation/Products")
Wheat <- read.csv("Wheat ProdSD4 SpilloverSD4 G40.csv", header = T)

## Make Wheat data frame
Wheat <- Wheat %>%
  mutate(Area = as.numeric(factor(Area))) %>%
  subset(select = -c(WheatProd, WheatProdLogDiff, WheatProdSD2, WheatProdSD3, Year))

### Check significance of all variables of the data frame
AllVarModel <- lm(WheatProdSD4 ~ ., data = Wheat)


# Set a seed for reproducibility
set.seed(144)

# Split the data into 70% training and 30% testing
split <- sample.split(Wheat$WheatProdSD4, SplitRatio = 0.7)

# Create the training and testing datasets
TrainWheat <- subset(Wheat, split == TRUE)
TestWheat <- subset(Wheat, split == FALSE)

# Fit a linear regression model
LinRegModel <- lm(WheatProdSD4 ~ ., data = TrainWheat)
summary(LinRegModel)
# Make predictions on the test dataset
PredWheat <- predict(LinRegModel, newdata = TestWheat)

# Make a Regression Tree model
RegTreeModel <- rpart(WheatProdSD4 ~ ., data = TrainWheat)
PredWheat <- predict(RegTreeModel, TestWheat)

# Make Random Forest Model
RandForModel <- randomForest(WheatProdSD4 ~ ., data = TrainWheat)
PredWheat <- predict(RandForModel, TestWheat)

# Calculate evaluation metrics
MAE <- mean(abs(PredWheat - TestWheat$WheatProdSD4))
MSE <- mean((PredWheat - TestWheat$WheatProdSD4)^2)
RMSE <- sqrt(mean((PredWheat - TestWheat$WheatProdSD4)^2))
R2 <- 1 - (sum((TestWheat$WheatProdSD4 - PredWheat)^2) / sum((TestWheat$WheatProdSD4 - mean(TestWheat$WheatProdSD4))^2))

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared (R2):", R2, "\n")


