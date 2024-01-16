## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
# USEFUL LIBRAIRES (INSTALLING AND) LOADING

#install.packages("knitr")
#install.packages("dplyr")
#install.packages("mltools")
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("purrr")
#install.packages("class")
#install.packages("caret")
#install.packages("factoextra")
#install.packages("Hmisc")
#install.packages("corrgram")
#install.packages("randomForest")
#install.packages("ggcorrplot")
#install.packages("tidyverse")
#install.packages("neuralnet")
#install.packages("gtools")
#install.packages("glmnet")
#install.packages(stringr)
#install.packages("mgcv")
#install.packages("cluster")
#install.packages("FactoMineR")
#install.packages("pander")
knitr::opts_chunk$set(echo = TRUE) # sets report
library(dplyr) # imports library to clean "Unknown" replacing with NA
library(mltools) # imports library to apply the one_hot encoder
library(data.table) # imports library to apply the one_hot encoder
library(ggplot2) # imports library to visualize graphs
library(purrr) # imports library to ease computations
library(class) # imports library for k-nn
library(caret) # imports library for data partition
library(GGally) # imports library for combining geometric objects with transformed data
library(factoextra) # imports library for extracting and visualizing the output of multivariate data analyses
library(Hmisc) # imports library for correlation
library(corrgram) # imports library for correlation
library(ggcorrplot) # imports library for correlation matrix
library(randomForest) # imports library for random forests
library(tidyverse)
library(modelr)
library(broom)
library(neuralnet)
library(gtools)
library(glmnet) # imports library for fitting generalized linear models via penalized maximum likelihood
library(stringr)
library(mgcv) # imports library for GAM
library(cluster) 
library(FactoMineR)
library(pander)
set.seed(42)


## -------------------------------------------------------------------------------------------------------------------------------
# IMPORTING CAR PRICES DATASET

# imports data-set as data-frame from .csv file with relative path
car_prices = read.csv("data/CarPrices.csv", sep = ",", dec = ".", header = T, colClasses = "character")

print("Cars dataframe:")
#str(car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
cleaned_car_prices = car_prices
numericalVariables <- c('wheelbase', 'carlength', 'carwidth', 'carheight', 'curbweight', 'enginesize', 'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 'citympg', 'highwaympg', 'price')
for (tempVar in numericalVariables) {
  cleaned_car_prices[[tempVar]] <- as.numeric(cleaned_car_prices[[tempVar]])}


## -------------------------------------------------------------------------------------------------------------------------------
# Creating a column for the company name
cleaned_car_prices$carCompany <- word(cleaned_car_prices$CarName, 1)

# Correcting some typos
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "porcshce"] <- "porsche"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "vokswagen"] <- "volkswagen"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "vw"] <- "volkswagen"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "toyouta"] <- "toyota"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "Nissan"] <- "nissan"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "maxda"] <- "mazda"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "alfa-romero"] <- "alfaromeo"
cleaned_car_prices$fuelsystem[cleaned_car_prices$fuelsystem == "mfi"] <- "mpfi"


## -------------------------------------------------------------------------------------------------------------------------------
encoded_car_prices <- cleaned_car_prices
categoricalVariables <- c('symboling', 'fueltype', 'aspiration', 'doornumber', 'carbody', 'drivewheel', 'enginelocation', 'enginetype', 'cylindernumber', 'fuelsystem', 'carCompany') 

fact_car_prices <- cleaned_car_prices
for (tempVar in categoricalVariables) {
  fact_car_prices[[tempVar]] <- as.factor(cleaned_car_prices[[tempVar]])}

encoded_car_prices <- one_hot(as.data.table(fact_car_prices))
colnames(encoded_car_prices)[which(names(encoded_car_prices) == "symboling_-1")] <- "symboling_minus_1"
colnames(encoded_car_prices)[which(names(encoded_car_prices) == "symboling_-2")] <- "symboling_minus_2"


## -------------------------------------------------------------------------------------------------------------------------------
corr_matrix = round(cor(subset(encoded_car_prices %>% select(numericalVariables))), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
numericalRegressors = numericalVariables[1:(length(numericalVariables) - 1)]
for (tempVar in numericalRegressors){
  plot(fact_car_prices[,tempVar], fact_car_prices$price, main=paste("Scatterplot: price over", tempVar), xlab=tempVar, ylab="price", pch=1)
  abline(lm(fact_car_prices$price~fact_car_prices[,tempVar]), col="red")
  lines(lowess(fact_car_prices[,tempVar],fact_car_prices$price), col="blue")}


## -------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
for (tempVar in categoricalVariables){
  boxplot(fact_car_prices$price ~ fact_car_prices[,tempVar], col = c("lightblue"), xlab = tempVar, ylab = "price", tick = FALSE, las = 2)}


## -------------------------------------------------------------------------------------------------------------------------------
ggplot(cleaned_car_prices, aes(x = wheelbase, y = carheight, col = carbody)) + geom_point()


## -------------------------------------------------------------------------------------------------------------------------------
eng_car_prices <- fact_car_prices
eng_car_prices$ledge = fact_car_prices$carlength-fact_car_prices$wheelbase
encoded_car_prices$ledge = eng_car_prices$ledge
cor(eng_car_prices$price, eng_car_prices$ledge)

eng_car_prices$mpg <- rowMeans(eng_car_prices[,c("highwaympg", "citympg")], na.rm=TRUE)
encoded_car_prices$mpg = eng_car_prices$mpg
cor(eng_car_prices$price, eng_car_prices$mpg)

numericalVariables <- append(numericalVariables, c("ledge", "mpg"), after = length(numericalVariables))
numericalRegressors <- append(numericalRegressors, c("ledge", "mpg"), after = length(numericalVariables))
numericalRegressors <- numericalRegressors[numericalRegressors != "highwaympg" & numericalRegressors != "citympg"]


## -------------------------------------------------------------------------------------------------------------------------------
norm_car_prices <- eng_car_prices[,c(numericalRegressors, "price", "ledge", "mpg")]
norm_car_prices <- as.data.frame(scale(norm_car_prices, center = apply(norm_car_prices, 2, min), scale = apply(norm_car_prices, 2, max) - apply(norm_car_prices, 2, min)))

priceNormCoeff1 <- (max(eng_car_prices$price) - min(eng_car_prices$price))
priceNormCoeff2 <- min(eng_car_prices$price)

norm_car_prices <- cbind(norm_car_prices, select(encoded_car_prices, -c(numericalVariables, "price", "car_ID", "CarName", "highwaympg", "citympg")))
NNregressors <- colnames(norm_car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))

SLR_enginesize = lm(formula = price ~ enginesize, data = eng_car_prices)
SLR_enginesize_hist <- hist(SLR_enginesize$residuals, main = paste("Residuals' of price SLR with engine size"))

SLR_ledge = lm(formula = price ~ ledge, data = eng_car_prices)
SLR_ledge_hist <-hist(SLR_ledge$residuals, main = paste("Residuals' of price SLR with ledge"))

SLR_mpg = lm(formula = price ~ mpg, data = eng_car_prices)
SLR_mpg_hist <-hist(SLR_mpg$residuals, main = paste("Residuals' of price SLR with mpg"))

SLR_carCompany = lm(formula = price ~ carCompany, data = eng_car_prices)
SLR_carCompany_hist <- hist(SLR_carCompany$residuals, main = paste("Residuals' of price SLR with car company"))


## -------------------------------------------------------------------------------------------------------------------------------
training_samples <- eng_car_prices$price %>% createDataPartition(p = 0.8, list = FALSE)
train_data <- eng_car_prices[training_samples,]
test_data <- eng_car_prices[-training_samples,]

norm_train_data <- norm_car_prices[training_samples,]
norm_test_data <- norm_car_prices[-training_samples,]


## -------------------------------------------------------------------------------------------------------------------------------
regressorsList <- colnames(eng_car_prices)
regressorsList <- regressorsList[!regressorsList == "car_ID" & !regressorsList == "CarName" & !regressorsList == "price" & !regressorsList == "highwaympg" & !regressorsList == "citympg"]


## -------------------------------------------------------------------------------------------------------------------------------
bestPerfDFCols = c("Record",  "ModelName", "RegressorsNum", "AIC", "BIC", "lambda", "alpha", "TreesNum", "LayersNum",  "MSEtrain", "RMSEtrain", "MSEtest", "RMSEtest", "R2", "Model")

best_perf_df = data.frame(matrix(nrow = 0, ncol = length(bestPerfDFCols))) 
colnames(best_perf_df) = bestPerfDFCols


## -------------------------------------------------------------------------------------------------------------------------------
AICBICModelsList <- list() 
intercept_only <- lm(price ~ 1, data = train_data[,c("price", regressorsList)]) 
all <- lm(price ~ ., data = train_data[,c("price", regressorsList)]) 

AIC_forward <- step(intercept_only, direction = "forward", scope = formula(all), trace = 0)
AIC_backward<- step(all, direction = "backward", scope = formula(all), trace = 0)
AIC_both <- step(intercept_only, direction = "both", scope = formula(all), trace = 0)

new_row <- data.frame(Record = NA, ModelName = "AIC forward - backward - forward/backward selection", RegressorsNum = length(coef(AIC_forward)) - 1, AIC = AIC(AIC_forward), BIC = BIC(AIC_forward), lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA, MSEtrain = mse(AIC_forward, train_data), RMSEtrain = rmse(AIC_forward, train_data), MSEtest = mse(AIC_forward, test_data), RMSEtest = rmse(AIC_forward, test_data), R2 = summary(AIC_forward)$r.squared, Model = NA)
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(AIC_forward,NULL)

BIC_forward <- step(intercept_only, direction = "forward", scope = formula(all), trace = 0, k = log(nrow(train_data)))

BIC_backward <- step(all, direction = "backward", scope = formula(all), trace = 0, k = log(nrow(train_data)))
new_row <- data.frame(Record = NA, ModelName = "BIC backward selection", RegressorsNum = length(coef(BIC_backward)) - 1, AIC = AIC(BIC_backward), BIC = BIC(BIC_backward), lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA, MSEtrain = mse(BIC_backward, train_data), RMSEtrain = rmse(BIC_backward, train_data), MSEtest = mse(BIC_backward, test_data), RMSEtest = rmse(BIC_backward, test_data), R2 = summary(BIC_backward)$r.squared, Model = NA)
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(BIC_backward,NULL)

BIC_both <- step(intercept_only, direction = "both", scope = formula(all), trace = 0, k = log(nrow(train_data)))
new_row <- data.frame(Record = NA, ModelName = "BIC forward - backward selection", RegressorsNum = length(coef(BIC_forward)) - 1, AIC = AIC(BIC_forward), BIC = BIC(BIC_forward), lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA, MSEtrain = mse(BIC_forward, train_data), RMSEtrain = rmse(BIC_forward, train_data), MSEtest = mse(BIC_forward, test_data), RMSEtest = rmse(BIC_forward, test_data), R2 = summary(BIC_forward)$r.squared, Model = NA)
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(BIC_forward,NULL)

best_perf_df$Model <- AICBICModelsList


## -------------------------------------------------------------------------------------------------------------------------------
EN_perf_df = data.frame(matrix(nrow = 0, ncol = length(bestPerfDFCols))) 
colnames(best_perf_df) = bestPerfDFCols
ENModelsList <- list()

for (alpha in seq(0, 1, by=0.05)) {
  tempCVModel <- cv.glmnet(data.matrix(train_data[,regressorsList]), train_data$price, alpha = alpha)
  tempModel <- glmnet(data.matrix(train_data[,regressorsList]), train_data$price, alpha = alpha, lambda = tempCVModel$lambda.min)
  
  ENModelsList[[length(ENModelsList)+1]] <- serialize(tempModel,NULL)
  tempMSEtrain = sum((train_data$price - predict(tempModel, newx = data.matrix(train_data[,regressorsList])))^2) / nrow(train_data)
  tempMSEtest = sum((test_data$price - predict(tempModel, newx = data.matrix(test_data[,regressorsList])))^2) / nrow(test_data)
  predicted <- predict(tempModel, newx = data.matrix(train_data[,regressorsList]), s = "lambda.min", type = "response")
  tempR2 <- (cor(predicted, train_data$price))^2 
  
  EN_perf_df <- rbind(EN_perf_df, data.frame(Record = NA, ModelName = "EL", RegressorsNum = NA, AIC = NA, BIC = NA, lambda = tempCVModel$lambda.min, alpha = alpha, TreesNum = NA, LayersNum = NA, MSEtrain = tempMSEtrain, RMSEtrain = sqrt(tempMSEtrain), MSEtest = tempMSEtest, RMSEtest = sqrt(tempMSEtrain), R2 = tempR2, Model = NA))}

EN_perf_df$Model <- ENModelsList

newRow = EN_perf_df[which.min(EN_perf_df$MSEtrain),]
newRow$Record <- "Elastic Network lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)

newRow = EN_perf_df[which.min(EN_perf_df$MSEtest),]
newRow$Record <- "Elastic Network lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
RFregressors = c("enginesize", "horsepower", "curbweight", "ledge", "mpg", "carCompany", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")


## -------------------------------------------------------------------------------------------------------------------------------
RF_perf_df = data.frame()
RFModelsList <- list()

for (tempVarNum in c(1:length(RFregressors))){
  tempVarCombinations <- (combn(RFregressors, tempVarNum, fun=NULL, simplify=FALSE))
  for (tempVarComb in tempVarCombinations){
    for (tempTreeNum in c(1:30)){
      tempModel = randomForest(formula = as.formula(paste("price ~ ", paste(unlist(tempVarComb), sep ="", collapse = " + "), sep = "")),
      data = train_data, ntree = tempTreeNum, keep.forest = TRUE, importance = TRUE)
      
      RFModelsList[[length(RFModelsList)+1]] <- serialize(tempModel,NULL)
      tempTSS <- sum((mean(test_data$price) - predict(tempModel, test_data))^2)
      tempRSS <- sum((test_data$price - predict(tempModel, test_data))^2)
      tempR2 <- 1 - (tempRSS / tempTSS)
      RF_perf_df <- rbind(RF_perf_df, data.frame(Record = NA, ModelName = paste("RF: ", paste(unlist(tempVarComb), sep = "", collapse = ", "), sep = ""), RegressorsNum = tempVarNum, AIC = NA, BIC = NA, lambda = NA, alpha = NA, TreesNum = tempTreeNum, LayersNum = NA, MSEtrain = mse(tempModel, train_data), RMSEtrain = rmse(tempModel, train_data), MSEtest = mse(tempModel, test_data), RMSEtest = rmse(tempModel, test_data), R2 = tempR2, Model = NA))}}}
RF_perf_df$Model <- RFModelsList
newRow = RF_perf_df[which.min(RF_perf_df$MSEtrain),]
newRow$Record <- "Random Forest lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)
newRow = RF_perf_df[which.min(RF_perf_df$MSEtest),]
newRow$Record <- "Random Forest lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
NN_perf_df = data.frame()
NNModelsList <- list()
maxLayers = 2
maxNodesperLayer = 18

for (tempLayersNum in c(1:maxLayers)){
  tempHiddenCombinations <- permutations(n=maxNodesperLayer,r=maxLayers,v=c(1:maxNodesperLayer),repeats.allowed=T)
  
  for (tempHidden in tempHiddenCombinations){
    tempModel <- neuralnet(formula = as.formula(paste("price ~ ", paste(unlist(NNregressors), sep ="", collapse = " + "), sep = "")), data = norm_train_data, hidden = tempHidden, linear.output = TRUE)
    
    NNModelsList[[length(NNModelsList)+1]] <- serialize(tempModel,NULL)
    tempMSEtrain = sum((train_data$price - (compute(tempModel, norm_train_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)/nrow(norm_test_data)
    tempMSEtest = sum((test_data$price - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)/nrow(norm_test_data)
    tempTSS <- sum((mean(test_data$price) - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)
    tempRSS <- sum((test_data$price - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)
    tempR2 <- 1 - (tempRSS / tempTSS)
    NN_perf_df <- rbind(NN_perf_df, data.frame(Record = NA, ModelName = "NN: all regressors", RegressorsNum = 25, AIC = NA, BIC = NA, lambda = NA, alpha = NA, TreesNum = NA, LayersNum = tempLayersNum, MSEtrain = tempMSEtrain, RMSEtrain = sqrt(tempMSEtrain), MSEtest = tempMSEtest, RMSEtest = sqrt(tempMSEtest), R2 = tempR2, Model = NA))}}
NN_perf_df$Model <- NNModelsList
newRow = NN_perf_df[which.min(NN_perf_df$MSEtrain),]
newRow$Record <- "Neural Network lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)
newRow = NN_perf_df[which.min(NN_perf_df$MSEtest),]
newRow$Record <- "Neural Network lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
plot(tempModel)
plot(test_data$price, compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 +priceNormCoeff2, col = "red", main = 'Real vs Predicted')
abline(0, 1, lwd = 2, col = "red")


## -------------------------------------------------------------------------------------------------------------------------------
cluster_car_prices <- cbind(norm_car_prices[,c(numericalVariables[numericalVariables != "citympg" & numericalVariables != "highwaympg"]),], cleaned_car_prices[categoricalVariables])


## -------------------------------------------------------------------------------------------------------------------------------
fviz_nbclust(cluster_car_prices[,c("wheelbase", "carheight")], FUNcluster = kmeans, method = "wss", iter.max=30)
tempClustersNum = 3

kMeans1 <- kmeans(cluster_car_prices[,c("wheelbase", "carheight")], centers = tempClustersNum, nstart=50, iter.max=100)
tab_kMeans1 = table(cluster_car_prices$carbody, kMeans1$cluster)
pander(prop.table(tab_kMeans1, 1), caption = "Table")

cluster_car_prices$sizeClustered = kMeans1$cluster
levels(cluster_car_prices$sizeClustered) <- c(levels(cluster_car_prices$sizeClustered), "S", "M", "L")
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '1'] <- "S"
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '2'] <- "L"
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '3'] <- "M"

cluster_car_prices %>% select(carbody, sizeClustered) %>% group_by(carbody, sizeClustered) %>% count() %>% ggplot(aes(x = carbody, y = n, fill = sizeClustered)) + geom_col(position = "fill") + scale_fill_brewer(palette = "Set3") + scale_y_continuous(labels = scales::percent) + labs(y = NULL) + coord_flip()


## -------------------------------------------------------------------------------------------------------------------------------
fviz_nbclust(cluster_car_prices[numericalRegressors], FUNcluster = kmeans, method = "silhouette", iter.max=30 )
tempClustersNum = 2

kMeans2 <- kmeans(cluster_car_prices[numericalRegressors], centers = tempClustersNum, nstart = 10, iter.max=100)

cluster_car_prices$priceLevel[cluster_car_prices$price >= 0.33] <- "Luxury"
cluster_car_prices$priceLevel[cluster_car_prices$price < 0.33 & cluster_car_prices$price >= 0.08] <- "Standard"
cluster_car_prices$priceLevel[cluster_car_prices$price < 0.08] <- "Economic"

fviz_cluster(kMeans2, data = cluster_car_prices[numericalRegressors]) + theme_minimal() + ggtitle("Title") 

tab_kMeans2 = table(cluster_car_prices$priceLevel, kMeans2$cluster)
pander(prop.table(tab_kMeans2, 1), caption = "Table")

cluster_car_prices$priceLevelClustered = kMeans2$cluster

levels(cluster_car_prices$priceLevelClustered) <- c(levels(cluster_car_prices$priceLevelClustered), "$$$", "$")
cluster_car_prices$priceLevelClustered[cluster_car_prices$priceLevelClustered == '1'] <- "$$$"
cluster_car_prices$priceLevelClustered[cluster_car_prices$priceLevelClustered == '2'] <- "$"

cluster_car_prices %>% select(priceLevel, priceLevelClustered) %>% group_by(priceLevel, priceLevelClustered) %>% count() %>%
  ggplot(aes(x = priceLevel, y = n, fill = priceLevelClustered)) + 
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL) +
  coord_flip()


## -------------------------------------------------------------------------------------------------------------------------------
distanceMatrixEuclidean <- dist(cluster_car_prices[numericalRegressors], method = "euclidean")

dendogramWardD2_Euclidean <- hclust(distanceMatrixEuclidean, method = "ward.D2")
tempClustersNum = 4
tempGroups <- cutree(dendogramWardD2_Euclidean, k = tempClustersNum)

cluster_car_prices$companyClustered1 = tempGroups
tab_hier_eucl_wd2 = table(cluster_car_prices$carCompany, cluster_car_prices$companyClustered1)
pander(prop.table(tab_hier_eucl_wd2, 1), caption = "Table")

dendogramComplete_Euclidean <- hclust(distanceMatrixEuclidean, method = "complete") 
tempClustersNum = 3
tempGroups <- cutree(dendogramComplete_Euclidean, k = tempClustersNum) 

cluster_car_prices$companyClustered2 = tempGroups
tab_hier_eucl_comp = table(cluster_car_prices$carCompany, cluster_car_prices$companyClustered2)
pander(prop.table(tab_hier_eucl_comp, 1), caption = "Table")


## -------------------------------------------------------------------------------------------------------------------------------
distanceMatrixCor = as.dist(1 - cor(t(cluster_car_prices[numericalRegressors])))

dendogramWardD2_Cor = hclust(distanceMatrixCor, method="ward.D2")
tempClustersNum = 4
tempGroups <- cutree(dendogramWardD2_Cor, k = tempClustersNum)

cluster_car_prices$fuelSystemClustered1 = tempGroups
tab_hier_cor_wd2 = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered1)
pander(prop.table(tab_hier_cor_wd2, 1), caption = "Table")

dendogramComplete_Cor = hclust(distanceMatrixCor, method="complete")
tempClustersNum = 4
tempGroups <- cutree(dendogramComplete_Cor, k = tempClustersNum)

cluster_car_prices$fuelSystemClustered2 = tempGroups
tab_hier_cor_comp = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered2)
pander(prop.table(tab_hier_cor_comp, 1), caption = "Table")


## -------------------------------------------------------------------------------------------------------------------------------
distanceMatrixGower <- cluster::daisy(select(eng_car_prices, -c("car_ID", "CarName", "carbody", "citympg", "highwaympg")), metric = "gower")

dendogramWardD2_Gow = hclust(distanceMatrixGower, method="ward.D2")
tempClustersNum = 3

tempGroups <- cutree(dendogramWardD2_Gow, k = tempClustersNum)

cluster_car_prices$fuelSystemClustered3 = tempGroups

tab_hier_gow_wd2 = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered3)
pander(prop.table(tab_hier_gow_wd2, 1), caption = "Table")

dendogramComplete_Gow = hclust(distanceMatrixGower, method="complete")
plot(dendogramComplete_Gow, cex=0.2)

tempClustersNum = 2

tempGroups <- cutree(dendogramComplete_Gow, k = tempClustersNum)

cluster_car_prices$cylindernumberClustered = tempGroups
tab_hier_gow_comp = table(cluster_car_prices$cylindernumber, cluster_car_prices$cylindernumberClustered)
pander(prop.table(tab_hier_gow_comp, 1), caption = "Table")


## -------------------------------------------------------------------------------------------------------------------------------
# Visualize the optimal number of clusters using Silhouette method
n_clusters <- c(2:8) 
lapply(n_clusters, function(x){
  pam_Fit <- pam(distanceMatrixGower, diss = TRUE, k = x)
  n_SW <- pam_Fit$silinfo$avg.width
  return(n_SW)
}) %>%
  as.numeric() %>% 
  as_tibble() %>%
  ggplot(aes(x = n_clusters, y = value)) + 
  geom_area(fill = "salmon") +
  ggtitle("Optimal Clusters") +
  labs(x = "Number of clusters", y = "Silhouette Width") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = n_clusters)

tempClustersNum <- 2

cluster_car_prices <- cbind(cluster_car_prices, TotalClusterization1 = as.factor(pam(distanceMatrixGower, diss = TRUE, tempClustersNum)$clustering))

tempClustersNum <- 5

cluster_car_prices <- cbind(cluster_car_prices, TotalClusterization2 = as.factor(pam(distanceMatrixGower, diss = TRUE, tempClustersNum)$clustering))

