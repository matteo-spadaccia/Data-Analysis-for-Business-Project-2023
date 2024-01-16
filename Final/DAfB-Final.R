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

#detach("package:carData", unload = TRUE)
#detach("package:GGally", unload = TRUE)

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
car_prices = read.csv("data/CarPrices.csv",
                      sep = ",",
                      dec = ".",
                      header = T,
                      colClasses = "character")

print("Cars dataframe:")
str(car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
# CLEANING THE DATASET

# checking for id duplicates - NO DUPLICATES FOUND
length(unique(car_prices$car_ID)) # checks for id duplicates
#n_occur <- data.frame(table(car_prices$car_ID)) # obtains a table with occurrences of each id
#n_occur[n_occur$Freq > 1,] # extracts the duplicated ids and their frequencies
#car_prices[car_prices$car_ID %in% n_occur$Var1[n_occur$Freq > 1],] # no duplicates row

# checking for raw duplicates (excluding ID) - NO DUPLICATES FOUND
nrow(car_prices[,-1] %>% distinct())

# checking for missing values - NO MISSING VALUES FOUND
sum(is.na(car_prices))

# checking for unary variables - NO UNARY VARIABLES FOUND
length(car_prices[sapply(car_prices,function(x) length(unique(x))<2)])

# correctly saving numerical values
cleaned_car_prices = car_prices
# generates a list of numerical variables saved as characters strings
numericalVariables <- c('wheelbase', 'carlength', 'carwidth', 'carheight', 'curbweight', 'enginesize', 'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 'citympg', 'highwaympg', 'price')
for (tempVar in numericalVariables) {
  cleaned_car_prices[[tempVar]] <- as.numeric(cleaned_car_prices[[tempVar]])
}


## -------------------------------------------------------------------------------------------------------------------------------
# Creating a column for the company name
cleaned_car_prices$carCompany <- word(cleaned_car_prices$CarName, 1)

# Correcting some typos
carCompanyList <- unique(cleaned_car_prices$carCompany)
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "porcshce"] <- "porsche"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "vokswagen"] <- "volkswagen"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "vw"] <- "volkswagen"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "toyouta"] <- "toyota"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "Nissan"] <- "nissan"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "maxda"] <- "mazda"
cleaned_car_prices$carCompany[cleaned_car_prices$carCompany == "alfa-romero"] <- "alfaromeo"

carCompanyList <- unique(cleaned_car_prices$fuelsystem)
cleaned_car_prices$fuelsystem[cleaned_car_prices$fuelsystem == "mfi"] <- "mpfi"

# printing the cleaned dataframe
print("Cleaned dataframe:")
str(cleaned_car_prices)
# showing cleaned data-frame's statistics
summary(cleaned_car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
# ENCODING CATEGORICAL VALUES

encoded_car_prices <- cleaned_car_prices # copies the data-frame into a new one to be encoded
categoricalVariables <- c('symboling', 'fueltype', 'aspiration', 'doornumber', 'carbody', 'drivewheel', 'enginelocation', 'enginetype', 'cylindernumber', 'fuelsystem', 'carCompany') 

# factorizing categorical values
fact_car_prices <- cleaned_car_prices # copies the data-frame into a new one to be factorized
for (tempVar in categoricalVariables) {  # iterates through the list of categorical variables to factorize them
  fact_car_prices[[tempVar]] <- as.factor(cleaned_car_prices[[tempVar]])
}

encoded_car_prices <- one_hot(as.data.table(fact_car_prices)) # encodes the categorical variables
colnames(encoded_car_prices)[which(names(encoded_car_prices) == "symboling_-1")] <- "symboling_minus_1"
colnames(encoded_car_prices)[which(names(encoded_car_prices) == "symboling_-2")] <- "symboling_minus_2"

# printing encoded data-frame
str(encoded_car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
# CORRELATION MATRIX

corr_matrix = round(cor(subset(encoded_car_prices %>% select(numericalVariables))), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
# SCATTERPLOTS

numericalRegressors = numericalVariables[1:(length(numericalVariables) - 1)]

for (tempVar in numericalRegressors){
  # Price over tempVar
  plot(fact_car_prices[,tempVar],
       fact_car_prices$price,
       main=paste("Scatterplot: price over", tempVar),
       xlab=tempVar,
       ylab="price",
       pch=1)

  abline(lm(fact_car_prices$price~fact_car_prices[,tempVar]), col="red") # regression line (y~x) 
  lines(lowess(fact_car_prices[,tempVar],fact_car_prices$price), col="blue") # lowess line (x,y)
}


## -------------------------------------------------------------------------------------------------------------------------------
# BOXBLOT OF THE VARIABLE TO PREDICT

boxplot(fact_car_prices$price, col = c("lightblue"), ylab = " ")


## -------------------------------------------------------------------------------------------------------------------------------
# PIE CHARTS

for (tempVar in categoricalVariables){
  pie(table(cleaned_car_prices[,tempVar]),
      main = paste("Pie chart of: ", paste(tempVar)),
      labels = paste(round(prop.table(table(cleaned_car_prices[,tempVar])) * 100, 2), "%"),
      col = rainbow(length(table(cleaned_car_prices[,tempVar]))),
      clockwise = TRUE)
  
  legend("topright",
         legend = names(table(cleaned_car_prices[,tempVar])),
         cex = 0.8,
         fill = rainbow(length(table(cleaned_car_prices[,tempVar]))))
}



## -------------------------------------------------------------------------------------------------------------------------------
# BOXPLOTS OF DISTRIBUTION OF CAR PRICES ACROSS DIFFERENT CATEGORICAL VARIABLES

for (tempVar in categoricalVariables){
  boxplot(fact_car_prices$price ~ fact_car_prices[,tempVar], col = c("lightblue"), xlab = tempVar, ylab = "price", tick = FALSE, las = 2)
}


## -------------------------------------------------------------------------------------------------------------------------------
# SCATTERPLOT OF CARHEIGHT OVER WHEELBASE COLOR-CODED BY CARBODY

ggplot(cleaned_car_prices, aes(x = wheelbase, y = carheight, col = carbody)) + geom_point()


## -------------------------------------------------------------------------------------------------------------------------------
# ENGINEERED FEATURE CREATION AND EDA

# data-frame containing feature engineered variables
eng_car_prices <- fact_car_prices

# ledge = carlenght - weelbase
eng_car_prices$ledge = fact_car_prices$carlength-fact_car_prices$wheelbase
encoded_car_prices$ledge = eng_car_prices$ledge
print("Correlation level among car prices and ledge: ")
print(cor(eng_car_prices$price, eng_car_prices$ledge))
ggplot(eng_car_prices,aes(x=ledge,y=price,col=carbody))+geom_point()

# mpg = highwaympg x citympg
eng_car_prices$mpg <- rowMeans(eng_car_prices[,c("highwaympg", "citympg")], na.rm=TRUE)
encoded_car_prices$mpg = eng_car_prices$mpg
print("Correlation level among car prices and mpg: ")
print(cor(eng_car_prices$price, eng_car_prices$mpg))
ggplot(eng_car_prices,aes(x=mpg,y=price,col=carbody))+geom_point() 

# adding the new variables to different lists of regressors that will be useful later
numericalVariables <- append(numericalVariables, c("ledge", "mpg"), after = length(numericalVariables))
numericalRegressors <- append(numericalRegressors, c("ledge", "mpg"), after = length(numericalVariables))
numericalRegressors <- numericalRegressors[numericalRegressors != "highwaympg" & numericalRegressors != "citympg"]


## -------------------------------------------------------------------------------------------------------------------------------
# BOX-PLOTS TO UNDERSTAND IF THE DATA IS SKEWED 

for (tempVar in numericalVariables){
  boxplot(eng_car_prices[tempVar], col = c("cornflowerblue"), ylab = tempVar)
  hist(eng_car_prices[tempVar], col = "lightblue", ylab = tempVar)
}


## -------------------------------------------------------------------------------------------------------------------------------
# FINDING AND REMOVING THE OUTLIERS 

# Function to identify outliers based on IQR
out_id_iqr <- function (x,     # vector of observations
                        m){   # multiplicative constant

  # Compute the interval bounds
  Q1 <- quantile(x, p = 0.25)
  Q3 <- quantile(x, p = 0.75)
  lb <- Q1 - m * IQR(x)
  ub <- Q3 + m * IQR(x)
  
  # Return the ids of the outliers
  out <- which(x < lb | x > ub)
  return(out)
}

# Example with some columns
for (tempVar in numericalVariables){
  eng_car_prices[out_id_iqr(eng_car_prices[,tempVar], 3),][,tempVar]
}


## -------------------------------------------------------------------------------------------------------------------------------
# NORMALIZATION OF THE DATA

norm_car_prices <- eng_car_prices[,c(numericalRegressors, "price", "ledge", "mpg")]
norm_car_prices <- as.data.frame(scale(norm_car_prices,
                                       center = apply(norm_car_prices, 2, min),
                                       scale = apply(norm_car_prices, 2, max) - apply(norm_car_prices, 2, min)))

priceNormCoeff1 <- (max(eng_car_prices$price) - min(eng_car_prices$price))
priceNormCoeff2 <- min(eng_car_prices$price)

norm_car_prices <- cbind(norm_car_prices,
                         select(encoded_car_prices,
                                -c(numericalVariables,
                                   "price", "car_ID", "CarName", "highwaympg", "citympg")))
NNregressors <- colnames(norm_car_prices)


## -------------------------------------------------------------------------------------------------------------------------------
# SINGLE LINEAR REGRESSION 

# NUMERICAL VARIABLES
# Fit the linear regression model with enginesize
SLR_enginesize = lm(formula = price ~ enginesize, data = eng_car_prices)
hist(SLR_enginesize$residuals, main = paste("Residuals' distribution of price SLR model with engine size")) # Evaluating its residuals

# Fit the linear regression model with curbweight
SLR_curbweight = lm(formula = price ~ curbweight, data = eng_car_prices)
hist(SLR_curbweight$residuals, main = paste("Residuals' distribution of price SLR model with curb weight")) # Evaluating its residuals

# Fit the linear regression model with horsepower
SLR_horsepower = lm(formula = price ~ horsepower, data = eng_car_prices)
hist(SLR_horsepower$residuals, main = paste("Residuals' distribution of price SLR model with horsepower")) # Evaluating its residuals

# Fit the linear regression model with carwidth
SLR_carwidth = lm(formula = price ~ carwidth, data = eng_car_prices)
hist(SLR_carwidth$residuals, main = paste("Residuals' distribution of price SLR model with car width")) # Evaluating its residuals


# ENGINEERED FEATURES
# Fit the linear regression model with ledge
SLR_ledge = lm(formula = price ~ ledge, data = eng_car_prices)
hist(SLR_ledge$residuals, main = paste("Residuals' distribution of price SLR model with ledge")) # Evaluating its residuals

# Fit the linear regression model with mpg
SLR_mpg = lm(formula = price ~ mpg, data = eng_car_prices)
hist(SLR_mpg$residuals, main = paste("Residuals' distribution of price SLR model with mpg")) # Evaluating its residuals


# CATEGORICAL FEATURES
# Fit the linear regression model with carCompany
SLR_carCompany = lm(formula = price ~ carCompany, data = eng_car_prices)
hist(SLR_carCompany$residuals, main = paste("Residuals' distribution of price SLR model with car company")) # Evaluating its residuals

# Fit the linear regression model with enginelocation
SLR_enginelocation = lm(formula = price ~ enginelocation, data = eng_car_prices)
hist(SLR_enginelocation$residuals, main = paste("Residuals' distribution of price SLR model with engine location")) # Evaluating its residuals

# Fit the linear regression model with enginetype
SLR_enginetype = lm(formula = price ~ enginetype, data = eng_car_prices)
hist(SLR_enginetype$residuals, main = paste("Residuals' distribution of price SLR model with engine type")) # Evaluating its residuals

# Fit the linear regression model with cylindernumber
SLR_cylindernumber = lm(formula = price ~ cylindernumber, data = eng_car_prices)
hist(SLR_cylindernumber$residuals, main = paste("Residuals' distribution of price SLR model with cylinder number")) # Evaluating its residuals

# Fit the linear regression model with fuelsystem
SLR_fuelsystem = lm(formula = price ~ fuelsystem, data = eng_car_prices)
hist(SLR_fuelsystem$residuals, main = paste("Residuals' distribution of price SLR model with fuel system")) # Evaluating its residuals


## -------------------------------------------------------------------------------------------------------------------------------
# SPLITING THE DATA INTO TRAINING AND TEST SET WITH VALIDATION SET APPROACH

training_samples <- eng_car_prices$price %>% createDataPartition(p = 0.8, list = FALSE)
train_data <- eng_car_prices[training_samples,]
test_data <- eng_car_prices[-training_samples,]

norm_train_data <- norm_car_prices[training_samples,]
norm_test_data <- norm_car_prices[-training_samples,]


## -------------------------------------------------------------------------------------------------------------------------------
# CREATING A LIST OF THE REGRESSORS 

regressorsList <- colnames(eng_car_prices)
regressorsList <- regressorsList[!regressorsList == "car_ID" & !regressorsList == "CarName" & !regressorsList == "price" & !regressorsList == "highwaympg" & !regressorsList == "citympg"]


## -------------------------------------------------------------------------------------------------------------------------------
# CREATING A DATAFRAME TO KEEP TRACK OF THE PERFORMANCE OF THE MODELS WE WILL IMPLEMENT

bestPerfDFCols = c("Record" , # In what is the best model
                   "ModelName", # Name given to the model
                   "RegressorsNum", # Number of regressors used
                   "AIC", # AIC score (for linear models only)
                   "BIC", # BIC score (for linear models only)
                   "lambda", # Value of lambda (for elastic network only)
                   "alpha", # Number of alpha (for elastic network only)
                   "TreesNum", # Number of trees used (for random forests only)
                   "LayersNum", # Number of layers used (for neural networks only)
                   "MSEtrain", # Value of mean square error in train data-set
                   "RMSEtrain", # Value of root mean square error in train data-set
                   "MSEtest", #  Value of mean square error in test data-set
                   "RMSEtest", #  Value of root mean square error in test data-set
                   "R2", # Value of R^2
                   "Model") # Actual model

best_perf_df = data.frame(matrix(nrow = 0, ncol = length(bestPerfDFCols))) 
colnames(best_perf_df) = bestPerfDFCols


## -------------------------------------------------------------------------------------------------------------------------------
# AIC AND BIC STEPWISE SELECTION

# creating list to store the serialized models
AICBICModelsList <- list() 

# define intercept-only model
intercept_only <- lm(price ~ 1, data = train_data[,c("price", regressorsList)]) 
# define model with all predictors
all <- lm(price ~ ., data = train_data[,c("price", regressorsList)]) 

# AIC
# AIC forward
AIC_forward <- step(intercept_only,
                    direction = "forward",
                    scope = formula(all),
                    trace = 0)

new_row <- data.frame(Record = NA,
                      ModelName = "AIC forward selection",
                      RegressorsNum = length(coef(AIC_forward)) - 1,
                      AIC = AIC(AIC_forward),
                      BIC = BIC(AIC_forward),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(AIC_forward, train_data),
                      RMSEtrain = rmse(AIC_forward, train_data),
                      MSEtest = mse(AIC_forward, test_data),
                      RMSEtest = rmse(AIC_forward, test_data),
                      R2 = summary(AIC_forward)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(AIC_forward,NULL)

# AIC backward
AIC_backward<- step(all,
                    direction = "backward",
                    scope = formula(all),
                    trace = 0)

new_row <- data.frame(Record = NA,
                      ModelName = "AIC backward selection",
                      RegressorsNum = length(coef(AIC_forward)) - 1,
                      AIC = AIC(AIC_backward),
                      BIC = BIC(AIC_backward),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(AIC_backward, train_data),
                      RMSEtrain = rmse(AIC_backward, train_data),
                      MSEtest = mse(AIC_backward, test_data),
                      RMSEtest = rmse(AIC_backward, test_data),
                      R2 = summary(AIC_backward)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(AIC_backward,NULL)


# AIC forward/backward
AIC_both <- step(intercept_only,
                 direction = "both",
                 scope = formula(all),
                 trace = 0,
                 cipolla = cavolfiore) #cute easter egg

new_row <- data.frame(Record = NA,
                      ModelName = "AIC forward/backward selection",
                      RegressorsNum = length(coef(AIC_both)) - 1,
                      AIC = AIC(AIC_both),
                      BIC = BIC(AIC_both),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(AIC_both, train_data),
                      RMSEtrain = rmse(AIC_both, train_data),
                      MSEtest = mse(AIC_both, test_data),
                      RMSEtest = rmse(AIC_both, test_data),
                      R2 = summary(AIC_both)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(AIC_both,NULL)


# BIC
# BIC forward
BIC_forward <- step(intercept_only,
                    direction = "forward",
                    scope = formula(all),
                    trace = 0,
                    k = log(nrow(train_data)))

new_row <- data.frame(Record = NA,
                      ModelName = "BIC forward selection",
                      RegressorsNum = length(coef(BIC_forward)) - 1,
                      AIC = AIC(BIC_forward),
                      BIC = BIC(BIC_forward),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(BIC_forward, train_data),
                      RMSEtrain = rmse(BIC_forward, train_data),
                      MSEtest = mse(BIC_forward, test_data),
                      RMSEtest = rmse(BIC_forward, test_data),
                      R2 = summary(BIC_forward)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(BIC_forward,NULL)

# BIC backward
BIC_backward <- step(all,
                     direction = "backward",
                     scope = formula(all),
                     trace = 0,
                     k = log(nrow(train_data)))

new_row <- data.frame(Record = NA,
                      ModelName = "BIC backward selection",
                      RegressorsNum = length(coef(BIC_backward)) - 1,
                      AIC = AIC(BIC_backward),
                      BIC = BIC(BIC_backward),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(BIC_backward, train_data),
                      RMSEtrain = rmse(BIC_backward, train_data),
                      MSEtest = mse(BIC_backward, test_data),
                      RMSEtest = rmse(BIC_backward, test_data),
                      R2 = summary(BIC_backward)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(BIC_backward,NULL)

# BIC forward/backward
BIC_both <- step(intercept_only,
                 direction = "both",
                 scope = formula(all),
                 trace = 0,
                 k = log(nrow(train_data)))

new_row <- data.frame(Record = NA,
                      ModelName = "BIC forward/backward selection",
                      RegressorsNum = length(coef(BIC_both)) - 1,
                      AIC = AIC(BIC_both),
                      BIC = BIC(BIC_both),
                      lambda = NA, alpha = NA, TreesNum = NA, LayersNum = NA,
                      MSEtrain = mse(BIC_both, train_data),
                      RMSEtrain = rmse(BIC_both, train_data),
                      MSEtest = mse(BIC_both, test_data),
                      RMSEtest = rmse(BIC_both, test_data),
                      R2 = summary(BIC_both)$r.squared,
                      Model = NA
                      )
best_perf_df <- rbind(best_perf_df, new_row)
AICBICModelsList[[length(AICBICModelsList)+1]] <- serialize(BIC_both,NULL)

best_perf_df$Model <- AICBICModelsList


## -------------------------------------------------------------------------------------------------------------------------------
# ELASTIC NET

EN_perf_df = data.frame(matrix(nrow = 0, ncol = length(bestPerfDFCols))) 
colnames(best_perf_df) = bestPerfDFCols
ENModelsList <- list()

for (alpha in seq(0, 1, by=0.05)) {
  
  #perform k-fold cross-validation to find optimal lambda value
  tempCVModel <- cv.glmnet(data.matrix(train_data[,regressorsList]),
                           train_data$price,
                           alpha = alpha)
  
  ##find coefficients of best model with optimal lambda value (that minimizes test MSE)
  tempModel <- glmnet(data.matrix(train_data[,regressorsList]),
                      train_data$price,
                      alpha = alpha,
                      lambda = tempCVModel$lambda.min)
  
  ENModelsList[[length(ENModelsList)+1]] <- serialize(tempModel,NULL)
  
  tempMSEtrain = sum((train_data$price - predict(tempModel, newx = data.matrix(train_data[,regressorsList])))^2) / nrow(train_data)
  
  tempMSEtest = sum((test_data$price - predict(tempModel, newx = data.matrix(test_data[,regressorsList])))^2) / nrow(test_data)
  
  predicted <- predict(tempModel, newx = data.matrix(train_data[,regressorsList]), s = "lambda.min", type = "response")
  tempR2 <- (cor(predicted, train_data$price))^2 
  
  EN_perf_df <- rbind(EN_perf_df,
                      data.frame(Record = NA,
                                 ModelName = "EL",
                                 RegressorsNum = NA,
                                 AIC = NA, BIC = NA,
                                 lambda = tempCVModel$lambda.min,
                                 alpha = alpha,
                                 TreesNum = NA, LayersNum = NA,
                                 MSEtrain = tempMSEtrain,
                                 RMSEtrain = sqrt(tempMSEtrain),
                                 MSEtest = tempMSEtest,
                                 RMSEtest = sqrt(tempMSEtrain),
                                 R2 = tempR2,
                                 Model = NA))
}

EN_perf_df$Model <- ENModelsList

# Best Elastic Network Models
newRow = EN_perf_df[which.min(EN_perf_df$MSEtrain),]
newRow$Record <- "Elastic Network lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)

newRow = EN_perf_df[which.min(EN_perf_df$MSEtest),]
newRow$Record <- "Elastic Network lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
RFregressors = c("enginesize", "horsepower", "curbweight", "ledge", "mpg", "carCompany", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")


## -------------------------------------------------------------------------------------------------------------------------------
# ALL RANDOM FORESTS (max 30 trees)
RF_perf_df = data.frame()
RFModelsList <- list()

for (tempVarNum in c(1:length(RFregressors))){
  tempVarCombinations <- (combn(RFregressors, tempVarNum, fun=NULL, simplify=FALSE))
  
  for (tempVarComb in tempVarCombinations){
    
    for (tempTreeNum in c(1:30)){
      tempModel = randomForest(formula = as.formula(paste("price ~ ",
                                                          paste(unlist(tempVarComb),
                                                                sep ="",
                                                                collapse = " + "),
                                                          sep = "")),
      data = train_data, ntree = tempTreeNum, keep.forest = TRUE, importance = TRUE)
      RFModelsList[[length(RFModelsList)+1]] <- serialize(tempModel,NULL)
      
      tempTSS <- sum((mean(test_data$price) - predict(tempModel, test_data))^2)
      tempRSS <- sum((test_data$price - predict(tempModel, test_data))^2)
      tempR2 <- 1 - (tempRSS / tempTSS)
      
      RF_perf_df <- rbind(RF_perf_df,
                          data.frame(Record = NA,
                                     ModelName = paste("RF: ",
                                                       paste(unlist(tempVarComb),
                                                             sep = "",
                                                             collapse = ", "),
                                                       sep = ""),
                                     RegressorsNum = tempVarNum,
                                     AIC = NA, BIC = NA, lambda = NA, alpha = NA,
                                     TreesNum = tempTreeNum,
                                     LayersNum = NA,
                                     MSEtrain = mse(tempModel, train_data),
                                     RMSEtrain = rmse(tempModel, train_data),
                                     MSEtest = mse(tempModel, test_data),
                                     RMSEtest = rmse(tempModel, test_data),
                                     R2 = tempR2,
                                     Model = NA))
    }
  }
}

RF_perf_df$Model <- RFModelsList

# best Random Forests consideration
newRow = RF_perf_df[which.min(RF_perf_df$MSEtrain),]
newRow$Record <- "Random Forest lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)

newRow = RF_perf_df[which.min(RF_perf_df$MSEtest),]
newRow$Record <- "Random Forest lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
# EVALUATING LOWER-ERROR RANDOM FORESTS'

# evaluating residuals distribution
tempModel <- unserialize(RF_perf_df$Model[[which.min(RF_perf_df$MSEtrain)]])
hist(predict(tempModel, newdata = train_data) - train_data$price, main = paste("Residuals' distribution of price for best RF for MSE on train"))

tempModel <- unserialize(RF_perf_df$Model[[which.min(RF_perf_df$MSEtest)]])
hist(predict(tempModel, newdata = test_data) - test_data$price, main = paste("Residuals' distribution of price for best RF for MSE on test"))

# evaluating best predicting Random Forest's attributes and its variables' importance
tempModel
tempImpData <- as.data.frame(importance(tempModel))
tempImpData$Var.Names <- row.names(tempImpData)

ggplot(tempImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment(aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())


## -------------------------------------------------------------------------------------------------------------------------------
# IMPLEMENTATION OF NEURAL NETWORKS

NN_perf_df = data.frame()
NNModelsList <- list()
maxLayers = 2
maxNodesperLayer = 18

for (tempLayersNum in c(1:maxLayers)){
  tempHiddenCombinations <- permutations(n=maxNodesperLayer,r=maxLayers,v=c(1:maxNodesperLayer),repeats.allowed=T)
  
  for (tempHidden in tempHiddenCombinations){
    tempModel <- neuralnet(formula = as.formula(paste("price ~ ",
                                                          paste(unlist(NNregressors),
                                                                sep ="",
                                                                collapse = " + "),
                                                          sep = "")),
                           data = norm_train_data,
                           hidden = tempHidden,
                           linear.output = TRUE)
    
    NNModelsList[[length(NNModelsList)+1]] <- serialize(tempModel,NULL)
    
    tempMSEtrain = sum((train_data$price - (compute(tempModel, norm_train_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)/nrow(norm_test_data)
    
    tempMSEtest = sum((test_data$price - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)/nrow(norm_test_data)

    tempTSS <- sum((mean(test_data$price) - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)
    tempRSS <- sum((test_data$price - (compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 + priceNormCoeff2))^2)
    tempR2 <- 1 - (tempRSS / tempTSS)
    
    NN_perf_df <- rbind(NN_perf_df,
                        data.frame(Record = NA,
                                   ModelName = "NN: all regressors",
                                   RegressorsNum = 25,
                                   AIC = NA, BIC = NA, lambda = NA, alpha = NA, TreesNum = NA,
                                   LayersNum = tempLayersNum,
                                   MSEtrain = tempMSEtrain,
                                   RMSEtrain = sqrt(tempMSEtrain),
                                   MSEtest = tempMSEtest,
                                   RMSEtest = sqrt(tempMSEtest),
                                   R2 = tempR2,
                                   Model = NA))
  }
}

NN_perf_df$Model <- NNModelsList

# best Neural Networks consideration
newRow = NN_perf_df[which.min(NN_perf_df$MSEtrain),]
newRow$Record <- "Neural Network lower MSE error on train"
best_perf_df <- rbind(best_perf_df, newRow)

newRow = NN_perf_df[which.min(NN_perf_df$MSEtest),]
newRow$Record <- "Neural Network lower MSE error on test"
best_perf_df <- rbind(best_perf_df, newRow)


## -------------------------------------------------------------------------------------------------------------------------------
# EVALUATING LOWER-ERROR NEURAL NETWORKS'

# evaluating residuals distribution
tempModel <- unserialize(NN_perf_df$Model[[which.min(NN_perf_df$MSEtrain)]])
hist((predict(tempModel, norm_train_data)* priceNormCoeff1 + priceNormCoeff2) - train_data$price)

tempModel <- unserialize(NN_perf_df$Model[[which.min(NN_perf_df$MSEtest)]])
hist((predict(tempModel, norm_test_data)* priceNormCoeff1 + priceNormCoeff2) - test_data$price, main = "")

# evaluating best predicting Neural Network's attributes
plot(tempModel)
plot(test_data$price, compute(tempModel, norm_test_data)$net.result * priceNormCoeff1 +priceNormCoeff2, col = "red", main = 'Real vs Predicted')
abline(0, 1, lwd = 2, col = "red")


## -------------------------------------------------------------------------------------------------------------------------------
# OBSERVING OUR BEST MODELS CHARACTERISTICS

rownames(best_perf_df) <- NULL
best_perf_df


## -------------------------------------------------------------------------------------------------------------------------------
# DATA PREPARATION

cluster_car_prices <- cbind(norm_car_prices[,c(numericalVariables[numericalVariables != "citympg" & numericalVariables != "highwaympg"]),],
                            cleaned_car_prices[categoricalVariables])


## -------------------------------------------------------------------------------------------------------------------------------
# FOR EDA SECTION:

ggplot(cluster_car_prices, aes(wheelbase, carheight, shape = factor(carbody))) + geom_point(aes(colour = factor(carbody)))


## -------------------------------------------------------------------------------------------------------------------------------
# K-MEANS CLUSTERING WITH TWO QUANTITATIVE VARIABLES

fviz_nbclust(cluster_car_prices[,c("wheelbase", "carheight")],
             FUNcluster = kmeans,
             method = "silhouette",
             iter.max=30 ) # optimal number of clusters: 2
fviz_nbclust(cluster_car_prices[,c("wheelbase", "carheight")],
             FUNcluster = kmeans,
             method = "wss",
             iter.max=30) # optimal number of clusters: 3
fviz_nbclust(cluster_car_prices[,c("wheelbase", "carheight")],
             FUNcluster = kmeans,
             method = "gap_stat",
             iter.max=30) # optimal number of clusters: 3

tempClustersNum = 3

kMeans1 <- kmeans(cluster_car_prices[,c("wheelbase", "carheight")], centers = tempClustersNum, nstart=50, iter.max=100)

kMeans1$tot.withinss # total within sum of squares (to minimize)
kMeans1$betweenss # between clusters sum of squares (to maximize)
# (overfitting is possible if, as seen in the plot, the clusters are overlapping a lot)
kMeans1 # BSS/TSS ratio should approach 1

ggplot(cluster_car_prices, aes(wheelbase, carheight, shape = factor(kMeans1$cluster))) + geom_point(aes(colour = factor(kMeans1$cluster)))

tab_kMeans1 = table(cluster_car_prices$carbody, kMeans1$cluster)
pander(prop.table(tab_kMeans1, 1), caption = "Table")

cluster_car_prices$sizeClustered = kMeans1$cluster

levels(cluster_car_prices$sizeClustered) <- c(levels(cluster_car_prices$sizeClustered), "S", "M", "L")
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '1'] <- "S"
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '2'] <- "L"
cluster_car_prices$sizeClustered[cluster_car_prices$sizeClustered == '3'] <- "M"

cluster_car_prices %>% select(carbody, sizeClustered) %>% group_by(carbody, sizeClustered) %>% count() %>%
  ggplot(aes(x = carbody, y = n, fill = sizeClustered)) + 
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL) +
  coord_flip()


## -------------------------------------------------------------------------------------------------------------------------------
# K-MEANS CLUSTERING WITH ALL THE QUANTITATIVE VARIABLES

fviz_nbclust(cluster_car_prices[numericalRegressors],
             FUNcluster = kmeans,
             method = "silhouette",
             iter.max=30 ) # optimal number of clusters: 2
fviz_nbclust(cluster_car_prices[numericalRegressors],
             FUNcluster = kmeans,
             method = "wss",
             iter.max=30) # optimal number of clusters: 2
fviz_nbclust(cluster_car_prices[numericalRegressors],
             FUNcluster = kmeans,
             method = "gap_stat",
             iter.max=30) # optimal number of clusters: 1

tempClustersNum = 2

kMeans2 <- kmeans(cluster_car_prices[numericalRegressors],
                       centers = tempClustersNum,
                       nstart = 10,
                       iter.max=100)

kMeans2$tot.withinss # total within sum of squares (to minimize)
kMeans2$betweenss # between clusters sum of squares (to maximize)
# (overfitting is possible if, as seen in the plot, the clusters are overlapping a lot)
kMeans2 # BSS/TSS ratio should approach 1

cluster_car_prices$priceLevel[cluster_car_prices$price >= 0.33] <- "Luxury"
cluster_car_prices$priceLevel[cluster_car_prices$price < 0.33 & cluster_car_prices$price >= 0.08] <- "Standard"
cluster_car_prices$priceLevel[cluster_car_prices$price < 0.08] <- "Economic"

fviz_cluster(kMeans2, data = cluster_car_prices[numericalRegressors]) +
  theme_minimal() + ggtitle("Title") 

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
# HIERARCHICAL CLUSTERING USING EUCLIDEAN DISTANCE MATRIX WITH ALL THE QUANTITATIVE VARIABLES

distanceMatrixEuclidean <- dist(cluster_car_prices[numericalRegressors],
                       method = "euclidean")


# WARD.D2 METHOD

dendogramWardD2_Euclidean <- hclust(distanceMatrixEuclidean, method = "ward.D2") 
plot(dendogramWardD2_Euclidean, cex=0.2)

tempClustersNum = 4

tempGroups <- cutree(dendogramWardD2_Euclidean, k = tempClustersNum)
rect.hclust(dendogramWardD2_Euclidean, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors],
                  cluster = tempGroups)) + theme_minimal() + ggtitle("Euclidian-distance-based Ward D2 clustering, k = 4") 

cluster_car_prices$companyClustered1 = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$companyClustered1)){
#    tempDF = cluster_car_prices[cluster_car_prices$companyClustered1 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_eucl_wd2 = table(cluster_car_prices$carCompany, cluster_car_prices$companyClustered1)
pander(prop.table(tab_hier_eucl_wd2, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$carCompany == "toyota",])


# COMPLETE METHOD

dendogramComplete_Euclidean <- hclust(distanceMatrixEuclidean, method = "complete") 
plot(dendogramComplete_Euclidean, cex=0.2)

tempClustersNum = 3

tempGroups <- cutree(dendogramComplete_Euclidean, k = tempClustersNum) 
rect.hclust(dendogramComplete_Euclidean, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors],
                  cluster = tempGroups)) + theme_minimal() + ggtitle("Euclidian-distance-based Complete clustering, k = 3")

cluster_car_prices$companyClustered2 = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$companyClustered2)){
#    tempDF = cluster_car_prices[cluster_car_prices$companyClustered2 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_eucl_comp = table(cluster_car_prices$carCompany, cluster_car_prices$companyClustered2)
pander(prop.table(tab_hier_eucl_comp, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$carCompany == "toyota",])


## -------------------------------------------------------------------------------------------------------------------------------
# HIERARCHICAL CLUSTERING USING CORRELATION DISTANCE MATRIX WITH ALL THE QUANTITATIVE VARIABLES

distanceMatrixCor = as.dist(1 - cor(t(cluster_car_prices[numericalRegressors])))


# Ward.D2 method

dendogramWardD2_Cor = hclust(distanceMatrixCor, method="ward.D2")
plot(dendogramWardD2_Cor, cex=0.2)

tempClustersNum = 4

tempGroups <- cutree(dendogramWardD2_Cor, k = tempClustersNum)
rect.hclust(dendogramWardD2_Cor, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors],
                  cluster = tempGroups))  + theme_minimal() + ggtitle("Correlation-distance-based Ward D2 clustering, k = 4") 
cluster_car_prices$fuelSystemClustered1 = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$fuelSystemClustered1)){
#    tempDF = cluster_car_prices[cluster_car_prices$fuelSystemClustered1 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_cor_wd2 = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered1)
pander(prop.table(tab_hier_cor_wd2, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$fuelsystem == "1bbl",])


# Complete method

dendogramComplete_Cor = hclust(distanceMatrixCor, method="complete")
plot(dendogramComplete_Cor, cex=0.2)

tempClustersNum = 4

tempGroups <- cutree(dendogramComplete_Cor, k = tempClustersNum)
rect.hclust(dendogramComplete_Cor, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors],
                  cluster = tempGroups))  + theme_minimal() + ggtitle("Correlation-distance-based Complete clustering, k = 4")

cluster_car_prices$fuelSystemClustered2 = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$fuelSystemClustered2)){
#    tempDF = cluster_car_prices[cluster_car_prices$fuelSystemClustered2 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_cor_comp = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered2)
pander(prop.table(tab_hier_cor_comp, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$fuelsystem == "1bbl",])


## -------------------------------------------------------------------------------------------------------------------------------
# CLUSTERING USING GOWER DISTANCE MATRIX WITH ALL THE VARIABLES

# Create a Gower distance matrix using cluster data
distanceMatrixGower <- cluster::daisy(select(eng_car_prices, -c("car_ID", "CarName", "carbody", "citympg", "highwaympg")), metric = "gower")


# Ward.D2 method

dendogramWardD2_Gow = hclust(distanceMatrixGower, method="ward.D2")
plot(dendogramWardD2_Gow, cex=0.2)

tempClustersNum = 3

tempGroups <- cutree(dendogramWardD2_Gow, k = tempClustersNum)
rect.hclust(dendogramWardD2_Gow, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors], cluster = tempGroups)) +
  theme_minimal() + ggtitle("Gower-distance-based Ward D2 clustering, k = 3")

cluster_car_prices$fuelSystemClustered3 = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$fuelSystemClustered3)){
#    tempDF = cluster_car_prices[cluster_car_prices$fuelSystemClustered3 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_gow_wd2 = table(cluster_car_prices$fuelsystem, cluster_car_prices$fuelSystemClustered3)
pander(prop.table(tab_hier_gow_wd2, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$fuelsystem == "1bbl",])


# Complete method

dendogramComplete_Gow = hclust(distanceMatrixGower, method="complete")
plot(dendogramComplete_Gow, cex=0.2)

tempClustersNum = 2

tempGroups <- cutree(dendogramComplete_Gow, k = tempClustersNum)
rect.hclust(dendogramComplete_Gow, k = tempClustersNum, border = "blue")

fviz_cluster(list(data = cluster_car_prices[numericalRegressors], cluster = tempGroups)) +
  theme_minimal() + ggtitle("Gower-distance-based Complete D2 clustering, k = 2")

cluster_car_prices$cylindernumberClustered = tempGroups

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$cylindernumberClustered)){
#    tempDF = cluster_car_prices[cluster_car_prices$cylindernumberClustered == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

tab_hier_gow_comp = table(cluster_car_prices$cylindernumber, cluster_car_prices$cylindernumberClustered)
pander(prop.table(tab_hier_gow_comp, 1), caption = "Table")

# count(cluster_car_prices[cluster_car_prices$cylindernumber == "four",])


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

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$TotalClusterization1)){
#    tempDF = cluster_car_prices[cluster_car_prices$TotalClusterization1 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

#table3 = table(cluster_car_prices$fuelsystem, cluster_car_prices$TotalClusterization1)
#pander(prop.table(table3, 1), caption = "Table")
#table3 = as.data.frame(prop.table(table3, 1))
##detach("package:GGally", unload = TRUE)
#table3 = table3 %>% rename(fuelsystem = Var1, TotalClusterization1 = Var2)

# count(cluster_car_prices[cluster_car_prices$fuelsystem == "1bbl",])

tempClustersNum <- 5

cluster_car_prices <- cbind(cluster_car_prices, TotalClusterization2 = as.factor(pam(distanceMatrixGower, diss = TRUE, tempClustersNum)$clustering))

#for (tempVar in colnames(cluster_car_prices)){
#  c = 0
#  for (tempG in unique(cluster_car_prices$TotalClusterization2)){
#    tempDF = cluster_car_prices[cluster_car_prices$TotalClusterization2 == tempG,]
#    c = c + count(unique(tempDF[tempVar]))
#    print(paste(tempVar, tempG, count(unique(tempDF[tempVar])), "/", count(unique(cluster_car_prices[tempVar]))))
#  }
#  print(paste(tempVar, ":", c, "/", count(unique(cluster_car_prices[tempVar])), "=", #c/count(unique(cluster_car_prices[tempVar]))))
#}

#table3 = table(cluster_car_prices$fuelsystem, cluster_car_prices$TotalClusterization2)
#pander(prop.table(table3, 1), caption = "Table")
#table3 = as.data.frame(prop.table(table3, 1))
##detach("package:GGally", unload = TRUE)
#table3 = table3 %>% rename(fuelsystem = Var1, TotalClusterization2 = Var2)

# count(cluster_car_prices[cluster_car_prices$fuelsystem == "1bbl",])


## -------------------------------------------------------------------------------------------------------------------------------
# SPECIFIC ANALYSIS OF tempClusterization

# HARD TO RUN!!!
# in case of need decomment the next line and insert the focus clusterization

# tempClusterization = "TotalClusterization1"

# Plot 1: Number of categorical tempVar per cluster
for (tempVar in categoricalVariables){
  print(
    cluster_car_prices[c(tempClusterization, tempVar)] %>%
      group_by_at(c(tempClusterization, tempVar)) %>%
      count() %>%
      ggplot(aes_string(x = tempClusterization, y = "n", fill = tempVar)) +
      geom_col() + 
      coord_flip() +
      labs(y = NULL) +
      theme(legend.position = "top")
  )
}

# Plot 2: Percentage of categorical tempVar per cluster
for (tempVar in categoricalVariables){
  print(
    cluster_car_prices[c(tempClusterization, tempVar)] %>%
      group_by_at(c(tempClusterization, tempVar)) %>%
      count() %>%
      ggplot(aes_string(x = tempClusterization, y = "n", fill = tempVar)) +
      geom_col(position = "fill") + 
      coord_flip() +
      labs(y = NULL) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top") +
#      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(labels = scales::percent)
  )
}

# Plot 3: Two categorical tempVars comparison with the clusters
for (tempVar1 in categoricalVariables){
  for (tempVar2 in categoricalVariables){
    print(
      cluster_car_prices[c(tempClusterization, tempVar1, tempVar2)] %>%
#        mutate(aspiration = fct_recode(aspiration, "standard" = "std", "turbo" = "turbo"), fueltype = fct_recode(fueltype, "gas" = "gas", "diesel" = "diesel")) %>%
        group_by_at(c(tempClusterization, tempVar1, tempVar2)) %>%
        count() %>%
        ggplot(aes_string(x = tempVar1, y = "n", fill = tempVar2)) +
        geom_col() +
        facet_grid(paste(". ~", tempClusterization)) +
        theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_brewer(palette = "Set1") +
        labs(x = NULL, y = NULL)
    )
  }
}

# Plot 4: Histogram of numerical tempVar bins grouped by clusters
for (tempVar in c(numericalRegressors, "price")){
  print(
    cluster_car_prices[c(tempClusterization, tempVar)] %>%
      ggplot(aes_string(x = tempVar, fill = tempClusterization)) +
      geom_histogram(data = cluster_car_prices,alpha = 0.4, bins = 30) +
      labs(x = tempVar, y = NULL)
  )
}

# Plot 5: Boxplot of numerical tempVar1 bins grouped by categorical tempVar2 and clusters
for (tempVar1 in numericalRegressors){
  for (tempVar2 in categoricalVariables){
    print(
      cluster_car_prices[c(tempClusterization, tempVar1, tempVar2)] %>%
      ggplot(aes_string(x = tempClusterization, y = tempVar1, fill = tempClusterization)) +
      geom_boxplot() +
#      theme(legend.position = "none") +
      facet_grid(paste(tempVar2, "~ .")) +
      labs(x = tempVar, y = NULL)
    )
  }
}

