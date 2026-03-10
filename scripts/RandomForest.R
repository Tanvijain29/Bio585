options("install.lock"=FALSE)



install.packages("caret")
install.packages("dplyr")
install.packages("dslabs")
install.packages("ggplot2")
install.packages("modeldata")
install.packages("purrr")
install.packages("recipes")
install.packages("rsample")
install.packages("stringr")
install.packages("tibble")
install.packages("tidyr")

install.packages("Rtools")

library(caret)
library(dplyr)
library(ggplot2)
library(modeldata)
library(purrr)
library(recipes)
library(rsample)
library(stringr)
library(tibble)
library(tidyr)


## load the data from the modeldata package
ibm_attrition <- modeldata::attrition

## get rid of all of the ordered factors in the dataset
## because this will break things downstream
ibm_df <- ibm_attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

head(ibm_df)

## make our training and test datasets
set.seed(01055852025)
## 70% testing, split by Attrition
split <- initial_split(ibm_df, prop = 0.8, strata = "Attrition")
train <- training(split)
test <- testing(split)

model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = train)
summary(model1)

newdata <- data.frame(MonthlyIncome=seq(min(ibm_df$MonthlyIncome), max(ibm_df$MonthlyIncome),len=500))
newdata$Attrition = predict(model1, newdata, type="response")

## plot the data
## note: Attrition is -1 because needs to convert to 0/1 rather than 1/2
plot(as.numeric(as.factor(Attrition))-1 ~ MonthlyIncome, data=ibm_df, col="red")
lines(Attrition ~ MonthlyIncome, data=newdata)


model2 <- glm(Attrition ~ Age, family = "binomial", data = train)
summary(model2)

newdata2 <- data.frame(Age=seq(min(ibm_df$Age), max(ibm_df$Age),len=500))
newdata2$Attrition = predict(model2, newdata2, type="response")

plot(as.numeric(as.factor(Attrition))-1 ~ Age, data=ibm_df, col="red")
lines(Attrition ~ Age, data=newdata2)

install.packages("AmesHousing")
install.packages("ranger")
library(AmesHousing)
library(dplyr)
library(ggplot2)
library(ranger)



house_df <- AmesHousing::make_ames()
house_split <- initial_split(house_df, prop = 0.8, strata = "Sale_Price")
house_train <- training(house_split)
house_test <- testing(house_split)


## get number of features as number of predictors
## calculated as number of columns minus number of response variables
all_features <- ncol(house_train)
n_features <- all_features - 1 

## train out of the box model as default to start with 
house_default <- ranger(Sale_Price ~ ., data = house_train, mtry = floor(n_features / 3), respect.unordered.factors = "order", seed = 01055852025)



rmse_default <- sqrt(house_default$prediction.error)

hyper_grid <- expand.grid(mtry = floor(n_features * c(.05, .15, .25, .333, .4)), min.node.size = c(1, 3, 5, 10), replace = c(TRUE, FALSE), sample.fraction = c(.5, .63, .8),rmse = NA)

## loop over every combination of hyperparameters
for(i in seq_len(nrow(hyper_grid))) {
  print(paste(i,"/",max(seq_len(nrow(hyper_grid)))))
  
  fit <- ranger(formula = Sale_Price ~ ., data = house_train,
                num.trees = n_features * 10, mtry = hyper_grid$mtry[i], min.node.size = hyper_grid$min.node.size[i], replace = hyper_grid$replace[i], sample.fraction = hyper_grid$sample.fraction[i], verbose = FALSE, seed = 01055852024, respect.unordered.factors = 'order',)
  
  ## calculate the OOB error of the model and save it
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

hyper_grid %>% arrange(rmse) %>% mutate(perc_gain = (rmse_default - rmse) / rmse_default * 100) %>% head(10)

model_impurity <- ranger(formula = Sale_Price ~ ., data = house_train, num.trees = 2000, mtry = 26, min.node.size = 1, sample.fraction = 0.8, replace = FALSE, importance = "impurity", respect.unordered.factors = "order", verbose = FALSE, seed = 01055852024)

model_permutation <- ranger(formula = Sale_Price ~ ., data = house_train, num.trees = 2000, mtry = 26, min.node.size = 1, sample.fraction = 0.8, replace = FALSE, importance = "permutation", respect.unordered.factors = "order", verbose = FALSE, seed = 01055852024)

length(which(hyper_grid$rmse < rmse_default))

hyper_grid %>% arrange(rmse) %>% head(1)


imp<- head(sort(model_impurity$variable.importance, decreasing = T), 10)

perm<- head(sort(model_permutation$variable.importance,  decreasing = T), 10)


barplot(imp, ylab = "Importance")
barplot(perm, ylab = "Importance")

?barplot






