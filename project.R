## System Preparation
rm(list = ls())   # Clear the current environment
library(caret)    # Load the caret package for training data
setwd('E:/Rdir/predmachlearn')
set.seed(35443)   # Setting the random seed to 35443, in order to make the research reproducible
## Getting, Sorting and Cleaning Data
pml_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
pml_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(pml_training, destfile = "pml-training.csv")
download.file(pml_testing, destfile = "pml-testing.csv")
dat <- read.table("./pml-training.csv", header = TRUE, sep = ",",  na.strings = c("","NA"))
### Subsetting the data into training and testing set
inTrain <- createDataPartition(dat$classe, p = 0.70, list = FALSE)
training <- dat[inTrain, ]
testing <- dat[-inTrain, ]
training <- training[, !colSums(is.na(training))]
testing <- testing[, !colSums(is.na(testing))]
identical(names(training), names(testing))


## Building Mazhine Learning Models
if (sum(dir() == "modFit_rf.RData")) {
        load(file = "./modFit_rf.RData") 
        } else {
        modFit_rf <- train(classe ~ ., data = training[, -c(1,2)], method = "rf")
#         modFit_gbm <- train(classe ~ ., data = training, method = "gbm")
        save(list = c("modFit_rf"), file = "./modFit_rf.RData")
}

prePca <- preProcess(training[, is.numeric(training[, -c(1,2,160)])],  method = "pca", pcaComp = 2)

# pred_gbm <- predict(modFit_gbm, testing)
# predComb <- data.frame(pred_rf, pred_gbm, classe = testing$classe)
# modFit_Comb <- train(classe ~., data = predComb, method = "rf")

## Diagnostics w/ cross validation
pred_rf <- predict(modFit_rf, testing)
# pred_gbm <- predict(modFit_gbm, testing)
confusionMatrix(pred_rf, testing$classe)
## Predict with 20 test samples

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
testing_cases_20 <- read.csv("./pml-testing.csv", header = TRUE, sep = ",",  na.strings = c("","NA"),strip.white = TRUE)
answers <- predict(modFit_rf , testing_cases_20)
print(answers)
pml_write_files(answers)

The model training is accomplished, let's take a look at the model.
```{r summ,cache=TRUE}
print(modFit_rf)
```
```{r fm, cache=TRUE}
print(modFit_rf$finalModel)
```