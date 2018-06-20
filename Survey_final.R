#Data set up upload and review of data
library(caret)
View(Survey_Key_and_Complete_KEY_REMOVED)
summary(Survey_Key_and_Complete_KEY_REMOVED)
names(Survey_Key_and_Complete_KEY_REMOVED)
is.na(Survey_Key_and_Complete_KEY_REMOVED)
setwd("~/Desktop/Austin DA/Course 2/Task 2")


#Data set description 
attributes(Survey_Key_and_Complete_KEY_REMOVED)
qqnorm(Survey_Key_and_Complete_KEY_REMOVED$salary)
qqnorm(Survey_Key_and_Complete_KEY_REMOVED$age)
qqnorm(Survey_Key_and_Complete_KEY_REMOVED$credit)

#Change of elevel, car, zipcode, brand to from numeric to factor
Survey_Key_and_Complete_KEY_REMOVED$elevel <-as.ordered(Survey_Key_and_Complete_KEY_REMOVED$elevel)
summary(Survey_Key_and_Complete_KEY_REMOVED$elevel)
levels(Survey_Key_and_Complete_KEY_REMOVED$elevel)
Survey_Key_and_Complete_KEY_REMOVED$car <- as.factor(Survey_Key_and_Complete_KEY_REMOVED$car)
summary(Survey_Key_and_Complete_KEY_REMOVED$car)
levels(Survey_Key_and_Complete_KEY_REMOVED$car)
Survey_Key_and_Complete_KEY_REMOVED$zipcode <- as.factor(Survey_Key_and_Complete_KEY_REMOVED$zipcode)
summary(Survey_Key_and_Complete_KEY_REMOVED$zipcode)
levels(Survey_Key_and_Complete_KEY_REMOVED$zipcode)
Survey_Key_and_Complete_KEY_REMOVED$brand <- as.factor(Survey_Key_and_Complete_KEY_REMOVED$brand)
summary(Survey_Key_and_Complete_KEY_REMOVED$brand)
levels(Survey_Key_and_Complete_KEY_REMOVED$brand)



#define an 75%/25% train/test split of the dataset 
set.seed(123)
inTrain <- createDataPartition(y = Survey_Key_and_Complete_KEY_REMOVED$brand, p = .75, list = FALSE)
inTrain
summary(inTrain)
str(inTrain)
levels(inTrain)

#Training the models
training <- Survey_Key_and_Complete_KEY_REMOVED[inTrain,]
testing <- Survey_Key_and_Complete_KEY_REMOVED[-inTrain,]

training
testing

str(training)
str(testing)

summary(training)
summary(testing)



#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
fitControl

#Install KNN from Class Library
library(class)

#Train KNN version "Machine leanring article" just experimenting do not use (reference only)
train_input <- as.matrix(training[,-5])
train_input
train_output <- as.vector(training[,5])
train_output
test_input <-as.matrix(testing[,-5])
test_input
prediction <- knn(train_input, test_input, train_output, k=5)





#KNN Test_main method_K=20
set.seed(123)
knnFit1 <- train(brand ~ ., data = training, method = "knn", trControl=fitControl, metric="Accuracy", tuneLength=20, preProc =c("center", "scale"))
knnFit1

#Install package neighbor
library(neighbr)

#Tune with KNN parameters


#predict variables 
predictors(knnFit1)

#make prediction_knn
set.seed(123)
PredBrand <- predict(knnFit1, testing)
PredBrand
summary(PredBrand)
table(PredBrand)


#performance Measurement
postResample(PredBrand, testing$brand)
View(postResample)

#plot predicted vs. actual 
plot(PredBrand, testing$brand)

#confusion matrix KNN
confusionMatrix(PredBrand, testing$brand)

#Import Incomplete Survey data
library(readr)
SurveyIncomplete <- read_csv("~/Desktop/Austin DA/Course 2/Task 2/SurveyIncomplete.csv")
is.na(SurveyIncomplete)
summary(SurveyIncomplete)
View(SurveyIncomplete)
attributes(SurveyIncomplete)

#Transform attributes in Incomplete Survey dataset
SurveyIncomplete$elevel <-as.ordered(SurveyIncomplete$elevel)
summary(SurveyIncomplete$elevel)
levels(SurveyIncomplete$elevel)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
summary(SurveyIncomplete$car)
levels(SurveyIncomplete$car)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
summary(SurveyIncomplete$zipcode)
levels(SurveyIncomplete$zipcode)
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
summary(SurveyIncomplete$brand)
levels(Survey_Key_and_Complete_KEY_REMOVED$brand)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
summary(SurveyIncomplete$brand)


#Prediction of Brand preference_knn model
set.seed(123)
testPredPreference<-predict(knnFit1,SurveyIncomplete)
testPredPreference
summary(testPredPreference)
postResample(testPredPreference, testing$brand)
confusionMatrix(testPredPreference, testing$brand)

#Tranfer KNN Predicts for brand to excel 
install.packages("WriteXLS")
library(WriteXLS)
WriteXLS("testPredPreference", ExcelFileName = "KNN_testpred.xlsx")

testPredPreference<-as.data.frame(testPredPreference)

write_excel_csv(testPredPreference, "", na = "NA", append = FALSE, col_names = TRUE)



testPredPreference
write_csv(testPredPreference)
?options
options(max.print = 5000000)
print(testPredPreference)

#Random Forrest analysis of survey data _ 50 trees
library(randomForest)
rfNews()
set.seed(123)
BrandForrest<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 18)
BrandForrest
table(BrandForrest)
plot(BrandForrest)
as.data.frame(BrandForrest)
confusionMatrix(BrandForrest)
sort(BrandForrest)
?RMSE






#Random Forrest at 25 trees
set.seed(123)
BrandForrest25<-randomForest(brand ~ ., data = training, method = "rf", metric = "Accuracy", importance = TRUE, ntree = 25)
BrandForrest25

#Test RF fit
set.seed(123)
fit.rf <- train(brand~., data = training, method="rf", metric= "accuracy", trControl=fitControl)
fit.rf

varImpPlot(BrandForrest25)



#Random Forrest at 75 trees
BrandForrest75<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 75)
BrandForrest75



#Random Forrest at 150 trees_best
BrandForrest150<-randomForest(brand ~ ., data = training, method = "class", metric = "accuracy", importance = TRUE, ntree = 150)
BrandForrest150
summary(BrandForrest145)
varImpPlot(BrandForrest150, col="blue", pch = 2)
table(prediction, testing$brand)
plot(BrandForrest175)


#accuracy in random forrest 150
set.seed(123)
modelfit<-randomForest(brand~., data = training)
modelfit
nrow(testing)
prediction<-predict(modelfit, testing)
prediction
prediction<-as.data.frame(prediction)
WriteXLS(prediction, ExcelFileName = "predictionforrest2.xlsx")




# Calculate the Variance
X <- na.omit(mpg[ind$test,-1])
var_hat <- randomForestInfJack(rf_fit, X, calibrate = TRUE)

#RMSE FOR RF 150
rmse150<- sqrt(mean(BrandForrest150 - testing$brand)^2)    

RMSE(BrandForrest150,testing$brand)


#Random Forrest at 175 trees
BrandForrest175<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 175)
BrandForrest175
postResample(BrandForrest175, testing$brand)

#Ransom Forrect at 200 trees
BrandForrest100<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 100)
BrandForrest100
postResample(BrandForrest200, testing$brand)

#Random Forrest at 500 trees
BrandForrest500<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 500)
BrandForrest500
summary(BrandForrest500)



#Random Forrect at 600
BrandForrest600<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 600)
BrandForrest600

BrandForrest700<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 700)
BrandForrest700

BrandForrest800<-randomForest(brand ~ ., data = training, method = "class", metric = "Accuracy", importance = TRUE, ntree = 800)
BrandForrest800

#RF150 Model performance measurements
install.packages(RRF)
install.packages("inTrees")
library(inTrees)
importance(BrandForrest150)
treeList <- RF2List(BrandForrest150)
treeList

fit$BrandForrest150[,'class.error']

#Prediction of Brand preference_knn model


#Prediction of Brand prefernce_random forrest model 

Brandforrestpred500 <- predict(BrandForrest500, SurveyIncomplete)
Brandforrestpred500
postResample(BrandForrest500, testing$brand)
BrandForrest500 <- as.data.frame(BrandForrest500)

#Tranfer RF Predicts for brand to excel 
install.packages("WriteXLS")
library(WriteXLS)
write.csv(Brandforrestpred700, ExcelFileName = "Brandforrest_pred700.xlsx")

Brandforrestpred700<-as.data.frame(Brandforrestpred700)



#rmse for Random Forrest
library(ggplot2)
library(gridExtra)
actual<-testing$brand
result <- data.frame(SurveyIncomplete$brand, Brandforrestpred500)
result
paste('Root Mean Square Error: ',mean(sqrt(BrandForrest500$mse)))


summary(Brandforrestpred500)

predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)

classes <- c("class1", "class2")
set.seed(1)
dat <- data.frame(obs =  factor(sample(classes, 50, replace = TRUE)),
                  pred = factor(sample(classes, 50, replace = TRUE)),
                  class1 = runif(50), class2 = runif(50))

defaultSummary(dat, lev = classes)
twoClassSummary(dat, lev = classes)
prSummary(dat, lev = classes)
mnLogLoss(dat, lev = classes)
