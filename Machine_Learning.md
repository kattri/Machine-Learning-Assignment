# Machine Learning Project: Activity Quality from Body Monitors
Keshav  
Monday, June 22, 2015  

##Outline

This assignment is based on the data being collected by Activity Monitors mounted on human Body with  data being made available by http://groupware.les.inf.puc-rio.br/har.In addition to passive accelerometer data an attempt is now being made to monitor the quality and correctness of the exercises being undertaken by  the subjects. In this particular case a data base regarding correctness of the dumbbell lifting has been created and made available where one correct way of lifting  dumbbells and 4 different methods of incorrectly lifting dumbbells have been identified(namely B,C,D and E). Project involves correlating the correctness of the exercise to the different accelerometer signals.

##Executive Summary
Based on training data set made available at https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv an attempt has been made to determine the  predictor variables  required for determination of the activity quality as defined in the Weight Lifting Data set. Data set has been divided in the ratio of 60:40 for cross validation. It is found that the quality is very accurately predicted by the accelerometer signals.

###Loading and Exploring the Data
Data was downloaded into the project repository and read using the read.csv


```r
setwd("C:/Users/hp/Desktop/Keshav/Coursera/Data Scienc Track/Practical Machine Learning/Project")
training<-read.csv("./pml-training.csv",header =TRUE,sep =",")
testing<-read.csv("./pml-testing.csv",header =TRUE,sep =",")
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
dim(training)
```

```
## [1] 19622   160
```

### Subsetting the Training and Test Data
Data was investigated using Summary command and it was found that the data relating to acceleration, roll,ptich and yaw of the sensors on waist, dumbbell, fore arm and arm only is important for prediction of the classe i.e activity quality. These data have a well defined prefix with the sensor name appended in the end. Both Training and Test Data Sets were subset including these and last parameter namely activity Quality or classe


```r
var1 <- c(grep("^accel", names(training)), grep("^gyros", names(training)), 
                  grep("^magnet", names(training)), grep("^roll", names(training)), 
                  grep("^pitch",  names(training)), grep("^yaw", names(training)), 
                  grep("^total", names(training)))                                                                             
training1<-training[,c(var1,160)]
testingProject<-testing[,c(var1,160)]
```

###Partitioning and Model Fitting on the Training Data Set
The Training data set  was partitioned in 60:40 ratio as training and testing data sets. Model Fitting has been done using randomForest Library and not train method of caret library as the later has been found to be extremely slow and has been reported so on the forums. 

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.2.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
inTrain<-createDataPartition(training1$classe,p=.6,list=FALSE)
training2<-training1[inTrain,]
testing2<-training1[-inTrain,]
set.seed(33587)
modFit <- randomForest(classe ~., training2)
print(modFit)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training2) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.7%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3345    3    0    0    0 0.0008960573
## B   10 2261    8    0    0 0.0078982010
## C    0   23 2026    5    0 0.0136319377
## D    0    0   20 1908    2 0.0113989637
## E    0    0    3    8 2154 0.0050808314
```

### Validation on the test Set
The model fitted above has a out of Bag error in the range of  .7% on the training data set and is a reasonable model. Now let us see the model performance on the testdata set partition of the training data set.


```r
result<-predict(modFit,testing2)
confusionMatrix(result, testing2$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2230   14    0    0    0
##          B    1 1502   13    0    0
##          C    1    2 1354   15    2
##          D    0    0    1 1271    6
##          E    0    0    0    0 1434
## 
## Overall Statistics
##                                           
##                Accuracy : 0.993           
##                  95% CI : (0.9909, 0.9947)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9911          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9991   0.9895   0.9898   0.9883   0.9945
## Specificity            0.9975   0.9978   0.9969   0.9989   1.0000
## Pos Pred Value         0.9938   0.9908   0.9854   0.9945   1.0000
## Neg Pred Value         0.9996   0.9975   0.9978   0.9977   0.9988
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2842   0.1914   0.1726   0.1620   0.1828
## Detection Prevalence   0.2860   0.1932   0.1751   0.1629   0.1828
## Balanced Accuracy      0.9983   0.9936   0.9933   0.9936   0.9972
```
Method has a accuracy in excess of 98%  which is simlar to the training data set thus indicating its applicability to this data set. The method now shall be applied to the testing data set for the assignment and results uploaded.


```r
result<-predict(modFit,testingProject)
result
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
These results have been uploaded onto the assignment section and have been found to be matching,

###Limited Predictors

The importance of various variables was noted down and variables with importance higher than 250 have bene investigated as predictors. There are only 9 of them. On fitting a model it seems that dropping 44 predcitors does not casue a major loss of predictabilty. This is also true in case of test data sets.


```r
wts<-varImp(modFit)
wts$names<-rownames(wts)
wt2<-wts[wts$Overall>250,]
wt2
```

```
##                    Overall             names
## magnet_dumbbell_x 290.8840 magnet_dumbbell_x
## magnet_dumbbell_y 402.9383 magnet_dumbbell_y
## magnet_dumbbell_z 437.4053 magnet_dumbbell_z
## roll_belt         774.9757         roll_belt
## roll_dumbbell     250.7230     roll_dumbbell
## roll_forearm      350.2116      roll_forearm
## pitch_belt        404.8228        pitch_belt
## pitch_forearm     464.2875     pitch_forearm
## yaw_belt          530.9003          yaw_belt
```

```r
set.seed(33587)
modFit2 <- randomForest(classe ~ accel_dumbbell_y +magnet_dumbbell_x+ magnet_dumbbell_y + magnet_dumbbell_z + roll_belt +roll_forearm +pitch_belt + pitch_forearm + yaw_belt             , training2)
print(modFit2)
```

```
## 
## Call:
##  randomForest(formula = classe ~ accel_dumbbell_y + magnet_dumbbell_x +      magnet_dumbbell_y + magnet_dumbbell_z + roll_belt + roll_forearm +      pitch_belt + pitch_forearm + yaw_belt, data = training2) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 1.67%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3319   17    9    3    0 0.008661888
## B   24 2202   40   13    0 0.033786749
## C    3   26 2009   16    0 0.021908471
## D    0    3   18 1907    2 0.011917098
## E    0   11    7    5 2142 0.010623557
```
Out of Bag  oob Rate has marginally increased from about .7 to 1.5 in this case which is acceptable.

### Validation for Limited Parameter Model with Test Data Subset
The accuracy still stays in the range of 98% which is acceptable



```r
result<-predict(modFit2,testing2)
confusionMatrix(result, testing2$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2219   34    1    0    0
##          B    5 1452   18    1   12
##          C    6   21 1341   13    5
##          D    2    9    8 1272    4
##          E    0    2    0    0 1421
## 
## Overall Statistics
##                                           
##                Accuracy : 0.982           
##                  95% CI : (0.9788, 0.9849)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9773          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9942   0.9565   0.9803   0.9891   0.9854
## Specificity            0.9938   0.9943   0.9931   0.9965   0.9997
## Pos Pred Value         0.9845   0.9758   0.9675   0.9822   0.9986
## Neg Pred Value         0.9977   0.9896   0.9958   0.9979   0.9967
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2828   0.1851   0.1709   0.1621   0.1811
## Detection Prevalence   0.2873   0.1897   0.1767   0.1651   0.1814
## Balanced Accuracy      0.9940   0.9754   0.9867   0.9928   0.9926
```

###Checking the test submission
Test Submissions are compared for both 52 as well as 9 predcitor models and are found to be equal.


```r
result2<-predict(modFit2,testingProject)
result1<-predict(modFit,testingProject)
table(result2,result1)
```

```
##        result1
## result2 A B C D E
##       A 7 0 0 0 0
##       B 0 8 0 0 0
##       C 0 0 1 0 0
##       D 0 0 0 1 0
##       E 0 0 0 0 3
```
Thus limited parameter model is selected with predictors comprising of the accelrations and magneti movements of dumbeel roll and pitch of the belt and forearm as well as yaw of the belt.
