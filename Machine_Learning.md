# Machine Learning Project: Activity Quality from Body Monitors
Keshav  
Monday, June 22, 2015  

##Outline

This assignment is based on the data being collected by Activity Monitors mounted on human Body with  data being made available by http://groupware.les.inf.puc-rio.br/har.In addition to passive accelerometer data an attempt is now being made to monitor the quality and correctness of the exercises being undertaken by  the subjects. In this particular case a data base regarding correctness of the dumbbell lifting has been created and made available where one correct way of lifting  dumbbells and 4 different methods of incorrectly lifting dumbbells have been identified(namely B,C,D and E). Project involves correlating the correctness of the exercise to the different accelerometer signals.

##Executive Summary
Based on training data set made available at https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv an attempt has been made to determine the  predictor variables  required for determination of the activity quality as defined in the Weight Lifting Data set. Data set has been divided in the ratio of 60:40 for cross validation. It is found that the quality is very accurately predicted by the accelerometer signals.

**Loading and Exploring the Data
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

**Subsetting the Training and Test Data

Data was investigated using Summary command and it was found that the data relating to acceleration, roll,ptich and yaw of the sensors on waist, dumbbell, fore arm and arm only is important for prediction of the classe i.e activity quality. These data have a well defined prefix with the sensor name appended in the end. Both Training and Test Data Sets were subset including these and last parameter namely activity Quality or classe


```r
var1 <- c(grep("^accel", names(training)), grep("^gyros", names(training)), 
                  grep("^magnet", names(training)), grep("^roll", names(training)), 
                  grep("^pitch",  names(training)), grep("^yaw", names(training)), 
                  grep("^total", names(training)))                                                                             
training1<-training[,c(var1,160)]
testingProject<-testing[,c(var1,160)]
```

**Partitioning and Model Fitting on the Training Data Set
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
##      A    B    C    D    E class.error
## A 3340    6    0    1    1 0.002389486
## B   16 2258    5    0    0 0.009214568
## C    0   11 2039    4    0 0.007302824
## D    0    0   28 1899    3 0.016062176
## E    0    0    1    6 2158 0.003233256
```

***Validation on the test Set
The model fitted above has a out of Bag error on only .7% on the training data set and is a reasonable model. Now let us see the model performance on the testdata set partition of the training data set.


```r
result<-predict(modFit,testing2)
confusionMatrix(result, testing2$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    7    0    0    0
##          B    1 1505    9    0    0
##          C    0    6 1358   11    0
##          D    0    0    1 1275    4
##          E    0    0    0    0 1438
## 
## Overall Statistics
##                                           
##                Accuracy : 0.995           
##                  95% CI : (0.9932, 0.9965)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9937          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9914   0.9927   0.9914   0.9972
## Specificity            0.9988   0.9984   0.9974   0.9992   1.0000
## Pos Pred Value         0.9969   0.9934   0.9876   0.9961   1.0000
## Neg Pred Value         0.9998   0.9979   0.9985   0.9983   0.9994
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1918   0.1731   0.1625   0.1833
## Detection Prevalence   0.2852   0.1931   0.1752   0.1631   0.1833
## Balanced Accuracy      0.9992   0.9949   0.9950   0.9953   0.9986
```
Method has a accuracy of 99.38% which is simlar to the training data set thus indicating its applicability to this data set. The method now shall be applied to the testing data set for the assignment and results uploaded.

```r
result<-predict(modFit,testingProject)
result
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
These results have been uploaded onto the assignment section na have been found to be matching,


