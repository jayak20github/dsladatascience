# Steps to  analyse

#download data set
#follow the Iris methodology
#look at data 
#normalize data
#clean data if needed
#break data into training and test
#run statistical algorithms
#focus on the one that you think will work best
#iterate through the process
#identify different attributes and using different algorithm
#put up presentation on GIT
#prepare presentation

install.packages("ggplot2")
install.packages("readr")
install.packages("caret")
install.packages("randomForest")
#install.packages("caTools")
install.packages("rpart.plot")

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret) # for predictive modeling
library(randomForest) # random forest
library(rpart.plot)

# Load data set from the file
system("ls /Users/jaya_kuppuswamy/Documents/Jaya/DataScience/Homework/Week2")
mushroom_filedata <- read.csv(file = "/Users/jaya_kuppuswamy/Documents/Jaya/DataScience/Homework/Week2/mushrooms.csv",header = TRUE,sep = ",")
head(mushroom_filedata)

#Check for missing data
mushroom_filedata[!complete.cases(mushroom_filedata),]

#Understand data
unique(mushroom_filedata$class)
dim(mushroom_filedata)
summary(mushroom_filedata)
str(mushroom_filedata)
levels(mushroom_filedata$class)
head(mushroom_filedata)

barplot(table(mushroom_filedata$class)) 

#break data into training and test
index <- createDataPartition(mushroom_filedata$class, p=0.80, list=FALSE)
testset<-mushroom_filedata[-index,]
trainset<-mushroom_filedata[index,]


#Comparing edible and poisonous mushrooms based on cap shape and surface
p = ggplot(trainset,aes(x=cap.shape,  y=cap.surface, color=class))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Edible','Poisonous'),values=c('darkgreen','red'))
print(p)

# Original Data
table(mushroom_filedata$class)/nrow(mushroom_filedata)  
  ##    e     p 
## 0.5179714 0.4820286
# Training Data
table(trainset$class)/nrow(trainset)  
##e     p 
##0.518 0.482 
# Testing Data
table(testset$class)/nrow(testset)  
##e         p 
##0.5178571 0.4821429 

#Comparing edible and poisonous mushrooms based on odor
p1 = ggplot(trainset,aes(x=class,  y=odor, color = class))
p1 + geom_jitter(alpha=0.2) +  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))
?geom_jitter

#Decision Tree classifier
install.packages("caret") 
install.packages("e1071") 
library(caret)
library(e1071)
dim(trainset)
set.seed(6000)

#Fit Random Forest Model on trainset
rf =randomForest(class ~ ., ntree=100, data =trainset)
plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf,  sort = T,n.var=10,main="Top 10 - Variable Importance")

# Predicting response variable
trainset$predicted.response = predict(rf , trainset)

# Create Confusion Matrix
print(confusionMatrix(data = trainset$predicted.response,  reference = trainset$class,positive = 'e'))

#Fit Random Forest Model on testset
rf =randomForest(class ~ ., ntree=100, data =testset)
plot(rf)

testset$predicted.response = predict(rf , testset)
print(confusionMatrix(data = testset$predicted.response,  reference = testset$class,positive = 'e'))



#GBM
?train

#Rpart on trainset

rpart_mod <-train(x=trainset,y=trainset$clas,method="rpart",tuneLength=5)
plot(varImp(rpart_mod),main="RPART - Variable Importance Plot")
rpart.plot(rpart_mod$finalModel) #<- creates the decision tree with better formatting

#Rpart on testset

rpart_test_mod <-train(x=testset,y=testset$clas,method="rpart",tuneLength=5)
plot(varImp(rpart_test_mod),main="RPART - Variable Importance Plot")
rpart.plot(rpart_test_mod$finalModel) #<- creates the decision tree with better formatting

