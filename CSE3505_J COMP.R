#credit card fraud detection
#importing library
library(ranger)
library(caret)
library(data.table)
#datset
data <- read.csv("creditcard.csv")
data
#DATA EXPLORATION
data.table(data)
#doing random statistical things
summary(data)
names(data)

#summmary of amount
summary(data$Amount)
sd(data$Amount)
#IQR:The interquartile range (IQR) measures the spread of the middle half of your data. It is the range for the middle 50% of your sample. Use the IQR to assess the variability where most of your values lie. Larger values indicate that the central portion of your data spread out further
IQR(data$Amount)
var(data$Amount)

#manipulation
data$Amount<-scale(data$Amount) #normalization
data2<-data[,-c(1)] #removing time from data
head(data2)

set.seed(12)
library(caTools)
sample_data<-sample.split(data2$Class,SplitRatio=0.80)

train_data<-subset(data2,sample_data==TRUE)
test_data<-subset(data2,sample_data= FALSE)

dim(train_data)
dim(test_data)
#logistic regression
Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

Logistic_Model1<- glm(Class~.,train_data,family=binomial())
summary(Logistic_Model1)
plot(Logistic_Model1)

#pROC curve
library(pROC)
lr.predict<-predict(Logistic_Model1,test_data,probability=TRUE)
auc.gb<-roc(test_data$Class,lr.predict,plot=TRUE,col="blue")

#fitting a decision tree
library(rpart)
library(rpart.plot)

decision_model<-rpart(Class ~ .,data,method="class")
predicted_val<-predict(decision_model,data,type="class")
probability <- predict(decision_model, data, type = 'prob')
rpart.plot(decision_model)



