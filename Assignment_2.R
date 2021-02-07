#load packages for exploratory data analysis
#PART 1
library(ggplot2)
library(dplyr)
library(corrplot)

#load the data
dataKidney<-read.csv('dataKidney.csv', header=TRUE, sep = ';')
str(dataKidney)

#lets remove unnecessary variables 
dataKidney =subset(dataKidney, select=c(Age, Blood_Pressure, Glucose, Coronary_Atery, Class))
dk = subset(dataKidney, select=c(Age, Blood_Pressure, Glucose, Coronary_Atery, Class))
str(dk)
any(is.na(dk))

#convert categorical values to factor
dk$Class=factor(dk$Class)
dk$Coronary_Atery=factor(dk$Coronary_Atery)

#VISUALIZATION OF DATA
par(mfrow=c(1,3))
#age
plot(dk$Age,pch=16,col='blue', xlab='Index', ylab='Age',main='Visualization of patient by Age')

#blood pressure
plot(dk$Blood_Pressure,pch=16,col='blue', xlab='Index', ylab='Blood pressure',main='Visualization of patient by Blood pressure')

#glucose
plot(dk$Glucose,pch=16,col='blue', xlab='Index', ylab='Glucose',main='Visualization of patient by Glucose')
par(mfrow=c(1,1))

#coronary artery
ggplot(dk, aes(Coronary_Atery))+
  geom_histogram(fill='red', stat='count')+
  labs(x='Coronary artery disease')

#class
ggplot(dk, aes(Class))+
  geom_histogram(fill='blue',stat='count')+
  labs(x='Chrnonic kidney disease')

#Basic data information, average values for each variable and minimum and maximum values
dataKidney%>%
  summarise_all(list(avg = mean, minimum=min, maximum=max))

#EXPLORATORY DATA ANALYSIS
#To understand findings look at report!
#First lets look at how age affects the other values as well as class label
#Blood pressure
ggplot(dk, aes(x=Blood_Pressure, y=Age, col=Class))+
  geom_point()+
  labs(x='Blood pressure', y='Age', col='Class')

#Glucose
ggplot(dk, aes(x=Glucose, y=Age, col=Class))+
  geom_point()+
  labs(x='Glucose', y='Age', col='Class')

#Glucose and blood pressure without age
ggplot(dk, aes(x=Glucose, y=Blood_Pressure, col=Class))+
  geom_point()+
  labs(x='Glucose', y='Blood pressure', col='Class')

#Lets look at how coronary artery disease affects class
ggplot(dk, aes(x=Coronary_Atery, fill=Class))+
  geom_bar(position = 'dodge')+
  labs(x='Coronary artery disease', col='Class')

#Lets look at the appearance of coronary artery disease compared to other variables
#blood pressure and age
ggplot(dk, aes(x=Blood_Pressure, y=Age, col=Coronary_Atery))+
  geom_point()+
  labs(x='Blood pressure', y='Age', col='Coronary artery disease')

#glucose and age
ggplot(dk, aes(x=Glucose, y=Age, col=Coronary_Atery))+
  geom_point()+
  labs(x='Glucose', y='Age', col='Coronary artery disease')

#lets group by the Class, in order to verify our initial analysis
dk%>%
  group_by(Class)%>%
  summarize_all(list(meanvars=mean))

#lets also look at the appearance of chronic kidney disease with the threshold values bp>80, age=>40, glucose levels=>150
dk%>%
  group_by(Class)%>%
  filter(Age>=40)%>%
  summarize(meanvars=mean(Age),minage= min(Age))

dk%>%
  group_by(Class)%>%
  filter(Age>=40)%>%
  count(Class)

dk%>%
  group_by(Class)%>%
  filter(Blood_Pressure>80)%>%
  summarize(meanvars=mean(Blood_Pressure), minbp=min(Blood_Pressure))

dk%>%
  group_by(Class)%>%
  filter(Blood_Pressure>80)%>%
  count(Class)

dk%>%
  group_by(Class)%>%
  filter(Glucose>=150)%>%
  summarize(meanvars=mean(Glucose), mingluc=min(Glucose))

dk%>%
  group_by(Class)%>%
  filter(Glucose>=150)%>%
  count(Class)

#with this we can see that the average variables for people with chronic kidney disease are: Age=59.4, BP=95, Glucose=256.
#with the minimum values or the threshold values being: bp: 90, glucose:157, and age: 40,
#from these all the patients with glucose over 150 had kidney disease, and all of the patients with blood pressure over 80 had kidney disease,
#age did not have a similar distribution as 39/160 patients aged 40 or over had kidney disease, so while kidney disease is more common in patients over
#40, 40y/o is still not a threshold value.
#These results align with our initial analysis

#next lets look at correlation
corrplot(cor(dk[,c(1,2,3,4,5)]),'number')
corrplot(cor(dk[,c(1,2,3,4,5)]),'ellipse')

#lets import necessary libraries for decision tree model and knn classifier
library(rpart)
library(dplyr)
library(ggplot2)
library(rpart.plot)
library(caTools)
library(MASS)
library(class)

#DECISION TREE

#lets check whether the data is balanced
table(dk$Class)
prop.table(table(dk$Class)) 

#it would appear that the data is not too balanced as around 74% of the data belongs to class false
#we are not interested in performance close to majority class, as it would indicate that the model is
#classifying most of the data in the majority class

#lets split the data
sample= sample.split(dk$Class,SplitRatio = 0.7)
train= subset(dk,sample==TRUE)
test = subset(dk, sample==FALSE)

#decision tree model
treemdl1=rpart(Class~., data=train, method='class', minbucket=1)

#lets visualize the model
rpart.plot(treemdl1)

#lets make the first predictions
treeTrain= predict(treemdl1, train, type='class')
treeTest= predict(treemdl1, test, type='class')

#now lets look at the confusion matrix for the training dataset
tab=table(train$Class, treeTrain)
tab
#it would appear that the tree correctly classified 107 data points and classified two data points falsely negative
#now lets look at the confusion matrix for the testing dataset
tab2 = table(test$Class, treeTest)
tab2
#in this run the model performed slightly worse as it correctly classified 42/47 data points, and 1 point as false positive and 4 points as false negative

#lets create the function to calculate accuracy, recall and precision
eval_class= function(tp,tn,fp,fn){
  accuracy=(tp+tn)/(tp+tn+fn+fp) #correctly classified points
  recall=(tp)/(tp+fn) #true positive out of all positive in dataset
  precision= tp/(tp+fp) #true positive out off all predicted positive
  res=c(accuracy, recall, precision)
  names(res)= c('Accuracy', 'Recall', 'Precision')
  return(res)
}

#now lets look at the models accuracy, precision and recall on the train and test data 
eval_class(tab[2,2],tab[1,1], tab[1,2], tab[2,1])
#this gives accuracy of 1, recall 1, and precision 1 so excellent on the training data
eval_class(tab2[2,2],tab2[1,1], tab2[1,2], tab2[2,1])
#this gives accuracy of 0.85, recall of 0.83 and precision of 0.66, so overall good, although for medical data the recall should be higher,
#as it means that the model correctly identifies positive cases only 83% of the time
#the model fit could and should be improved, as it does not generalize too well

#DECISION TREE WITH MODEL SELECTION 
sample= sample.split(dk, SplitRatio = 0.8)
trainall= subset(dk, sample==TRUE) #training+ validation
test= subset(dk, sample==FALSE) #testing data

#a dataframe where we will collect the results 
results = data.frame(Parameter=1:50,
                     Training= vector(mode='numeric', length=50),
                     Validation= vector(mode='numeric',length=50))

#number of runs
runs=100 
resultMatTrain=matrix(0, nrow=runs, ncol=50)
resultMatVal=matrix(0, nrow=runs, ncol=50)

for(j in 1:runs){
  #make second data split
  sampleval= sample.split(trainall$Class, SplitRatio = 0.75)
  train= subset(trainall, sampleval==TRUE) #training data
  val = subset(trainall, sampleval==FALSE) #validation data
  
  for(i in 1:50){
    treemdl1 = rpart(Class~.,data = train, method='class', minbucket=i)
    resultMatTrain[j,i] = mean(predict(treemdl1, train, type='class')==train$Class)
    resultMatVal[j,i] = mean(predict(treemdl1, val, type='class')==val$Class)
  }
}

results$Training =colMeans(resultMatTrain)
results$Validation=colMeans(resultMatVal)

#visualize results dataframe
ggplot(results, aes(x=Parameter, y=Training))+
  geom_line(col='blue')+
  geom_line(aes(x=Parameter, y=Validation), col='red')+
  labs(x='Minimum leave size', y='Accuracy')+
  ggtitle('Training data (blue) vs Validation data (red) accuracy')

#from the graph we can see how from 1-3 the model is overfit and after 3 or 4 the model starts to underfit, so the best parameter is somewhere around 3

#parameter selection 
bestparameter = which.max(results$Validation)

#Lets train the final model
treemdl1 = rpart(Class~., data = trainall, method='class', minbucket=bestparameter)
rpart.plot(treemdl1)
#lets quickly look at recall and precision
treeTrain= predict(treemdl1, trainall, type='class')
treeTest= predict(treemdl1, test, type='class')

#now lets look at the confusion matrix for the training dataset
tab3=table(trainall$Class, treeTrain)
tab3
eval_class(tab3[2,2],tab3[1,1], tab3[1,2], tab3[2,1])
#Results: accuracy 0.95, recall 0.82, and precision of 1.

#now lets look at the confusion matrix for the testing dataset
tab4 = table(test$Class, treeTest)
tab4
eval_class(tab4[2,2],tab4[1,1], tab4[1,2], tab4[2,1])
#Results: accuracy 0.9, recall 0.63, and precision of 1. 

#recall also improved slightly to 0.78 and precision improved to 1.0

#KNN CLASSIFIER
#lets first normalize the data data is dk
library(scales)
dk$Age = rescale(dk$Age, to=c(0,1))
dk$Blood_Pressure=rescale(dk$Blood_Pressure, to=c(0,1))
dk$Glucose = rescale(dk$Glucose, to=c(0,1))

#lets do model selection
sample= sample.split(dk, SplitRatio = 0.8)
trainall= subset(dk, sample==TRUE) #training+ validation
test= subset(dk, sample==FALSE) #testing data

#a dataframe where we will collect the results 
results = data.frame(Parameter=1:50,
                     Training= vector(mode='numeric', length=50),
                     Validation= vector(mode='numeric',length=50))
#number of runs
runs=100 
resultMatTrain=matrix(0, nrow=runs, ncol=50)
resultMatVal=matrix(0, nrow=runs, ncol=50)

for(j in 1:runs){
  #make second data split
  sampleval= sample.split(trainall$Class, SplitRatio = 0.75)
  train= subset(trainall, sampleval==TRUE) #training data
  val = subset(trainall, sampleval==FALSE) #validation data
  
  for(i in 1:50){
    resultMatTrain[j,i] = mean(knn(train[,-5], train[,-5], train$Class, i)==train$Class)
    resultMatVal[j,i] = mean(knn(train[,-5], val[,-5], train$Class,i)==val$Class)
  }
}

results$Training =colMeans(resultMatTrain)
results$Validation=colMeans(resultMatVal)

#visualize results dataframe
ggplot(results, aes(x=Parameter, y=Training))+
  geom_line(col='blue')+
  geom_line(aes(x=Parameter, y=Validation), col='red')+
  labs(x='Minimum neighbours', y='Accuracy')+
  ggtitle('Training data (blue) vs Validation data (red) accuracy')

#the best parameter appears to be around n=1, I don't believe that the model would generalize well with parameter 1.
#the model was tested with parameter 1, and the conclusion was an overfit model. Thus for best parameter
#the second best validation min neighbour was chosen 3. 
#bestparameter=which.max(results$Validation)
bestparameter = 3

#lets retrain our model with the n=3
modelTrain= knn(trainall[,-5], trainall[,-5],trainall$Class,bestparameter)
modelTest = knn(trainall[,-5], test[,-5], trainall$Class, bestparameter)

#confusion matrix
tab5 = table(trainall$Class, modelTrain)
eval_class(tab5[2,2], tab5[1,1], tab5[1,2], tab5[2,1])

#on training data model produced accuracy of 0.94, recall of 0.79, and precision of 1

tab6= table(test$Class, modelTest)
eval_class(tab6[2,2], tab6[1,1], tab6[1,2], tab6[2,1])
#on testing data model produced accuracy of 0.94, recall of 0.75 and precision of 1, overall a good model at generalizing.

#PART 2

library(dplyr)
library(NbClust)
library(purrr)
library(ggplot2)
library(corrplot)

data = read.csv('wholesale.csv', header=TRUE, sep=',')
str(data)

#lets check whether there is any missing data
any(is.na(data))

#lets have a look at the data 
#channel and region appear to be variables which represent some categorical data so we will not do anything to them yet
par(mfrow=c(2,3))
#fresh
plot(data$Fresh, pch=16, col='blue', xlab='Index', ylab='Annual spending on fresh products', main='Fresh product')
#milk
plot(data$Milk, pch=16, col='red', xlab='Index', ylab='Annual spending on milk products', main='Milk product')
#grocery
plot(data$Grocery, pch=16, col='green', xlab='Index', ylab='Annual spending on grocery products', main='Grocery product')
#frozen
plot(data$Frozen, pch=16, col= 'purple', xlab='Index', ylab='Annual spending on frozen products', main='Frozen product')
#detergents_paper
plot(data$Detergents_Paper, pch=16, col='orange', xlab='Index', ylab='Annual spending on detergents and paper products', main='Detergents and paper product')
#delicatessen
plot(data$Delicassen, pch=16, xlab='Index', ylab='Annual spending on delicatessen products', main='Delicatessen product')

#we can notice that in all products most of the costumers stay in a similar range with other consumers of the same product, with a smaller group spending anually more

#lets now quickly visualize the distribution of consumers in the channel and region columns
#region
ggplot(data, aes(Region))+
  geom_histogram(fill='blue')
#it would appear that most of the costumers are from region 3

#channel
ggplot(data, aes(factor(Channel)))+
  geom_bar(fill='blue')
#it would appear that most of the costumers used sales channel 1

#EXPLORATORY DATA ANALYSIS
#lets see whether there is any correlation between any of the values
par(mfrow=c(1,1))
corrplot(cor(data[,c(1,2,3,4,5,6,7,8)]),'number')
#We can notice that there is strong positive correlation between grocery and Detergents_paper. 
#Additionally there is positive correlation between: grocery and milk, detergents_paper and channel, 
#grocery and channel, milk and detergents_paper, milk and channel.
#There is also some slight positive correlation between: delicassen and milk, delicassen and frozen, frozen and fresh.
corrplot(cor(data[,c(1,2,3,4,5,6,7,8)]),'ellipse')

#lets visualize the ones that have strong correlation and good correlation, when colored by channel.
ggplot(data, aes(x=Detergents_Paper, y=Grocery, col=factor(Channel)))+
  geom_point()+
  labs(x='Detergents and paper annually', y='Grocery annually', col='Channel')
#it would appear that those who used the first sales channel used annually less money than those that used channel two. 
#additionally it appears that hose who spent annually more on grocery also spent more on detergents and paper.

ggplot(data, aes(x=Detergents_Paper, y=Milk, col=factor(Channel)))+
  geom_point()+
  labs(x='Detergents and paper annually', y='Milk annually', col='Channel')

#there seems to be similar results for the detergents and paper and milk, with a little more variety

ggplot(data, aes(x=Milk, y=Grocery, col=factor(Channel)))+
  geom_point()+
  labs(x='Milk annually', y='Grocery annually', col='Channel')

#Similar results. It would appear that the channel that costumers buy from has an effect on the amount of money used annually
#lets group by channel and look at average spending, minimum spending, and maximum spending for each variable by channel
data%>%
  group_by(Channel)%>%
  summarize_all(list(meanvars=mean))

#The initial analysis seems to be correct, for almost all the variables those costumers that used the 2 sales channel, 
#spent on average more money. For all variables except fresh and frozen products.

data%>%
  group_by(Channel)%>%
  summarize_all(list(minimum=min))

#The minimum spending was higher for costumers that used channel 2, all except delicatessen products.

data%>%
  group_by(Channel)%>%
  summarize_all(list(maximum=max))

#Maximum results are similar to the average ones, with the addition of delicatessen product maximum being also lower along with fresh and frozen. 
#For those products that had higher correlation among each other the channel classification seems to be consistent, with channel 2 having higher spending in those product departments. 

#LETS NORMALIZE THE DATA
library(scales)

str(data2)

data2=data
data2$Channel = rescale(data$Channel, to=c(0,1))
data2$Region=rescale(data$Region, to=c(0,1))
data2$Fresh = rescale(data$Fresh, to=c(0,1))
data2$Milk = rescale(data$Milk, to=c(0,1))
data2$Grocery = rescale(data$Grocery, to=c(0,1))
data2$Frozen = rescale(data$Frozen, to=c(0,1))
data2$Detergents_Paper = rescale(data$Detergents_Paper, to=c(0,1))
data2$Delicassen = rescale(data$Delicassen, to=c(0,1))
str(data2)

#normalization seems to have succeeded
#LETS USE KMEANS ALGORITHM WITH THE 4METHODS TO DETERMINE OPTIMAL NUMBER OF CLUSTERS
#Elbow method
tot_within_ss=map_dbl(1:10, function(k){
  model=kmeans(data2,centers=k,nstart=25)
  model$tot.withinss
})
plot(1:10, tot_within_ss, type='o', xlab = 'Number of clusters', ylab='Total within sum of squares', main='Elbow method for simple example')

#It would appear that the optimal number of clusters is 4

#Silhouette method
silClust=NbClust(data2, dist='euclidean', min.nc=2, max.nc=10, method = 'kmeans', index='silhouette')

#Gap statistic method
gapClust=NbClust(data2, dist='euclidean', min.nc=2, max.nc=10, method = 'kmeans', index='gap')

#Calinski-Harabasz method
chClust=NbClust(data2, dist='euclidean', min.nc=2, max.nc=10, method = 'kmeans', index='ch')

#Visualization
par(mfrow=c(1,3))
plot(2:10, silClust$All.index, type='o', col='blue', xlab='Num of Clust', ylab='Silhouette Value')
plot(2:10, gapClust$All.index, type='o', col='blue', xlab='Num of Clust', ylab='Gap statistic')
plot(2:10, chClust$All.index, type='o', col='blue', xlab='Num of Clust', ylab='CH index')

#from the visualization it seems that it will not be too easy to decide on a number of clusters
#in the silhouette method the ideal number of clusters would be 3
#in the gap statistics method the ideal number of clusters would be 2
# in the CH method the ideal number of clusters would be 9, which is a huge difference when compared to the other methods, 


#lets make the model with the ideal amount of clusters
kmeansmdl= kmeans(data2, centers=3, nstart=25)

#lets do a quick analysis on the final model
newdata<-data%>%
  mutate(member=kmeansmdl$cluster)

newdata%>%
  group_by(member)%>%
  summarize_all(list(meanvars=mean))

newdata%>%
  group_by(member)%>%
  summarize_all(list(sdvars=sd))

par(mfrow= c(1,1))

plot(data2, col=kmeansmdl$cluster, pch=16)

#results are inconclusive