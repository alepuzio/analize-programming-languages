#Qualitative analisys
#copy dataset

dataset.compare.languages.qualitative <- my.safe.copy.dataframe(dataset.compare.languages)
#delete useless columns

#new column about the number of supported paradigma
dataset.compare.languages.qualitative$Num.Paradigma <- 
  (dataset.compare.languages.qualitative$Imperative+dataset.compare.languages.qualitative$OOP+
  dataset.compare.languages.qualitative$Functional + dataset.compare.languages.qualitative$Procedural+
  dataset.compare.languages.qualitative$Generic + dataset.compare.languages.qualitative$Reflective+
  dataset.compare.languages.qualitative$Event.Driven)/10

#verify Java
dataset.compare.languages.qualitative[dataset.compare.languages.qualitative$Num.Paradigma == 7,]$Language
#verify that every language has at least one paradigma
dataset.compare.languages.qualitative[dataset.compare.languages.qualitative$Num.Paradigma == 0,]$Language


#Count the uses
dataset.compare.languages.qualitative$Num.Uses <- str_count(dataset.compare.languages.qualitative$Use, ',')+1
dataset.compare.languages.qualitative[dataset.compare.languages.qualitative$Use == "" ,]$Num.Uses <-0

#count the number of other paradigm
dataset.compare.languages.qualitative$Num.Other <- str_count(dataset.compare.languages.qualitative$Other, ',')+1
dataset.compare.languages.qualitative[dataset.compare.languages.qualitative$Other == "" ,]$Num.Other <-0

dataset.compare.languages.qualitative <- my.safe.copy.dataframe(dataset.compare.languages.qualitative)
dataset.compare.languages.qualitative$Num.Standard <- str_count(dataset.compare.languages.qualitative$Standardized, ',')+1
#correggere, se contiene no
dataset.compare.languages.qualitative[dataset.compare.languages.qualitative$Standardized == "No" ,]$Num.Standard <-0



dataset.compare.languages.qualitative$Standardized <- NULL
dataset.compare.languages.qualitative$Use<-NULL
dataset.compare.languages.qualitative$Other <- NULL

#Load libraries to drawing the decision trees

library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
#Loading required package: bitops

#Create train and test dataset using sample() function

#from an idea of https://www.edureka.co/blog/knn-algorithm-in-r/
set.seed(123)
dat.d <- sample(1:nrow(dataset.compare.languages.qualitative),size=nrow(dataset.compare.languages.qualitative)*0.7,replace = FALSE) #random selection of 70% data.
train <- dataset.compare.languages.qualitative[dat.d,] # 70% training data
test <- dataset.compare.languages.qualitative[-dat.d,] # remaining 30% test data
#Relation between Functional paradigma and some use cases
formula.functional.use.cases <- Functional ~ Prototype.Based.Programming+ Data.Processing  + Domain.Specific + Symbolic.Computing
treemodel <- rpart (formula.functional.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Functional)
fancyRpartPlot(treemodel)


#Relation between Web and mobile and number of use case
formula.reflective.use.cases <- Web +Mobile ~ Num.Uses
treemodel <- rpart (formula.reflective.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Reflective)
fancyRpartPlot(treemodel)


formula.reflective.use.cases <- Num.Paradigma ~  Functional 
treemodel <- rpart (formula.reflective.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Reflective)
fancyRpartPlot(treemodel)

colnames(dataset.compare.languages.qualitative)

dataset.compare.languages.qualitative <- dataset.compare.languages.qualitative[,36:39]
correlation.data <- dataset.compare.languages.qualitative
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")

#linear regression
