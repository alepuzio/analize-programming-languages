#Qualitative analisys
#copy dataset

dataset.compare.languages.quantitative <- my.safe.copy.dataframe(dataset.compare.languages)

#new column about the number of supported paradigma
dataset.compare.languages.quantitative$Num.Paradigma <- 
  (dataset.compare.languages.quantitative$Imperative+dataset.compare.languages.quantitative$OOP+
  dataset.compare.languages.quantitative$Functional + dataset.compare.languages.quantitative$Procedural+
  dataset.compare.languages.quantitative$Generic + dataset.compare.languages.quantitative$Reflective+
  dataset.compare.languages.quantitative$Event.Driven)/10

#verify that every language has at least one paradigma
dataset.compare.languages.quantitative[dataset.compare.languages.quantitative$Num.Paradigma == 0,]$Language


#Count the uses
dataset.compare.languages.quantitative$Num.Uses <- str_count(dataset.compare.languages.quantitative$Use, ',')+1
dataset.compare.languages.quantitative[dataset.compare.languages.quantitative$Use == "" ,]$Num.Uses <-0

#count the number of other paradigm
dataset.compare.languages.quantitative$Num.Other <- str_count(dataset.compare.languages.quantitative$Other, ',')+1
dataset.compare.languages.quantitative[dataset.compare.languages.quantitative$Other == "" ,]$Num.Other <-0

dataset.compare.languages.quantitative <- my.safe.copy.dataframe(dataset.compare.languages.quantitative)
dataset.compare.languages.quantitative$Num.Standard <- str_count(dataset.compare.languages.quantitative$Standardized, ',')+1
dataset.compare.languages.quantitative[dataset.compare.languages.quantitative$Standardized == "No" ,]$Num.Standard <-0

#delete the useless columns: they are categorical columns
dataset.compare.languages.quantitative$Standardized <- NULL
dataset.compare.languages.quantitative$Use<-NULL
dataset.compare.languages.quantitative$Other <- NULL


#Create train and test dataset using sample() function

#from an idea of https://www.edureka.co/blog/knn-algorithm-in-r/
set.seed(123)
dat.d <- sample(1:nrow(dataset.compare.languages.quantitative),size=nrow(dataset.compare.languages.quantitative)*0.7,replace = FALSE) #random selection of 70% data.
train <- dataset.compare.languages.quantitative[dat.d,] # 70% training data
test <- dataset.compare.languages.quantitative[-dat.d,] # remaining 30% test data

# Tree Decision
#Load libraries to drawing the decision trees
library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")

#Relation between Functional paradigma and some use cases
formula.functional.use.cases <- Functional ~ Prototype.Based.Programming+ Data.Processing  + Domain.Specific + Symbolic.Computing
treemodel <- rpart (formula.functional.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Functional)
fancyRpartPlot(treemodel)

#Relation between Web and mobile and number of use case
formula.reflective.web.mobile.use.cases <- Web +Mobile ~ Num.Uses 
treemodel <- rpart (formula.reflective.web.mobile.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Num.Uses)
fancyRpartPlot(treemodel)

#Relation between the number of the paradigmas and the functional paradigma
formula.num.paradigma.functional <- Num.Paradigma ~  Functional 
treemodel <- rpart (formula.num.paradigma.functional, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Num.Paradigma)
fancyRpartPlot(treemodel)

#Studing the correlation between the numeric columns
dataset.compare.languages.quantitative <- dataset.compare.languages.quantitative[,36:39]
correlation.data <- dataset.compare.languages.quantitative
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")
# The highest correlation is the correlation between number of use cases and number of paradigma

#linear regression of th num uses in function of Num paradigma
formula.num.cases.num.paradigma <- Num.Uses ~  Num.Paradigma
model <- lm(formula.num.cases.num.paradigma, data = train)
prediction <- predict (model, newdata = test)
table(prediction, test$Num.Uses)
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")
summary(model)


#SVM between number of paradigma of number of uses
library("e1071")
formula.num.cases.num.paradigma <- Num.Paradigma ~  Num.Uses
tune <- tune.svm(formula.num.cases.num.paradigma, data =train, gamma = 10^(-6:-1), cost=10^(-4:-1))
summary(tune)

model <- svm(formula.num.cases.num.paradigma, data =train, method = "c-classification", kernel="radial",probabilyt=T,gamma=0.001, cost=10000)
prediction <- predict(model, test)
table(prediction, test$Num.Uses)

# load libraries
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # draw plot

# function to calculate k
# - dataset.to.cluster: dataframe with the data
# - num.cluster: number of the clusters hwere the data are grouped
calculate.k.means <- function(dataset.to.cluster, num.cluster) {
  final <- kmeans(dataset.to.cluster, num.cluster, nstart = 25)
  final
}


# Apply K-Means algorithm
set.seed(1234)
dataset.k.means <- my.safe.copy.dataframe(dataset.compare.languages.quantitative)
dataset.k.means$Language <- dataset.compare.languages$Language

dataset.k.means.column <- c("Num.Paradigma" ,"Num.Uses","Num.Other","Num.Standard")
clusters <- calculate.k.means(dataset.k.means[,dataset.k.means.column], 10)
o <- order(clusters$cluster)
data.frame(dataset.k.means$Language[o], clusters$cluster[o])

#It draws the plots from https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html
plot(dataset.k.means$Num.Standard, dataset.k.means$Num.Uses, type="n", xlim=c(0,9), xlab="Num Standards", 
     ylab="Num Uses")
text(x=dataset.k.means$Num.Standard, y=dataset.k.means$Num.Uses, 
     labels=dataset.k.means$Language,col=clusters$cluster)

