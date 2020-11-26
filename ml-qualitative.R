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
formula.reflective.web.mobile.use.cases <- Web +Mobile ~ Num.Uses 
treemodel <- rpart (formula.reflective.web.mobile.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Num.Uses)
fancyRpartPlot(treemodel)

#sistemare
formula.num.paradigma.functional <- Num.Paradigma ~  Functional 
treemodel <- rpart (formula.num.paradigma.functional, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Num.Paradigma)
fancyRpartPlot(treemodel)

colnames(dataset.compare.languages.qualitative)

dataset.compare.languages.qualitative <- dataset.compare.languages.qualitative[,36:39]
correlation.data <- dataset.compare.languages.qualitative
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")

#linear regression of th num uses in function of Num paradigma
formula.num.cases.num.paradigma <- Num.Uses ~  Num.Paradigma
model <- lm(formula.num.cases.num.paradigma, data = train)
prediction <- predict (model, newdata = test)
table(prediction, test$Num.Uses)
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")

summary(model)


#SVM
library("e1071")
formula.num.cases.num.paradigma <- Num.Uses ~  Num.Paradigma
tune <- tune.svm(formula.num.cases.num.paradigma, data =train, gamma = 10^(-6:-1), cost=10^(-4:-1))
summary(tune)

model <- svm(formula.num.cases.num.paradigma, data =train, method = "c-classification", kernel="radial",probabilyt=T,gamma=0.001, cost=10000)
prediction <- predict(model, test$Num.Uses, probability=T)
table(prediction, test$Num.Uses)

# load libraries
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # draw plot
# function to evalutate the k
# - dataset.to.draw: dataframe with the data
# - function.to.draw: I'm using kmeans
# - method.to.draw: wss, silhouette
draw.fviz_nbclust <- function(dataset.to.draw, function.to.draw, method.to.draw ) {
  set.seed(123)
  fviz_nbclust(dataset.to.draw, function.to.draw, method.to.draw)
}

# function to calculate k
# - dataset.to.cluster: dataframe with the data
# - num.cluster: number of the clusters hwere the data are grouped
calculate.k.means <- function(dataset.to.cluster, num.cluster) {
  final <- kmeans(dataset.to.cluster, num.cluster, nstart = 25)
  final
}


data("USAccDeaths")
df <- scale(USArrests) # Scaling the data
colnames(USArrests)
head(USArrests,2)

set.seed(1234)
dataset.k.means <- my.safe.copy.dataframe(dataset.compare.languages.qualitative)

dataset.k.means$Language<-dataset.compare.languages$Language
colnames(dataset.k.means)
head(dataset.k.means)


dataset.k.means.column <- c("Num.Paradigma" ,"Num.Uses","Num.Other","Num.Standard")
clusters <- calculate.k.means(dataset.k.means[,dataset.k.means.column], 10)
o=order(clusters$cluster)
data.frame(dataset.k.means$Language[o],clusters$cluster[o])

#from https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html
plot(dataset.k.means$Num.Standard, dataset.k.means$Num.Uses, type="n", xlim=c(0,9), xlab="Num Standards", 
     ylab="Num Uses")

text(x=dataset.k.means$Num.Standard, y=dataset.k.means$Num.Uses, 
     labels=dataset.k.means$Language,col=clusters$cluster)

