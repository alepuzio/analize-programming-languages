print ("copy dataset")
dataset.compare.languages.qualitative <- my.safe.copy.dataframe(dataset.compare.languages)

#delete useless columns
dataset.compare.languages.qualitative$Standardized <- NULL
dataset.compare.languages.qualitative$Use<-NULL
dataset.compare.languages.qualitative$Other <- NULL

print ("create the Flexibility column as goal column")



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

final.k2 <- calculate.k.means(dataset.compare.languages.qualitative, 2)

library("rpart")

#index <- which(1:length(dataset.compare.languages.qualitative[,1])%%5 ==0 )
#train <- dataset.compare.languages.qualitative[-index,]
#test <- dataset.compare.languages.qualitative[-index,]

#from an idea of https://www.edureka.co/blog/knn-algorithm-in-r/
set.seed(123)
dat.d <- sample(1:nrow(dataset.compare.languages.qualitative),size=nrow(dataset.compare.languages.qualitative)*0.7,replace = FALSE) #random selection of 70% data.
train <- dataset.compare.languages.qualitative[dat.d,] # 70% training data
test <- dataset.compare.languages.qualitative[-dat.d,] # remaining 30% test data

library(rattle)
library(rpart.plot)
library(RColorBrewer)

formula.functional.use.cases <- Functional ~Prototype.Based.Programming+ Data.Processing + Parallel.Application + Domain.Specific + Symbolic.Computing
treemodel <- rpart (formula.functional.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
fancyRpartPlot(treemodel)

prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Functional)

# Reflective
formula.oop.use.cases <- Reflective ~Event.driven + Business + Domain.Specific + Mobile + GUI + Web+Highly.domain.specific+RAD
treemodel <- rpart (formula.oop.use.cases, data = train, method = "class")
fancyRpartPlot(treemodel)

formula.oop.use.cases <- OOP ~ .
treemodel <- rpart (formula.oop.use.cases, data = train, method = "class")
fancyRpartPlot(treemodel)

dim(train)
train[train$OOP =='Yes',]
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Application)


#random forest
install.packages("randomForest")
library("randomForest")

model <- randomForest(Application ~ OOP + Functional+Imperative , data = train, nTree = 100)
prediction <- predict(model, newata=test,type='class')

#cluster


#Install class package
install.packages('class')
# Load class package
library(class)
colnames(dataset.compare.languages.qualitative)
#Creating seperate dataframe for 'realtime' feature which is our target.
train.labels <- dataset.compare.languages.qualitative[dat.d,1]
test.labels <-dataset.compare.languages.qualitative[-dat.d,1]
train
knn.4 <- knn(train=train, test=test, cl=train.labels, k=4)
knn.27 <- knn(train=train, test=test, cl=train.labels, k=27)


#######################à
install.packages("gbm")
#faccio KNN