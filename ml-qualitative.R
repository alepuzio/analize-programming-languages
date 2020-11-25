print ("copy dataset")
dataset.compare.languages.qualitative <- my.safe.copy.dataframe(dataset.compare.languages)

#delete useless columns
dataset.compare.languages.qualitative$Standardized <- NULL
dataset.compare.languages.qualitative$Use<-NULL
dataset.compare.languages.qualitative$Other <- NULL



library("rpart")
library("rattle")
library("rpart.plot")
library("RColorBrewer")

#from an idea of https://www.edureka.co/blog/knn-algorithm-in-r/
set.seed(123)
dat.d <- sample(1:nrow(dataset.compare.languages.qualitative),size=nrow(dataset.compare.languages.qualitative)*0.7,replace = FALSE) #random selection of 70% data.
train <- dataset.compare.languages.qualitative[dat.d,] # 70% training data
test <- dataset.compare.languages.qualitative[-dat.d,] # remaining 30% test data

print("Relation between Functional paradigma and some use cases")
formula.functional.use.cases <- Functional ~ Prototype.Based.Programming+ Data.Processing  + Domain.Specific + Symbolic.Computing
treemodel <- rpart (formula.functional.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Functional)
fancyRpartPlot(treemodel)


# Reflective
print("Relation between Reflective paradigma and some use cases")
formula.reflective.use.cases <- Reflective ~Event.driven + Business + Domain.Specific + Mobile + GUI + Web+Highly.domain.specific+RAD
treemodel <- rpart (formula.reflective.use.cases, data = train, method = "class", minsplit=2, minbucket=1)
prediction <- predict (treemodel, newdata = test, type = 'class')
table(prediction, test$Reflective)
fancyRpartPlot(treemodel)




