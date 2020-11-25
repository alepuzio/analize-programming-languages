#Count the uses
dataset.compare.languages.num.uses <- my.safe.copy.dataframe(dataset.compare.languages)
unique(dataset.compare.languages.num.uses$Use)
print("Create a new numeric column that has the number of Use Case")
dataset.compare.languages.num.uses$Num.Uses <- str_count(dataset.compare.languages.num.uses$Use, ',')+1
dataset.compare.languages.num.uses[dataset.compare.languages.num.uses$Use == "" ,]$Num.Uses <-0

#count the number of other paradigm
print("Create a new numeric column that has the number of Other paradigms")
dataset.compare.languages.others <- my.safe.copy.dataframe(dataset.compare.languages.num.uses)
dataset.compare.languages.others$Num.Other <- str_count(dataset.compare.languages.others$Other, ',')+1
dataset.compare.languages.others[dataset.compare.languages.others$Other == "" ,]$Num.Other <-0
print("delete Other column")
dataset.compare.languages.others$Other <- NULL

print("Create a new numeric column that has the number of Standard")
dataset.compare.languages.standards <- my.safe.copy.dataframe(dataset.compare.languages.others)
dataset.compare.languages.standards$Num.Standard <- str_count(dataset.compare.languages.standards$Standardized, ',')+1
#correggere, se contiene no
dataset.compare.languages.standards[dataset.compare.languages.standards$Standardized == "No" ,]$Num.Standard <-0

dataset.compare.languages.standards$Standardized <- NULL
dataset.compare.languages.standards$Use<-NULL
dataset.compare.languages.standards$Language<-NULL




dataset.compare.languages.quantitative <- data.frame(lapply(dataset.compare.languages.standards, function(x) {
  gsub("Yes", "2",x )
}))
dataset.compare.languages.quantitative <- data.frame(lapply(dataset.compare.languages.quantitative, function(x) {
  gsub("No", "0",x )
}))
dataset.compare.languages.quantitative <- data.frame(lapply(dataset.compare.languages.quantitative, function(x) {
  gsub("Parzial", "1",x )
}))


colnames(dataset.compare.languages.quantitative)
# rifare
print("Visualization the relation between some columns")
library("hexbin")
ggplot(dataset.compare.languages.quantitative, aes(x = Num.Standard, y = Num.Uses)) + geom_hex(bins = 20)

ggplot(dataset.compare.languages.quantitative, aes(x = OOP, y = Num.Uses)) + geom_hex(bins = 20)
ggplot(dataset.compare.languages.quantitative, aes(x = Imperative, y = Num.Uses)) + geom_hex(bins = 20)
ggplot(dataset.compare.languages.quantitative, aes(x = Functional, y = Num.Uses)) + geom_hex(bins = 20)

#transform the column in numeric
#dataset.compare.languages.quantitative %>% mutate_if(is.character,as.numeric)
head(dataset.compare.languages.quantitative,1)
status(dataset.compare.languages.quantitative)

print("Transform column with number in numeric columns")
dataset.compare.languages.quantitative[,1:36] <- sapply(dataset.compare.languages.quantitative[,1:36],as.numeric)
apply(correlation.data, 2, is.numeric)
#OK


#study correlation
summary(dataset.compare.languages.quantitative)
colnames(dataset.compare.languages.quantitative)

correlation.data <- cbind(dataset.compare.languages.quantitative[,1:6],dataset.compare.languages.quantitative[,30:36])
#correlation.data <- (dataset.compare.languages.quantitative[,1:6])
head(correlation.data, 1)
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")
