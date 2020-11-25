#ML quantitative regression
#study correlation
summary(dataset.compare.languages.quantitative)
colnames(dataset.compare.languages.quantitative)

correlation.data <- cbind(dataset.compare.languages.quantitative[,1:6],dataset.compare.languages.quantitative[,30:36])
#correlation.data <- (dataset.compare.languages.quantitative[,1:6])
head(correlation.data, 1)
cor.table = cor(correlation.data)
corrplot(cor.table, method = "number",type = "lower")
