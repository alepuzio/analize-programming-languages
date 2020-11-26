
#basic plot
# **qnorm()** doens't work

boxplot(dataset.compare.languages.qualitative.qualitative)
hist(dataset.compare.languages.qualitative.qualitative)
plot(dataset.compare.languages.qualitative.qualitative)

# barplot
bar_paradigma <- ggplot(dataset.compare.languages.qualitative, aes(x = Num.Paradigma)) +  geom_bar(fill = "orange", width = 0.7) + xlab("Imperative Paradigm") + ylab("Number of Languages")  + coord_flip() 

bar_uses <- ggplot(dataset.compare.languages.qualitative, aes(x = Num.Uses)) +  geom_bar(fill = "red", width = 0.7) + xlab("OOP Paradigm") + ylab("Number of Languages")  + coord_flip() 
bar_other <- ggplot(dataset.compare.languages.qualitative, aes(x = Num.Other)) +  geom_bar(fill = "blue", width = 0.7) + xlab("Functional Paradigm") + ylab("Number of Languages")  + coord_flip() 
bar_standards <- ggplot(dataset.compare.languages.qualitative, aes(x = Num.Standars)) +  geom_bar(fill = "green", width = 0.7) + xlab("Procedural Paradigm") + ylab("Number of Languages")  + coord_flip() 
#bar_generic <- ggplot(dataset.compare.languages.qualitative, aes(x = Generic)) +  geom_bar(fill = "yellow", width = 0.7) + xlab("Generic Paradigm") + ylab("Number of Languages") + coord_flip() 

#bar_reflective <- ggplot(dataset.compare.languages.qualitative, aes(x = Reflective)) +  geom_bar(fill = "purple", width = 0.7) + xlab("Reflective") + ylab("Number of Languages")  + coord_flip() 


bar_paradigma
bar_uses
bar_other
bar_standards
#bar_generic 
#bar_reflective

# show distributions of the data da mettere in ML
plot_imperative <- ggplot(dataset.compare.languages.qualitative,  aes(Imperative)) + geom_histogram(fill = "green", bins = "30") + labs(title = "# Imperative Languages")
plot_oop <- ggplot(dataset.compare.languages.qualitative, aes(OOP)) + geom_histogram(fill = "red", bins = "30") + labs(title = "# OOP Languages ")
plot_functional <- ggplot(dataset.compare.languages.qualitative,  aes(Functional)) + geom_histogram(fill = "yellow", bins = "30") + labs(title = "# Functional Languages")
plot_procedural <- ggplot(dataset.compare.languages.qualitative, aes(Procedural)) + geom_histogram(fill = "blue", bins = "30") + labs(title = "# Procedural Languages")


plot_grid(plot_imperative,
          plot_oop,
          plot_functional,
          plot_procedural,          
          labels = c('Imperative','OOP','Functional', 'Procedural'),
          label_x = 1,
          ncol = 2,
          nrow = 2)



