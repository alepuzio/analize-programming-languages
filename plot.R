# qualitative
# plot
bar_imperative <- ggplot(dataset.compare.languages, aes(x = Imperative)) +  geom_bar(fill = "orange", width = 0.7) + xlab("Imperative Paradigm") + ylab("Number of Languages")  + coord_flip() 

bar_oop <- ggplot(dataset.compare.languages, aes(x = OOP)) +  geom_bar(fill = "red", width = 0.7) + xlab("OOP Paradigm") + ylab("Number of Languages")  + coord_flip() 
bar_functional <- ggplot(dataset.compare.languages, aes(x = Functional)) +  geom_bar(fill = "blue", width = 0.7) + xlab("Functional Paradigm") + ylab("Number of Languages")  + coord_flip() 
bar_procedural <- ggplot(dataset.compare.languages, aes(x = Procedural)) +  geom_bar(fill = "green", width = 0.7) + xlab("Procedural Paradigm") + ylab("Number of Languages")  + coord_flip() 
bar_generic <- ggplot(dataset.compare.languages, aes(x = Generic)) +  geom_bar(fill = "yellow", width = 0.7) + xlab("Generic Paradigm") + ylab("Number of Languages") + coord_flip() 

bar_reflective <- ggplot(dataset.compare.languages, aes(x = Reflective)) +  geom_bar(fill = "purple", width = 0.7) + xlab("Reflective") + ylab("Number of Languages")  + coord_flip() 


bar_imperative
bar_oop
bar_functional
bar_procedural
bar_generic 
bar_reflective

# show distributions of the data da mettere in ML
plot_imperative <- ggplot(dataset.compare.languages,  aes(Imperative)) + geom_histogram(fill = "green", bins = "30") + labs(title = "# Imperative Languages")
plot_oop <- ggplot(dataset.compare.languages, aes(OOP)) + geom_histogram(fill = "red", bins = "30") + labs(title = "# OOP Languages ")
plot_functional <- ggplot(dataset.compare.languages,  aes(Functional)) + geom_histogram(fill = "yellow", bins = "30") + labs(title = "# Functional Languages")
plot_procedural <- ggplot(dataset.compare.languages, aes(Procedural)) + geom_histogram(fill = "blue", bins = "30") + labs(title = "# Procedural Languages")


plot_grid(plot_imperative,
          plot_oop,
          plot_functional,
          plot_procedural,          
          labels = c('Imperative','OOP','Functional', 'Procedural'),
          label_x = 1,
          ncol = 2,
          nrow = 2)



