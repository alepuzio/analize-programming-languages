
# load libraries
library("Amelia")
library("caret")
library("corrplot")
library("cowplot")
library("dplyr")
library("funModeling") 
library("ggplot2")
library("Hmisc")
library("MASS")
library("mlbench")
library("plyr")
library("rvest")
library("stringr")
library("tidyverse") # metapackage of all tidyverse packages

# It read the data from wikipedia at the number-th table and 
# return the data.frame
# url: url for desired website to be scraped
# number: number-th of the tabel in the page HTML
my.raw.wikipedia <- function(url, number) {
  print(paste("my.raw.wikipedia(", url ,",", number ,")"))
  webpage <- read_html(url)  #output "xml_document" "xml_node"
  my.raw.wikipedia <- html_table(webpage, fill=TRUE)[[number]]
  my.raw.wikipedia
}

# safe copy of the data.frame X
my.safe.copy.dataframe <- function(x){
  my.safe.copy.dataframe  = x
  my.safe.copy.dataframe
}

# function used to save the dataframe without any elaboration
# can be a good alternative to use 
# the web scraping
# the file is a CSV file with
# ',' as separator and decimal in ','
# - x: data.frame
# - name: name of the file to write
# - directory : default current directory, it indicates
#               the dir where put the file 'name'
my.save.data.on.file <- function(x, name, directory = ".\\") {
  if (!grepl('raw_', name , fixed = TRUE)){
    print(paste("Error, ", name,  " is not a file for raw dataframe, it lacks the prefix raw_"))
  } else if(0 == nrow(x) | 0 == ncol(x) ){
    print("Error, it's a empty dataframe")
  } else if (file.exists(directory = '.\\input\\',file=name) ){
    print(paste("Existing file [", name,"] in disk", sep = ''))
  }
  else {
    print(paste("Save raw dataframe [", name,"] in disk", sep = ''))
    write.table(x
                , file = paste(
                  paste('.\\', directory, sep ="")
                  , name
                  ,'.csv'
                  , sep = '')
                , sep = ';'
                , row.names = TRUE
                , dec = '.'
    )
  }
}
#EDA of the dataset read by web scraping

url <- "https://en.wikipedia.org/wiki/Comparison_of_programming_languages"
raw.compare.languages <- my.raw.wikipedia(url,2)
dataset.compare.languages <- my.safe.copy.dataframe(raw.compare.languages)
colnames(dataset.compare.languages ) <- c("Language","Use","Imperative",
                                          "OOP",  "Functional", "Procedural",
                                          "Generic","Reflective", "Event.Driven", 
                                          "Other", "Standardized")
#There are some missing values?
missmap(dataset.compare.languages) 
# there are some NA values?
which(is.na(dataset.compare.languages))

dim(dataset.compare.languages) #134X11
head(dataset.compare.languages ,1)
colnames(dataset.compare.languages)
summary(dataset.compare.languages)
status(dataset.compare.languages)

freq(dataset.compare.languages$Imperative)
freq(dataset.compare.languages$OOP)
freq(dataset.compare.languages$Functional)
freq(dataset.compare.languages$Procedural)
freq(dataset.compare.languages$Generic)
freq(dataset.compare.languages$Reflective)


dataset.compare.languages$Imperative = ifelse(dataset.compare.languages$Imperative == 'Yes',10,0)

dataset.compare.languages$OOP = ifelse(dataset.compare.languages$OOP == 'Yes',10,ifelse(dataset.compare.languages$OOP == 'No',0,5))
#Functional

dataset.compare.languages$Functional = ifelse(dataset.compare.languages$Functional == 'Yes',10,ifelse(dataset.compare.languages$Functional == 'No',0,5))
#Procedural

dataset.compare.languages$Procedural = ifelse(dataset.compare.languages$Procedural == 'Yes',10,ifelse(dataset.compare.languages$Procedural == 'No',0,5))
#Generic

dataset.compare.languages$Generic = ifelse(dataset.compare.languages$Generic == 'Yes',10,ifelse(dataset.compare.languages$Generic == 'No',0,5))
#Reflective

dataset.compare.languages$Reflective = ifelse(dataset.compare.languages$Reflective == 'Yes',10,ifelse(dataset.compare.languages$Reflective == 'No',0,5))
#Event-driven

dataset.compare.languages$Event.Driven = ifelse(dataset.compare.languages$Event.Driven == 'Yes',10,ifelse(dataset.compare.languages$Event.Driven == 'No',0,5))
#Run freq() after cleaning the data

freq(dataset.compare.languages$Imperative)
freq(dataset.compare.languages$OOP)
freq(dataset.compare.languages$Functional)
freq(dataset.compare.languages$Procedural)
freq(dataset.compare.languages$Generic)
freq(dataset.compare.languages$Reflective)
freq(dataset.compare.languages$Event.Driven)

#It's better. After the normalization

describe(dataset.compare.languages)
#uhm.. ugly representations: I normalize the data

dataset.compare.languages$Agile <- ifelse(grepl("agile", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Application <- ifelse(grepl("Application", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Business <- ifelse(grepl("business", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Client.Side <- ifelse(grepl("client-side", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Data.Processing <- ifelse(grepl("data processing", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Domain.Specific <- ifelse(grepl("domain-specific", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Education <- ifelse(grepl("education", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Embedded <- ifelse(grepl("embedded", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Game <- ifelse(grepl("game", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Game.Scripting <- ifelse(grepl("game scripting", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$General <- ifelse(grepl("general", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$General.Purpose <- ifelse(grepl("general purpose", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$GUI <- ifelse(grepl("GUI automation (macros)", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Highly.domain.specific <- ifelse(grepl("Highly domain-specific", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Integration <- ifelse(grepl("Integration", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Low.Level.Operations <- ifelse(grepl("low-level operations", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Mobile <- ifelse(grepl("mobile", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Parallel.Application <- ifelse(grepl("Parallel application", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Prototype.Based.Programming <- ifelse(grepl("Prototype-based programming", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$RAD <- ifelse(grepl("RAD", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Realtime <- ifelse(grepl("realtime", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Scripting <- ifelse(grepl("scripting", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Server.side <- ifelse(grepl("server-side", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Shell <- ifelse(grepl("Shell", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Symbolic.Computing <- ifelse(grepl("symbolic computing", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$System <- ifelse(grepl("system", dataset.compare.languages$Use), 10, 0)
dataset.compare.languages$Web <- ifelse(grepl("web", dataset.compare.languages$Use), 10, 0)
#Better...

describe(dataset.compare.languages)
