# load libraries
library("Amelia")#verify miss values
library("caret")
library("corrplot")
library("cowplot")
library("dplyr")
library("funModeling") 
library("ggplot2")
library("Hmisc")
library("MASS")
library("mlbench") #verify miss values
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
  webpage <- read_html(url)
  #output "xml_document" "xml_node"
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



#compration 
url.compare.languages <-'https://en.wikipedia.org/wiki/Comparison_of_programming_languages'
raw.compare.languages <- my.raw.wikipedia(url.compare.languages , 2)

# TODO error to analizye
#my.save.data.on.file(x = raw.compare.languages , name = "raw_compare_languages")

#EDA over dataset by web scraping
dataset.compare.languages <- my.safe.copy.dataframe(raw.compare.languages)
colnames(dataset.compare.languages ) <- c("Language","Use","Imperative",
                                          "OOP",  "Functional", "Procedural",
                                          "Generic","Reflective", "Event.driven", 
                                          "Other", "Standardized")
missmap(dataset.compare.languages) # no missing values
which(is.na(dataset.compare.languages)) # no NA values

dim(dataset.compare.languages) #134X11
head(dataset.compare.languages ,1)
colnames(dataset.compare.languages)
summary(dataset.compare.languages)
status(dataset.compare.languages)


print("analize the values")
freq(dataset.compare.languages$Imperative)
freq(dataset.compare.languages$OOP)
freq(dataset.compare.languages$Functional)
freq(dataset.compare.languages$Procedural)
freq(dataset.compare.languages$Generic)
freq(dataset.compare.languages$Reflective)

# uhm... there are some values with unclear meaning
# I allow only the values: No, Yes, Parzial
#manage the empty values
## Imperative
dataset.compare.languages[dataset.compare.languages[3] == "",][3] <- "No"

## Object-Oriented
dataset.compare.languages[dataset.compare.languages[4] == "",][4] <- "No"
dataset.compare.languages[dataset.compare.languages[4] != "No"
                          & dataset.compare.languages[4] != "Yes"
                          ,][4]  <- "Parzial"

#TODO sistemarle
#my.explicit.dataset <- function(number.column) {
#  print(paste("my.explicit.dataset(", number.column ,")"))
#  dataset.compare.languages[dataset.compare.languages[number.column] == ""][number.column] <- "No"
#  dataset.compare.languages[dataset.compare.languages[number.column] != "No" 
#                            & dataset.compare.languages[number.column] != "Yes"][number.column] <- "Parzial"
#}
  



## Functional

#my.explicit.dataset(5)
dataset.compare.languages[dataset.compare.languages[5] == "",][5] <- "No"
dataset.compare.languages[dataset.compare.languages[5] != "No"
                          & dataset.compare.languages[5] !="Yes" 
                          ,][5] <- "Parzial"

## "Procedural"
dataset.compare.languages[dataset.compare.languages[6] == "",][6] <- "No"
dataset.compare.languages[dataset.compare.languages[6] != "No"
                          & dataset.compare.languages[6] !="Yes" 
                          ,][6] <- "Parzial"
## "Generic"
dataset.compare.languages[dataset.compare.languages[7] == "",][7] <- "No"
dataset.compare.languages[dataset.compare.languages[7] != "No"
                          & dataset.compare.languages[7] !="Yes" 
                          ,][7] <- "Parzial"


## "Reflective"
dataset.compare.languages[dataset.compare.languages[8] == "",][8] <- "No"
dataset.compare.languages[dataset.compare.languages[8] != "No"
                          & dataset.compare.languages[8] !="Yes" 
                          ,][8] <- "Parzial"

## "Event-driven"

dataset.compare.languages[dataset.compare.languages[9] == "",][9] <- "No"
dataset.compare.languages[dataset.compare.languages[9] != "No"
                          & dataset.compare.languages[9] !="Yes" 
                          ,][9] <- "Parzial"

## "Other"
##decompose


#"Standardized"
print("Run freq() after cleaning the data")
freq(dataset.compare.languages$Imperative)
freq(dataset.compare.languages$OOP)
freq(dataset.compare.languages$Functional)
freq(dataset.compare.languages$Procedural)
freq(dataset.compare.languages$Generic)
freq(dataset.compare.languages$Reflective)
print("It\'s better")

print("After the normalization")
summary (dataset.compare.languages) #only categorical columns

print("Run table()")
table(dataset.compare.languages$Imperative)
table(dataset.compare.languages$OOP)
table(dataset.compare.languages$Functional)
table(dataset.compare.languages$Procedural)
table(dataset.compare.languages$Generic)
table(dataset.compare.languages$Reflective)

print("Run describe()")
describe(dataset.compare.languages)

print(" uhm.. ugly representations: I normalize the data")
dataset.compare.languages$Agile <- ifelse(grepl("agile", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Application <- ifelse(grepl("Application", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Business <- ifelse(grepl("business", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Client.Side <- ifelse(grepl("client-side", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Data.Processing <- ifelse(grepl("data processing", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Domain.Specific <- ifelse(grepl("domain-specific", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Education <- ifelse(grepl("education", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Embedded <- ifelse(grepl("embedded", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Game <- ifelse(grepl("game", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Game.Scripting <- ifelse(grepl("game scripting", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$General.Purpose <- ifelse(grepl("general", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$General.Purpose <- ifelse(grepl("general purpose", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$GUI <- ifelse(grepl("GUI automation (macros)", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Highly.domain.specific <- ifelse(grepl("Highly domain-specific", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Integration <- ifelse(grepl("Integration", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Low.Level.Operations <- ifelse(grepl("low-level operations", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Mobile <- ifelse(grepl("mobile", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Parallel.Application <- ifelse(grepl("Parallel application", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Prototype.Based.Programming <- ifelse(grepl("Prototype-based programming", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$RAD <- ifelse(grepl("RAD", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Realtime <- ifelse(grepl("realtime", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Scripting <- ifelse(grepl("scripting", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Server.side <- ifelse(grepl("server-side", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Shell <- ifelse(grepl("Shell", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Symbolic.Computing <- ifelse(grepl("symbolic computing", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$System <- ifelse(grepl("system", dataset.compare.languages$Use), "Yes", "No")
dataset.compare.languages$Web <- ifelse(grepl("web", dataset.compare.languages$Use), "Yes", "No")

print("better...")
describe(dataset.compare.languages)



#tye check evolutvia
#https://en.wikipedia.org/wiki/Comparison_of_programming_languages_by_type_system