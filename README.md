# Analize the language programming
--------------------------------------
This project was born reading [Wikipedia](http://en.wikipedia.org) and [Kaggle](http://www.kaggle.com).

This application runs the web scraping to Wikipedia, writes a file with the data and analize the dataset about the programming languages.


## Feature

* [O] Reading and merging the data of languages about the error handling
* [O] Calculating the accuracy of the models
* [O] Using some other pages of wikipedia not by web scraping but using the archives

## Bug

* [O]  Organize the structure of the project and code following the [CONTRIBUTING.md](https://github.com/alepuzio/analize-programming-languages/blob/master/CONTRIBUTING.md) file.

## Legend: 
* [O] Doing in future in an apposite branch
* [X] Done in the past and delivered in master

## Status CI Integration
 
I use [Travis](https://travis-ci.org/)
[![Build Status](https://travis-ci.org/alepuzio/analize-programming-language.svg?branch=master)](https://travis-ci.org/alepuzio/analize-programming-languages)

## Getting started

### Prerequisites

* [R programming language](http://www.r-lang.org)
* Package Amelia       (install.package("Amelia")    
* Package caret        (install.package("caret")     
* Package cluster      (install.package("cluster")     
* Package corrplot     (install.package("corrplot")     
* Package cowplot      (install.package("cowplot")     
* Package dplyr        (install.package("dplyr")    
* Package e1071        (install.package("e1071")    
* Package factoextra   (install.package("factoextra")
* Package funModeling  (install.package("funModeling")
* Package ggplot2      (install.package("ggplot2")
* Package gridExtra    (install.package("gridExtra")
* Package hexbin       (install.package("hexbin")
* Package Hmisc        (install.package("Hmisc")
* Package MASS         (install.package("MASS")
* Package mlbench      (install.package("mlbench")
* Package plyr         (install.package("plyr")
* Package rattle       (install.package("rattle")
* Package RColorBrewer (install.package("RColorBrewer")
* Package rpart        (install.package("rpart")
* Package rpart.plot   (install.package("rpart.plot")
* Package rvest        (install.package("rvest")
* Package stringr      (install.package("stringr")
* Package tidyverse    (install.package("tidyverse")

### Installing

- Clone the project with *git-clone* (or download directly it)
- Have fun!

## Running the tests

- In this version the unit test are inside the module and are not completed. TDD, sorry :(

### Break down into to end to end tests

No indications

### Coding styles sheets

Please read the file [CONTRIBUTING.md](https://github.com/alepuzio/analize-programming-languages/blob/master/CONTRIBUTING.md)

## Deployment
 
 - No package built (sorry, I'm a beginner in R :) )
 
### Built with:

* [Rstudio](http://www.rstudio.com) - one of the best IDE I know

## Contributing

Please read the [CONTRIBUTING.md](https://github.com/alepuzio/analize-programming-languages/blob/master/CONTRIBUTING.md) for the details about the code of conduct and the process for submitting pull requests.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/alepuzio/analize-programming-languages/tags). 

## Authors

* **Alessandro Puzielli** - *creator* - [Alepuzio](https://github.com/alepuzio)

See also the list of [contributors](https://github.com/alepuzio/analize-programming-languages/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

#Acknowledgments

* **PurpleBooth** - to publish an [excellent template](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2) of README that I used in this project 
* **Yegor256** - to write the post [Elegant READMEs](https://www.yegor256.com/2019/04/23/elegant-readme.html) about the README file and the [An Open Code Base Is Not Yet an Open Source Project](https://www.yegor256.com/2018/05/08/open-source-attributes.html) for the Open Source languages
