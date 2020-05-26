#####################################################################################################
# Script id   : 00. Load Package.R
# Script Name : 패키지 로드
# Author      : 김상훈
# Date        : 2020.04.26
#####################################################################################################
## 패키지 로드
#####################################################################################################
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c(  
  "dplyr",
  "RColorBrewer",
  "KoNLP",
  "data.table",
  "wordcloud2",
  "tm",
  "RPostgreSQL",
  "stringr",
  "rlist",
  "slam",
  "topicmodels",
  "lda",
  "rvest",
  "arules",
  "arulesViz",
  "igraph",
  "XML",
  "plyr",
  "httr",
  "webshot",
  "htmlwidgets",
  "ggplot2",
  "magrittr",
  "network",
  "GGally",
  "writexl",
  "readxl"
)
ipak(packages)
###########################################################################################################################################################
###########################################################################################################################################################
