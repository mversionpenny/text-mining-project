#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", 
                      "wordcloud", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(stringr)

#### Books ####
bookpath = "les_rois_maudits/txt_woaccents"
txtbook1 = file.path(bookpath, "[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt")
txtbook2 = file.path(bookpath, "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt")
txtbook3 = file.path(bookpath, "[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt")
txtbook4 = file.path(bookpath, "[Rois Maudits-4] La Loi des males - Druon,Maurice.txt")
txtbook5 = file.path(bookpath, "[Rois Maudits-5] La Louve de France - Druon,Maurice.txt")
txtbook6 = file.path(bookpath, "[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt")
txtbook7 = file.path(bookpath, "[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt")

#### Import modules ####
source("preprocessing.R")

#################################################
####           Analyze Book 1                ####
#################################################
### Investigate the problem with characters' names
book1 <- readLines(txtbook1, encoding = "UTF-8")
book1 <- removeWords(book1, c("de ", "du ", "d'", "le ", "la "))
book1 <- book1[1:which(book1=="REPERTOIRE")]
firstname = "Edouard"
sum(str_count(book1, firstname))
sum(str_count(book1, paste(firstname,'[:blank:]*[:punct:]+', sep=''))) +
  sum(str_count(book1, paste(firstname,'[:blank:]+[:lower:]+', sep='')))

### Preprocessing and simple visualization
book1 <- prepare.text(txtbook1)
book1.corpus <- creat.corpus(book1)
