#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", 
                      "wordcloud", "stringr","networkD3","rJava", "mallet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(stringr)
library(networkD3)

#### Books (cut) ####
# path to txt files containnig the books (without bibliography at the end)
bookpath = "les_rois_maudits/final_txt/"
txtbook1 = file.path(bookpath,"[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt")
txtbook2 = file.path(bookpath, "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt")
txtbook3 = file.path(bookpath,"[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt")
txtbook4 = file.path(bookpath,"[Rois Maudits-4] La Loi des males - Druon,Maurice.txt")
txtbook5 = file.path(bookpath,"[Rois Maudits-5] La Louve de France - Druon,Maurice.txt")
txtbook6 = file.path(bookpath,"[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt")
txtbook7 = file.path(bookpath,"[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt")
# path to txt files containing the name of characters
character_path = "les_rois_maudits/characters/"

#### Import modules ####
source("preprocessing.R")

#################################################
####           Analyze Book 1                ####
#################################################
### Preprocessing and simple visualization
# path to stopwords :
stopwords_fr_path <- "Stop-words-french-utf8.txt"
stopwords_fr <- readLines(stopwords_fr_path)
book1 <- prepare.text(txtbook1,stopwords_fr)

book1.corpus <- VCorpus(VectorSource(book1))

book1.tdm <- TermDocumentMatrix(book1.corpus, control=list(wordLengths=c(2,Inf)))


source("networks.R")
characters.vector.book1 <- getCharactersVector(file.path(character_path, "characters1_clean_lowercase.txt"))
getNetworkWithAssocs(book1.tdm, characters.vector.book1, cor=0.05)
m.disp <- getTopicsModelling(book1, stopwords_fr_path, 20)
getNetworkWithLDA(m.disp, characters.vector.book1)
