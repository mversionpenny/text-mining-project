#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", 
                      "wordcloud", "stringr","networkD3","rJava", "mallet", 
                      "word2vec", "XML", "devtools", "statnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(stringr)
library(networkD3)
library(statnet)

# TODO : how to force???
library(devtools)
install_github("mukul13/rword2vec")
library(rword2vec)
# install_github("bmschmidt/wordVectors")
# library(wordVectors)
ls("package:rword2vec")

#### Books (cut) ####
# path to txt files containnig the books (without bibliography at the end)
bookpath = "les_rois_maudits/txt_processed_name/"
final_bookpath = "les_rois_maudits/final_txt/"
txtbook1 = "[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt"
txtbook2 = "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt"
txtbook3 = "[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt"
txtbook4 = "[Rois Maudits-4] La Loi des males - Druon,Maurice.txt"
txtbook5 = "[Rois Maudits-5] La Louve de France - Druon,Maurice.txt"
txtbook6 = "[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt"
txtbook7 = "[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt"
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
book1 <- prepare.text(file.path(bookpath,txtbook1),stopwords_fr)
# saving book1 entirely prepared
dir.create(final_bookpath,showWarnings = F)
writeLines(book1, file.path(final_bookpath,txtbook1))

book1.corpus <- VCorpus(VectorSource(book1))

book1.tdm <- TermDocumentMatrix(book1.corpus, control=list(wordLengths=c(2,Inf)))


source("networks.R")
characters.vector.book1 <- getCharactersVector(file.path(character_path, "characters1_clean_lowercase.txt"))

##### Get network with co-occurences ####
co.oc.low <- getNetworkWithAssocs(book1.tdm, characters.vector.book1, cor=0.05)
co.oc.high <- getNetworkWithAssocs(book1.tdm, characters.vector.book1, cor=0.15)
co.oc.low.igraph <- 
  getIgraph(co.oc.low$x$links$source, co.oc.low$x$links$target, 
            co.oc.low$x$links$value, characters.vector.book1)
co.oc.high.igraph <- 
  getIgraph(co.oc.high$x$links$source, co.oc.high$x$links$target, 
            co.oc.high$x$links$value, characters.vector.book1)

tkplot(co.oc.low.igraph, vertex.color="gold", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=0.9, vertex.label.dist=0.5, edge.curved=0.2)
tkplot(co.oc.high.igraph, vertex.color="gold", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=0.9, vertex.label.dist=0.5, edge.curved=0.2)

##### Get network with LDA (topic-modelling) ####
m.disp <- getTopicsModelling(book1, stopwords_fr_path, 30)
lda <- getNetworkWithLDA(m.disp, characters.vector.book1, sankey=F)
lda.igraph <- 
  getIgraph(lda$x$links$source, lda$x$links$target, 
            lda$x$links$value, characters.vector.book1)
tkplot(lda.igraph, vertex.color="gold", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=0.9, vertex.label.dist=0.5, edge.curved=0.2)

##### Get network with Word2Vec ####
# do not run if you want the same result as us
# model <-word2vec(train_file = file.path(final_bookpath,txtbook1),
#                  output_file = "vec.bin",binary=1)
w2v <- getNetworkWithWord2Vec("vec.bin", characters.vector.book1, dist = 25)
w2v.igraph <- 
  getIgraph(w2v$x$links$source, w2v$x$links$target, 
            w2v$x$links$value, characters.vector.book1)
tkplot(w2v.igraph, vertex.color="gold", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=0.9, vertex.label.dist=0.5, edge.curved=0.2)
# ana=word_analogy(
#   file_name = "vec.bin",
#   search_words = "margueritedebourgogne philippedaunay blanchedebourgogne",
#   num = 20)
# 
# ana=word_analogy(
#   file_name = "vec.bin",
#   search_words = "philippeiv charlesdevalois blanchedebourgogne",
#   num = 20)
# 
# ana=word_analogy(
#   file_name = "vec.bin",
#   search_words = "robertdartois mahautdebourgogne charlesdevalois",
#   num = 20)
# ana=word_analogy(
#   file_name = "vec.bin",
#   search_words = "jeannedebourgogne margueritedebourgogne philippedaunay",
#   num = 20)
# ana=word_analogy(
#   file_name = "vec.bin",
#   search_words = "louisv louisdevreux robertdartois",
#   num = 20)

### tests igraph ###


plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5) 

test <- getIgraph(w2v$x$links$source, w2v$x$links$target, w2v$x$links$value, characters.vector.book1)
plot(test, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=0.5) 
plot <- tkplot(test, vertex.color="gold", vertex.shape="sphere", vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.9, vertex.label.dist=0.5, edge.curved=0.2) 


hs <- hub_score(test, weights=NA)$vector
plot(test, vertex.size=hs*10, main="Hubs")
as <- authority_score(test, weights=NA)$vector
plot(test, vertex.size=as*30, main="Authorities")
