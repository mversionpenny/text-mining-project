#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", 
                      "wordcloud", "stringr","networkD3","rJava", "mallet", 
                      "XML", "devtools", "statnet","igraph")
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
library(igraph)

library(devtools)


install_github("mukul13/rword2vec")
library(rword2vec)

#### Books (cut) ####
# path to txt files containnig the books (without bibliography at the end)
bookpath = "les_rois_maudits/txt_processed_name/"
final_bookpath = "les_rois_maudits/final_txt/"
txtbook1 = "[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt"
# txtbook2 = "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt"
# txtbook3 = "[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt"
# txtbook4 = "[Rois Maudits-4] La Loi des males - Druon,Maurice.txt"
# txtbook5 = "[Rois Maudits-5] La Louve de France - Druon,Maurice.txt"
# txtbook6 = "[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt"
# txtbook7 = "[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt"
# path to txt files containing the name of characters
character_path = "les_rois_maudits/characters/"

#### Import modules ####
source("preprocessing.R")
source("networks.R")


################################################################################
########                           Book1                                ########
################################################################################

#### Preprocessing and simple visualization ####
# path to stopwords :
stopwords_fr_path <- "Stop-words-french-utf8.txt"
stopwords_fr <- readLines(stopwords_fr_path, encoding = "UTF-8")
book1 <- prepare.text(file.path(bookpath,txtbook1),stopwords_fr)
# saving book1 entirely prepared
dir.create(final_bookpath,showWarnings = F)
writeLines(book1, file.path(final_bookpath,txtbook1))

book1.corpus <- VCorpus(VectorSource(book1))

book1.tdm <- TermDocumentMatrix(book1.corpus, control=list(wordLengths=c(3,Inf)))

simple.visu(book1.tdm)

characters.vector.book1 <- 
  getCharactersVector(file.path(character_path, "characters1_clean_lowercase.txt"))

character.labels <- readLines(file.path(character_path, "characters1_clean.txt"))
##### Get network with co-occurences ####
co.oc.low <- getNetworkWithAssocs(book1.tdm, characters.vector.book1, cor=0.05)
co.oc.low
co.oc.low.igraph <- 
  getIgraph(co.oc.low$x$links$source, co.oc.low$x$links$target, 
            co.oc.low$x$links$value, characters.vector.book1)
tkplot(co.oc.low.igraph, vertex.color="lightblue", vertex.shape="circle", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

co.oc.high <- getNetworkWithAssocs(book1.tdm, characters.vector.book1, cor=0.15)
co.oc.high
co.oc.high.igraph <- 
  getIgraph(co.oc.high$x$links$source, co.oc.high$x$links$target, 
            co.oc.high$x$links$value, characters.vector.book1)
tkplot(co.oc.high.igraph, vertex.color="lightblue", vertex.shape="circle", 
       vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

##### Get network with LDA (topic-modelling) ####
# Do not run if you want the same results as us
# m.disp1 <- getTopicsModelling(book1, stopwords_fr_path, 30)
# save(m.disp1, file = "m.disp1.RData")
load("m.disp1.RData")
lda1 <- getNetworkWithLDA(m.disp1, characters.vector.book1, sankey=T)
lda1
lda.igraph.1 <- 
  getIgraph(lda1$x$links$source, lda1$x$links$target, 
            lda1$x$links$value, characters.vector.book1)
tkplot(lda.igraph.1, vertex.color="lightblue", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

# Do not run if you want the same results as us
# m.disp2 <- getTopicsModelling(book1, stopwords_fr_path, 200)
# save(m.disp2, file = "m.disp2.RData")
load("m.disp2.RData")
lda2 <- getNetworkWithLDA(m.disp2, characters.vector.book1, sankey=T)
lda2
lda.igraph.2 <- 
  getIgraph(lda2$x$links$source, lda2$x$links$target, 
            lda2$x$links$value, characters.vector.book1)
tkplot(lda.igraph.2, vertex.color="lightblue", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

##### Get network with LDA 200 ponderated ####
hs <- hub_score(lda.igraph.2, weights=NA)$vector
tkplot(lda.igraph.2, vertex.size=hs*12,  vertex.label.color="black", 
       vertex.label.dist=0.7,vertex.color="lightblue")

##### Get network with Word2Vec ####
# do not run if you want the same result as us
# model <-word2vec(train_file = file.path(final_bookpath,txtbook1),
#                  output_file = "vec.bin",binary=1)
w2v <- getNetworkWithWord2Vec("vec.bin", characters.vector.book1, dist = 25)
w2v
w2v.igraph <- 
  getIgraph(w2v$x$links$source, w2v$x$links$target, 
            w2v$x$links$value, characters.vector.book1)
tkplot(w2v.igraph, vertex.color="lightblue", vertex.shape="sphere", vertex.size=12, 
       vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

## find analogies ##
ana.1 <- word_analogy(
  file_name = "vec.bin",
  search_words = "margueritedebourgogne philippedaunay blanchedebourgogne",
  num = 10)

ana.2 <- word_analogy(
  file_name = "vec.bin",
  search_words = "philippedaunay gautierdaunay blanchedebourgogne",
  num = 10)
ana.2

ana.3 <- word_analogy(
  file_name = "vec.bin",
  search_words = "robertdartois isabelle louisv",
  num = 10)
ana.3

ana.4 <- word_analogy(
  file_name = "vec.bin",
  search_words = "geoffroydecharnay jacquesdemolay margueritedebourgogne",
  num = 10)
ana.4

ana.5 <- word_analogy(
  file_name = "vec.bin",
  search_words = "mahautdebourgogne robertdartois eliabeldecressay",
  num = 10)
ana.5

#### Sentiment Analysis ####
# do not run if you want the same result as us
# m.disp.sentiment.analysis <- getTopicsModelling(book1, stopwords_fr_path, 200, num.top.words = 30)
# save(m.disp, file = "m.disp.sentiment.analysis.RData")
load("m.disp.sentiment.analysis.RData")

# Vectors for sentiments
negatif <- c("colere","recriminer" ,"haine", "furieux", "rage", "deteste",
             "hais", "crier", "hurler", "haissent", "froide", "tortures")
positif <- c("gai", "tendre", "aime", "aimes", "aiment", "joie", "amant", 
             "douceur", "amour", "joui", "amant","compagnon")
famille <- c("freres","frere", "cousin", "cousins", "soeurs", "soeur", "pere", 
             "mere", "tante", "oncle", "epoux", "epouse", "mari", "bellesoeurs",
             "fils", "fille")


## Negative sentiments ##
lda.negatif <- getNetworkWithLDASentimentAnalysis(m.disp, 
                                                  characters.vector.book1, 
                                                  sentimentvec= negatif,sankey=T)
lda.negatif.igraph <- 
  getIgraph(lda.negatif$x$links$source, lda.negatif$x$links$target, 
            lda.negatif$x$links$value, characters.vector.book1)
tkplot(lda.negatif.igraph, vertex.color="lightblue", vertex.shape="sphere", 
       vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)

## Positive sentiments ##
lda.positif <- getNetworkWithLDASentimentAnalysis(m.disp, 
                                                  characters.vector.book1, 
                                                  sentimentvec= positif,sankey=T)
lda.positif.igraph <- 
  getIgraph(lda.positif$x$links$source, lda.positif$x$links$target, 
            lda.positif$x$links$value, characters.vector.book1)
tkplot(lda.positif.igraph, vertex.color="lightblue", vertex.shape="sphere", 
       vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)


## Familiar sentiments ##
lda.famille <- getNetworkWithLDASentimentAnalysis(m.disp, 
                                                characters.vector.book1, 
                                                sentimentvec= famille,sankey=T)

lda.famille.igraph <- 
  getIgraph(lda.famille$x$links$source, lda.famille$x$links$target, 
            lda.famille$x$links$value, characters.vector.book1)
tkplot(lda.famille.igraph, vertex.color="lightblue", vertex.shape="sphere", 
       vertex.size=12, vertex.frame.color="gray", vertex.label.color="black", 
       vertex.label.cex=1.2, vertex.label.dist=0.5, edge.curved=0.2,
       canvas.width = 700, canvas.height = 700)




