this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

library(rJava)
library(mallet)
source("projet.R")


# Reading the data file

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')


lda.id <- 1:length(book)

# setting the delimiters
token.regexp <- "\\p{L}[\\p{L}\\p{P}]+\\p{L}"

# using my own stop list for English
stoplist <- "./Stop-words-french-utf8.txt"
#stoplist <- stopwords("french")
  
# including stopwords
mallet.instances <- mallet.import(as.character(lda.id), book, stoplist, token.regexp = token.regexp)
#mallet.instances <- mallet.import(as.character(lda.id), book, "data/empty.txt", token.regexp = token.regexp)

# Estimation of LDA parameters

# number of expected topics
k <- 40

# preparation
topic.model <- MalletLDA(num.topics=k)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

# train the model
topic.model$train(1000)
topic.model$maximize(10)

#Check the vocabulary

head(vocabulary, 20)
head(word.freqs)

# What are the top words?

index.frequent.words <- order(word.freqs$term.freq, decreasing=T)
freq <- word.freqs[index.frequent.words[1:50],]$term.freq
names(freq) <- word.freqs[index.frequent.words[1:50],]$words
barplot(freq, las=2)

# inspect p(z/d) and p(w/z)

# docs x topics matrix
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)

# topics x words matrix
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

topic.todisp <- 1:40
m.disp <- do.call(cbind,
                  sapply(topic.todisp,
                         function(x) format(mallet.top.words(topic.model, topic.words[x,]))))
colnames(m.disp) <- sapply(topic.todisp, function(x) c(paste("z",x), "p(w/z)"))
m.disp

# What is the topic distribution of one particular document?

# numdoc <- 2
# book[numdoc]
# best.topics <- order(doc.topics[numdoc,], decreasing=T)[1:25]
# rbind(best.topics,sprintf("%.3f",doc.topics[numdoc, best.topics]))
# 
# m.disp <- do.call(cbind,
#                   sapply(best.topics,
#                          function(x) format(mallet.top.words(topic.model, topic.words[x,]))))
# colnames(m.disp) <- sapply(best.topics, function(x) c(paste("z",x), "p(w/z)"))
# m.disp

# Computing the top documents for one particular topic.

pz <- colSums(doc.topics)/(length(book))
# p(d/z) = p(d/z)*p(d)/p(z) with uniform p(d)
num.pdz <- doc.topics * (1/length(book))
pd.z <- t(num.pdz) * (1/pz) # the last term is optional, doesn't change the ranking

# Infering the topic distribution for a new document.

# First, get the list of words from your new document.


ch <- "jeanne"
ch.processed <- unlist(strsplit(tolower(ch), "[^[:alpha:]]"))

index.w <- match(ch.processed,vocabulary)
index.w <- index.w[which(!is.na(index.w))]

message("Index of words within the vocabulary:")
print(index.w)

# Then compute p(z/d):
  
pz.newdoc <- function(z)
{
  return(2^(sum(log2(topic.words[z,index.w])))*pz[z])
}

pz.ch <- sapply(1:k, pz.newdoc)
pz.ch <- pz.ch / sum(pz.ch)

message("Top topics:")
print(order(pz.ch, decreasing=T)[1:10])
message("p(z/d):")
sprintf("%.3f",sort(pz.ch, decreasing=T)[1:10])

mallet.top.words(topic.model, topic.words[9,])

getNetworkWithAssocs <- function(tdm, characters.list, characters.vector){
  
  nodes <- data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  
  for(j in 1:(length(characters.list)-1)){
    lengthJ <- length(characters.list[[j]])
    character <- characters.list[[j]][1]
    assocs <- sapply(names(unlist(findAssocs(tdm, character, 0.15))), 
                     function(x) return(gsub(paste(character,".", sep=""), "", x)))
    if(lengthJ==1) resultJ <- 1
    else resultJ <- length(which(characters.list[[j]] %in% assocs))
    
    if(resultJ >= lengthJ/2){
      for(k in (j+1):length(characters.list)){
        lengthK <- length(characters.list[[k]])
        resultK <- length(which(characters.list[[k]] %in% assocs))
        if(resultK > lengthK/2){
          vec1 <- c(vec1,(j-1))
          vec2 <- c(vec2,(k-1))
          vec3 <- c(vec3,1)
        }
        
      }
    }
  }
  
  links <- data.frame(source=vec1, target=vec2, value=vec3)
  
  # forceNetwork(Links = links, Nodes = nodes,
  #              Source = "source", Target = "target",
  #              Value = "value", NodeID = "name",
  #              Group = "group", opacity = 0.8)
  
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "m", fontSize = 12, nodeWidth = 30)
}

getNetworkWithLDA <- function(m.disp, characters.list, characters.vector){
  nodes <- data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  
  i <- 1
  nb.topics <- ncol(m.disp)
  nb.words <- nrow(m.disp)
  while(i < nb.topics) {
    for(j in 1:(length(characters.list)-1)){
      lengthJ <- length(characters.list[[j]])
      resultJ <- length(which(characters.list[[j]] %in% m.disp[,i]))
      if(resultJ >= lengthJ/2){
        for(k in (j+1):length(characters.list)){
          lengthK <- length(characters.list[[k]])
          
          resultK <- length(which(characters.list[[k]] %in% m.disp[,i]))
          if(resultK >= lengthK/2){
            vec1 <- c(vec1,(j-1))
            vec2 <- c(vec2,(k-1))
            vec3 <- c(vec3,1)
          }

        }
      }
    }
    i <- i + 2
  }
  
  links <- data.frame(source=vec1, target=vec2, value=vec3)

  forceNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               Group = "group", opacity = 0.8)
  
  # sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
  #                 Target = "target", Value = "value", NodeID = "name",
  #                 units = "m", fontSize = 12, nodeWidth = 30)

  
}



