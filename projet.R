#------------------- Margot Selosse -------------------
#--------------- Text-mining : projet -----------------
#------------------ 20 dec. 2016 ----------------------

#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", "wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# includes libraries we need
library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)

#### Creating corpus / cleaning data / tdm####
book <- readLines("./les_rois_maudits/txt/rois_maudits.txt", encoding = "UTF-8")
book <- book[!(is.na(book) | book=="")]

# gettinr rid of punctuation
 f1 <-  function(x) return(gsub("[[:punct:]]"," ",x))
 
 f2 <-  function(x) return(gsub("Philippe le Bel","philippeiv",x))
 f3 <-  function(x) return(gsub("Philippe IV","philippeiv",x))
 f4 <-  function(x) return(gsub("Philippe V","philippev",x))
 f5 <-  function(x) return(gsub("Philippe VI","philippevi",x))
 f6 <-  function(x) return(gsub("Philippe III","philippeiii",x))
 
 f7 <-  function(x) return(gsub("Louis V","louisv",x))
 f8 <-  function(x) return(gsub("Louis Hutin","louisv",x))

  
book <- sapply(book,f1,USE.NAMES = FALSE)
book <- sapply(book,f2,USE.NAMES = FALSE)
book <- sapply(book,f3,USE.NAMES = FALSE)
book <- sapply(book,f4,USE.NAMES = FALSE)
book <- sapply(book,f5,USE.NAMES = FALSE)
book <- sapply(book,f6,USE.NAMES = FALSE)
book <- sapply(book,f7,USE.NAMES = FALSE)
book <- sapply(book,f8,USE.NAMES = FALSE)

book.corpus <- VCorpus(VectorSource(book))
lapply(book.corpus[1:30], as.character)

#to lower case:
book.corpus.processed <- tm_map(book.corpus, content_transformer(tolower))
# get rid of punctuation
#book.corpus.processed <- tm_map(book.corpus, removePunctuation)
# remove numbers
book.corpus.processed <- tm_map(book.corpus.processed, removeNumbers)
# remove stop-words ("le"...)
book.corpus.processed <- tm_map(book.corpus.processed, removeWords,stopwords("french"))

# create tdm
tdm <- TermDocumentMatrix(book.corpus.processed, control=list(wordLengths=c(2,Inf)))

# fin frequent terms
findFreqTerms(tdm,50)
tdm.matrix <- as.matrix(tdm)
sums <- rowSums(tdm.matrix)
sorted <- sort(sums,decreasing = TRUE)
barplot(sorted[1:50], type = 'h', las=2)

#### Wordcloud ####
colors <- brewer.pal(4,"Paired")
wordcloud(names(sorted[1:20]), sorted[1:20], colors=colors)


getCharactersList <- function(file_name){
  file <- readLines(file_name, encoding = "UTF-8")
  characters.list <- vector("list", length(file))
  for(i in 1:length(file)){
    char = tolower(file[i]) # unlist(strsplit("Charles d'Artois", split="de "))
    char = unlist(strsplit(char, split=" de "))
    char = unlist(strsplit(char, split=" d'"))
    char = unlist(strsplit(char, split=" "))
    characters.list[[i]] <- char
  }
  return(characters.list)
}

getCharactersVector <- function(file_name){
  file <- readLines(file_name, encoding = "UTF-8")
  characters.vector <- rep("", length(file))
  for(i in 1:length(file)){
    char = tolower(file[i]) # unlist(strsplit("Charles d'Artois", split="de "))
    characters.vector[i] <- char
  }
  return(characters.vector)
}



# install.packages("igraph")
# install.packages("networkD3")

library(igraph)
library(networkD3)
# Load data
data(MisLinks)
data(MisNodes)

# Plot
# forceNetwork(Links = MisLinks, Nodes = MisNodes,
#              Source = "source", Target = "target",
#              Value = "value", NodeID = "name",
#              Group = "group", opacity = 0.8)




#1.
cosine <- function(vec1,vec2){
  #we compute the scalar product of the two vectors
  scalar <- sum(vec1 * vec2)
  # then the product of the norms
  norm_product <- norm(vec1, type="2") * norm(vec2, type="2")
  if(norm_product == 0){
    return(0)
  }
  else{
    return(scalar/norm_product)
  }
}

#2.
m <- as.matrix(tdm)
# build a vector from the m matrix and the words q (given by user)
query2vector <- function(m,q){
  #initialisation
  result <- integer(dim(m)[1])
  # I think there is a vectorized way to do that : 
  for (qtext in q) {
    index <- which(rownames(m) == qtext)
    if(length(index)>0){
      result[index] = 1
    }
  }
  return(result)
}

#3.
# returns a sorted list of documents regarding their cosinus similarity ith the qury
# ex : test <- run_query(m,c("whale","captain"))
run_query <- function(m, query){
  query_vector <- query2vector(m, query)
  cosinus <- numeric(dim(m)[2])
  for (i in seq_along(1:dim(m)[2])) {
    cosinus[i] <- cosine(query_vector,m[,i])
  }
  cosinus <- sort(cosinus, decreasing = TRUE, index.return =TRUE)$ix
  return(cosinus)
}


#4.
words <- c("jeanne","philippe", "bel") # can be whatever we want!
test <- run_query(m,words)
book[test[1:10]]


fakeNetwork <- function(){
  nodes <- data.frame(name = c("Bob", "Alice","Philippe", "Jean", "Sophie", "Vincent"), group=rep(1,6))
  # vec1 <- c("Bob", "Alice", "Alice", "Philipe", "Jean", "Jean", "Sophie", "Vincent", "Vincent" )
  # vec2 <- c("Alice", "Jean", "Vincent", "Alice", "Bob", "Sophie", "Vincent", "Bob", "Jean")
  vec1 <- c(1, 2, 2, 3, 4, 4, 5, 0, 0)
  vec2 <- c(2, 4, 0, 2, 1, 5, 0, 1, 4)
  vec3 <- c(10,21,1,2,3,37,5,67,5)
  links <- data.frame(source=vec1, target=vec2, value=vec3)
  forceNetwork(Links = links, Nodes = nodes,
               Source = "source", Target = "target",
               Value = "value", NodeID = "name",
               Group = "group", opacity = 0.8)
}

findAssocs(tdm, "philippeiv", 0.15)

firstNetwork <- function(tdm, characters.list, characters.vector){
  nodes <- data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  for(i in 1:length(characters.list)){
    character <- characters.list[[i]][1]
    assocs <- sapply(names(unlist(findAssocs(tdm, character, 0.2))), 
                     function(x) return(gsub(paste(character,".", sep=""), "", x)))
    for(k in i:length(characters.list)){
      if(characters.list[[k]][1] %in% assocs){
        vec1 <- c(vec1,(i-1))
        vec2 <- c(vec2,(k-1))
        vec3 <- c(vec3,1)
      }
    }
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

