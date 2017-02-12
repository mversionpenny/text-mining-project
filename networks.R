library(rJava)
library(mallet)
library(XML)

getCharactersVector <- function(file_name){
  file <- readLines(file_name, encoding = "UTF-8")
  characters.vector <- rep("", length(file))
  for(i in 1:length(file)){
    characters.vector[i] <- file[i]
  }
  return(characters.vector)
}

getTopicsModelling <- function(book, stoplist, k){
  lda.id <- 1:length(book)
  # setting the delimiters
  token.regexp <- "\\p{L}[\\p{L}\\p{P}]+\\p{L}"
  # using my own stop list for English
  stoplist <- stoplist
  # including stopwords
  mallet.instances <- 
    mallet.import(as.character(lda.id), book1, stoplist, 
                  token.regexp = token.regexp)
  
  # Estimation of LDA parameters

  # preparation
  topic.model <- MalletLDA(num.topics=k)
  topic.model$loadDocuments(mallet.instances)

  # train the model
  topic.model$train(1500)
  topic.model$maximize(15)
  # topics x words matrix
  topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
  
  topic.todisp <- 1:k
  m.disp <- do.call(cbind,
              sapply(topic.todisp,
                function(x) format(mallet.top.words(topic.model, topic.words[x,]))))
  colnames(m.disp) <- sapply(topic.todisp, function(x) c(paste("z",x), "p(w/z)"))
  return(m.disp)
}

getWord2Vec <- function(text){
  return(0)
}

getNetworkWithAssocs <- function(tdm, characters.vector, cor, sankey=T){
  nodes <- 
    data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  
  for(j in 1:(length(characters.vector)-1)){
    character <- characters.vector[j]
    assocs <- sapply(names(unlist(findAssocs(tdm, character, cor))), 
                function(x) return(gsub(paste(character,".", sep=""), "", x)))
    
    for(k in (j+1):length(characters.vector)){
      if (characters.vector[k] %in% assocs){
        vec1 <- c(vec1,(j-1))
        vec2 <- c(vec2,(k-1))
        vec3 <- c(vec3,1)
      }
    }
  }

  links <- data.frame(source=vec1, target=vec2, value=vec3)

  if(sankey==T){
    return(sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "m", fontSize = 12, nodeWidth = 30))
  }
  else{
    return(forceNetwork(Links = links, Nodes = nodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8))
  }
  
  
}

getNetworkWithLDA <- function(m.disp, characters.vector, sankey=T){
  nodes <- 
    data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  
  i <- 1
  nb.topics <- ncol(m.disp)
  nb.words <- nrow(m.disp)
  while(i < nb.topics) {
    for(j in 1:(length(characters.vector)-1)){
      if(characters.vector[j] %in% m.disp[,i]){
        for(k in (j+1):length(characters.vector)){
          if(characters.vector[k] %in% m.disp[,i]){
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
  
  
  if(sankey==T){
    return(sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "m", fontSize = 12, nodeWidth = 30))
  }
  else{
    return(forceNetwork(Links = links, Nodes = nodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8))
  }
}

getNetworkWithWord2Vec <- function(word2vecFile, characters.vector, dist=20, sankey=T){
  nodes <- 
    data.frame(name = characters.vector, group=rep(1,length(characters.vector)))
  vec1 <- c()
  vec2 <- c()
  vec3 <- c()
  

  for(j in 1:(length(characters.vector)-1)){
    character <- characters.vector[j]
    nearest=distance(file_name = "vec.bin",search_word = character,num = dist)
    for(k in (j+1):length(characters.vector)){
      if(characters.vector[k] %in% nearest$word){
        vec1 <- c(vec1,(j-1))
        vec2 <- c(vec2,(k-1))
        vec3 <- c(vec3,1)
      }
    }
      
  }

  
  links <- data.frame(source=vec1, target=vec2, value=vec3)
  
  
  if(sankey==T){
    return(sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                         Target = "target", Value = "value", NodeID = "name",
                         units = "m", fontSize = 12, nodeWidth = 30))
  }
  else{
    return(forceNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        Group = "group", opacity = 0.8))
  }
}