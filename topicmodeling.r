
library(rJava)
library(mallet)


# Reading the data file

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
setwd("D:/master-DM/cours/text-mining/projet/")
hp <- readLines(".//les_rois_maudits//txt//rois_maudits.txt", encoding="UTF-8")
hp <- hp[!(is.na(hp) | hp=="")]
# f <-  function(x) return(gsub("\u2019"," ",x))
# hp <- sapply(hp,f,USE.NAMES = FALSE)
lda.id <- 1:length(hp)

# setting the delimiters
token.regexp <- "\\p{L}[\\p{L}\\p{P}]+\\p{L}"

# using my own stop list for English
stoplist <- "./Stop-words-french.txt"
#stoplist <- stopwords("french")
  
# including stopwords
mallet.instances <- mallet.import(as.character(lda.id), hp, stoplist, token.regexp = token.regexp)
#mallet.instances <- mallet.import(as.character(lda.id), hp, "data/empty.txt", token.regexp = token.regexp)

# Estimation of LDA parameters

# number of expected topics
k <- 20

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

topic.todisp <- 1:20
m.disp <- do.call(cbind,
                  sapply(topic.todisp,
                         function(x) format(mallet.top.words(topic.model, topic.words[x,]))))
colnames(m.disp) <- sapply(topic.todisp, function(x) c(paste("z",x), "p(w/z)"))
m.disp

# What is the topic distribution of one particular document?

numdoc <- 2
hp[numdoc]
best.topics <- order(doc.topics[numdoc,], decreasing=T)[1:10]
rbind(best.topics,sprintf("%.3f",doc.topics[numdoc, best.topics]))

m.disp <- do.call(cbind,
                  sapply(best.topics,
                         function(x) format(mallet.top.words(topic.model, topic.words[x,]))))
colnames(m.disp) <- sapply(best.topics, function(x) c(paste("z",x), "p(w/z)"))
m.disp

# Computing the top documents for one particular topic.

pz <- colSums(doc.topics)/(length(hp))
# p(d/z) = p(d/z)*p(d)/p(z) with uniform p(d)
num.pdz <- doc.topics * (1/length(hp))
pd.z <- t(num.pdz) * (1/pz) # the last term is optional, doesn't change the ranking

# Infering the topic distribution for a new document.

# First, get the list of words from your new document.

# ch <- "Harry lives number four private drive under the stairs."
# ch <- "He likes playing quidditch and chasing the golden snitch."
# ch <- "Harry lives number four private drive under the stairs. He likes playing quidditch and chasing the golden snitch."
ch <- "Philippe le Bel"
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

