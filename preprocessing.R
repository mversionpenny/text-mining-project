#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
# includes libraries we need
library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)


####
# +-------------------------------------------------------------------------+
# | *Function : prepare.text                                                |
# | *Description: read the text file and prepare the document (remove       |
# |  punctuation, change uppercase to lowercase, ...)                       |
# |                                                                         |
# | *Inputs: - txtfile: path to text file                                   |
# |          - stopWords: character vectors containning the stopwords       |
# | *Outputs: - book: vector containing the lines of the text file          |
# +-------------------------------------------------------------------------+
prepare.text <- function(txtFile, stopWords){
  book <- readLines(txtFile, encoding = "UTF-8")
  book <- book[!(is.na(book) | book=="")]
  
  # to lower case
  book <- sapply(book, tolower, USE.NAMES = F)
  
  # remove stopword
  book <- removeWords(book, stopWords)
  
  # remove punctuation
  book <- removePunctuation(book)
  
  # remove numbers
  book <- removeNumbers(book)
  
  return(book)
}

simple.visu <- function(tdm){
  # fin frequent terms
  findFreqTerms(tdm,50)
  tdm.matrix <- as.matrix(tdm)
  sums <- rowSums(tdm.matrix)
  sorted <- sort(sums,decreasing = TRUE)
  barplot(sorted[1:50], type = 'h', las=2)
  
  # word cloud
  colors <- brewer.pal(8,"Set1")
  wordcloud(names(sorted[1:50]), sorted[1:50], colors=colors)
}
