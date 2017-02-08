#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
# includes libraries we need
library(dplyr)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)

# +-------------------------------------------------------------------------+
# | *Function : prepare.text                                                |
# | *Description: read the text file and prepare the document (remove       |
# |  punctuation, change uppercase to lowercase, ...)                       |
# |                                                                         |
# | *Inputs: - txtfile: path to text file                                   |
# | *Outputs: - book: vector containing the lines of the text file          |
# +-------------------------------------------------------------------------+
prepare.text <- function(txtfile){
  book <- readLines(txtfile, encoding = "UTF-8")
  book <- book[!(is.na(book) | book=="")]
  
  # get rid of punctuation
  book <- sapply(book,function(x) gsub("[[:punct:]]"," ",x),USE.NAMES = FALSE)
  
  # remove space from characters' names
  book <- sapply(book,function(x) gsub("Philippe le Bel","philippeiv",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Philippe IV","philippeiv",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Philippe V","philippev",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Philippe VI","philippevi",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Philippe III","philippeiii",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Louis V","louisv",x),USE.NAMES = FALSE)
  book <- sapply(book,function(x) gsub("Louis Hutin","louisv",x),USE.NAMES = FALSE)
  
  # to lower case
  book <- sapply(book, tolower, USE.NAMES = F)
  
  return(book)
}

# +-------------------------------------------------------------------------+
# | *Function : prepare.text                                                |
# | *Description: read the text file and prepare the document (remove       |
# |  punctuation, change uppercase to lowercase, ...)                       |
# |                                                                         |
# | *Inputs: - book: text vector containing the document                    |
# | *Outputs: - book.corpus: corpus of the document                         |
# +-------------------------------------------------------------------------+
creat.corpus <- function(book){
  book.corpus <- VCorpus(VectorSource(book))

  # remove numbers
  book.corpus <- tm_map(book.corpus, removeNumbers)
  # remove stop-words ("le"...)
  book.corpus <- tm_map(book.corpus, removeWords,stopwords("french"))
  
  return(book.corpus)
}

