#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
# Clean the list of character names
# ONLY for the book 1 of Les Rois Maudits

# install missing packages
list.of.packages <- c("stringr", "rstudioapi", "tm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# import library
library(stringr)
library(tm)

#### Read the txt files ####
book <- readLines("les_rois_maudits/txt_cut/[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt",
                  encoding = 'UTF-8')
characters <- readLines("les_rois_maudits/characters/characters1.txt")
characters.mat <- str_split(characters, " ", simplify = TRUE)

#######################################################################
####                  Automatic cleaning                           ####
#######################################################################
rm.indx.appear1 <- c()
rm.indx.dupname <- c()
for (i in 1:(length(characters)-1)){
  # if the name appear only 1 times => remove
  appear <- sum(str_count(book, characters[i])) 
  if (appear < 2){
    #print(paste(characters[i], "appear 1"))
    rm.indx.appear1 <- c(rm.indx.appear1, i)
  }
  else{
    # if the name appear has only firstname and the firstname is duplicated => remove
    if (characters.mat[i,2]=="" & characters.mat[i+1,1] == characters.mat[i,1]){
      #print(paste(characters[i], "duplicate firstname"))
      rm.indx.dupname <- c(rm.indx.dupname, i)
    }
  }
}
# check the last name
appear <- sum(str_count(book, characters[length(characters)])) 
if (appear < 2){
  #print(paste(characters[length(characters)], "appear1"))
  rm.indx.appear1 <- c(rm.indx.appear1, length(characters))
}

#######################################################################
####                    Manual cleaning                            ####
#######################################################################
## Replace Alain with Alain de Pareilles 
book <- unlist(str_replace_all(book, "Alain de Pareilles|Alain", "Alain de Pareilles"))

## Remove Amaury (appear only once as a character and once as a place)
rm.indx.extra <- c(3)

## Relace Baglioni and Guccio Baglioni with Guccio
book <- unlist(str_replace_all(book, "Guccio Baglioni|Guccio|Baglioni", "Guccio Baglioni"))
rm.indx.extra <- c(rm.indx.extra, 6)

## Replace Beatrice with Beatrice d'Hirson
book <- unlist(str_replace_all(book, "Beatrice d'Hirson|Beatrice", "Beatrice d'Hirson"))

## Replace Blanche with Blanche de Bourgogne
book <- unlist(str_replace_all(book, "Blanche de Bourgogne|Blanche", "Blanche de Bourgogne"))
characters <- unlist(str_replace_all(characters, "Blanche", "Blanche de Bourgogne"))

## Replace Boniface with Boniface VVV
book <- unlist(str_replace_all(book, "Boniface VIII|Boniface", "Boniface VIII"))

## Replace 'de Valois', 'Valois' with Charles de Valois, remove Monseigneur de Valois, Valois, de Valois
book <- unlist(str_replace_all(book, "Charles de Valois|de Valois|Valois", "Charles de Valois"))
rm.indx.extra <- c(rm.indx.extra, 74, 110)

## Replace Eliabel, dame de Cressay, Madame de Cressay with Eliabel de Cressay
book <- unlist(str_replace_all(book, "Eliabel de Cressay|Eliabel|[Mm]adame de Cressay|dame de Cressay", "Eliabel de Cressay"))
characters <- unlist(str_replace_all(characters, "Eliabel", "Eliabel de Cressay"))

## Replace Enguerrand with Enguerrand de Marigny
book <- unlist(str_replace_all(book, "Enguerrand de Marigny|Enguerrand", "Enguerrand de Marigny"))

## Replace the character Frere Renaud with Renaud 
characters <- unlist(str_replace_all(characters, "Frere Renaud", "Renaud"))

## Replace Gautier d'Aunay le pere with GautierdAunaylePere to seperate this
# characer from his son
book <- unlist(str_replace_all(book, "Gautier d'Aunay le pere", "GautierdAunaylePere"))

## Gautier -> Gautier d'Aunay
book <- unlist(str_replace_all(book, "Gautier ", "Gautier d'Aunay "))

## Replace de Nogaret, Nogaret with Guillaume de Nogaret
book <- unlist(str_replace_all(book, "Guillaume de Nogaret|de Nogaret|Nogaret", "Guillaume de Nogaret"))

## Replace de Bouille with Hugues de Bouville
book <- unlist(str_replace_all(book, "Hugues de Bouville|de Bouville|Bouville", "Hugues de Bouville"))

## Replace Jacques, de Molay, Molay with Jacques de Molay
book <- unlist(str_replace_all(book, "Jacques de Molay|de Molay|Molay|Jacques", "Jacques de Molay"))

## Jeanne -> Jeanne de Bourgogne
book <- unlist(str_replace_all(book, "Jeanne de Bourgogne|Jeanne", "Jeanne de Bourgogne"))
characters <- unlist(str_replace_all(characters, "Jeanne", "Jeanne de Bourgogne"))

## Hutin, Louis Hutin, Louis de Navarre -> Louis V
# roi de Navarre -> roi Louis V (for word2vec)
# Monseigneur de Navarre -> Monseigneur Louis V (for word2vec)
book <- unlist(str_replace_all(book, "Louis Hutin|Hutin|Louis de Navarre", "Louis V"))
book <- unlist(str_replace_all(book, "roi de Navarre", "roi Louis V"))
book <- unlist(str_replace_all(book, "Monseigneur de Navarre", "Monseigneur Louis V"))
characters <- c(characters, "Louis V")
rm.indx.extra <- c(rm.indx.extra, 53, 56)

#delete character 'de Navarre'
rm.indx.extra <- c(rm.indx.extra, 111)

## d'Evreux -> Louis d'Evreux
book <- unlist(str_replace_all(book, "Louis d'Evreux|d'Evreux", "Louis d'Evreux"))

## Marguerite, Marguerite de Navarre -> Marguerite de Bourgogne
# reine de Navarre -> reine Marguerite de Bourgogne
book <- unlist(str_replace_all(book, "Marguerite de Bourgogne|Marguerite", "Marguerite de Bourgogne"))
book <- unlist(str_replace_all(book, "reine de Navarre", "reine Marguerite de Bourgogne"))
characters <- unlist(str_replace_all(characters, "Marguerite", "Marguerite de Bourgogne"))

## Marie -> Marie de Cressay
book <- unlist(str_replace_all(book, "Marie de Cressay|Marie", "Marie de Cressay"))

## remove characters containning "Monseigneur", "Messire" (if not already remove), remove Magrigny
rm.indx.extra <- c(rm.indx.extra, 61, 64, 65, 67:69, 71:74)

## Character Pape Clement -> Clement
characters <- unlist(str_replace_all(characters, "Pape Clement", "Clement"))

## Philippe le Bel, Philippe quatrieme -> Philippe IV
book <- unlist(str_replace_all(book, "Philippe le Bel|Philippe quatrieme", "Philippe IV"))
characters <- unlist(str_replace_all(characters, "Philippe le Bel", "Philippe IV"))


## compte(Monsigneur) de Poitiers -> comte(Monsigneur) Philippe de Poitiers
book <- unlist(str_replace_all(book, "comte de Poitiers", "compte Philippe de Poitiers"))
book <- unlist(str_replace_all(book, "Monseigneur de Poitiers", "Monseigneur Philippe de Poitiers"))

## Madame(comtesse) de Poitiers -> Madame(comtesse) Jeanne de Bourgogne
book <- unlist(str_replace_all(book, "comtesse de Poitiers", "comptesse Jeanne de Bourgogne"))
book <- unlist(str_replace_all(book, "Madame de Poitiers", "Madame Jeanne de Bourgogne"))

## Mahaut, Mahaut d'Artois, comtesse d'Artois -> Mahaut de Bourgogne
book <- unlist(str_replace_all(book, "Mahaut de Bourgogne|Mahaut d'Artois|Mahaut|comtesse d'Artois",
                               "Mahaut de Bourgogne"))
characters <- c(characters, "Mahaut de Bourgogne")

## Robert, d(D)'Artois -> Robert d'Artois (except l'hotel d'Artois, comtesse de Bourgogne et d'Artois, Robert de Bethune)
book <- unlist(str_replace_all(book, "l'hotel d'Artois", "l'hotel Artois"))
book <- unlist(str_replace_all(book, "Robert de Bethune", "deBethune"))
book <- unlist(str_replace_all(book, "comtesse de Bourgogne et d'Artois", "comtesse de Bourgogne et Artois"))
book <- unlist(str_replace_all(book, "Robert d'Artois|Robert|[Dd]'Artois", "Robert d'Artois"))

## remove Martin, Marigny, Santa Maria
rm.indx.extra <- c(rm.indx.extra, 62, 63, 103)

## Spinello, Tolomei -> Spinello Tolomei
book <- unlist(str_replace_all(book, "Spinello Tolomei|Spinello|Tolomei", "Spinello Tolomei"))

## Thierry -> Thierry d'Hirson
book <- unlist(str_replace_all(book, "Thierry", "Thierry d'Hirson"))

## reine d'Angleterre -> reine Isabelle
book <- unlist(str_replace_all(book, "reine d'Angleterre", "reine Isabelle"))

## 'le roi' without name after (Edouard, Louis or Philippe) -> Philipple IV
leroi <- c(which(str_detect(book, "le roi [^[A-Z]]")==TRUE), which(str_detect(book, "le roi[:punct:]")==TRUE))
book[leroi] <- unlist(str_replace_all(book[leroi], "le roi", "Philippe IV"))

## Seperate Edouard
edouard <- which(str_detect(book, "Edouard")==TRUE)
son <- c(3969, 3977)
book[son] <- unlist(str_replace_all(book[son], "Edouard", "Edouard III"))
father <- edouard [! edouard %in% son]
book[father] <- unlist(str_replace_all(book[father], "Edouard II|Edouard", "Edouard II"))
characters <- c(characters, 'Edouard III')

## Seperate Pierre
latille <- c(5382, 5554)
cressay <- which(str_detect(book, "Pierre et Jean")==TRUE)
cressay <- c(cressay, 5132)
book[latille] <- unlist(str_replace_all(book[latille], "Pierre", "Pierre de Latille"))
book[cressay] <- unlist(str_replace_all(book[cressay], "Pierre", "Pierre de Cressay"))
# Pierre de Cressay was deleted because it appeared only once, but now need to be readded
characters <- c(characters, "Pierre de Cressay")

#######################################################################
####                  Final save                                   ####
#######################################################################

## Create new character list 
characters <- characters[-c(rm.indx.appear1, rm.indx.dupname, rm.indx.extra)]
clean.char <- unlist(str_replace_all(characters, "[:blank:]", ""))
clean.char <- removePunctuation(clean.char)
clean.char <- sapply(clean.char, tolower, USE.NAMES = F)

writeLines(characters, "les_rois_maudits/characters/characters1_clean.txt")
writeLines(clean.char, "les_rois_maudits/characters/characters1_clean_lowercase.txt")

## Replace name in book
for (i in 1:length(characters)){
  book <- unlist(str_replace_all(book, characters[i], clean.char[i]))
}

dir.create("les_rois_maudits/final_txt/", showWarnings = FALSE) 
writeLines(book, "les_rois_maudits/final_txt/[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt")


#unlist(str_extract_all(book, "le roi [^A-Z]"))

