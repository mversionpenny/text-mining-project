#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Cut the bibliography at the end of all the books

list.of.packages <- c("rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# Books
bookpath = "les_rois_maudits/txt_woaccents"
txtbook1 = "[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt"
txtbook2 = "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt"
txtbook3 = "[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt"
txtbook4 = "[Rois Maudits-4] La Loi des males - Druon,Maurice.txt"
txtbook5 = "[Rois Maudits-5] La Louve de France - Druon,Maurice.txt"
txtbook6 = "[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt"
txtbook7 = "[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt"
newbookpath = "les_rois_maudits/txt_cut"
dir.create(newbookpath, showWarnings = FALSE) 

# Loop
for (i in 1:7){
  path = file.path(bookpath, get(paste("txtbook", i, sep="")))
  book <- readLines(path, encoding = "UTF-8" , skipNul = T)
  book <- book[1:(which(book=="REPERTOIRE" | book=="FIN")[1]-1)]
  newpath =  file.path(newbookpath, get(paste("txtbook", i, sep="")))
  writeLines(book, newpath)
}

