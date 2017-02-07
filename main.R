#----------------------- Margot Selosse, Hoai Thu Nguyen -----------------------
#----------------------------- Text Mining project ----------------------------- 
#---------------------------------- 2016/2017 ----------------------------------
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "dplyr", "tm", "NLP", "wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

#### Books ####
bookpath = "les_rois_maudits/txt_woaccents/"
txtbook1 = bookpath + "[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt"
txtbook2 = bookpath + "[Rois Maudits-2] La Reine etranglee - Druon,Maurice.txt"
txtbook3 = bookpath + "[Rois Maudits-3] Les Poisons de la couro - Druon,Maurice.txt"
txtbook4 = bookpath + "[Rois Maudits-4] La Loi des males - Druon,Maurice.txt"
txtbook5 = bookpath + "[Rois Maudits-5] La Louve de France - Druon,Maurice.txt"
txtbook6 = bookpath + "[Rois Maudits-6] Le Lis et le Lion - Druon,Maurice.txt"
txtbook7 = bookpath + "[Rois Maudits-7] Quand un roi perd la Fr - Druon,Maurice.txt"

#### Analyze Book 1 ####
