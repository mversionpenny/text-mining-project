#!/usr/bin/env python
# -*- coding: utf-8 -*- #

import sys
import os
from unidecode import unidecode

def delete_accents(iFile, oFile):
    with open(iFile, "rb") as f:
        text = unidecode(f.read().decode('utf-8'))
    with open(oFile, "w") as p:
        p.write(text)
    f.close()
    p.close()

if __name__ == "__main__":
    #print sys.argv[1:]
    #main(sys.argv[1], sys.argv[2])

    os.chdir(os.path.dirname(os.path.abspath(sys.argv[0])))

    delete_accents("Stop-words-french.txt", "Stop-words-french-utf8.txt")

    SOURCE = "les_rois_maudits/txt/"
    TARGET = "les_rois_maudits/txt_woaccents/"

    list_files = os.listdir(SOURCE)
    for file in list_files:
        delete_accents(SOURCE+file, TARGET+file)
