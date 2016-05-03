rm(list = ls())

if ("jabarthold" %in% list.files("/Users/")) {
setwd("/Users/jabarthold/Dropbox/Projects/014_sociality/e0lxAnimals/")
}

if ("Viktualia" %in% list.files("/Users/")) {
    setwd("/Users/Viktualia/Dropbox/Projects/014_sociality/e0lxAnimals/")
}

library(devtools)
library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(grid)
library(popbio)
library(ggrepel)

# source some functions and other bits and bobs
source_url("https://raw.githubusercontent.com/jonesor/compadreDB/master/Functions/subsetDB.R")
# I changed the respective functions. They are in 03_fcts.R
#source_url("https://raw.githubusercontent.com/jonesor/compadreDB/master/Functions/QSDConverge.R")
#source_url("https://raw.githubusercontent.com/jonesor/compadreDB/master/Functions/makeLifeTable.R") # stage to age function
