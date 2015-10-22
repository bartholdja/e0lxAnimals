rm(list = ls())
setwd("/Users/jabarthold/Dropbox/Projects/014_sociality/e0lxAnimals/")
library(devtools)
library(MASS)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)
library(mgcv)

source_gist("6cc00cd36ae3b8d0532f") # linear model equation annotation
source_url("https://raw.githubusercontent.com/jonesor/compadreDB/master/Functions/makeLifeTable.R") # stage to age functino