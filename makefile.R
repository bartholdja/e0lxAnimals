# Make file for analysis ----------------------------------------------------
# dependencies:
rm(list = ls())
# run the source code
if ("jabarthold" %in% list.files("/Users/")) {
  source("/Users/jabarthold/Dropbox/Projects/020_paceShape/scr/animals/01_init.R")
} else {
  source("/Users/Viktualia/Dropbox/Projects/020_paceShape/scr/animals/01_init.R")
}

source("scr/animals/02_input.R")
source("scr/animals/03_fcts.R")
source("scr/animals/04_process.R")
source("scr/animals/05_plot.R")
