# load the comadre data
load("data/COMADRE_v.X.X.X.RData")

# load the generated life tables
if("lt.rds" %in% list.files("data")) {
  lt <- readRDS("data/lt.rds")
  out <- readRDS("data/out.rds")}

# load the human life tables from the primate paper
load("data/lifeTabs3.Rdata")  # object is called ltList
