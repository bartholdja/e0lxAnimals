load("data/COMADRE_v.X.X.X.RData")

if("lt.rds" %in% list.files("data")) {
  lt <- readRDS("data/lt.rds")
  out <- readRDS("data/out.rds")}

if("outsim.rds" %in% list.files("data")) {
  outsim <- readRDS("data/outsim.rds")
}