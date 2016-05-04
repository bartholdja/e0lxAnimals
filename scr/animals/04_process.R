### Comadre data
# This scripts generates lifetables from Comadre matrices, calculates pace and shapes measures, and 
# stores them in an output object


# subset comadre to only the mean matrix per study, to only those matrices without any 
# issues with the survival probabilities, those that have more than 3 matrix dimensions
# and that come from unmanipulated observations
tempMetadata <- subset(comadre$metadata, MatrixComposite == "Mean" & SurvivalIssue < 1.01 &
                         MatrixSplit == "Divided" & MatrixDimension > 3 & MatrixTreatment == "Unmanipulated")
id <- as.numeric(rownames(tempMetadata))
comadre <- subsetDB(comadre, id)

# replace a few NA's in SpeciesAccepted with information provided by Owen
comadre$metadata$SpeciesAccepted[which(is.na(comadre$metadata$SpeciesAccepted))] <-
  comadre$metadata$SpeciesAuthor[which(is.na(comadre$metadata$SpeciesAccepted))]  
  
# get the life table from the matrices
options(warn=1)
for (i in 1:length(comadre$mat)) {
  if (i == 1) {
    ind <- factor(rep(NA, 111), levels = c("ok", "NA in matU or matF", "convergence not reached",
                                           "last lx > 1000", "convAge too small"))
    lt <- list()  # initiate list to save life tables
    out <- data_frame(id = NA)  # initiate data frame for calculated measures
  }
  # there is still some with NA in the matU, exclude those
  if (is.na(sum(comadre$mat[[i]]$matU))) {
    ind[i] <- "NA in matU or matF"
    next
  }
  temp <- makeLifeTable(matU = comadre$mat[[i]]$matU,
                        matF = comadre$mat[[i]]$matF,
                        startLife = which(comadre$matrixClass[[i]]$MatrixClassOrganized == "active")[1])
  temp <- temp[KeeplxInd(temp$lx, cutOff = 0.01), ] # keep lx up to 0.01 percent still alive
  
  convAge <- qsdConverge(matU = comadre$mat[[i]]$matU, 
                         startLife = which(comadre$matrixClass[[i]]$MatrixClassOrganized == "active")[1],
                         conv = 0.05)
  
  ## I put a "return NA" into the qsdConverge function to indicate when convAge is not computable
  if(is.na(convAge)) {
    ind[i] <- "convergence not reached"
    next
  }
  
  # After 1000 years still more than 1% of a synthetic cohort alive
  if(length(which(temp$lx >= 0.01)) >= 1000) {
    ind[i] <- "last lx > 1000"
    next
  }
  
  # Exclude those observations where convergence age is reached before 99% of a synthetic cohort are dead
  if(convAge < temp$lx[length(temp$lx)]) {
    ind[i] <- "convAge too small"
    next
  }
  k <- length(lt) + 1
  lt[[k]] <- temp
  out[k ,1] <- i
  ind[i] <- "ok"
  rm(k, temp, convAge)
}  
# make species name the list name of each lt entry 
names(lt) <-comadre$metadata$SpeciesAccepted[unlist(out[ , 1])]

# subset comadre to only the "ok" matrices
comadre <- subsetDB(comadre, which(ind == "ok"))


# replace the annual periodicity of some observations with the right value provided by Owen 
comadre$metadata$AnnualPeriodicity[which(comadre$metadata$AnnualPeriodicity == "0")] <- "0.125"
comadre$metadata$AnnualPeriodicity[which(comadre$metadata$AnnualPeriodicity == "error")] <- "1.000000"
comadre$metadata$AnnualPeriodicity[which(comadre$metadata$AnnualPeriodicity == "356.000000")] <- "365.000000"

# replace the NA's in annual periodicity with 1 (this needs to be checked)
comadre$metadata$AnnualPeriodicity[is.na(comadre$metadata$AnnualPeriodicity)] <- "1.000000"

# turn the character into numeric
comadre$metadata$AnnualPeriodicity <- as.numeric(as.character(comadre$metadata$AnnualPeriodicity))

# calculate tau from birth
tau0 <- lapply(1:length(lt), function(x){
  CalcTau(lt[[x]]$lx, ageInt = comadre$metadata$AnnualPeriodicity[x])
})
out$tau0 <- unlist(tau0)

# calculate e0
e0 <- lapply(1:length(lt), function(x){
  CalcE0(lt[[x]]$lx, ageInt = comadre$metadata$AnnualPeriodicity[x])
})
out$e0 <- unlist(e0)

# calculate lbar from birth
out$lbar0 <- out$tau0/out$e0

# shape from birth
out$shape0 <- 1 - 2*(1 - out$lbar0)

# stage at first reproduction
xMat <- lapply(1:length(comadre$mat), function(x){
  lifeTimeRepEvents(matU = comadre$mat[[x]]$matU, 
                    matF = comadre$mat[[x]]$matF, 
                    startLife = which(comadre$matrixClass[[x]]$MatrixClassOrganized == "active")[1])$La
}) 
out$xMat <- unlist(xMat)

# Life expectancy from stage of first reproduction
eMat <- lapply(1:length(lt), function(x){
  CalcE0(lx = lt[[x]]$lx[which(lt[[x]]$x >= out$xMat[x])],
         ageInt = comadre$metadata$AnnualPeriodicity[x])
})
out$eMat <- unlist(eMat)

# tau from stage of first reproduction
tauMat <- lapply(1:length(lt), function(x){
  CalcTau(lx = lt[[x]]$lx[which(lt[[x]]$x >= out$xMat[x])],
          ageInt = comadre$metadata$AnnualPeriodicity[x])
})
out$tauMat <- unlist(tauMat)

# lbar from stage of first reproduction
out$lbarMat <- out$tauMat/out$eMat

# shape from stage of first reproduction
out$shapeMat <- 1 - 2*(1 - out$lbarMat)

out <- cbind(comadre$metadata, out)

# Replace the class "Reptilia " with a spaces with "Reptilia"
out$Class[which(out$Class == "Reptilia ")] <- "Reptilia"

# Add a data source variable
out$dataSource = "comadre"

# Add a variable to indicate
# randomly pick one matrix per species (where there are multiples) from those of equal quality specification
idNow <- 1:nrow(comadre$metadata)
tempDat <- data_frame(dur = comadre$metadata$StudyDuration,
                      dim =  comadre$metadata$MatrixDimension,
                      spec = comadre$metadata$SpeciesAccepted, 
                      id = idNow)
idNew <- NULL
for (i in 1:length(unique(tempDat$spec))) {
  idNew[i] <- filter(tempDat, spec == unique(tempDat$spec)[[i]]) %>% PickMat(.)
  
}
out$singleObs <- rep(0, nrow(out))
out$singleObs[idNew] <- 1

# add the human populations from Colchero et al paper
out1 <- data.frame(matrix(ncol = ncol(out), nrow = 6))
names(out1) <- names(out)

out1$SpeciesAccepted <- "Homo_sapiens"
out1$GenusAccepted <- "Homo"
out1$Family <- "Hominidae"
out1$Order <- "Primates"
out1$Class <- "Mammalia"
out1$CommonName <- c("Historic Sweden", "Liberia",
                     "Hadza", "Ache", "Modern Sweden", "Modern Japan")


ageIntList = lapply(1:length(ltList), function(x) {
  c(diff(ltList[[x]]$age), 1)
})

tau0 <- lapply(1:length(ltList), function(x){
  CalcTau(ltList[[x]]$lx.f, ageInt = ageIntList[[x]])
})
out1$tau0 <- unlist(tau0)

# calculate e0
e0 <- lapply(1:length(ltList), function(x){
  CalcE0(ltList[[x]]$lx.f, ageInt = ageIntList[[x]])
})
out1$e0 <- unlist(e0)

# calculate lbar from birth
out1$lbar0 <- out1$tau0/out1$e0

# shape from birth
out1$shape0 <- 1 - 2*(1 - out1$lbar0)

# age at maturity
out1$xMat <- 12

# tau from age at maturity
tauMat <- lapply(1:length(ltList), function(x){
  CalcTau(lx = ltList[[x]]$lx.f[max(which(ltList[[x]]$age <= out1$xMat[x])):length(ltList[[x]]$lx.f)],
          ageInt = ageIntList[[x]][max(which(ltList[[x]]$age <= out1$xMat[x])):length(ltList[[x]]$lx.f)])
})
out1$tauMat <- unlist(tauMat)

# calculate e at maturity
eMat <- lapply(1:length(ltList), function(x){
  CalcE0(lx = ltList[[x]]$lx.f[max(which(ltList[[x]]$age <= out1$xMat[x])):length(ltList[[x]]$lx.f)],
         ageInt = ageIntList[[x]][max(which(ltList[[x]]$age <= out1$xMat[x])):length(ltList[[x]]$lx.f)])
})
out1$eMat <- unlist(eMat)

# calculate lbar from birth
out1$lbarMat <- out1$tauMat/out1$eMat

# shape from birth 
out1$shapeMat <- 1 - 2*(1 - out1$lbarMat)


# add the values for liberia calcualted from continous Siler model
source("scr/animals/liberiaContinuous.R")
out1$lbar0[out1$CommonName== "Liberia"] <- lbar0
out1$e0[out1$CommonName == "Liberia"] <- e0
out1$tau0[out1$CommonName == "Liberia"] <- tau0
out1$shape0[out1$CommonName == "Liberia"] <- shape0
out1$lbarMat[out1$CommonName== "Liberia"] <- lbarMat
out1$eMat[out1$CommonName == "Liberia"] <- eMat
out1$tauMat[out1$CommonName == "Liberia"] <- tauMat
out1$shapeMat[out1$CommonName == "Liberia"] <- shapeMat
out1$dataSource <- "Colchero et al"
out1$singleObs <- c(rep(0, 5), 1)

out <- rbind(out, out1)

# save the lifetables list as rds
saveRDS(lt, "data/ltAll.rds")

# save the out object
saveRDS(out, "data/outAll.rds")
