### Comadre data
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
comadre <- subsetDB(comadre, idNew)
rm(tempDat, tempMetadata)


if(!"lt.rds" %in% list.files("data")) {
# get the life table from the matrices
  options(warn=1)
  for (i in 1:length(comadre$mat)) {
    if (i == 1) {
      ind <- factor(rep(NA, 111), levels = c("ok", "NA in matU", "convAge not computable",
                                             "last lx > 1000", "convAge too small"))
      lt <- list()  # initiate list to save life tables
      out <- data_frame(id = NA)  # initiate data frame for calculated measures
    }
    # there is still some with NA in the matU, exclude those
    if (is.na(sum(comadre$mat[[i]]$matU))) {
      ind[i] <- "NA in matU"
      next
    }
    temp <- makeLifeTable(matU = comadre$mat[[i]]$matU)
    temp <- temp[KeeplxInd(temp$lx), ] # keep lx up to 0.01 percent still alive
    rm(convAge)
    convAge <- qsdConverge(matU = comadre$mat[[i]]$matU, 
                          startLife = which(comadre$matrixClass[[i]]$MatrixClassOrganized == "active")[1],
                           conv = 0.05)
    
    if(!is.finite(convAge)) {
      ind[i] <- "convAge not computable"
      next
    }
    
    if(length(which(temp$lx >= 0.01)) >= 1000) {
      ind[i] <- "last lx > 1000"
      next
    }
    
    if(convAge < temp$lx[length(temp$lx)]) {
      ind[i] <- "convAge too small"
      next
    }
    k <- length(lt) + 1
    lt[[k]] <- temp
    out[k ,1] <- i
    ind[i] <- "ok"
    rm(k, temp)
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

  # add the Genus, Family, order, Class to out
  out$speciesEpithet <- comadre$metadata$SpeciesEpithetAccepted[unlist(out[ , 1])]
  out$genus <- comadre$metadata$GenusAccepted[unlist(out[ , 1])]
  out$family <- comadre$metadata$Family[unlist(out[ , 1])]
  out$order <- comadre$metadata$Order[unlist(out[ , 1])]
  out$class  <- comadre$metadata$Class[unlist(out[ , 1])]
  
  # calculate tau
  tau <- lapply(1:length(lt), function(x){
    CalcTau(lt[[x]]$lx, ageInt = comadre$metadata$AnnualPeriodicity[x])
  })
  out$tau <- unlist(tau)
  
  # calculate e0
  e0 <- lapply(1:length(lt), function(x){
    CalcE0(lt[[x]]$lx, ageInt = comadre$metadata$AnnualPeriodicity[x])
  })
  out$e0 <- unlist(e0)
  
  # calculate lbar
  out$lbar <- out$tau/out$e0
  
  # Replace the class "Reptilia " with a spaces with "Reptilia"
  out$class[which(out$class == "Reptilia ")] <- "Reptilia"
  
  # save the lifetables list as rds
  saveRDS(lt, "data/lt.rds")
  
  # save the out object
  saveRDS(out, "data/out.rds")
}


# add the human populations
out1 <- data_frame(pop = names(ltList), 
                   species = "sapiens", 
                   genus = "Homo", 
                   family = "Hominidae", 
                   order = "Primates", 
                   class = "Mammalia", 
                   tau = rep(NA, length(ltList)), e0 = rep(NA, length(ltList)), lbar = rep(NA, length(ltList)))

ageIntList = lapply(1:length(ltList), function(x) {
  c(diff(ltList[[x]]$age), 1)
})

tau1 <- lapply(1:length(ltList), function(x){
  CalcTau(ltList[[x]]$lx.f, ageInt = ageIntList[[x]])
})
out1$tau <- unlist(tau1)

# calculate e0
e01 <- lapply(1:length(ltList), function(x){
  CalcE0(ltList[[x]]$lx.f, ageInt = ageIntList[[x]])
})
out1$e0 <- unlist(e01)

# calculate lbar
out1$lbar <- out1$tau/out1$e0

source("scr/liberiaContinuous.R")
out1$lbar[out1$pop == "Liberia"] <- lbar
out1$e0[out1$pop == "Liberia"] <- e0
