### Comadre data
if(!"lt.rds" %in% list.files("data")) {
# get the life table from the matrices
  options(warn=1)
  for (i in 1:length(comadre$mat)) {
    if (i == 1) {
      lt <- list()  # initiate list to save life tables
      out <- data_frame(id = NA)  # initiate data frame for calculated measures
    }
    # exclude a few missing data and erraneous cases
    if(sum(matU = comadre$mat[[i]][[which(names(comadre$mat[[i]]) == "matU")]], na.rm = T) == 0 | 
         is.na(sum(matU = comadre$mat[[i]][[which(names(comadre$mat[[i]]) == "matU")]], na.rm = F)) |
         sum(comadre$mat[[i]]$matU) > 65) {  
      next
    }
    
    temp <- makeLifeTable(matU = comadre$mat[[i]][[which(names(comadre$mat[[i]]) == "matU")]],
                          matF = comadre$mat[[i]][[which(names(comadre$mat[[i]]) == "matF")]])
    temp <- temp[KeeplxInd(temp$lx), ] # keep lx up to 0.01 percent still alive

    if(length(which(temp$lx >= 0.01)) < 1000) {  # exclude some other erraneous cases
      k <- length(lt) + 1
      lt[[k]] <- temp
      out[k ,1] <- i
      rm(k)
    } 
    rm(temp)
  }  
  
  # make species name the list name of each lt entry 
  names(lt) <-comadre$metadata$SpeciesAccepted[unlist(out[ , 1])]
  
  # save the lifetables list as rds
  saveRDS(lt, "data/lt.rds")
  
  # add the Genus, Family, order, Class to out
  out$speciesEpithet <- comadre$metadata$SpeciesEpithetAccepted[unlist(out[ , 1])]
  out$genus <- comadre$metadata$GenusAccepted[unlist(out[ , 1])]
  out$family <- comadre$metadata$Family[unlist(out[ , 1])]
  out$order <- comadre$metadata$Order[unlist(out[ , 1])]
  out$class  <- comadre$metadata$Class[unlist(out[ , 1])]
  
  # calculate tau
  tau <- lapply(1:length(lt), function(x){
    CalcTau(lt[[x]]$lx)
  })
  out$tau <- unlist(tau)
  
  # calculate e0
  e0 <- lapply(1:length(lt), function(x){
    CalcE0(lt[[x]]$lx)
  })
  out$e0 <- unlist(e0)
  
  # calculate lbar
  out$lbar <- out$tau/out$e0
  
  # data frame with non-ridiculous tau values
  out <- out[(!out$tau %in% tail(sort(out$tau), 5)), ]
  
  # save the out object
  saveRDS(out, "data/out.rds")
}

### Simulation
if(!"outsim.rds" %in% list.files("data")) {
  types <- c("all", "infant", "senescent", "constant")
  outsim <- list()
  ageInt <- 0.1
  age <- seq(0, 1000, ageInt)
  nsim <- 1000
  
  for (i in 1:4) {
    print(i)  
    silPars <- t(replicate(nsim, DrawPars(type = types[i])))
    colnames(silPars) <- c("a0", "a1", "c", "b0", "b1")  
    
    # initiate list for interim-storing the simulated life tables
    ltsim <- list()
    # initiate data frame for ccalculcated measues
    outsim[[i]] <- data_frame(id = 1:nsim)  # initiate data frame for calculated measures
    
    for (j in 1:nrow(silPars)) {
      lx <- CalcSurv(silPars[j, ], age)
      mux <- CalcMort(silPars[j, ], age)
      keepInd <- KeeplxInd(lx)
      ltsim[[j]] <- data.frame( x = age[keepInd], lx = lx[keepInd], mux = mux[keepInd])
    }
    
    # Caculate some measures
    tau <- lapply(1:length(ltsim), function(x){
      CalcTau(ltsim[[x]]$lx, ageInt)
    })
    outsim[[i]]$tau <- unlist(tau)
    
    # calculate e0
    e0 <- lapply(1:length(ltsim), function(x){
      CalcE0(ltsim[[x]]$lx, ageInt)
    })
    outsim[[i]]$e0 <- unlist(e0)
    
    # calculate lbar
    outsim[[i]]$lbar <- outsim[[i]]$tau/outsim[[i]]$e0
    
    # calculate Gini
    Gini <- lapply(1:length(ltsim), function(x){
      CalcGini(ltsim[[x]]$lx, ageInt)
    })
    outsim[[i]]$Gini <- unlist(Gini)
    outsim[[i]]  <- mutate(outsim[[i]], "a0" = silPars[ , 1], "a1" = silPars[ , 2], "c" = silPars[ , 3],
           "b0" = silPars[ , 4], "b1" = silPars[ , 5])
    rm(ltsim)
  }  
  # save the outsim object
  
  names(outsim)[1:4] <- c("all", "infant", "senescent", "constant")
  saveRDS(outsim, "data/outsim.rds")
  rm(Gini, e0, tau, silPars, age, ageInt, i, j)
} 
