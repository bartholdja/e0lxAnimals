# function to pick one mean matrix per species

# quasi stable distribution reached at this age
qsdConverge <- function(matU, matF, startLife, conv) {
  #Function to determine the cutoff age at quasi-convergence for lx and mx (Code adapted from H. Caswell's matlab code):
  
  nSteps = 1000
  uDim = dim(matU)
  eig = eigen.analysis(matU)
  qsd = eig$stable.stage
  qsd = as.numeric(t(matrix(qsd / sum(qsd))))
  
  #Set up a cohort
  nzero = rep(0, uDim[1]) #Set a population vector of zeros
  nzero[startLife] = 1 #Set the first stage to = 1
  n = nzero # Rename for convenience
  
  #Iterate the cohort (n = cohort population vector, p = proportional structure)
  dist = p = NULL
  survMatrix1 <- matU
  for (j in 1:nSteps){ #j represent years of iteration
    p = n / sum(n) #Get the proportional distribution
    dist[j] = 0.5 * (sum(abs(p - qsd)))
    n = survMatrix1 %*% n #Multiply the u and n matrices to iterate
  }
  #Find the ages for convergence to conv. (default = 0.05).
  #i.e. within 5% of the QSD.
  if(min(dist, na.rm = T) < conv) {
    convage = min(which(dist < conv)) }
  if(min(dist, na.rm = T) >= conv | sum(!is.na(dist)) == 0) {
    convage = NA
    warning("Convergence not reached") }
  return(convage) 
}


# life table from matrix
makeLifeTable <- function(matU, matF, matC = NULL, startLife){
  
  matDim = ncol(matU)
  nSteps = 1000
  
  #Age-specific survivorship (lx) (See top function on page 120 in Caswell 2001):
  matUtemp = matU
  survivorship = array(NA, dim = c(nSteps, matDim))
  for (o in 1:nSteps){
    survivorship[o, ] = colSums(matUtemp %*% matU)
    matUtemp = matUtemp %*% matU
  }
  
  lx = survivorship[, startLife]
  lx = c(1, lx[1:(length(lx) - 1)])
  
  #Make room for dx and qx under assumption of 0.5 in age gap distributions
  
  #Start to assemble output object
  out = data.frame(x = 0:(length(lx)-1),lx = lx)
  
  
  #Age-specific fertility (mx, Caswell 2001, p. 120)
  ageFertility = array(0, dim = c(nSteps, matDim))
  fertMatrix = array(0, dim = c(nSteps, matDim))
  matUtemp2 = matU
  e = matrix(rep(1, matDim))
  for (q in 1:nSteps) {
    fertMatrix = matF %*% matUtemp2 * (as.numeric((ginv(diag(t(e) %*% matUtemp2)))))
    ageFertility[q, ] = colSums(fertMatrix)
    matUtemp2 = matUtemp2 %*% matU
  }  
  mx = ageFertility[, startLife]
  mx = c(0, mx[1:(length(mx) - 1)])
  if(!missing(matF)){
    if(sum(matF,na.rm=T)==0){
      mx <- rep(NA, length(mx))
    }
    out$mx = mx
  }
  
  #  if(!missing(matC)){
  #    if(sum(matC,na.rm=T)==0){
  #      warning("matC contains only 0 values")
  #    }
  #    #Age-specific clonality (cx)
  #    ageClonality = array(0, dim = c(nSteps, matDim))
  #    clonMatrix = array(0, dim = c(nSteps, matDim))
  #    matUtemp2 = matU
  #    e = matrix(rep(1, matDim))
  #    for (q in 1:nSteps) {
  #      clonMatrix = matC %*% matUtemp2 * (as.numeric((ginv(diag(t(e) %*% matUtemp2)))))
  #      ageClonality[q, ] = colSums(clonMatrix)
  #      matUtemp2 = matUtemp2 %*% matU
  #    }  
  #    cx = ageClonality[, startLife]
  #    cx = c(0, cx[1:(length(cx) - 1)])
  #    out$cx = cx
  #  }
  
  return(out)
}

# Keep lx until a only a cutoff percentage of a synthetic cohort is still alive (e.g. 0.01)
KeeplxInd <- function(lx, cutOff) {
  return(which(lx >= cutOff))
}

# by the quality criteria "maximum dimension" plus "study duration" (at least for comadre that seems to work quite well, manually checked).
# If the quality is equal among different matrices then the functions
# picks one at random. It returns the id of the matrix
PickMat <- function(x) { #names in dat: dur (duration) ,dim = dimension, spec = species, id = id
  
  temp <- rowSums(select(x, dim, dur), na.rm = T)
  indEquMat <- which(temp == max(temp)) # ind of matrices with equal quality standard
  if(length(indEquMat) > 1) {
    idMat <- unlist(x[indEquMat, ] %>% select(id) %>% sample_n(1))
  }
  if(length(indEquMat) == 1) {
    idMat <- unlist(x$id[indEquMat])
  }  
  return(idMat)
}

# Joint life expectancy tau (integral of lx squared over x)
# by midpoint approximation
CalcTau <- function(lx, ageInt) {
  if(length(lx) == 0) {
    temp <- NA
  } else {
  lxNow <- c(lx, 0)
  lxNow <- lxNow/lxNow[1]
  lxNew <- na.omit(as.vector(DescTools::Midx(lxNow)))
  temp <- sum(lxNew^2 * (1/ageInt))}
  return(temp)
}

# Life expectancy (integral of lx over x)
# by midpoint approximation
CalcE0 <- function(lx, ageInt) {
  if(length(lx) == 0) {
    temp <- NA
  } else {
  lxNow <- c(lx, 0)
  lxNow <- lxNow/lxNow[1]
  lxNew <- na.omit(as.vector(DescTools::Midx(lxNow)))
  temp <- sum(lxNew * (1/ageInt))}
  return(temp)
}

# Gini Coefficient of lx
CalcGini <- function(lx, ageInt) {
  e0 <- CalcE0(lx = lx, ageInt = ageInt)
  tau <- CalcTau(lx = lx, ageInt = ageInt)
  return(1 -  (tau/e0))
}

# lbar of lxc
CalcLbar <- function(lx, ageInt) {
  e0 <- CalcE0(lx = lx, ageInt = ageInt)
  tau <- CalcTau(lx = lx, ageInt = ageInt)
  return((tau/e0))
}

# Siler survival function
CalcSurv <- function(th, ...) UseMethod("CalcSurv")

CalcSurv.matrix <- function(th, x) {
  exp(th[, 1]/th[, 2] * (exp(-th[, 2] * x) - 1) - th[, 3] * x + 
        th[, 4]/th[, 5] * (1 - exp(th[, 5] * x)))
}

CalcSurv.numeric <- function(th, x) {
  exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x + 
        
        th[4]/th[5] * (1 - exp(th[5] * x)))
}


#Function to determine probability of reaching reproduction, age at maturity and reproductive lifespan (Code adapted from H. Caswell's matlab code):
lifeTimeRepEvents <- function(matU, matF, startLife) {
  out <- data.frame(Eta = NA, Lmax = NA, pRep = NA,
                    eFirstRep = NA, La = NA)
  
  matUIssue <- missing(matU) | sum(matU, na.rm=T)==0
  matFIssue <- missing(matF) | sum(matF, na.rm=T)==0

  if(matUIssue == T) {
    warning('matU missing or only contains 0 values')
    out$Eta = out$Lmax = out$pRep = out$eFirstRep = out$La = NA
  } else {
    if(matFIssue == T) {
      warning('matF missing or only contains 0 values')
      out$pRep = out$La = out$eFirstRep = NA
      
      uDim = dim(matU)[1]
      surv = colSums(matU)
      
      #Mean life expectancy (from Caswell's fundamental matrix approach 2001)
      N = solve(diag(uDim) - matU)
      Eta=colSums(N)[startLife]
      
      out$Eta = Eta
      
      #Max lifespan (from Morris and Doak book 2002)
      popVector=rep(0,dim(matU)[1])
      popVector[startLife]=100
      lifespanLeftover=matrix(0,1000,1)
      for (n in 1:1000){
        lifespanLeftover[n]=sum(popVector)
        popVector=matU%*%popVector
      }
      Lmax=min(which(lifespanLeftover<1))
      if(Lmax==Inf) {Lmax=999}
      
      out$Lmax=Lmax
    }
    if(matFIssue == F){
      
      uDim = dim(matU)[1]
      surv = colSums(matU)
      repLifeStages = colSums(matF)
      repLifeStages[which(repLifeStages>0)] = 1
      
      #Mean life expectancy (from Caswell's fundamental matrix approach 2001)
      N = solve(diag(uDim) - matU)
      Eta=colSums(N)[startLife]
      
      out$Eta = Eta
      
      #Max lifespan (from Morris and Doak book 2002)
      popVector=rep(0,dim(matU)[1])
      popVector[startLife]=100
      lifespanLeftover=matrix(0,1000,1)
      for (n in 1:1000){
        lifespanLeftover[n]=sum(popVector)
        popVector=matU%*%popVector
      }
      Lmax=min(which(lifespanLeftover<1))
      if(Lmax==Inf) {Lmax=999}
      
      out$Lmax=Lmax
      
      #Probability of survival to first reprod event
      Uprime = matU
      Uprime[,which(repLifeStages==1)] = 0
      Mprime = matrix(0,2,uDim)
      for (p in 1:uDim[1]) {
        if (repLifeStages[p]==1) Mprime[2,p] = 1 else
          Mprime[1,p] = 1-surv[p]
      }
      Bprime = Mprime%*%(ginv(diag(uDim)-Uprime))
      pRep = Bprime[2,startLife]
      
      out$pRep = pRep
      
      #Age at first reproduction (La; Caswell 2001, p 124)
      D = diag(c(Bprime[2,]))
      Uprimecond = D%*%Uprime%*%ginv(D)
      expTimeReprod = colSums(ginv(diag(uDim)-Uprimecond))
      La = expTimeReprod[startLife]
      
      out$La = La
      
      #Mean life expectancy conditional on entering the life cycle in the first reproductive stage
      firstRepLifeStage = min(which(repLifeStages==1))
      N = solve(diag(uDim[1])-matU)
      eFirstRep = colSums(N)[firstRepLifeStage]
      
      out$eFirstRep = eFirstRep
      
      #Life expectancy from mean maturity
      #remainingMatureLifeExpectancy = colSums(N)[startLife]-La
      
      #out$remainingMatureLifeExpectancy = remainingMatureLifeExpectancy
    }
  }
  return(out)
}

# ggplot2 color scale
#gg_color_hue <- function(n) {
#  hues = seq(15, 375, length=n+1)
#  hcl(h=hues, l=65, c=100)[1:n]
#}


# @return PDF output to disk.
ExportPDF <- function (.x, .path, .width, .height) {  # input object contains
  pdf(.path, width = 0.4*.width, height = 0.4*.height,
      useDingbats = FALSE, onefile=TRUE) # avoid problems with missing fonts
  for (i in 1:length(.x)) {
    grid.newpage()
    vp <- viewport(x = 0.5, y = 0.5,
                   width = unit(.width, "cm"),
                   height = unit(.height, "cm"))
    pushViewport(vp)
    
    print(.x[[i]], vp = vp)}
  dev.off()
}

# Siler survival function
CalcSurv<- function(x, th) {
  
  evalInf <- th[1] * (exp(-th[2] * 0.5) - 1) # evaluates to 0 if either the rate or the level parameter is 0
  evalSen <- th[4] * (1 - exp(th[5] * 0.5)) # evaluates to 0 if either the rate or the level parameter is 0
  
  # if infant and senescent mortality is 0
  if(evalInf == 0 & evalSen == 0) {
    surv <- exp(-th[3] * x)
  }
  
  # if senescent mortality is 0
  if(evalSen == 0 & evalInf != 0) {
    surv <- exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x)
  }
  
  # if infant mortality is 0
  if(evalInf == 0 & evalSen != 0) {
    surv <- exp(-th[3] * x + th[4]/th[5] * (1 - exp(th[5] * x)))
  }
  
  # if neither infant nor senescent mortality is 0
  if(evalInf != 0 & evalSen != 0) {
    surv <- exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x + 
                  th[4]/th[5] * (1 - exp(th[5] * x)))
  }
  return(surv)
}

# Siler survival function squared (for calculation of the integral of lx using the function integrate().)
CalcSurvSqu<- function(x, th) {
  
  evalInf <- th[1] * (exp(-th[2] * 0.5) - 1) # evaluates to 0 if either the rate or the level parameter is 0
  evalSen <- th[4] * (1 - exp(th[5] * 0.5)) # evaluates to 0 if either the rate or the level parameter is 0
  
  # if infant and senescent mortality is 0
  if(evalInf == 0 & evalSen == 0) {
    surv <- (exp(-th[3] * x))^2
  }
  
  # if senescent mortality is 0
  if(evalSen == 0 & evalInf != 0) {
    surv <- (exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x))^2
  }
  
  # if infant mortality is 0
  if(evalInf == 0 & evalSen != 0) {
    surv <- (exp(-th[3] * x + th[4]/th[5] * (1 - exp(th[5] * x))))^2
  }
  
  # if neither infant nor senescent mortality is 0
  if(evalInf != 0 & evalSen != 0) {
    surv <- (exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x + 
                   th[4]/th[5] * (1 - exp(th[5] * x))))^2
  }
  return(surv)
}


