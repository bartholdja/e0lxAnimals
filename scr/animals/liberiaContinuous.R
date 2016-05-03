smort <- function(x, b) exp(b[1] - b[2] * x) + b[3] + exp(b[4] + b[5] * x) # Fernando's Siler parameterisation
smort1 <- function(x, th) th[1] *exp(-th[2] * x) + th[3] + th[4] * exp(th[5] * x)  # my Siler parameterisation
bf <- c(0.239379841567792, 1.54353074219561, 0.062099937068153, 
        -7.29298737126152, 0.0800925933810958)
thf <- bf
thf[c(1,4)] <- exp(bf[c(1,4)])

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

KeeplxInd <- function(lx, cutOff) {
  return(which(lx >= cutOff))
}

#####

age <- seq(0, 5000, 0.2)

lx <- CalcSurv(x = age, th = thf)
keepInd <- KeeplxInd(lx, cutOff = 0.001) # to determine upper 


e0<- integrate(f = CalcSurv, th = thf, lower = 0, upper = age[keepInd[length(keepInd)]])[[1]]
tau <- integrate(f = CalcSurvSqu, th = thf, lower = 0, upper = age[keepInd[length(keepInd)]])[[1]]
lbar <- tau/e0

gini <- 2 * lbar - 1
