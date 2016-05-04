# This script uses the Siler model fit of Colchero et al to calculate the quantities for Liberia since
# the liberia life table has 5-year age intervals at the beginning of the lifetable while the population
# has very high infant mortality

smort <- function(x, b) exp(b[1] - b[2] * x) + b[3] + exp(b[4] + b[5] * x) # Fernando's Siler parameterisation
smort1 <- function(x, th) th[1] *exp(-th[2] * x) + th[3] + th[4] * exp(th[5] * x)  # my Siler parameterisation
bf <- c(0.239379841567792, 1.54353074219561, 0.062099937068153, 
        -7.29298737126152, 0.0800925933810958)
thf <- bf
thf[c(1,4)] <- exp(bf[c(1,4)])

#####

age <- seq(0, 5000, 0.2)

lx <- CalcSurv(x = age, th = thf)
keepInd <- KeeplxInd(lx, cutOff = 0.001) # to determine upper 


e0<- integrate(f = CalcSurv, th = thf, lower = 0, upper = age[keepInd[length(keepInd)]])[[1]]
tau0 <- integrate(f = CalcSurvSqu, th = thf, lower = 0, upper = age[keepInd[length(keepInd)]])[[1]]
lbar0 <- tau0/e0

shape0 <- 2 * lbar0 - 1

eMat<- integrate(f = CalcSurv, th = thf, lower = 12, upper = age[keepInd[length(keepInd)]])[[1]]
tauMat <- integrate(f = CalcSurvSqu, th = thf, lower = 12, upper = age[keepInd[length(keepInd)]])[[1]]
lbarMat <- tauMat/eMat

shapeMat <- 2 * lbarMat - 1
