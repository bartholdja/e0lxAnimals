# Overlapping life span tau
CalcTau <- function(lx, ageInt) {
  sum(lx^2 * (1/ageInt))
}

# Life expectancy
CalcE0 <- function(lx, ageInt) {
  sum(lx * (1/ageInt))
}

# Gini Coefficient of lxc
CalcGini <- function(lx, ageInt) {
  1 -  (sum(lx^2 * (1/ageInt)) / sum(lx * (1/ageInt)))
}

CalcSurv <- function(th, ...) UseMethod("CalcSurv")

CalcSurv.matrix <- function(th, x) {
  exp(th[, 1]/th[, 2] * (exp(-th[, 2] * x) - 1) - th[, 3] * x + 
        th[, 4]/th[, 5] * (1 - exp(th[, 5] * x)))
}

CalcSurv.numeric <- function(th, x) {
  exp(th[1]/th[2] * (exp(-th[2] * x) - 1) - th[3] * x + 
        
        th[4]/th[5] * (1 - exp(th[5] * x)))
}

# Keep lx until 0.99 percent are dead
KeeplxInd <- function(lx) {
  return(which(lx >= 0.01))
}

# function to pick one mean matrix per species
# by quality conditions maximum dimension and duration
# and if equal among different matrices then pic randomly
# the function returns the id of the matrix
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

# qsd
qsdConverge <- function(matU, startLife, conv){
  #Function to determine the cutoff age at quasi-convergence for lx and mx (Code adapted from H. Caswell's matlab code):
  nSteps = 1000
  uDim = dim(matU)
  eig = eigen.analysis(matU)
  qsd = eig$stable.stage
  qsd = as.numeric(t(matrix(qsd / sum(qsd))))
  
  #Set up a cohort
  nzero = rep(0, uDim[1]) #Set a population vector of zeros
  nzero[startLife] = 1 #Set the first stage to = 1
  n = nzero #Rename for convenience
  
  #Iterate the cohort (n= cohort population vector, p = proportional structure)
  dist = p = NULL
  survMatrix1 <- matU
  for (j in 1:nSteps){ #j represent years of iteration
    p = n / sum(n) #Get the proportional distribution
    dist[j] = 0.5 * (sum(abs(p - qsd)))
    n = survMatrix1 %*% n #Multiply the u and n matrices to iterate
  }
  #Find the ages for convergence to conv. (default = 0.05).
  #i.e. within 5% of the QSD.
  convage = min(which(dist < conv))
  return(convage) 
}

# Calculate the lx

makeLifeTable <- function(matU, matF = NULL, matC = NULL, startLife = 1, nSteps = 1000){
  # function adapted from the original MakeLifeTable to only calculate lx (not mx)
  matDim = ncol(matU)
  
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
  return(out)
}

# ggplot2 color scale
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}