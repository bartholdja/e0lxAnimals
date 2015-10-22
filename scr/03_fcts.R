# Overlapping life span tau
CalcTau <- function(lx, ageInt = 1) {
  sum(lx^2 * ageInt)
}

# Life expectancy
CalcE0 <- function(lx, ageInt = 1) {
  sum(lx * ageInt)
}

# Gini Coefficient of lx
CalcGini <- function(lx, ageInt = 1) {
  1 -  (sum(lx^2 * ageInt) / sum(lx * ageInt))
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

CalcMort <- function(th, ...) UseMethod("CalcMort")

CalcMort.matrix <- function(th, x) {
  th[, 1] * exp(-th[, 2] * x) + th[, 3] + th[, 4] * exp(th[, 5] * x)
}

CalcMort.numeric <- function(th, x) {
  th[1] * exp(-th[2] * x) + th[3] + th[4] * exp(th[5] * x)
}

# function to draw Siler parameters
DrawPars <- function(type) { #type is either "all", "infant", "senescent", "constant"
  if(type == "all") {
  a0 <- runif(1, min = 0.00001, max = 2)
  a1 <- runif(1, min = 0.00001, max = 1)
  c <- runif(1, min = 0.00001, max = 0.1)
  b0 <- runif(1, min = 0.00001, max = 0.03)
  b1 <- runif(1, min = 0.00001, max = 0.3)
  }
  if(type == "infant") {
    a0 <- runif(1, min = 1, max = 6)
    a1 <- runif(1, min = 0, max = 0.75)
    c <- b0 <- b1 <- 0.00001
  }
  if(type == "senescent") {
    a0 <- a1 <- c <- 0.00001
    b0 <- runif(1, min = 0.00001, max = 0.15)
    b1 <- runif(1, min = 0.00001, max = 0.3)
  }
  if(type == "constant") {
    a0 <- a1 <- b0 <- b1 <- 0.00001
    c <- runif(1, min = 0.01, max = 0.5)
  }
  th <- c(a0, a1, c, b0, b1)
  return(th)
}
  
# Keep lx until 0.99 percent are dead
KeeplxInd <- function(lx) {
  return(which(lx >= 0.01))
}

Keeplx <- function(lx) {
  return(lx[which(lx >= 0.01)])
}
  
# @return PDF output to disk.
ExportPDF <- function (.x, .path, .width, .height) {
  pdf(.path, width = 0.4*.width, height = 0.4*.height,
      useDingbats = FALSE, onefile=FALSE) # avoid problems with missing fonts
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(.width, "cm"),
                 height = unit(.height, "cm"))
  pushViewport(vp)
  
  print(.x, vp = vp)
  dev.off()
}
