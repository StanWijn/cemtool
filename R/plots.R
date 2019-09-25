
#' @import diagram
#' @importFrom grDevices recordPlot
#' @importFrom graphics legend matplot
#'
require(diagram)

first <- function(HS){
  DiffMat  <- matrix(data = 0, nrow = HS, ncol = HS)
  AA <- as.data.frame(DiffMat)
  
  AA[[2,1]] <- "p.A"
  AA[[HS,1]] <- "p.Z"
  AA[[HS,2]] <- "p.Y"
  
  
  if(HS>3){
    AA[[3,1]] <- "p.B"
    AA[[3,2]] <- "p.C"
    AA[[HS,3]] <- "p.X"
  }
  if(HS>4){
    AA[[4,1]] <- "p.D"
    AA[[4,2]] <- "p.E"
    AA[[4,3]] <- "p.F"
    AA[[HS,4]] <- "p.W"
    
  }
  if(HS>5){
    AA[[5,1]] <- "p.G"
    AA[[5,2]] <- "p.H"
    AA[[5,3]] <- "p.I"
    AA[[5,4]] <- "p.J"
    AA[[HS,5]] <- "p.V"
  }
  
  
  
  
  #--
  name  <-  if(HS==3){
    c("1", "2", "3")
  } else if(HS==4){
   c("1", "2", "3", "4")
  } else if(HS==5){
    c("1", "2", "3", "4", "5")
  } else if(HS==6){
    c("1", "2", "3", "4", "5", "6")
  }
  plotmat(A = AA, pos = if(HS==6){NULL} else if (HS==5){c(1,HS-3,2)} else if(HS==4) {c(1,2,1)} else if(HS==3){c(1,2)}
          , name = name, box.type =  "square", curve = if(HS==3){0.0} else if(HS==6){.0} else {0.0},
          lwd = 0.9, cex.txt = 0.8, box.prop = 0.5, box.size = 0.08, self.shiftx = 2,box.lwd = 1,
          arr.pos = 0.4, shadow.size = 0, arr.length = 0.3, 
          arr.type = "curved",
          box.cex = 0.8,
          main = "Markov model structure")
  
}


second <- function(HS, v.n){
  DiffMat  <- matrix(data = 0, nrow = HS, ncol = HS)
  AA <- as.data.frame(DiffMat)

    AA[[2,1]] <- "p.A"
    AA[[HS,1]] <- "p.Z"
    AA[[HS,2]] <- "p.Y"


if(HS>3){
AA[[3,1]] <- "p.B"
AA[[3,2]] <- "p.C"
AA[[HS,3]] <- "p.X"
}
if(HS>4){
  AA[[4,1]] <- "p.D"
  AA[[4,2]] <- "p.E"
  AA[[4,3]] <- "p.F"
  AA[[HS,4]] <- "p.W"

}
if(HS>5){
  AA[[5,1]] <- "p.G"
  AA[[5,2]] <- "p.H"
  AA[[5,3]] <- "p.I"
  AA[[5,4]] <- "p.J"
  AA[[HS,5]] <- "p.V"
}
 



  #--
  name  <- v.n

  plotmat(A = AA, pos = if(HS==6){NULL} else if (HS==5){c(1,HS-3,2)} else if(HS==4) {c(1,2,1)} else if(HS==3){c(1,2)}
          , name = name, box.type =  "square", curve = if(HS==3){0.0} else if(HS==6){.0} else {0.0},
          lwd = 0.9, cex.txt = 0.8, box.prop = 0.5, box.size = 0.08, self.shiftx = 2,box.lwd = 1,
          arr.pos = 0.4, shadow.size = 0, arr.length = 0.3, 
          arr.type = "curved",
          box.cex = 0.8,
          main = "Markov model structure")

}

