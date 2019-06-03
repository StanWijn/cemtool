
#' @import diagram
#'
require(diagram)

first <- function(HS){
   DiffMat  <- matrix(data = 0, nrow = HS, ncol = HS)
   AA <- as.data.frame(DiffMat)


   plotmat(A = AA, pos = c(1,HS-2,1), box.type =  "diamond", curve = NA,
           lwd = 1, cex.txt = 0.7, box.prop = 0.6, box.size = 0.08, self.shiftx = 2,box.lwd = 1,
           arr.pos = 0.4,
           arr.type = "curved",
           #txt.yadj = 1, txt.xadj = 1,
           box.cex = 0.8,
           main = "Markov model")

  }


second <- function(HS){
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
  plotmat(A = AA, pos = c(1,HS-2,1), name = name, box.type =  "diamond", curve = NA,
         lwd = 1, cex.txt = 0.7, box.prop = 0.6, box.size = 0.08, self.shiftx = 2,box.lwd = 1,
         arr.pos = 0.4, 
          arr.type = "curved",
          #txt.yadj = 1, txt.xadj = 1,
         box.cex = 0.8,
                    main = "Markov model")

}
