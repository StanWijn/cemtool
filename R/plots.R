
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
  #  p.HD <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[1], "to", tail(v.n, n=1), "?")))))
  #  p.HS1 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[1], "to", v.n[2], "?")))))
  #  p.S1D <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[2], "to", tail(v.n, n=1), "?")))))
  #  if(HS>3){
  #    p.HS2 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[1], "to", v.n[3], "?")))))
  #    p.S1S2 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[2], "to", v.n[3], "?")))))
  #    p.S2D <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[3], "to", tail(v.n, n=1), "?")))))
  #  }
  #  if(HS>4){
  #    p.HS3  <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[1], "to", v.n[4], "?")))))
  #    p.S1S3 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[2], "to", v.n[4], "?")))))
  #    p.S2S3 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[3], "to", v.n[4], "?")))))
  #    p.S3D  <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[4], "to", tail(v.n, n=1), "?")))))
  #  }
  #  if(HS>5){
  #    p.HS4 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[1], "to", v.n[5], "?")))))
  #    p.S1S4 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[2], "to", v.n[5], "?")))))
  #    p.S2S4 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[3], "to", v.n[5], "?")))))
  #    p.S3S4 <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[4], "to", v.n[5], "?")))))
  #    p.S4D  <- eval(parse(text=(readline(cat("What is the probability to move from", v.n[5], "to", tail(v.n, n=1), "?")))))
  #  }







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
