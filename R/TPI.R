# Transition probability input

TPI <- function(){

p.Z <- 0
p.A <- 0
p.Y <- 0
if(HS>3){
p.B <- 0
p.C <- 0
p.X <- 0
}
if(HS>4){
p.D  <- 0
p.E <- 0
p.F <- 0
p.W  <- 0
}
if(HS>5){
p.G <- 0
p.H <- 0
p.I <- 0
p.J <- 0
p.V  <- 0
}


c.1  <- 0
c.2 <- 0
c.absorb  <- 0

if(HS>3){
  c.3 <- 0
}
if(HS>4){
  c.4 <- 0
}
if(HS>5){
  c.5 <- 0
}

u.1  <- 0
u.2 <- 0
u.absorb  <- 0

if(HS>3){
  u.3 <- 0
}
if(HS>4){
  u.4 <- 0
}
if(HS>5){
  u.5 <- 0
}

if(HS==3){
  input <- data.frame(
    p.A = p.A,
    p.Y = p.Y,
    p.Z = p.Z,
    c.1  = c.1 ,
    c.2 = c.2,
    c.absorb  = c.absorb ,
    u.1  = u.1 ,
    u.2 = u.2,
    u.absorb  = u.absorb )
} else if(HS==4){
  input <- data.frame(
    p.A = p.A,
    p.B = p.B,
    p.C = p.C,
    p.X = p.X,
    p.Y = p.Y,
    p.Z = p.Z,
    c.1  = c.1 ,
    c.2 = c.2,
    c.3= c.3,
    c.absorb  = c.absorb ,
    u.1  = u.1 ,
    u.2 = u.2,
    u.3 = u.3,
    u.absorb  = u.absorb  )
} else if(HS==5){
  input <- data.frame(
    p.A = p.A,
    p.B = p.B,
    p.C = p.C,
    p.D = p.D,
    p.E = p.E,
    p.F = p.F,
    p.W = p.W,
    p.X = p.X,
    p.Y = p.Y,
    p.Z = p.Z,
    c.1 = c.1,
    c.2 = c.2,
    c.3 = c.3,
    c.4 = c.4,
    c.absorb  = c.absorb ,
    u.1  = u.1 ,
    u.2 = u.2,
    u.3 = u.3,
    u.4 = u.4,
    u.absorb  = u.absorb )
} else if(HS==6){
  input <- data.frame(
    p.A = p.A,
    p.B = p.B,
    p.C = p.C,
    p.D = p.D,
    p.E = p.E,
    p.F = p.F,
    p.G = p.G,
    p.H = p.H,
    p.I = p.I,
    p.J = p.J,
    p.V = p.V,
    p.W = p.W,
    p.X = p.X,
    p.Y = p.Y,
    p.Z = p.Z,
    c.1 = c.1,
    c.2 = c.2,
    c.3 = c.3,
    c.4 = c.4,
    c.5 = c.5,
    c.absorb  = c.absorb ,
    u.1  = u.1 ,
    u.2 = u.2,
    u.3 = u.3,
    u.4 = u.4,
    u.5 = u.5,
    u.absorb  = u.absorb)

}
input$c.Tr <- 0
input[2,]<- input[1,]

rownames(input) <- c(Strategies[1], Strategies[2])

if(HS==3){
  colnames(input) <- c("p.A", "p.Y", "p.Z",
                       paste("c.", tolower(v.n[1]), sep = ""), #c.1
                       paste("c.", tolower(v.n[2]), sep = ""), #c.2
                       paste("c.", tolower(tail(v.n,1)), sep = ""),
                       paste("u.", tolower(v.n[1]), sep = ""), #c.1
                       paste("u.", tolower(v.n[2]), sep = ""), #c.2
                       paste("u.", tolower(tail(v.n,1)), sep = ""),
                       "c.treatment")
} else if(HS==4){
  colnames(input) <- c("p.A", "p.B", "p.C", "p.X", "p.Y", "p.Z",
                       paste("c.", tolower(v.n[1]), sep = ""), #c.1
                       paste("c.", tolower(v.n[2]), sep = ""), #c.2
                       paste("c.", tolower(v.n[3]), sep = ""), #c.3
                       paste("c.", tolower(tail(v.n,1)), sep = ""),
                       paste("u.", tolower(v.n[1]), sep = ""), #c.1
                       paste("u.", tolower(v.n[2]), sep = ""), #c.2
                       paste("u.", tolower(v.n[3]), sep = ""), #c.3
                       paste("u.", tolower(tail(v.n,1)), sep = ""),
                       "c.treatment")

} else if (HS==5){
  colnames(input) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.W', "p.X", "p.Y", "p.Z",
                       paste("c.", tolower(v.n[1]), sep = ""), #c.1
                       paste("c.", tolower(v.n[2]), sep = ""), #c.2
                       paste("c.", tolower(v.n[3]), sep = ""), #c.3
                       paste("c.", tolower(v.n[4]), sep = ""), #c.3
                       paste("c.", tolower(tail(v.n,1)), sep = ""),
                       paste("u.", tolower(v.n[1]), sep = ""), #c.1
                       paste("u.", tolower(v.n[2]), sep = ""), #c.2
                       paste("u.", tolower(v.n[3]), sep = ""), #c.3
                       paste("u.", tolower(v.n[4]), sep = ""), #c.3
                       paste("u.", tolower(tail(v.n,1)), sep = ""),
                       "c.treatment")



} else if (HS==6){
  colnames(input) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.G', "p.H", "p.I", "p.J",
                       'p.V', 'p.W', "p.X", "p.Y", "p.Z",
                       paste("c.", tolower(v.n[1]), sep = ""), #c.1
                       paste("c.", tolower(v.n[2]), sep = ""), #c.2
                       paste("c.", tolower(v.n[3]), sep = ""), #c.3
                       paste("c.", tolower(v.n[4]), sep = ""), #c.3
                       paste("c.", tolower(v.n[5]), sep = ""), #c.3
                       paste("c.", tolower(tail(v.n,1)), sep = ""),
                       paste("u.", tolower(v.n[1]), sep = ""), #c.1
                       paste("u.", tolower(v.n[2]), sep = ""), #c.2
                       paste("u.", tolower(v.n[3]), sep = ""), #c.3
                       paste("u.", tolower(v.n[4]), sep = ""), #c.3
                       paste("u.", tolower(v.n[5]), sep = ""), #c.3
                       paste("u.", tolower(tail(v.n,1)), sep = ""),
                       "c.treatment")
   }


return(input)

}


#BACK-UP 12-04-2019 from text input to table input
## Transition probability input
#
#TPI <- function(){
#
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
#
#
#  c.H  <- as.numeric(readline(cat("What does it cost to be in", v.n[1], "for one cycle?")))
#  c.S1 <- as.numeric(readline(cat("What does it cost to be in", v.n[2], "for one cycle?")))
#  c.D  <- as.numeric(readline(cat("What does it cost to be in", tail(v.n, n=1), "for one cycle?")))
#
#  if(HS>3){
#    c.S2 <- as.numeric(readline(cat("What does it cost to be in", v.n[3], "for one cycle?")))
#  } else if(HS>4){
#    c.S3 <- as.numeric(readline(cat("What does it cost to be in", v.n[4], "for one cycle?")))
#  } else if(HS>5){
#    c.S4 <- as.numeric(readline(cat("What does it cost to be in", v.n[5], "for one cycle?")))
#  }
#
#  u.H  <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", v.n[1], "healthstate?")))
#  u.S1 <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", v.n[2], "healthstate?")))
#  u.D  <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", tail(v.n, n=1), "healthstate?")))
#
#  if(HS>3){
#    u.S2 <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", v.n[3], "healthstate?")))
#  } else if(HS>4){
#    u.S3 <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", v.n[4], "healthstate?")))
#  } else if(HS>5){
#    u.S4 <- as.numeric(readline(cat("What utility value / effect do patients have that are in ", v.n[5], "healthstate?")))
#  }
#
#  if(HS==3){
#    input <- data.frame(
#      p.HD = p.HD,
#      p.HS1 = p.HS1,
#      p.S1D = p.S1D,
#      c.H  = c.H ,
#      c.S1 = c.S1,
#      c.D  = c.D ,
#      u.H  = u.H ,
#      u.S1 = u.S1,
#      u.D  = u.D )
#  } else if(HS==4){
#    input <- data.frame(
#      p.HD = p.HD,
#      p.HS1 = p.HS1,
#      p.S1D = p.S1D,
#      p.HS2 = p.HS2,
#      p.S1S2= p.S1S2,
#      p.S2D = p.S2D,
#      c.H  = c.H ,
#      c.S1 = c.S1,
#      c.S2 = c.S2,
#      c.D  = c.D ,
#      u.H  = u.H ,
#      u.S1 = u.S1,
#      u.S2 = u.S2,
#      u.D  = u.D    )
#  } else if(HS==5){
#    input <- data.frame(
#      p.HD = p.HD,
#      p.HS1 = p.HS1,
#      p.S1D = p.S1D,
#      p.HS2 = p.HS2,
#      p.S1S2= p.S1S2,
#      p.S2D = p.S2D,
#      p.HS3  = p.HS3 ,
#      p.S1S3 = p.S1S3,
#      p.S2S3 = p.S2S3,
#      p.S3D  = p.S3D,
#      c.H  = c.H ,
#      c.S1 = c.S1,
#      c.S2 = c.S2,
#      c.S3 = c.S3,
#      c.D  = c.D ,
#      u.H  = u.H ,
#      u.S1 = u.S1,
#      u.S2 = u.S2,
#      u.S3 = u.S3,
#      u.D  = u.D )
#  } else if(HS==6){
#    input <- data.frame(
#      p.HD = p.HD,
#      p.HS1 = p.HS1,
#      p.S1D = p.S1D,
#      p.HS2 = p.HS2,
#      p.S1S2= p.S1S2,
#      p.S2D = p.S2D,
#      p.HS3  = p.HS3 ,
#      p.S1S3 = p.S1S3,
#      p.S2S3 = p.S2S3,
#      p.S3D  = p.S3D,
#      p.HS4  = p.HS4,
#      p.S1S4 = p.S1S4,
#      p.S2S4 = p.S2S4,
#      p.S3S4 = p.S3S4,
#      p.S4D  = p.S4D,
#      c.H  = c.H ,
#      c.S1 = c.S1,
#      c.S2 = c.S2,
#      c.S3 = c.S3,
#      c.S4 = c.S4,
#      c.D  = c.D ,
#      u.H  = u.H ,
#      u.S1 = u.S1,
#      u.S2 = u.S2,
#      u.S3 = u.S3,
#      u.S4 = u.S4,
#      u.D  = u.D )
#
#  }
#  input$c.Tr <- 0
#  input[2,]<- input[1,]
#
#  row.names(input) <- c(Strategies[1], Strategies[2])
#  return(input)
#
#}
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
