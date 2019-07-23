# Transition probability input

TPI <- function(HS, Strategies, v.n){

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


