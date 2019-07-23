# transition matrix builder

TMB <- function(params, v.n, HS){
  with(as.list(params), {
# - create transition probability matrix

m.P <- matrix(0, nrow = HS, ncol = HS, dimnames = list(v.n, v.n))
m.P[tail(v.n, n=1), tail(v.n, n=1)] <- 1

if(HS==3){
  m.P[v.n[1], v.n[1]]  <- 1 - (p.A + p.Z)
  m.P[v.n[1], v.n[2]] <- p.A
  m.P[v.n[1], tail(v.n, n=1)]  <- p.Z

  m.P[v.n[2], v.n[2]] <- 1 -  p.Y
  m.P[v.n[2], tail(v.n, n=1)]  <- p.Y

} else if(HS==4){
  m.P[v.n[1], v.n[1]]  <- 1 - (p.A + p.Z + p.B)
  m.P[v.n[1], v.n[2]] <- p.A
  m.P[v.n[1], tail(v.n, n=1)]  <- p.Z

  m.P[v.n[2], v.n[2]] <- 1 -  (p.Y + p.C)
  m.P[v.n[2], tail(v.n, n=1)]  <- p.Y

  m.P[v.n[1], v.n[3]] <- p.B
  m.P[v.n[2], v.n[3]] <- p.C
  m.P[v.n[3], v.n[3]] <- 1 - p.X
  m.P[v.n[3], tail(v.n, n=1)] <- p.X

} else if(HS==5){
  m.P[v.n[1], v.n[1]]  <- 1 - (p.A + p.Z + p.B + p.D)
  m.P[v.n[1], v.n[2]] <- p.A
  m.P[v.n[1], v.n[3]] <- p.B
  m.P[v.n[1], v.n[4]]  <- p.D
  m.P[v.n[1], tail(v.n, n=1)]  <- p.Z

  m.P[v.n[2], v.n[2]] <- 1 -  (p.Y + p.C + p.F)
  m.P[v.n[2], v.n[3]] <- p.C
  m.P[v.n[2], v.n[4]]  <- p.E
  m.P[v.n[2], tail(v.n, n=1)]  <- p.Y

  m.P[v.n[3], v.n[3]] <- 1 - p.X - p.F
  m.P[v.n[3], v.n[4]]  <- p.F
  m.P[v.n[3], tail(v.n, n=1)] <- p.X

  m.P[v.n[4], v.n[4]] <- 1 - p.W
  m.P[v.n[4], tail(v.n, n=1)]  <- p.W


} else if (HS==6){
  m.P[v.n[1], v.n[1]]  <- 1 - (p.A + p.Z + p.B + p.D + p.G)
  m.P[v.n[1], v.n[2]] <- p.A
  m.P[v.n[1], v.n[3]] <- p.B
  m.P[v.n[1], v.n[4]]  <- p.D
  m.P[v.n[1], v.n[5]] <- p.G
  m.P[v.n[1], tail(v.n, n=1)]  <- p.Z

  m.P[v.n[2], v.n[2]] <- 1 -  (p.Y + p.C + p.F + p.H)
  m.P[v.n[2], v.n[3]] <- p.C
  m.P[v.n[2], v.n[4]]  <- p.E
  m.P[v.n[2], v.n[5]] <- p.H
  m.P[v.n[2], tail(v.n, n=1)]  <- p.Y

  m.P[v.n[3], v.n[3]] <- 1 - p.X - p.F - p.I
  m.P[v.n[3], v.n[4]]  <- p.F
  m.P[v.n[3], v.n[5]] <- p.I
  m.P[v.n[3], tail(v.n, n=1)] <- p.X

  m.P[v.n[4], v.n[4]] <- 1 - p.W - p.J
  m.P[v.n[4], v.n[5]] <- p.J
  m.P[v.n[4], tail(v.n, n=1)]  <- p.W

  m.P[v.n[5], v.n[5]] <- 1 - p.V
  m.P[v.n[5], tail(v.n, n=1)] <- p.V
}

return(m.P)
  }
)
}

