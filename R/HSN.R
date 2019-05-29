#'@export
HSN <- function(){
  HS1 <- readline("What is the name for the start state?: ")
  HS2 <- HSF()
  dead <- readline("What is the name for the death/absorption state?: ")

  n.t <<-  (as.integer(readline("How many cycles do you want in years?: ")))
  cat("Currently we only support the comparison of two strategies: An intervention vs control.")
  intervention <- readline("What is the name of your intervention?:   ")
  control <- readline("What is the name of your control?:   ")
  Strategies <<- c(control, intervention)
  v.n <<- c(HS1, HS2, dead)
  second(HS)
  n.s <<- HS
}
