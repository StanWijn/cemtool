########  Cost effectiveness model  #############
########      S.R.W. Wijn MSc, 2019     #############
#################################################
#Markov model in R: Deterministic
#' @export
#'

cemprob <-function(){ 
  
  cat("Step 2: Collect model input:", "\n")
  
  cat("Currently, we have limited the transition matrix that patients can only progress to the next state.
    (example: from healthy to sick, but not recover from sick to healthy)", "\n",
      "If you want them to recover, you can alter the transition matrix at the end.", "\n")
  
  cat("--------------------------------------------", "\n")
  
  cat("Press enter to open another window in which you can define the transition probabilities, 
    the costs and utility value associated with the healthstate. Close the window when you want to continue.
    
    Please enter all the probabilities as digits. 5% chance to move from healthy to death is thus: 0.05.", "\n")
  if(interactive()) readkey()
  modelinput <<- TPI()
  # create empty Markov trace
  m.M_treatment <<- m.M <<- matrix(NA, nrow = n.t + 1, ncol = n.s, dimnames = list(paste("cycle", 0:n.t, sep = " "), v.n))
  m.M[1,] <<- m.M_treatment[1,] <<- c(1, rep(0, times = HS-1))
  
  
  cat("--------------------------------------------", "\n")
  
  runtable <- function(){
    editTable(modelinput)
    
    if(interactive()) readkey()
    
    cat("--------------------------------------------", "\n")
  }
  
  runtable()
  
  # define discount function
  v.dwc <<-  1 / ((1 + d.rc) ^ (0:n.t))
  v.dwe <<-  1 / ((1 + d.re) ^ (0:n.t))
  
  input <- modelinput2
  if(HS==3){
    colnames(input) <- c("p.A", "p.Y", "p.Z",
                         'c.1', 'c.2', 'c.absorb',"c.Tr",
                         'u.1', 'u.2', 'u.absorb')
  } else if(HS==4){
    colnames(input) <- c("p.A", "p.B", "p.C", "p.X", "p.Y", "p.Z",
                         'c.1', 'c.2', 'c.3', 'c.absorb','c.Tr',
                         'u.1', 'u.2', 'u.3', 'u.absorb')
  } else if (HS==5){
    colnames(input) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.W', "p.X", "p.Y", "p.Z",
                         'c.1', 'c.2', 'c.3', 'c.4', 'c.absorb','c.Tr',
                         'u.1', 'u.2', 'u.3', 'u.4', 'u.absorb')
  } else if (HS==6){
    colnames(input) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.G', "p.H", "p.I", "p.J", 'p.V', 'p.W', "p.X", "p.Y", "p.Z",
                         'c.1', 'c.2', 'c.3', 'c.4', 'c.5', 'c.absorb','c.Tr',
                         'u.1', 'u.2', 'u.3', 'u.4', 'u.5', 'u.absorb')
  }
  
  modelinput2 <<- input
  m.P <<-TMB(input[1,])
  m.P_treatment <<- TMB(input[2,])
  plot1 <- recordPlot(second(HS))
  
  cat("Do you want to alter the transition probability matrix? Yes / No", "\n")
  prompt_matrix <- if (interactive())  askYesNo("Do you want to alter the transition probability matrix?", "\n")
  
  
  if(prompt_matrix == FALSE | is.na(prompt_matrix)){
    cat("--------------------------------------------", "\n")
    cemrun()
  } else if(prompt_matrix == TRUE){
    editmatrix(m.P)
    if(interactive()) readkey()
    
    cat("--------------------------------------------", "\n")
    cemrun()
  }
  

}


cemtpm <- function(){
  editmatrix(m.P)
  if(interactive()) readkey()
  
  cat("--------------------------------------------", "\n")
  cemrun()
  
}




