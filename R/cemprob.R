
#' Second step of the cemtool: Define the transition probabilities, cost and utility values
#' 
#' 
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model. 
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. 
#' 
#' @return The following variables will be created in the Global Enviroment:
#' @return --- Empty Markov trace matrices for both strategies (m.M and m.M_treatment)
#' @return --- A dataframe with the modelinput (modelinput)
#' @return --- Discount rate for costs (d.rc) and effects (d.re)
#' @return The function will automatically run \code{cemtpm()} after finishing
#' 
#' @details 
#' There are multiple software systems that can be used to build Markov models for cost-effectiveness analyses
#' like TreeAge, Excel or R. Although there are numerous advantages to use R over the others, 
#' the biggest downside is the steep learning curve from R. 
#' The \code{cemtool()}  package aims to close this gap by introducing a step-by-step tool
#' to guide users in building a default Markov models. 
#' The tool guides the user though the steps of the development of a Markov model 
#' and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities,
#' no calculations or coding is required.
#' 
#' -- \strong{Required variables in the Global Enviroment before the \code{cemprob()} function will work:}
#' 
#' - Number of healhstates (HS)\cr  
#' - Names of the healthstates (HS1, HS2 ... HSn, dead)\cr  
#' - Names of both the intervention (intervention) and usual care strategy (control)\cr  
#' - Number of cycles (n.t)\cr  
#' - Vector of the strategy names (Strategies)\cr  
#' - Vector of the healhstate names (v.n) \cr  
#' 
#' These can be generated with the \code{cemtool()} function.
#' 
#' @author S.R.W. Wijn MSc <stan.wijn@@radboudumc.nl>
#' @export
#' @examples
#' \dontrun{
#' cemtool() # Start from stratch, please clear the objects from the Global Enviroment
#' cemprob() # Start from the second phase (definding the parameters) 
#' cemtpm()  # Start from the third phase (modify the transition probability matrix)
#' cemrun()  # Run the model with the current m.M markov trace and m.P transition probability matrix
#' }
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
  #modelinput <<- TPI()
  assign('modelinput', TPI(), envir = cemtool.env)
  # create empty Markov trace
  #m.M_treatment <<- m.M <<- matrix(NA, nrow = n.t + 1, ncol = n.s, dimnames = list(paste("cycle", 0:n.t, sep = " "), v.n))
  assign('m.M_treatment', matrix(NA, nrow = n.t + 1, ncol = n.s, dimnames = list(paste("cycle", 0:n.t, sep = " "), v.n)), envir = cemtool.env)
  assign('m.M', matrix(NA, nrow = n.t + 1, ncol = n.s, dimnames = list(paste("cycle", 0:n.t, sep = " "), v.n)), envir = cemtool.env)
 
  m.M[1,] <- m.M_treatment[1,] <- c(1, rep(0, times = HS-1))
  
  
  cat("--------------------------------------------", "\n")
  
  runtable <- function(){
    editTable(modelinput)
    
    if(interactive()) readkey()
    
    cat("--------------------------------------------", "\n")
  }
  
  runtable()
  
  # define discount function
  assign(v.dwc, 1 / ((1 + d.rc) ^ (0:n.t)), envir = cemtool.env)
  assign(v.dwe, 1 / ((1 + d.re) ^ (0:n.t)), envir = cemtool.env)

  input <- modelinput
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
  
 # modelinput <<- input
  assign('modelinput', input, envir = cemtool.env)
  assign('m.P', TMB(input[1,]), envir = cemtool.env)
  assign('m.P_treatment', TMB(input[2,]), envir = cemtool.env)
  second(HS)
  assign('plot1', recordPlot(second(HS)), envir = cemtool.env)
 # plot1 <- recordPlot(second(HS))
  
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


#' OPTIONAL: Third step of the cemtool: Alter the transition probability matrix
#' 
#'  
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model.
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required.
#' 
#' @return The following variables will be created in the Global Enviroment:
#' @return --- Modified transition probability matrix for both strategies (M.P and m.P_treatment).
#' @return The function will automatically run \code{cemrun()} after finishing
#' 
#' @details 
#' There are multiple software systems that can be used to build Markov models for cost-effectiveness analyses
#' like TreeAge, Excel or R. Although there are numerous advantages to use R over the others, 
#' the biggest downside is the steep learning curve from R. 
#' The \code{cemtool()}  package aims to close this gap by introducing a step-by-step tool
#' to guide users in building a default Markov models. 
#' The tool guides the user though the steps of the development of a Markov model 
#' and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities,
#' no calculations or coding is required.
#' 
#' -- \strong{Required variables in the Global Enviroment before the \code{cemprob()} function will work:}
#' 
#' - Number of healhstates (HS)\cr  
#' - Names of the healthstates (HS1, HS2 ... HSn, dead)\cr  
#' - Names of both the intervention (intervention) and usual care strategy (control)\cr  
#' - Number of cycles (n.t)\cr  
#' - Vector of the strategy names (Strategies)\cr  
#' - Vector of the healhstate names (v.n) \cr  
#' 
#' These can be generated with the \code{cemtool()} function.
#' 
#' @author S.R.W. Wijn MSc <stan.wijn@@radboudumc.nl>
#' @export
#' @examples
#' \dontrun{
#' cemtool() # Start from stratch, please clear the objects from the Global Enviroment
#' cemprob() # Start from the second phase (definding the parameters) 
#' cemtpm()  # Start from the third phase (modify the transition probability matrix)
#' cemrun()  # Run the model with the current m.M markov trace and m.P transition probability matrix
#' }
#' 
cemtpm <- function(){
  if(interactive()) readkey()
  editmatrix(m.P)
  
  if(interactive()) readkey()
  
  cat("--------------------------------------------", "\n")
  cemrun()
  
}




