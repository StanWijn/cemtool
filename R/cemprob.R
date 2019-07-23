
#' Second step of the cemtool: Define the transition probabilities, cost and utility values
#' 
#' 
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model. 
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. 
#' 
#' @param HS Number of healthstates
#' @param HS1 String with name of healthstate 1
#' @param HS2 String with name of healthstate 2
#' @param HS3 String with name of healthstate 3
#' @param HS4 String with name of healthstate 4
#' @param HS5 String with name of healthstate 5
#' @param dead String with name of absorption / death state
#' @param n.t Number of cycles
#' @param control String with name of the usual care strategy
#' @param intervention String with name of the intervention strategy
#' 
#' @return The following variables will be created in the saved in the cemtool environment:
#' @return --- Empty Markov trace matrices for both strategies (m.M and m.M_treatment)
#' @return --- A dataframe with the modelinput (modelinput)
#' @return --- Discount rate for costs (d.rc) and effects (d.re)
#' @return The function will automatically run \code{cemtpm()} after finishing
#' 
#' @details 
#' All input arguments can be generated and saved with the \code{cemtool()} function. (cemtool.env <- cemtool())
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
#' 
#' @author S.R.W. Wijn MSc <stan.wijn@@radboudumc.nl>
#' @export
#' @examples
#' \dontrun{
#' cemtool() # Start from stratch (clear the cemtool environment from the Global Environment)
#' cemprob() # Start from the second phase (definding the parameters) 
#' cemtpm()  # Start from the third phase (modify the transition probability matrix)
#' cemrun()  # Run the model with the current m.M markov trace and m.P transition probability matrix
#' 
#' cemtool.env <- cemtool() # To save all input for further modification
#' }
cemprob <-function(HS = cemtool.env$HS, HS1 = cemtool.env$HS1, HS2 = cemtool.env$HS2, HS3 = cemtool.env$HS3,
                   HS4 = cemtool.env$HS4, HS5 = cemtool.env$HS5, dead = cemtool.env$dead, n.t = cemtool.env$n.t,
                   control = cemtool.env$control, intervention = cemtool.env$intervention){ 
  cemtool.env$Strategies <-  c(control, intervention)
   if(HS==3){
    cemtool.env$v.n <- c(HS1, HS2, dead)
  } else if(HS==4){
    cemtool.env$v.n <- c(HS1, HS2, HS3, dead)
    
  } else if(HS==5){
    cemtool.env$v.n <- c(HS1, HS2, HS3, HS4, dead)
    
  } else if(HS==6){
    cemtool.env$v.n <- c(HS1, HS2, HS3,
                         HS4, HS5, dead)}
  cemtool_step2()
  cemtool_step3()
  return(cemtool.env)
}


#' OPTIONAL: Third step of the cemtool: Alter the transition probability matrix
#' 
#'  
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model.
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. 
#' 
#' @param HS Number of healthstates
#' @param HS1 String with name of healthstate 1
#' @param HS2 String with name of healthstate 2
#' @param HS3 String with name of healthstate 3
#' @param HS4 String with name of healthstate 4
#' @param HS5 String with name of healthstate 5
#' @param dead String with name of absorption / death state
#' @param n.t Number of cycles
#' @param control String with name of the usual care strategy
#' @param intervention String with name of the intervention strategy
#' @param d.rc Discount rate for costs
#' @param d.re Discount rate for effects
#' @param m.M Matrix showing the Markov trace of usual care, nrow = n.t + 1, ncol = HS
#' @param m.M_treatment Matrix showing the Markov trace of intervention strategy, nrow = n.t + 1, ncol = HS
#' @param m.P Matrix showing the transition probability matrix of the usual care, nrow = HS, ncol = HS
#' @param m.P_treatment Matrix showing the transition probability matrix of the intervention strategy, nrow = HS, ncol = HS
#' @param modelinput Matrix with 2 rows that include all the transition probabilities, costs and effects.
#' 
#' @return The following variables will be created in the saved in the cemtool environment:
#' @return --- Modified transition probability matrix for both strategies (M.P and m.P_treatment).
#' @return The function will automatically run \code{cemrun()} after finishing
#' 
#' 
#' @details 
#' All input arguments can be generated and saved with the \code{cemtool()} function. (cemtool.env <- cemtool())
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
#' 
#' @author S.R.W. Wijn MSc <stan.wijn@@radboudumc.nl>
#' @export
#' @examples
#' \dontrun{
#' cemtool() # Start from stratch (clear the cemtool environment from the Global Environment)
#' cemprob() # Start from the second phase (definding the parameters) 
#' cemtpm()  # Start from the third phase (modify the transition probability matrix)
#' cemrun()  # Run the model with the current m.M markov trace and m.P transition probability matrix
#' 
#' cemtool.env <- cemtool() # To save all input for further modification
#' }
#' 
cemtpm <- function(HS = cemtool.env$HS, HS1 = cemtool.env$HS1, HS2 = cemtool.env$HS2, HS3 = cemtool.env$HS3,
                   HS4 = cemtool.env$HS4, HS5 = cemtool.env$HS5, dead = cemtool.env$dead, n.t = cemtool.env$n.t,
                   control = cemtool.env$control, intervention = cemtool.env$intervention, 
                   d.rc = cemtool.env$d.rc, d.re = cemtool.env$d.rc, 
                   m.M = cemtool.env$m.M, m.M_treatment = cemtool.env$m.M_treatment,
                   m.P = cemtool.env$m.P, m.P_treatment = cemtool.env$m.P_treatment, modelinput = cemtool.env$modelinput){
  cemtool.env$Strategies <-  c(control, intervention)
  cemtool.env$v.dwc <- 1 / ((1 + d.rc) ^ (0:n.t))
  cemtool.env$v.dwe <- 1 / ((1 + d.re) ^ (0:n.t))
  if(HS==3){
    cemtool.env$v.n <- c(HS1, HS2, dead)
  } else if(HS==4){
    cemtool.env$v.n <- c(HS1, HS2, HS3, dead)
    
  } else if(HS==5){
    cemtool.env$v.n <- c(HS1, HS2, HS3, HS4, dead)
    
  } else if(HS==6){
    cemtool.env$v.n <- c(HS1, HS2, HS3,
                         HS4, HS5, dead)}

  
  
  cemtool_step3()
  return(cemtool.env)
}




