
#' Second step of the cemtool: Define the transition probabilities, cost and utility values
#' 
#' 
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model. 
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. 
#' 
#' @return The following variables will be created in the saved in the cemtool enviroment:
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
#' cemtool() # Start from stratch (clear the cemtool environment from the Global Environment)
#' cemprob() # Start from the second phase (definding the parameters) 
#' cemtpm()  # Start from the third phase (modify the transition probability matrix)
#' cemrun()  # Run the model with the current m.M markov trace and m.P transition probability matrix
#' 
#' cemtool.env <- cemtool() # To save all input for further modification
#' }
cemprob <-function(HS = cemtool.env$HS, HS1 = cemtool.env$HS1, HS2 = cemtool.env$HS2, HS3 = cemtool.env$HS3,
                   HS4 = cemtool.env$HS4, HS5 = cemtool.env$HS5, dead = cemtool.env$dead, n.t = cemtool.env$n.t,
                   control = cemtool.env$control, intervention = cemtool.env$intervention, Strategies = cemtool.env$Strategies,
                   v.n = cemtool.env$v.n){ 
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
#' @return The following variables will be created in the saved in the cemtool enviroment:
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
                   control = cemtool.env$control, intervention = cemtool.env$intervention, Strategies = cemtool.env$Strategies,
                   v.n = cemtool.env$v.n,
                   d.rc = cemtool.env$d.rc, v.dwc = cemtool.env$v.dwc, d.re = cemtool.env$d.rc, v.dwe = cemtool.env$v.dwe,
                   m.M = cemtool.env$m.M, m.M_treatment = cemtool.env$m.M_treatment,
                   m.P = cemtool.env$m.P, m.P_treatment = cemtool.env$m.P_treatment, modelinput = cemtool.env$modelinput,
                   plot1 = cemtool.env$plot1){
  cemtool_step3()
  return(cemtool.env)
}




