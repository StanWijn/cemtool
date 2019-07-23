
#' Default function to start the cemtool package
#' 
#'
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model.
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required.
#' 
#' @return The following variables will be created in the saved in the cemtool environment:
#' @return --- Number of healhstates (HS)
#' @return --- Names of the healthstates (HS1, HS2 ... HSn, dead)
#' @return --- Names of both the intervention (intervention) and usual care strategy (control)
#' @return --- Number of cycles (n.t)
#' @return --- Vector of the strategy names (Strategies) and a vector of the healhstate names (v.n). 
#' 
#' @return The function will automatically run \code{cemprob()} after finishing
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
#' \cr
#' 
#' -- \strong{Required knowledge / information:}
#' 
#' \emph{Model structure} \cr  
#' The structure of the Markov model: how many healthstates, the names of the healthstates etc.
#' 
#' \emph{Transition probabilities}  \cr  
#' The probabilities to move from one healthstate to another
#' 
#' \emph{Costs} \cr  
#' The costs for a patient to be on the healhstates for one cycle / year
#' 
#' \emph{Effects} \cr  
#' The effect size associated with the healthstate. For QALY a value between 1 and 0, from perfect health to death.
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
#' 

cemtool <-function(){ 
  cemtool_intro()
  basic()
  cemtool_step2()
  cemtool_step3()
  return(cemtool.env)
  }


utils::globalVariables(c("HS", "Strategies","v.n", "control", "intervention",
                        "HS1", "HS2", "HS3", "HS4", "HS5", "HS6",
                         "n.t",  "m.M", "m.M_treatment",
                        "modelinput", "d.rc", "d.re", "m.P", 
                        "m.P_treatment", "dead", 'plot1', 'plot2',
                        'v.dwc', 'v.dwe'))

cemtool.env <- new.env()


