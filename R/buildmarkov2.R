
#' Run the Markov model with input generated from \code{cemtool()}, \code{cemprob()},\code{cemtpm()}
#' 
#' 
#' The \code{cemtool()} package provides a step-by-step tool to guide users in building a default Markov model.
#' The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. 
#' The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required.
#'
#' @return The following variables will be created in the Global Enviroment:
#' @return --- Full Markov trace (m.M and m.M_treatment)
#' @return --- Calculate the costs and effects for both strategies. 
#' @return --- Results are saved (table_output) and shown in the console. 
#' @return --- The model structure and markov trace are both plotted and saved (plot1 and plot2)
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
#' - Number of healhstates (HS) \cr  
#' - Names of the healthstates (HS1, HS2 ... HSn, dead) \cr  
#' - Names of both the intervention (intervention) and usual care strategy (control) \cr  
#' - Number of cycles (n.t)\cr  
#' - A vector of the strategy names (Strategies)\cr  
#' - A vector of the healhstate names (v.n) \cr
#' 
#' These can be generated with the \code{cemtool()} function.
#' 
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
cemrun <-function(HS = cemtool.env$HS, HS1 = cemtool.env$HS1, HS2 = cemtool.env$HS2, HS3 = cemtool.env$HS3,
                  HS4 = cemtool.env$HS4, HS5 = cemtool.env$HS5, dead = cemtool.env$dead, n.t = cemtool.env$n.t,
                  control = cemtool.env$control, intervention = cemtool.env$intervention, Strategies = cemtool.env$Strategies,
                  v.n = cemtool.env$v.n,
                  d.rc = cemtool.env$d.rc, v.dwc = cemtool.env$v.dwc, d.re = cemtool.env$d.rc, v.dwe = cemtool.env$v.dwe,
                  m.M = cemtool.env$m.M, m.M_treatment = cemtool.env$m.M_treatment,
                  m.P = cemtool.env$m.P, m.P_treatment = cemtool.env$m.P_treatment, modelinput = cemtool.env$modelinput,
                  plot1 = cemtool.env$plot1){
  input <- cemtool.env$modelinput


    for (t in 1:cemtool.env$n.t){
      ######### using transition matrices ###########
      # calculate the proportion of the cohort in each state at time t
      cemtool.env$m.M[t + 1, ] <- t(cemtool.env$m.M[t, ])    %*% cemtool.env$m.P
      cemtool.env$m.M_treatment[t + 1, ] <- t(cemtool.env$m.M_treatment[t, ])    %*% cemtool.env$m.P_treatment

  }

 
  matplot(0:cemtool.env$n.t, cemtool.env$m.M, type = 'l',
          ylab = "Probability of state occupancy",
          xlab = "Cycle",
          main = "Markov Trace")
  legend("topright", cemtool.env$v.n, col = 1:cemtool.env$HS,lty = 1:cemtool.env$HS, bty = "n")  # add a legend to the graph
  assign('plot2', recordPlot(), envir = cemtool.env)

  cat("Inspect the Markov Trace (of the usual care strategy) in the plot window to the right and check if is as you expected.")
 
  if(interactive()) readkey()

  # calculate discounted costs and effects

  inputtable <- input

  if(cemtool.env$HS==3){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2,"u.absorb"])
  } else if (cemtool.env$HS==4){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2,"u.absorb"])

  } else if (cemtool.env$HS==5){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1, "c.4"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1, "u.4"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2, "c.4"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2, "u.4"], inputtable[2,"u.absorb"])
  } else if (cemtool.env$HS==6){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1, "c.4"], inputtable[1, "c.5"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1, "u.4"], inputtable[1, "u.5"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2, "c.4"], inputtable[2, "c.5"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2, "u.4"], inputtable[2, "u.5"], inputtable[2,"u.absorb"])
  }

  t.c <- cemtool.env$m.M %*% t.c
  t.c_tr <- cemtool.env$m.M_treatment %*% t.c_tr
  t.u <- cemtool.env$m.M %*% t.u
  t.u_tr <- cemtool.env$m.M_treatment %*% t.u_tr

  t.c <- t(t.c) %*% cemtool.env$v.dwc
  t.c <- t.c + inputtable[1, "c.Tr"]
  t.c_tr <- t(t.c_tr) %*% cemtool.env$v.dwc
  t.c_tr <- t.c_tr + inputtable[2, "c.Tr"]
  t.e <- t(t.u) %*% cemtool.env$v.dwe
  t.e_tr <- t(t.u_tr) %*% cemtool.env$v.dwe

  # calculate lifelong per patient discounted cost and QALYs.
  costdiff    <- t.c_tr - t.c      # calculate the difference in discounted costs between the two strategies
  names(costdiff)   <- ""
  effectdiff <-  t.e_tr - t.e      # calculate the difference in discounted effects between the two strategies
  names(effectdiff)   <- ""
  ICER        <- costdiff / effectdiff                 # calculate the ICER
  names(ICER) <- "ICER"
  results     <- c(costdiff, effectdiff, ICER)         # combine the results

  # create full incremental cost-effectiveness analysis table
  C <- round(c(t.c, t.c_tr), 2)  # bind and round the total costs of the two strategies
  E <- round(c(t.e, t.e_tr), 2)  # bind and round the total effects of the two strategies

  costdiff   <- c("", as.character(round(costdiff, 2)))   # round the delta of the costs (No Treatment is reference)
  effectdiff   <- c("", as.character(round(effectdiff, 2)))   # round the delta of the effects (No Treatment is reference)
  ICER <- c("", as.character(round(ICER, 2))) # round the ICER

  table_output <- cbind(cemtool.env$Strategies, C, E, costdiff, effectdiff, ICER)     # combine all data in a table
  table_output <- as.data.frame(table_output)    # create a data frame
  names(table_output) <- c("", "Cost", "QALY",  "Costs diff", "QALYs diff", "ICER")
  cemtool.env$table_output <- table_output  

  cat("--------------------------------------------", "\n")
  cat("Results:", "\n")
  print(table_output)
  cat("\n")
  cat("--------------------------------------------", "\n")
  if(interactive()) readkey()
  
  cat("To rerun the analysis with alterations: 
      Type cemprob() in the console and press enter to redo step 2 (Markov model input)
      Type cemtpm() in the console and press enter to redo step 3 (Transition probability matrix)
      Type cemrun() in the console and press enter to run the model with the existing m.M and m.P tables (which you can alter by yourself)", " \n",
      "To start over, type cemtool() in the console and press enter", "\n",
      "To save the results: cemtool.env <- cemtool()", "\n",
      "\n",
      "\n",
      "When you save the results, cemtool.env$table_output will show the result table. 
      cemtool.env$plot1 and cemtool.env$plot2 include the model structure and Markov trace.
      cemtool.env$m.M and cemtool.env$m.M_treatment show the Markov trace
      cemtool.env$m.P and cemtool.env$m.P_treatment show the transition probability matrix", "\n")
     
  cat("--------------------------------------------", "\n")
 
  cat("Finished", "\n")
  return(cemtool.env)
  }



