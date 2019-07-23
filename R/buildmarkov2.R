
#' Run the Markov model with input generated from \code{cemtool()}, \code{cemprob()},\code{cemtpm()}
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
#' @return The following variables will be created in the saved in the cemtool enviroment:
#' @return --- Full Markov trace (m.M and m.M_treatment)
#' @return --- Calculate the costs and effects for both strategies. 
#' @return --- Results are saved (table_output) and shown in the console. 
#' @return --- The model structure and markov trace are both plotted and saved (plot1 and plot2)
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
cemrun <-function(HS = cemtool.env$HS, HS1 = cemtool.env$HS1, HS2 = cemtool.env$HS2, HS3 = cemtool.env$HS3,
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



