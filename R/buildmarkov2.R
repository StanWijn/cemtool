########  Cost effectiveness model  #############
########      S.R.W. Wijn MSc, 2018     #############
#################################################
#Markov model in R: Deterministic


cemrun <-function(){
  input <- modelinput2


    for (t in 1:n.t){
      ######### using transition matrices ###########
      # calculate the proportion of the cohort in each state at time t
      m.M[t + 1, ] <- t(m.M[t, ])    %*% m.P
      m.M_treatment[t + 1, ] <- t(m.M_treatment[t, ])    %*% m.P_treatment
     # close the loop
  }



  matplot(0:n.t, m.M, type = 'l',
          ylab = "Probability of state occupancy",
          xlab = "Cycle",
          main = "Markov Trace")
  legend("topright", v.n, col = 1:n.s,lty = 1:n.s, bty = "n")  # add a legend to the graph
  cat("Inspect the Markov Trace in the plot window to the right and check if is as you expected.")

  if(interactive()) readkey()

  # calculate discounted costs and effects

  inputtable <- input

  if(HS==3){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2,"u.absorb"])
  } else if (HS==4){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2,"u.absorb"])

  } else if (HS==5){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1, "c.4"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1, "u.4"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2, "c.4"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2, "u.4"], inputtable[2,"u.absorb"])
  } else if (HS==6){
    t.c    <- c(inputtable[1,"c.1"],inputtable[1,"c.2"], inputtable[1, "c.3"], inputtable[1, "c.4"], inputtable[1, "c.5"], inputtable[1,"c.absorb"])
    t.u    <- c(inputtable[1,"u.1"],inputtable[1,"u.2"], inputtable[1, "u.3"], inputtable[1, "u.4"], inputtable[1, "u.5"], inputtable[1,"u.absorb"])
    t.c_tr <- c(inputtable[2,"c.1"],inputtable[2,"c.2"], inputtable[2, "c.3"], inputtable[2, "c.4"], inputtable[2, "c.5"], inputtable[2,"c.absorb"])
    t.u_tr <- c(inputtable[2,"u.1"],inputtable[2,"u.2"], inputtable[2, "u.3"], inputtable[2, "u.4"], inputtable[2, "u.5"], inputtable[2,"u.absorb"])
  }

  t.c <- m.M %*% t.c
  t.c_tr <- m.M_treatment %*% t.c_tr
  t.u <- m.M %*% t.u
  t.u_tr <- m.M_treatment %*% t.u_tr

  t.c <- t(t.c) %*% v.dwc
  t.c <- t.c + inputtable[1, "c.Tr"]
  t.c_tr <- t(t.c_tr) %*% v.dwc
  t.c_tr <- t.c_tr + inputtable[2, "c.Tr"]
  t.e <- t(t.u) %*% v.dwe
  t.e_tr <- t(t.u_tr) %*% v.dwe

  # calculate lifelong per patient discounted cost and QALYs.
  costdiff    <- t.c_tr - t.c      # calculate the difference in discounted costs between the two strategies
  names(costdiff)   <- "Incremental costs"
  effectdiff <-  t.e_tr - t.e      # calculate the difference in discounted effects between the two strategies
  names(effectdiff)   <- "QALYs gained"
  ICER        <- costdiff / effectdiff                 # calculate the ICER
  names(ICER) <- "ICER"
  results     <- c(costdiff, effectdiff, ICER)         # combine the results

  # create full incremental cost-effectiveness analysis table
  C <- round(c(t.c, t.c_tr), 2)  # bind and round the total costs of the two strategies
  E <- round(c(t.e, t.e_tr), 2)  # bind and round the total effects of the two strategies

  costdiff   <- c("", as.character(round(costdiff, 2)))   # round the delta of the costs (No Treatment is reference)
  effectdiff   <- c("", as.character(round(effectdiff, 2)))   # round the delta of the effects (No Treatment is reference)
  ICER <- c("", as.character(round(ICER, 2))) # round the ICER

  table_output <- cbind(Strategies, C, E, costdiff, effectdiff, ICER)     # combine all data in a table
  table_output <- as.data.frame(table_output)    # create a data frame
                                              # print the table
  m.M <<- m.M
  m.M_treatment <<- m.M_treatment
  table_output <<- table_output
  cat("--------------------------------------------", "\n")
  cat("You can find the result table under table_output in your Global Enviroment. 
      m.M and m.M_treatment show the Markov trace
      m.P and m.P_treatment show the transition probability matrix", "\n",
      
      "To rerun the analysis with your own alterations type: cemrun() and press enter in the console", " \n")
  cat("--------------------------------------------", "\n")
  cat("Result table:", "\n")
  return(table_output)
  cat("--------------------------------------------", "\n")
  cat("Finished", "\n")
  }



