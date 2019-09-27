cemtool_intro <- function(){

  cat("Welcome to 'cemtool': the interactive cost-effectiveness model tool build by Radboudumc.
S.R.W. Wijn (version 0.4)

This tool allows you to build a simple Markov model in R.

If you want to save your results, please cancel this session (by pressing escape) and run: cemtool.env <- cemtool() in the console.

You can cancel the tool at any time by pressing escape.

For more background information, please visit: https://github.com/StanWijn/cemtool
If you find any bugs or errors, please report them to Stan.Wijn@radboudumc.nl" , "\n")

  if(interactive()) readkey()

  cat("--------------------------------------------", "\n")

  cat("
Our tool will guide you through several steps to complete your own cost-effectiveness model.

Keep in mind that a model structure and transition probabilities are required of the Markov model.

Example: the probability that a healthy patient becomes sick. ", "\n")

  if(interactive()) readkey()

  cat("--------------------------------------------", "\n")

  cat("Step 1: Basic model details: ", "\n")

  cat("First you have to define the number of health states of your model (for example: healthy, sick and death),
      the names of those health states and the names of the strategies that you want to compare
        (for example: usual care vs new treatment)", "\n",
      "Press enter to open another window in which you can define the names.
    After you are done, close it to continue", "\n")

  if(interactive()) readkey()

}


cemtool_step2 <- function(){

  cat("--------------------------------------------", "\n")

  cat("Step 2: Collect model input:", "\n")

  cat("We have limited the model input so that patients can only progress to the next state.
    (example: from healthy to sick, but not recover from sick to healthy)", "\n",
      "If you want them to recover, you can alter the transition matrix in step 3: transition probability matrix.", "\n")

  cat("--------------------------------------------", "\n")

  cat("Press enter to open another window in which you can define the transition probabilities,
    the costs and utility value associated with each health state. Close the window when you want to continue.

    Please enter all the probabilities as digits (5% chance to move from healthy to death is 0.05", "\n")
  if(interactive()) readkey()
  cemtool.env$modelinput <- TPI(cemtool.env$HS, cemtool.env$Strategies, cemtool.env$v.n)

  # create empty Markov trace
  assign('m.M_treatment', matrix(NA, nrow = cemtool.env$n.t + 1, ncol = cemtool.env$HS,
                                 dimnames = list(paste("cycle", 0:cemtool.env$n.t, sep = " "), cemtool.env$v.n)), envir = cemtool.env)
  assign('m.M', matrix(NA, nrow = cemtool.env$n.t + 1, ncol = cemtool.env$HS,
                       dimnames = list(paste("cycle", 0:cemtool.env$n.t, sep = " "), cemtool.env$v.n)), envir = cemtool.env)

  cemtool.env$m.M[1,] <- cemtool.env$m.M_treatment[1,] <- c(1, rep(0, times = cemtool.env$HS-1))


  cat("--------------------------------------------", "\n")

  runtable <- function(){
    editTable(cemtool.env$modelinput, cemtool.env$HS, cemtool.env$v.n)

    if(interactive()) readkey()

    cat("--------------------------------------------", "\n")
  }

  runtable()

  # define discount function
  assign('v.dwc', 1 / ((1 + cemtool.env$d.rc) ^ (0:cemtool.env$n.t)), envir = cemtool.env)
  assign('v.dwe', 1 / ((1 + cemtool.env$d.re) ^ (0:cemtool.env$n.t)), envir = cemtool.env)


  if(cemtool.env$HS==3){
    colnames(cemtool.env$modelinput) <- c("p.A", "p.Y", "p.Z",
                                          'c.1', 'c.2', 'c.absorb',"c.Tr",
                                          'u.1', 'u.2', 'u.absorb')
  } else if(cemtool.env$HS==4){
    colnames(cemtool.env$modelinput) <- c("p.A", "p.B", "p.C", "p.X", "p.Y", "p.Z",
                                          'c.1', 'c.2', 'c.3', 'c.absorb','c.Tr',
                                          'u.1', 'u.2', 'u.3', 'u.absorb')
  } else if (cemtool.env$HS==5){
    colnames(cemtool.env$modelinput) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.W', "p.X", "p.Y", "p.Z",
                                          'c.1', 'c.2', 'c.3', 'c.4', 'c.absorb','c.Tr',
                                          'u.1', 'u.2', 'u.3', 'u.4', 'u.absorb')
  } else if (cemtool.env$HS==6){
    colnames(cemtool.env$modelinput) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.G', "p.H", "p.I", "p.J", 'p.V', 'p.W', "p.X", "p.Y", "p.Z",
                                          'c.1', 'c.2', 'c.3', 'c.4', 'c.5', 'c.absorb','c.Tr',
                                          'u.1', 'u.2', 'u.3', 'u.4', 'u.5', 'u.absorb')
  }


  assign('m.P', TMB(cemtool.env$modelinput[1,], cemtool.env$v.n, cemtool.env$HS), envir = cemtool.env)
  assign('m.P_treatment', TMB(cemtool.env$modelinput[2,], cemtool.env$v.n, cemtool.env$HS), envir = cemtool.env)
  second(cemtool.env$HS, cemtool.env$v.n)
  assign('plot1', recordPlot(second(cemtool.env$HS, cemtool.env$v.n)), envir = cemtool.env)

}


cemtool_step3 <- function(HS = cemtool.env$HS,
                          HS1 = cemtool.env$HS1,
                          HS2 = cemtool.env$HS2,
                          HS3 = cemtool.env$HS3,
                          HS4 = cemtool.env$HS4,
                          HS5 = cemtool.env$HS5,
                          dead = cemtool.env$dead,
                          n.t = cemtool.env$n.t,
                          control = cemtool.env$control,
                          intervention = cemtool.env$intervention,
                          Strategies = cemtool.env$Strategies,
                          v.n = cemtool.env$v.n){
  cat("OPTIONAL: Do you want to alter the transition probability matrix? Yes / No", "\n")
  prompt_matrix <- if (interactive())  askYesNo("OPTIONAL: Do you want to alter the transition probability matrix?", "\n")


  if(prompt_matrix == FALSE | is.na(prompt_matrix)){
    cat("--------------------------------------------", "\n")
    cemrun()
  } else if(prompt_matrix == TRUE){
    editmatrix(cemtool.env$m.P, cemtool.env$m.P_treatment)
    if(interactive()) readkey()

    cat("--------------------------------------------", "\n")
    cemrun()
  }
}
