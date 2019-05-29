########  Cost effectiveness model  #############
########      S.R.W. Wijn MSc, 2018     #############
#################################################
#Markov model in R: Deterministic
#' @export
#'


cemtool <-function(){ 

cat("Welcome to 'cemtool': the interactive cost-effectiveness model tool build by Radboudumc.
S.R.W. Wijn (version 0.2)

This tool allows you to build a simple Markov model in R.

For more background information, please visit: https://github.com/StanWijn/cemtool
If you find any bugs or errors, please report them to Stan.Wijn@radboudumc.nl")

if(interactive()) readkey()

cat("--------------------------------------------", "\n")

cat("
Our tool will guide you through several steps to complete your own model.
Keep in mind that you already need to have a model structure and the transition probabilities of the Markov model.

Example: the probability that a healthy patient becomes sick. ", "\n")

if(interactive()) readkey()

cat("--------------------------------------------", "\n")

cat("
Step 1: Basic model details: ")

#input healhstates:
cat("
Currently it is only possible to build 3 to 6 healthstate models, these include a start and death state.", "\n")
HS <<- as.numeric(readline("How many healthstates does the model have?  "))

first(HS)
cat("You have selected", HS, "healthstates. Now name the states and provide basis information of your model.", "\n")

if(interactive()) readkey()

cat("--------------------------------------------", "\n")

# Starting variables. The amount of cycles and population size.
HSN ()

cat("--------------------------------------------", "\n")

cat("Step 2: Collect model input.", "\n")

cat("Currently, we have limited the transition matrix that patients can only progress to the next state.", "\n",

"If you want them to recover, you can alter the transition matrix at the end.", "\n")

if(interactive()) readkey()

cat("--------------------------------------------", "\n")

cat("Please enter all the probabilities as digits. 5% chance to move from healthy to death is thus: 0.05.", "\n")
if(interactive()) readkey()

cat("--------------------------------------------", "\n")
modelinput <<- TPI()

d.rc <- as.numeric(readline("What is the discount rate for costs? (as decimals: 3% -> 0.03)   "))
v.dwc <<-  1 / ((1 + d.rc) ^ (0:n.t))
d.re <- as.numeric(readline("What is the discount rate for effects/utilities? (as decimals: 3% -> 0.03)    "))
v.dwe <<-  1 / ((1 + d.re) ^ (0:n.t))

# create Markov trace
m.M_treatment <<- m.M <<- matrix(NA, nrow = n.t + 1, ncol = n.s, dimnames = list(paste("cycle", 0:n.t, sep = " "), v.n))
m.M[1,] <<- m.M_treatment[1,] <<- c(1, rep(0, times = HS-1))

cat("The model input has been collected.
    Please insert a treatment effect, either in the transition probabilities or in the costs and/or QALYS.
    You can edit both the model input and transition matrix of both strategies by opening: input.
    Example: If you want to change the costs of the intervention for sick patients and increase the utility of that state.
    When you press enter the model input table will open so you can alter the parameters.
    When you are ready, close the pop-up.")


#modelinput

if(interactive()) readkey()

cat("--------------------------------------------", "\n")

runtable <- function(){
  editTable(modelinput)
  #modelinput2 <- table.txt
  if(interactive()) readkey()

  cat("--------------------------------------------", "\n")
}

runtable()


cat("--------------------------------------------", "\n")

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
                       'u.1', 'u.2', 'u.3', 'u.4', 'u.absorb'
  )

} else if (HS==6){
  colnames(input) <- c("p.A", "p.B", "p.C", 'p.D', 'p.E', 'p.F', 'p.G', "p.H", "p.I", "p.J",
                       'c.1', 'c.2', 'c.3', 'c.4', 'c.5', 'c.absorb','c.Tr',
                       'u.1', 'u.2', 'u.3', 'u.4', 'u.5', 'u.absorb'
  )
}

modelinput2 <<- input
m.P <<-TMB(input[1,])
m.P_treatment <<- TMB(input[2,])


cat("Do you want to alter the transition probability matrix? Yes / No", "\n")
prompt_matrix <- if (interactive())  askYesNo("Do you want to alter the transition probability matrix?", "\n")


if(prompt_matrix == FALSE | is.na(prompt_matrix)){
  ceomrun()
} else if(prompt_matrix == TRUE){
  editmatrix(m.P)
  if(interactive()) readkey()

  cat("--------------------------------------------", "\n")
  ceomrun()
}

}

