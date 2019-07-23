.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to 'cemtool': the interactive cost-effectiveness model tool build by Radboudumc.
                        Enter 'cemtool()' in the console to start
                        To save the results: cemtool.env <- cemtool()")
}