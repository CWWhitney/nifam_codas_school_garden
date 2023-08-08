# function to make variables and test the model
# used only in model construction

# use random function from another script
# figure out random functions (a family in decisionSupport)
# source("functions/random.default.r")
# source("functions/random.R")

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
