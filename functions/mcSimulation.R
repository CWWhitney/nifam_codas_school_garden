# Monte Carlo Simulation
library(decisionSupport)
# use random function from another script
# figure out random functions (a family in decisionSupport)
# source("functions/random.default.r")
# source("functions/random.R")
source("functions/estimate_read_csv.R")
# MC Simulation function
mcSimulation <- function (estimate, model_function, ..., numberOfModelRuns, randomMethod = "calculate", 
          functionSyntax = "data.frameNames", relativeTolerance = 0.05, 
          verbosity = 0) 
{
  x <- decisionSupport::random(rho = estimate, n = numberOfModelRuns, method = randomMethod, 
              relativeTolerance = relativeTolerance)
  if (functionSyntax == "data.frameNames") {
    y <- model_function(as.data.frame(x), ...)
  }
  else if (functionSyntax == "matrixNames") {
    y <- model_function(as.matrix(x), ...)
  }
  else if (functionSyntax == "plainNamesDeprecated") {
    warning("functionSyntax=\"plainNamesDeprecated\" is deprecated. Please use \n             functionSyntax=\"plainNames\" instead.")
    y <- do.call(what = rbind, args = lapply(X = apply(X = x, 
                                                       MARGIN = 1, FUN = model_function), FUN = unlist))
    if (any(colnames(y) == as.character(1:ncol(y)))) 
      colnames(y) <- paste("output_", c(1:ncol(y)), sep = "")
  }
  else if (functionSyntax == "plainNames") {
    model_function_ <- function(x) {
      e <- as.list(sapply(X = row.names(estimate), FUN = function(i) as.numeric(x[i])))
      eval(expr = body(model_function), envir = e)
    }
    y <- do.call(what = rbind, args = lapply(X = apply(X = x, 
                                                       MARGIN = 1, FUN = model_function_), FUN = unlist))
    if (any(colnames(y) == as.character(1:ncol(y)))) 
      colnames(y) <- paste("output_", c(1:ncol(y)), sep = "")
  }
  else stop("functionSyntax=", functionSyntax, "is not defined!")
  if (is.null(names(y)) && is.null(colnames(y))) {
    if (is.null(ncol(y))) {
      y <- data.frame(y)
      colnames(y) <- paste("output_", c(1:ncol(y)), sep = "")
    }
    else {
      colnames(y) <- paste("output_", c(1:ncol(y)), sep = "")
    }
  }
  returnObject <- list(y = data.frame(y), x = data.frame(x))
  returnObject$call <- match.call()
  class(returnObject) <- cbind("mcSimulation", class(returnObject))
  return(returnObject)
}
