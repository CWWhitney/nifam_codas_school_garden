# For use in the read csv functions
# read the estimate functionfrom anotehr file
source("functions/estimate.R")

as.estimate <- function (..., correlation_matrix = NULL) 
{
  marginal <- data.frame(..., stringsAsFactors = FALSE)
  # NULL from base R 
  if (is.null(marginal$distribution)) 
    stop("no \"distribution\" column!")
  if (is.null(marginal$lower)) 
    stop("no \"lower\" column!")
  if (is.null(marginal$upper)) 
    stop("no \"upper\" column!")
  # Use estimate function from another file
  estimate(distribution = marginal[["distribution"]], 
           lower = marginal[["lower"]], 
           upper = marginal[["upper"]], 
      marginal[!names(marginal) %in% c("distribution", "lower", "upper")], 
           correlation_matrix = correlation_matrix)
}