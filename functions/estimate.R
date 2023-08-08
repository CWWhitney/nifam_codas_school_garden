# for use in the as.estimate function
estimate <- function (distribution, lower, upper, ..., correlation_matrix = NULL) 
{
  if (any(is.null(distribution) || !is.character(distribution))) 
    stop("\"distribution\" must be supplied as character string.")
  if (any(is.null(lower) || any(is.na(lower <- as.numeric(lower))))) 
    stop("\"lower\" must be supplied as numeric.")
  if (any(is.null(upper) || any(is.na(upper <- as.numeric(upper))))) 
    stop("\"upper\" must be supplied as numeric.")
  if (length(distribution) != length(lower) || length(distribution) != 
      length(upper)) 
    stop("All input dimensions must be equal.")
  if (any(lower > upper)) 
    stop("\"lower > upper\" for some variables")
  marginalOptional <- if (missing(...)) 
    NULL
  else data.frame(..., stringsAsFactors = FALSE)
  median <- NULL
  if (!is.null(marginalOptional)) 
    for (i in names(marginalOptional)) {
      if (i == "variable" && any(!is.character(marginalOptional[[i]]))) 
        stop("Optional argument \"", i, "\" is not character for all entries.")
      if (i == "method" && any(!is.character(marginalOptional[[i]]))) 
        stop("Optional argument \"", i, "\" is not character for all entries.")
      if (i == "median") {
        median <- marginalOptional[[i]]
        marginalOptional <- marginalOptional[!names(marginalOptional) %in% 
                                               "median"]
        if (!is.null(median)) {
          if (length(distribution) != length(median)) 
            stop("\"median\" is not of the right length.")
          median[!is.na(median) & is.character(median) & 
                   median == "mean"] <- rowMeans(cbind(lower, 
                                                       upper))[!is.na(median) & is.character(median) & 
                                                                 median == "mean"]
          median[!is.na(median) & is.character(median) & 
                   median == ""] <- rep(NA, length(distribution))[!is.na(median) & 
                                                                    is.character(median) & median == ""]
          if (any(is.na(median) & !is.na(median <- as.numeric(median)))) 
            stop("\"median\": all values must be one of the following: \"numeric\", \"character\" \n                 with value \"mean\", \"NA\" or \"\".")
          else if (any(!is.na(median) & (lower > median | 
                                         median > upper))) 
            stop("It must hold: \"lower <= median <= upper\" for all entries")
        }
        else median <- rep(NA, length(distribution))
      }
    }
  if (is.null(median)) 
    median <- rep(NA, length(distribution))
  if (!is.null(correlation_matrix)) {
    if (!is.matrix(correlation_matrix)) 
      correlation_matrix <- as.matrix(correlation_matrix)
    if (!identical(correlation_matrix, t(correlation_matrix))) 
      stop("correlationMatrix must be a symmetric matrix.")
    if (!identical(as.vector(diag(correlation_matrix)), rep(1, 
                                                            nrow(correlation_matrix)))) 
      stop("All diagonal elements of \"correlation_matrix\"  must be equal to 1.")
    if (any(abs(correlation_matrix) > 1)) 
      stop("All values of \"correlation_matrix\" must be  >= -1 and <= 1.")
  }
  if (as.logical(length(marginalOptional))) 
    marginal <- data.frame(distribution = distribution, lower = lower, 
                           median = median, upper = upper, marginalOptional, 
                           row.names = row.names(marginalOptional), stringsAsFactors = FALSE)
  else marginal <- data.frame(distribution = distribution, 
                              lower = lower, median = median, upper = upper, row.names = row.names(marginalOptional), 
                              stringsAsFactors = FALSE)
  if (!is.null(marginal$variable)) {
    rownames(marginal) <- marginal$variable
    marginal <- marginal[!colnames(marginal) %in% "variable"]
  }
  marginal <- subset(marginal, row.names(marginal) != "")
  if (is.null(marginal$distribution)) 
    stop("marginal must be supplied with a distribution column.")
  returnObject = list(marginal = marginal, correlation_matrix = correlation_matrix)
  class(returnObject) <- "estimate"
  returnObject
}