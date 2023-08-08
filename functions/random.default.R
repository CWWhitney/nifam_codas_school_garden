# Random default function 

random.default <- function (rho = list(distribution = "norm", probabilities = c(0.05, 
                                                                                0.95), quantiles = c(-qnorm(0.95), qnorm(0.95))), n, method = "fit", 
                            relativeTolerance = 0.05, ...) 
{
  if (!is.list(rho)) 
    stop("rho must be a list with elements \"distribution\", \"probabilities\" and \"quantiles\"")
  if (is.null(rho[["distribution"]])) 
    stop("rho[[\"distribution\"]] must be supplied.")
  if (is.null(rho[["probabilities"]]) || !all(!is.na(as.numeric(rho[["probabilities"]])))) 
    stop("rho[\"probabilities\"] must be supplied.")
  if (is.null(rho[["quantiles"]]) || !all(!is.na(as.numeric(rho[["quantiles"]])))) 
    stop("rho[\"quantiles\"] must be supplied.")
  if (length(rho[["probabilities"]]) != length(rho[["quantiles"]])) 
    stop("length(rho[[\"probabilities\"]])!=length(rho[[\"quantiles\"]])")
  if (match(rho["distribution"], "const", nomatch = 0)) {
    stop("const not implemented, yet")
  }
  else if (method == "fit") {
    if (match(rho["distribution"], c("norm", "beta", "cauchy", 
                                     "logis", "t", "chisq", "exp", "f", "gamma", "lnorm", 
                                     "unif", "weibull", "triang", "gompertz"), nomatch = 0)) {
      x <- rdistq_fit(distribution = rho["distribution"], 
                      n = n, percentiles = as.numeric(rho[["probabilities"]]), 
                      quantiles = as.numeric(rho[["quantiles"]]), relativeTolerance = relativeTolerance, 
                      ...)
    }
    else stop("\"", rho[["distribution"]], "\" is not a valid distribution type for method=\"", 
              method, "\".")
  }
  else stop("method must be \"fit\".")
  x
}