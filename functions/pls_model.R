# PlS

pls_model <- function (object, resultName = NULL, variables.x = names(object$x), 
          method = "oscorespls", scale = TRUE, ncomp = 2, ...) 
{
  if (!requireNamespace("pls", quietly = TRUE)) 
    stop("Package \"pls\" needed. Please install it.", call. = FALSE)
  if (is.list(object$y)) {
    if (!is.null(resultName)) {
      y <- object$y[[resultName]]
    }
    else {
      if (length(names(object$y)) == 1) {
        y <- unlist(object$y)
      }
      else stop("No component of the model function chosen!")
    }
  }
  else {
    y <- object$y
  }
  x <- as.matrix((object$x)[variables.x])
  x <- x[, which(apply(x, 2, sd) > 0)]
  plsrResults <- pls::plsr(y ~ x, method = method, scale = scale, 
                           ncomp = ncomp, ...)
  plsrResults
}