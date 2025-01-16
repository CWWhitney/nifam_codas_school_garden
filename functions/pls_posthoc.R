pls_posthoc <- function(plsrResults, ncomp = NULL, threshold = 0.8) {
  if (!inherits(plsrResults, "mvr")) {
    stop("plsrResults is not class 'mvr'. Please provide a valid PLS model object.")
  }
  
  # Extract the number of components
  ncomp <- if (is.null(ncomp)) plsrResults$ncomp else ncomp
  
  # Extract R-squared values for Y using explained variance
  r_squared <- summary(plsrResults)$val$R2[ncomp, drop = FALSE]
  explained_variance_x <- summary(plsrResults)$val$Xvar
  explained_variance_y <- summary(plsrResults)$val$Yvar
  
  # Define a VIP calculation function
  VIP <- function(object) {
    if (object$method != "oscorespls") {
      stop("VIP calculation only implemented for orthogonal scores algorithm. Use 'method = \"oscorespls\"'.")
    }
    if (nrow(object$Yloadings) > 1) {
      stop("VIP calculation only implemented for single-response models.")
    }
    SS <- c(object$Yloadings) ^ 2 * colSums(object$scores ^ 2)
    Wnorm2 <- colSums(object$loading.weights ^ 2)
    SSW <- sweep(object$loading.weights ^ 2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
  }
  
  vipResult <- if (plsrResults$ncomp == 1) {
    VIP(plsrResults)
  } else {
    VIP(plsrResults)[, "Comp 1"]
  }
  
  # Extract coefficients
  coef <- plsrResults$coefficients[, , 1]
  
  # Create a comprehensive data frame
  pls_outputs <- data.frame(
    Variable = names(vipResult),
    VIP = vipResult,
    Coefficient = coef
  )
  
  # Filter variables based on the VIP threshold
  important_vars <- subset(pls_outputs, VIP > threshold)
  
  # Display summary information
  cat("PLS Model Summary:\n")
  cat("Number of Components:", ncomp, "\n")
  cat("R-squared Value for Y:", r_squared, "\n")
  cat("% Variance Explained in X:", explained_variance_x[ncomp], "\n")
  cat("% Variance Explained in Y:", explained_variance_y[ncomp], "\n")
  
  # Display the table of important variables
  cat("\nImportant Variables (VIP > ", threshold, "):\n", sep = "")
  print(important_vars)
  
  # Return all model outputs for further use
  list(
    plsrResults = plsrResults,
    r_squared = r_squared,
    explained_variance_x = explained_variance_x,
    explained_variance_y = explained_variance_y,
    important_vars = important_vars
  )
}
