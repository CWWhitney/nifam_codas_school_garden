

empirical_EVPI_test <- function (mc, test_var_name, out_var_name) {
  # Calculate the standard deviation and mean of the test variable
  sd_tv <- sd(mc[, test_var_name])
  mean_tv <- mean(mc[, test_var_name])
  
  # If the test variable has no variation, return NA results
  if (sd_tv == 0) {
    return(list(expected_gain = NA, EVPI_do = 0, EVPI_dont = 0))
  }
  
  # Order the data by the test variable
  mcs <- mc[order(mc[, test_var_name]), ]
  
  # Fit the LOESS model with error handling
  loessmod <- tryCatch({
    fANCOVA::loess.as(mcs[, test_var_name], mcs[, out_var_name], criterion = "gcv")
  }, error = function(e) {
    return(NULL) # Return NULL if the LOESS fitting fails
  })
  
  # If LOESS fitting fails, return NA results
  if (is.null(loessmod)) {
    return(list(expected_gain = NA, EVPI_do = 0, EVPI_dont = 0))
  }
  
  # Predict the smoothed values
  out_var_sm <- tryCatch({
    predict(loessmod, mcs[, test_var_name])
  }, error = function(e) {
    return(rep(NA, length(mcs[, test_var_name]))) # Return NA for all predictions if predict fails
  })
  
  # Calculate weights using normal distribution
  weight <- stats::dnorm(mcs[, test_var_name], mean = mean_tv, sd = sd_tv)
  
  # Weighted output variable
  out_var_weight <- out_var_sm * weight
  
  # Calculate positive and negative areas (AUC)
  out_var_pos <- ifelse(out_var_weight >= 0, out_var_weight, 0)
  diffsx <- mcs[, test_var_name] - c(mcs[c(1, 1:(nrow(mcs) - 1)), test_var_name])
  auc_pos <- sum(out_var_pos * diffsx, na.rm = TRUE)
  
  out_var_neg <- ifelse(out_var_weight < 0, out_var_weight, 0)
  auc_neg <- sum(out_var_neg * diffsx, na.rm = TRUE)
  
  # Calculate expected gain
  exp_gain <- auc_neg + auc_pos
  
  # Create the result list with consistent structure
  res <- list(
    expected_gain = exp_gain, 
    recommendation = ifelse(exp_gain > 0, "do", "don't"), 
    EVPI_do = -auc_neg, 
    EVPI_dont = auc_pos, 
    test_var_data = mcs[, test_var_name], 
    out_var_data = mcs[, out_var_name], 
    out_var_sm = out_var_sm, 
    weight = weight, 
    out_var_weight = out_var_weight, 
    test_var_name = test_var_name, 
    out_var_name = out_var_name
  )
  
  # Assign class attributes for consistency
  attr(res, "class") <- c("EVPI_res", "list")
  
  return(invisible(res))
}
