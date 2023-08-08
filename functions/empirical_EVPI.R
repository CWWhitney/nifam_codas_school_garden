# Create the empirical_EVPI function 
empirical_EVPI <- function (mc, test_var_name, out_var_name) 
{
  sd_tv <- sd(mc[, test_var_name])
  mean_tv <- mean(mc[, test_var_name])
  if (sd_tv == 0) {
    return(list(expected_gain = NA, EVPI_do = 0, EVPI_dont = 0))
  }
  mcs <- mc[order(mc[, test_var_name]), ]
  loessmod <- fANCOVA::loess.as(mcs[, test_var_name], mcs[, 
                                                          out_var_name], criterion = "gcv")
  out_var_sm <- predict(loessmod, mcs[, test_var_name])
  weight <- stats::dnorm(mcs[, test_var_name], mean = mean_tv, 
                         sd = sd_tv)
  out_var_weight <- out_var_sm * weight
  out_var_pos <- ifelse(out_var_weight >= 0, out_var_weight, 
                        0)
  diffsx <- mcs[, test_var_name] - c(mcs[c(1, 1:(nrow(mcs) - 
                                                   1)), test_var_name])
  auc_pos <- sum(out_var_pos * diffsx)
  out_var_neg <- ifelse(out_var_weight < 0, out_var_weight, 
                        0)
  auc_neg <- sum(out_var_neg * diffsx)
  exp_gain <- auc_neg + auc_pos
  res <- list(expected_gain = exp_gain, 
              recommendation = ifelse(exp_gain > 0, "do", "don't"), 
              EVPI_do = -auc_neg, EVPI_dont = auc_pos, 
              test_var_data = mcs[, test_var_name], 
              out_var_data = mcs[, out_var_name], 
              out_var_sm = out_var_sm, 
              weight = weight, 
              out_var_weight = out_var_weight, 
              test_var_name = test_var_name, 
              out_var_name = out_var_name)
  attr(res, "class") <- c("EVPI_res", "list")
  return(invisible(res))
}