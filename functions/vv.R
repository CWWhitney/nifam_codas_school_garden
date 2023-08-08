# value_varier function
# for creating lists with some predefined variation

vv <- function (var_mean, var_CV, n, distribution = "normal", absolute_trend = NA, 
          relative_trend = NA, lower_limit = NA, upper_limit = NA) 
{
  if (distribution == "normal") {
    if (is.na(absolute_trend) & is.na(relative_trend)) 
      annual_means <- var_mean
    if (!is.na(absolute_trend) & is.na(relative_trend)) 
      annual_means <- rep(var_mean, n) + absolute_trend * 
        c(0:(n - 1))
    if (is.na(absolute_trend) & !is.na(relative_trend)) 
      annual_means <- rep(var_mean, n) * (1 + relative_trend/100)^c(0:(n - 
                                                                         1))
    if (!is.na(absolute_trend) & !is.na(relative_trend)) 
      print("both absolute and relative trend specified. returning only original means")
    out <- rnorm(n, annual_means, abs(annual_means * var_CV/100))
    if (!is.na(lower_limit)) 
      out <- sapply(out, function(x) max(x, lower_limit))
    if (!is.na(upper_limit)) 
      out <- sapply(out, function(x) min(x, upper_limit))
    return(out)
  }
}
