# Discounting values
discount <- function (x, discount_rate, calculate_NPV = FALSE) 
{
  disc <- c(1:length(x))
  disc <- 1/(1 + discount_rate/100)^(disc - 1)
  discounted <- x * disc
  if (calculate_NPV) 
    discounted <- sum(discounted)
  return(discounted)
}