# Function to program chances of events
# mostly for risks
chance_event <- function (chance, value_if = 1, value_if_not = 0, n = 1, CV_if = 0, 
          CV_if_not = CV_if, one_draw = FALSE) 
{
  if (!(length(value_if) == length(value_if_not))) 
    stop("value_if and value_if_not are not of the same length")
  if (length(value_if) == 1) 
    if (n > 1) {
      value_if <- vv(value_if, CV_if, n)
      value_if_not <- vv(value_if_not, CV_if_not, n)
    }
  if (!one_draw) 
    occurrence <- rbinom(length(value_if), 1, chance)
  else occurrence <- rbinom(1, 1, chance)
  return(occurrence * value_if + (1 - occurrence) * value_if_not)
}