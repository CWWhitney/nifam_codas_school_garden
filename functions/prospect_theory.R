# Prospect theory 

# Prospect Theory is a simplified model of decision making, 
# actual decisions can be influenced by a variety of complex factors beyond the theory's scope.

# To incorporate Prospect Theory into our NPV forecasting process, 
# We could consider adjusting our decision-making framework, presentation strategies, 
# and risk assessment methods to align with the psychological factors outlined by the theory. 
# a basic illustration without all possible Prospect Theory variations or complexities

prospect_theory <- function(outcome_years = c(3, 5, 2, 8, 4, 6), 
                            reference_point = 4, 
                            # Value function parameters
                            concave_param = 0.5,# Concave for gains
                            convex_param = 0.8, # Convex for losses
                            loss_aversion = 2 ) # Loss aversion factor
  
  {

# Calculate prospect values for each outcome
prospect_values <- numeric(length(outcome_years))
for (i in 1:length(outcome_years)) {
  outcome <- outcome_years[i]
  
  if (outcome >= reference_point) {
    prospect_values[i] <- (outcome - reference_point)^concave_param
  } else {
    prospect_values[i] <- -loss_aversion * (reference_point - outcome)^convex_param
  }
}

# Choose the outcome with the highest prospect value
chosen_index <- which.max(prospect_values)
chosen_outcome <- outcome_years[chosen_index]

cat("Outcome years:", outcome_years, "\n")
cat("Prospect values:", prospect_values, "\n")
cat("Chosen outcome:", chosen_outcome, "\n")
}
