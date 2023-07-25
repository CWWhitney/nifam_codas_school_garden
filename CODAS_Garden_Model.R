library(decisionSupport)
# School gardens in Hanoi ####

make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# Model ####

example_decision_function <- function(x, varnames){
  
# Cost####

establishment_cost <- rep(0, number_of_years)
maintenance_cost <- rep(0, number_of_years)
harvest_value <- rep(0, number_of_years)


establishment_cost[1]<- equipment_cost + #consider to use cut-off value based on land area and number of participants
  construction_cost +  # labor cost (2-3 people/day) + machine cost to setup garden system
  teacher_training_cost # cost for training teacher on gardening
establishment_cost[2:5] <- 0

maintenance_cost[1] <- 0
maintenance_cost[2:5]<- input_cost + #fertilizer, irrigation, electricity
  maintaining_labor + teacher_salary_cost

# Add up all costs ####
total_cost <- establishment_cost + maintenance_cost

# Benefits and Risks ####
canteen_yes_no <- chance_event(if_school_has_canteen, value_if = 1, value_if_not = 0)

harvest_value <- if (canteen_yes_no == 1) {
  harvest_value = vv(canteen_savings, CV_value, number_of_years)
} else {
  harvest_value = vv(sale_of_yield, CV_value, number_of_years)
}

# here we get a bit abstract but we do not want to leave this out
# learning is worth something to us 
# and we want to make sure this is part of the recommendation
# we use things like savings on after school programs, tutor-ships,
# hiring more teaching staff, organizing more events for kids
# we call this 'extra_cirricular_savings'

# education quality is correlate (highly) to the learning and to 
# other values like outside investment (i.e. parents invest)
# and increased enrollment by 
# creating a good impression and gaining reputation
ed_quality_yes_no <- chance_event(if_quality_education, value_if = 1, value_if_not = 0)

#savings on learning
learning_value <- if (ed_quality_yes_no) {
  learning_value = vv(extra_cirricular_savings, CV_value, number_of_years)
} else {
  learning_value = 0
}

#investments from outside
outside_investment <- if (ed_quality_yes_no) {
  outside_investment = vv(outside_investment_value, CV_value, number_of_years)
} else {
  outside_investment = 0
}

#earnings from increased enrollment
increased_enrollment <- if (ed_quality_yes_no) {
  increased_enrollment = vv(increased_enrollment_value, CV_value, number_of_years)
} else {
  increased_enrollment = 0
}

#It takes time to get a good reputation
# make year 1 a zero
increased_enrollment[1] <- 0 

# Add up all benefits ####
total_benefit <- harvest_value + learning_value + outside_investment + increased_enrollment

# Final result of the costs and benefits
garden_intervention_result <- total_benefit - total_cost

# The difference is inherent in our calculation so we do not need the 
# comparative NPV here, just discount the intervention result
NPV_interv <-
  discount(garden_intervention_result, discount_rate, calculate_NPV = TRUE)

# Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
# the variables will be called output_1, _2, etc.

return(list(NPV_garden = NPV_interv,
            Cashflow_garden = garden_intervention_result))
}

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("example_input_table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)
