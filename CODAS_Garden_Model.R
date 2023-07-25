library(decisionSupport)
# School gardens in Hanoi ####

make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# Model ####

#Cost####

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


total_cost <- establishment_cost + maintenance_cost

#Benefit####
harvest_value <- if (if_school_has_canteen) {
  harvest_value = vv(canteen_savings, CV_value, number_of_years)
} else {
  harvest_value = vv(sale_of_yield, CV_value, number_of_years)
}



#Risks####

