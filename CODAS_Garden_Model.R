# School gardens in Hanoi ####


inputs_school_garden

make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# Model ####

number_of_years <- 

establishment_cost <- rep(0, number_of_years)
maintenance_cost <- rep(0, number_of_years)

establishment_cost[1]<- equipment_cost + #consider to use cut-off value based on land area and number of participants
  construction_cost +  # labor cost (2-3 people/day) + machine cost to setup garden system
  teacher_training_cost # cost for training teacher on gardening
establishment_cost[2:5] <- 0

maintenance_cost[1] <- 0
maintenance_cost[2: 5]<- input_cost + #fertilizer, irrigation, electricity
  maintaining_labor + teacher_salary_cost


total_cost <- establishment_cost + maintenance_cost + educational_cost

