library(decisionSupport)
# School gardens in Hanoi ####
# primary and secondary school

make_variables <- function(est,n=1)
{x <- decisionSupport::random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(decisionSupport::estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# Model ####

school_garden_function <- function(x, varnames){
  
  # Cost####
  
  # establishment_cost <- rep(0, number_of_years)
  # maintenance_cost <- rep(0, number_of_years)
  # harvest_value <- rep(0, number_of_years)
  # 
  
  establishment_cost_year_one <- equipment_cost + #consider to use cut-off value based on land area and number of students
    construction_cost +  # labor cost (2-3 people/day) + machine cost to setup garden system
    teacher_training_cost # cost for training teacher on gardening
  
  
  maintenance_cost_annual <- input_cost + #fertilizer, irrigation, electricity
    maintaining_labor + # technical staff etc
    teacher_salary_cost # extra costs for teachers to work on the garden
  
  total_cost <- vv(maintenance_cost_annual, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                         relative_trend = inflation_rate) #percentage of increase each year
  
  # Add up all costs ####
  # management plus establishment costs in the first year

  total_cost[1] <- establishment_cost_year_one + maintenance_cost_annual #make sure the first is establishment_cost_year_one
  
  
  # Benefits and Risks ####
  canteen_yes_no <- chance_event(if_school_has_canteen, 
                                 value_if = 1, 
                                 value_if_not = 0)
  
  harvest_value <- if (canteen_yes_no == 1) {
    harvest_value = vv(canteen_savings, CV_value, 
                       number_of_years,
                       relative_trend = inflation_rate) #percentage of increase each year
  } else {
    harvest_value = vv(sale_of_yield, CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) #percentage of increase each year
  }
  
  # here we get a bit abstract but we do not want to leave this out
  # learning is worth something to us 
  # and we want to make sure this is part of the recommendation
  # we use things like savings on after school programs, tutor-ships,
  # hiring more teaching staff, organizing more events for kids
  # we call this 'extra_cirricular_savings'
  
  # education quality is correlated (highly) to the learning and to 
  # other values like outside investment (i.e. parents invest)
  # and increased enrollment by 
  # creating a good impression and gaining reputation
  
  #savings on learning
  learning_value <- vv(extra_cirricular_savings, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) * if_quality_education
  
  #investments from outside
  outside_investment <- vv(outside_investment_value, 
                           CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * if_quality_education
  
  #earnings from increased enrollment
  increased_enrollment <-  vv(increased_enrollment_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * if_quality_education
  
  
  #It takes time to get a good reputation
  # make year 1 a zero
  increased_enrollment[1] <- 0 
  
  # Add up all benefits ####
  total_benefit <- harvest_value + learning_value + outside_investment + increased_enrollment
    
  # Final result of the costs and benefits
  garden_intervention_result <- total_benefit - total_cost
  
  ## Alternative land-use result / costs and benefits
  total_benefit_no <- vv(value_of_non_garden_land_use, 
                         var_CV = CV_Value, 
                         n = number_of_years)
  
  total_cost_no <- vv(costs_of_non_garden_land_use, 
                         var_CV = CV_Value, 
                         n = number_of_years)
  
  
  no_intervention_result <- total_benefit_no - total_cost_no
  
  # The difference is inherent in our calculation so we do not need the 
  # comparative NPV here, just discount the intervention result
  # calculate the Net Present Value (NPV) with with the specified discount rate
  NPV_interv <-
    discount(x = garden_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # NPV no intervention ####
  
  NPV_no_interv <-
    discount(x = no_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  
  return(list(NPV_garden = NPV_interv,
              Cashflow_garden = garden_intervention_result))
}

garden_simulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("inputs_school_garden.csv"),
  model_function = school_garden_function,
  numberOfModelRuns = 1000, #run 1000 times
  functionSyntax = "plainNames"
)

decisionSupport::plot_distributions(mcSimulation_object = garden_simulation_results, 
                                    vars = "NPV_garden",
                                    method = 'hist_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = garden_simulation_results, 
                                    vars = "NPV_garden",
                                    method = 'boxplot')

# Cashflow 

plot_cashflow(mcSimulation_object = garden_simulation_results, 
              cashflow_var_name = "Cashflow_garden")

# PLS

pls_result <- plsr.mcSimulation(object = garden_simulation_results,
                                resultName = names(garden_simulation_results$y)[1], 
                                ncomp = 1)

input_table <- read.csv("inputs_school_garden.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0)

# EVPI 

#here we subset the outputs from the mcSimulation function (y) 
# by selecting the correct variables
# be sure to run the multi_EVPI only on the variables that the we want
mcSimulation_table <- data.frame(garden_simulation_results$x, 
                                 garden_simulation_results$y[1])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_garden")

plot_evpi(evpi, decision_vars = "NPV_garden")
