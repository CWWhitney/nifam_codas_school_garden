
# School gardens in urban Hanoi ####
# for teaching nutrition 
# and STEM at primary and secondary schools
# 2nd year to start garden running well
# 3rd year before education plan fully running well

# make variables for testing our model (only for construction)
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(decisionSupport::estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# value varier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model ####

school_garden_function <- function(x, varnames){
  
  # Costs####
  
  # Establishment costs 
  
  # possible chance_event options: 
      # family contribution? Will they pay a bit? 
  family_pays_establishment_yes_no <- chance_event(if_family_pays_establishment, # some above the table (mostly under the table)
                                                   value_if = 1, 
                                                   value_if_not = 0)
  
  garden_construction_cost <- if (family_pays_establishment_yes_no == 1) {
    construction_cost * # labor cost (2-3 people/day) + machine cost to setup garden system
         establishment_family_portion_paid # the family pays a bit
  } else {
    construction_cost
  }
  
  # Could be subject to cut-off values based on land area and number of students
  garden_establishment_costs <- compost_starting + # getting started with the compost
    worm_starting + # maintaining the compost and breaking down residue
    livestock_costs +  # costs of establishing animals in the garden (small birds, rabbits, fish)
    garden_designing_costs + # garden design costs (hiring a planner) 
    equipment_cost + # this is a high value because we may need a lot of equipment, netting, trellis for plants to climb
    # could be a smart system (full automation)... 
    garden_construction_cost  
    
  STEM_establishment_costs <- teaching_equipment + # teaching equipment for sciences (science oriented training)
    # consider 'if else' for aquatic vs. soil vs. rooftop in available space 
    # (not all have soil but all have space)
    teacher_training_cost  # cost for training teacher on gardening
    # this is low because we see it as a benefit partly because of 
    # training for the teachers in STEM and other topics like transdiscipinary and other topics
    # we save time and money on the training, which would otherwise have been spent on other training
    # teacher's cn also save money on other training courses for these topics 
    # that they otherwise would have had to take
    # requires training on 5 or 7 subjects (biology etc.) for 12 days
    
  establishment_cost_year_one <- school_board_planning + 
    garden_establishment_costs + 
    STEM_establishment_costs 
  
  garden_maintenance_cost <- maintaining_labor + # technical staff etc
            # 2-3 hours per day to manage a garden of this rough size
    seed_costs + # seeds and seedlings each year
    fertilizer + # EM and other helpers for compost
    plant_protection + # IPM for plant protection
    # Circular garden with animals, trees, plants, fish
   livestock_maint # costs of maintaining animals in the garden
  
  STEM_maintenance_cost <- teacher_salary_cost +  # extra costs for teachers to work on the garden
    annual_teacher_training + # annual teacher training 12 days on 6 subjects
    # low because it is run by the teachers who have already been trained
    teaching_equipment_annual + # reagents, colors, paper, apps
    teaching_tools # children's garden tools, gloves, hoes, basket etc.
    
  maintenance_cost_annual <- garden_maintenance_cost + STEM_maintenance_cost
  
  # Add up all annual costs
  total_cost <- vv(maintenance_cost_annual, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                         relative_trend = inflation_rate) #percentage of increase each year
  
  # Calculate management plus establishment costs in the first year
  total_cost[1] <- establishment_cost_year_one + maintenance_cost_annual #make sure the first is establishment_cost_year_one
  
  # Risks ####
  
  # These are 'ex-ante' risks, or risks understood when making a decision
  # we use these to multiply the values for the relevant benefits
  # the minimum values are effectively a reduction in the benefits
  # used to multiply benefits (by a number 90% likely)
  
  garden_function_risk <-  min(if_biophysical_good, 
                               if_students_like, # damage garden
                               if_parents_like, #  support
                               if_community_likes, #damage garden
                               if_effective_manage) # well managed garden
  
  garden_nutrition_risk <- min(if_students_like, # eat veg/change behavior
                               if_garden_yield_enough, # goes to hh and school canteen
                               if_parents_like, #  support and buy garden product
                               if_garden_healthy, # good food from the garden
                               if_effective_manage) # well managed garden
  # ex-ante education risks
  education_risk <- min(if_students_like, # pay attention
                        if_teachers_like,# teach effectively
                        if_parents_like,# Allow students to attend
                        if_effective_teaching, # closely related to the next
                        if_effective_training) # but possibly non-correlary
  
  # ex-ante community risks
  community_risk <- min(if_parents_like, #  support and promote
                        if_community_likes, # support and promote
                        if_effective_manage) # well managed garden makes good impression
  
  # ex-ante ecological risks
  ecological_risk <- min(if_offer_green_space, # offer green space
                        if_reduce_polution) # offer habitat
  
  # Benefits and Risks ####
  
  canteen_yes_no <- chance_event(if_school_has_canteen, 
                                 # private schools have but others not so much
                                 value_if = 1, 
                                 value_if_not = 0)
  
  # parents pay for the canteen food / the school will sell to parents
  # never eat in the canteen
  harvest_value <- if (canteen_yes_no == 1) {
    harvest_value = vv(canteen_savings, CV_value, 
                       number_of_years,
                       #inflation -> percentage of increase each year
                       relative_trend = inflation_rate) * garden_function_risk 
                           # account for risk that the garden is not fully functional
  } else {
    harvest_value = vv(sale_of_yield, CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) * garden_function_risk 
                                          
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
  
  # The savings are also in formal education 
  # the school meets some of the KPI for education with the garden
  # such as local enterprise and local economics 
  # Ministry decree to edu. to benefit local economy (35 sessions of 45 min./yr)
  # private school has more time than this 
  education_savings <- formal_edu_savings + extra_cirricular_savings
  
  #savings on learning
  learning_value <- vv(education_savings, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) * education_risk
  
  # Reputation goes up ####
  # through community building, green running award, planting trees, environment ecology groups
  # school events in garden connect community, leads to
  
  #investments from outside
  # i.e. sponsors from local business 
  outside_investment <- vv(outside_investment_value, # related to networking
                           CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * community_risk
  
  community_value <-  vv(school_event_value, # i.e. seedlings for sale
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) * community_risk
  
  # earnings from increased enrollment
  tuition_raise_yes_no <- chance_event(if_increase_tuition, 
                                 value_if = 1, 
                                 value_if_not = 0)
  
  increased_enrollment <- if (tuition_raise_yes_no == 1) {
    increased_enrollment <-  vv(tuition_increase + increased_enrollment_value, #tuition increase 
                                # this is a contentious issue with a lot of discussion
                                # keeping a low value and low chance for now
                                CV_value, 
                                number_of_years, 
                                relative_trend = inflation_rate) * education_risk 
  } else {
    increased_enrollment <-  vv(increased_enrollment_value,
                                CV_value, 
                                number_of_years, 
                                relative_trend = inflation_rate) * education_risk
  }
  
  #It takes time to get a good reputation
  # make year 1 a zero
  increased_enrollment[1] <- 0 
  
  #health benefits from gardens
  health_value <- child_veg_access + child_healthier_choices 
  
  health_related_value <-  vv(health_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * garden_nutrition_risk
  
  # green space environment
  environmental_value <- green_space_value + reduce_polution_value 
  
  # some discussion of carbon credit values (not included)
  environment_related_value <-  vv(environmental_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * ecological_risk
  
  
  # Add up all benefits ####
  total_benefit <- harvest_value + learning_value + 
                    outside_investment + increased_enrollment + 
                    health_related_value + environment_related_value + 
                    community_value
    
  # Final result of the costs and benefits
  garden_intervention_result <- total_benefit - total_cost
  
  ## Alternative land-use result / costs and benefits
  
  # the above-board earnings from parking (much will be under the table)
  parking_yes_no <- chance_event(if_parking, # some above the table (mostly under the table)
                                 value_if = 1, 
                                 value_if_not = 0)
  
  non_garden_value <- if (parking_yes_no == 1) {
     vv(value_of_non_garden_land_use + parking_value, #tuition increase 
                            # this is a contentious issue with a lot of discussion
                            # keeping a low value and low chance for now
                            CV_value, 
                            number_of_years, 
                            relative_trend = inflation_rate) 
  } else {
     vv(value_of_non_garden_land_use,
                            CV_value, 
                            number_of_years, 
                            relative_trend = inflation_rate)
  }
  
  
  total_benefit_no <- vv(non_garden_value + 
                           school_board_planning, # loss of playground etc.
                         var_CV = CV_value, 
                         n = number_of_years, 
                         relative_trend = inflation_rate)
  
  total_cost_no <- vv(costs_of_non_garden_land_use, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                      relative_trend = inflation_rate)
  
  # subtract 
  
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
              NPV_no_garden = NPV_no_interv,
              decision = NPV_interv - NPV_no_interv,
              total_costs = sum(total_cost),
              Cashflow_garden = garden_intervention_result))
}
