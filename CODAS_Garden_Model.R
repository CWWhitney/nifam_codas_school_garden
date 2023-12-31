# Model of the school garden (see Index.RMD for the explaination and posthoc)

# We need these functions to run our model ####
# value_varrier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model testing ####

# make variables for testing our model (only for construction)
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(decisionSupport::estimate_read_csv(paste("inputs_school_garden.csv",sep="")))

# The model ####

school_garden_function <- function(x, varnames){
  
  # Costs####
  
  # Establishment costs 
  
  # chance_event options: 
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
  
  ## Garden establishment ####
  
  garden_establishment_costs <- compost_starting + # getting started with the compost
    worm_starting + # maintaining the compost and breaking down residue
    livestock_costs +  # costs of establishing animals in the garden (small birds, rabbits, fish)
    garden_designing_costs + # garden design costs (hiring a planner) 
    equipment_cost + # this is a high value because we may need a lot of equipment, netting, trellis for plants to climb
    # considering the range, this could be simple or a smart system (full automation)... 
    garden_construction_cost  
  
  # garden establishment cost values based on garden land area 
  garden_establishment_costs <- if (size_of_garden > expensive_garden_size) {
    # of the garden size is large then increase the establishment costs
    # increase by 
    garden_establishment_costs = garden_establishment_costs * cost_increase_expensive_garden_size
  } else {
    garden_establishment_costs = garden_establishment_costs 
  }
  
  ## STEM Garden establishment ####
  #costs if with STEM education
  STEM_establishment_costs <- teaching_equipment + # teaching equipment for sciences (science oriented training)
    # includes microscopes and other highly technical education equipment
    # consider 'if else' for aquatic vs. soil vs. rooftop in available space 
    # (not all have soil but all have space)
    teacher_training_cost  # cost for training teacher on gardening
        # this is low because we see it as a benefit partly because of 
        # training for the teachers in STEM and other topics like transdiscipinary and other topics
        # we save time and money on the training, which would otherwise have been spent on other training
        # teacher's cn also save money on other training courses for these topics 
        # that they otherwise would have had to take
        # requires training on 5 or 7 subjects (biology etc.) for 12 days
  
  # cut-off values for number of students
        
  #establishment costs if passive (no STEM) education ####
  establishment_cost_year_one <- school_board_planning + 
    garden_establishment_costs
  #establishment costs if with STEM education ####
  establishment_cost_year_one_STEM <- school_board_planning + 
    garden_establishment_costs + 
    STEM_establishment_costs 
  
  # Maintenance costs ####
  
  # maintenance costs for the garden (with or without STEM)
  garden_maintenance_cost <-
    maintaining_labor + # technical staff etc
    # 2-3 hours per day to manage a garden of this rough size
    seed_costs + # seeds and seedlings each year
    fertilizer + # EM and other helpers for compost
    plant_protection + # IPM for plant protection
    # Circular garden with animals, trees, plants, fish (Bac Tom option)
    livestock_maint # costs of maintaining animals in the garden
  
  # garden maint. cost values based on garden land area 
  garden_maintenance_cost <- if (size_of_garden > expensive_garden_size) {
    # of the garden size is large then increase the establishment costs
    # increase by 
    garden_maintenance_cost = garden_establishment_costs * cost_increase_expensive_garden_size
  } else {
    garden_maintenance_cost = garden_establishment_costs 
  }
  

  ## maintenance costs if with STEM education
  STEM_maintenance_cost <-
    teacher_salary_cost +  # extra costs for teachers to work on the garden
    annual_teacher_training + # annual teacher training 12 days on 6 subjects
    # low because it is run by the teachers who have already been trained
    teaching_equipment_annual + # reagents, colors, paper, apps
    teaching_tools # children's garden tools, gloves, hoes, basket etc.
  
  # annual maintenance costs if passive (no STEM) education
  maintenance_cost_annual <- garden_maintenance_cost +
    # still need children's garden tools, gloves, hoes, basket etc.
    teaching_tools +
    # annual teacher training just for passive garden activity
    annual_teacher_training * 0.1
  
  ## annual maintenance costs if with STEM education
  maintenance_cost_annual_STEM <- garden_maintenance_cost +
    STEM_maintenance_cost
  
   # Add up all annual maintenance costs garden (no STEM)
  total_cost <- vv(maintenance_cost_annual, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                         relative_trend = inflation_rate) #percentage of increase each year
  # Add up all annual maintenance costs garden with STEM
  total_cost_STEM <- vv(maintenance_cost_annual_STEM, 
                   var_CV = CV_value, 
                   n = number_of_years, 
                   relative_trend = inflation_rate) #percentage of increase each year
  
  # Calculate management plus establishment costs in the first year
  total_cost[1] <- establishment_cost_year_one + 
      maintenance_cost_annual #make sure the first is establishment_cost_year_one
  # Calculate management plus establishment costs in the first year with STEM
  total_cost_STEM[1] <- establishment_cost_year_one_STEM + 
      maintenance_cost_annual_STEM
  
  
  # Risks ####
  
  # These are 'ex-ante' risks, or risks understood when making a decision
  # we use these to multiply the values for the relevant benefits
  # the minimum values are effectively a reduction in the benefits
  # used to multiply benefits (by a number 90% likely)
  # not differentiated by passive and STEM education
  
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
  # same for no STEM and STEM
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
  # this will be applied in the STEM case only 
    
  education_savings <- formal_edu_savings #(not much savings here with no STEM)
  
  education_savings_STEM <- formal_edu_savings_STEM + 
    extra_cirricular_savings
  
  #savings on learning
  learning_value <- vv(education_savings, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) * education_risk
  # In the 2nd year the garden is expected to start running well.
  learning_value[1] <- 0
  #savings on learning with STEM education
  learning_value_STEM <- vv(education_savings_STEM, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate) * education_risk
  
  # The 3rd year is when the STEM education plan will be fully running.
  learning_value_STEM[1:2] <- 0
  
  # Reputation goes up ####
  # through community building, green running award, 
  # planting trees, environment ecology groups
  # school events in garden connect community, leads to
  # greater access and awareness, positive change in choices around food
  
  #investments from outside
  # i.e. sponsors from local business 
  outside_investment <- vv(outside_investment_value, # related to networking
                           CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * community_risk
  outside_investment_STEM <- vv(outside_investment_value_STEM, # related to networking
                           CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * community_risk
  
  # Same for STEM and no STEM
  # the community appreciates the garden 
  # they come to the school and take part in school events
  # the school benefits from the event by selling products
  # maybe products from the garden or increased sales of other school products
  community_value <-  vv(school_event_value*school_event_freq, # i.e. seedlings for sale
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) * community_risk
  
  # Increased enrollment ####
  # earnings from increased enrollment without STEM
    increased_enrollment <-  vv(increased_enrollment_value,
                                CV_value, 
                                number_of_years, 
                                relative_trend = inflation_rate) * education_risk

  # Increased enrollment with STEM
    increased_enrollment_STEM <-  vv(increased_enrollment_value,
                                CV_value, 
                                number_of_years, 
                                relative_trend = inflation_rate) * education_risk
  
  # It takes time to get a good reputation
  # make year 1 a zero
  increased_enrollment[1] <- 0 
  increased_enrollment_STEM[1] <- 0 
  
  # Health related values ####
  # These are critical and extremely important but also somewhat intangible
  # here we determine the value of vegetable access with some proxy values
  child_veg_access <- child_veg_health_care_savings + 
    child_veg_school_performance_value + 
    child_veg_community_engagement_value 
    
  # here we determine the value of healthier choices with some proxy values
  child_healthier_choices <- child_garden_health_care_savings + 
    child_garden_school_performance_value + 
    child_garden_community_engagement_value  
  
  # Need to consider these values carefully as they differ between options
  # health benefits from gardens no STEM
  health_value <- child_veg_access + child_healthier_choices  + 
    garden_mental_health_value
  
  health_related_value <-  vv(health_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * garden_nutrition_risk
  # health benefits from gardens with STEM
  # here we determine the value of healthier choices with some proxy values
  child_healthier_choices_STEM <- child_STEM_health_care_savings + 
    child_STEM_school_performance_value + 
    child_STEM_community_engagement_value  
  # Assuming more formal STEM education time in the garden leads to 
  # better health choices but does not change access (same garden)
      
  health_value_STEM <- child_veg_access + child_healthier_choices_STEM  + 
    garden_mental_health_value
  
  health_related_value_STEM <-  vv(health_value_STEM, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * garden_nutrition_risk
  
  # green space environment
  # Assume same for STEM and no STEM
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
  
  # Add up all benefits with STEM ####
  total_benefit_STEM <- harvest_value + learning_value_STEM + 
    outside_investment_STEM + increased_enrollment_STEM + 
    health_related_value_STEM + environment_related_value + 
    community_value
    
  # Final result of the costs and benefits no STEM
  garden_intervention_result <- total_benefit - total_cost
  
  # Final result of the costs and benefits STEM
  garden_intervention_result_STEM <- total_benefit_STEM - total_cost_STEM
  
  ## Alternative land-use result / costs and benefits
  
  # the above-board earnings from parking (much will be under the table)
  parking_yes_no <- chance_event(if_parking, # some above the table (mostly under the table)
                                 value_if = 1, 
                                 value_if_not = 0)
  
  non_garden_value <- if (parking_yes_no == 1) {
     vv(value_of_non_garden_land_use + parking_value, 
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
  
  
  total_benefit_no <- vv(non_garden_value + # loss of playground etc.
                         school_board_planning, # time savings for board
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
  
  NPV_interv_STEM <-
    discount(x = garden_intervention_result_STEM, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # NPV no intervention ####
  NPV_no_interv <-
    discount(x = no_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  ### END of garden model script ###
  
  # Beware, if we do not name our outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  return(list(NPV_garden = NPV_interv,
              NPV_garden_STEM = NPV_interv_STEM,
              NPV_no_garden = NPV_no_interv,
              decision = NPV_interv - NPV_no_interv,
              decision_STEM = NPV_interv_STEM - NPV_no_interv,
              total_costs = sum(total_cost),
              total_costs_STEM = sum(total_cost_STEM),
              Cashflow_garden = garden_intervention_result, 
              Cashflow_garden_STEM = garden_intervention_result_STEM))
}

