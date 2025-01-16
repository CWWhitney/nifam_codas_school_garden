# Creating Pareto Sets
# Include the MC model of the garden options
source("Garden_Model.R")

# Create estimates
estimates = read.csv("data/inputs_school_garden.csv")
estimates = estimates[estimates$variable !="", ]

# Divides variables into controllable (modifiable by decision-makers) 
# and uncontrollable (external or stochastic factors).
controllable_estimates = estimates[estimates$control_status == "controllable", ]
uncontrollable_estimates = estimates[estimates$control_status != "controllable", ]

# determine number of runs for the optimization test 
n_mc_runs = 500

# placeholder variables
money = c()
biodiversity = c()
health = c()

# Use estimate_read_csv to create a set of estimates
est = decisionSupport::estimate_read_csv(paste("data/inputs_school_garden.csv",sep=""))

# Random draw from estimates
mc_draws <- decisionSupport::random(rho=est,n=n_mc_runs)

# Placeholder vector
decision_vars = vector()

# Placeholders
lower = c()
upper = c()
is_binary = c()
decision_var_names = c()

# Initializes decision_vars and assigns bounds for each controllable variable
# based on input distributions Binary variables (e.g., yes/no decisions) are
# bounded between 0 and 1. Continuous variables are assigned bounds derived from
# lower and upper in the input data. These variables will be optimized
# to evaluate Pareto-optimal trade-offs.
for (i in 1:nrow(controllable_estimates)) {
    if(controllable_estimates$distribution[i] == "tnorm_0_1"){
        val = 1
        lower = append(lower, 0)
        upper = append(upper, 1)
        is_binary = append(is_binary, TRUE)
    } else {
        val = (controllable_estimates$upper[i] + controllable_estimates$lower[i]) / 2
        lower = append(lower, controllable_estimates$lower[i])
        upper = append(upper, controllable_estimates$upper[i])
        is_binary = append(is_binary, FALSE)
    } 
    decision_vars = append(decision_vars, val)
    decision_var_names = append(decision_var_names, controllable_estimates$variable[i])
}

# Controllable variables assigned values from the decision vector
# Uncontrollable variables are drawn stochastically from mc_draws. 
# Runs the school_garden_function for each iteration, appending
# results to money, biodiversity, and health. Returns the negative mean
# of each objective (scaled), as NSGA-II minimizes objectives.
fitness <- function(decision_vars) {
    for (mc_i in 1:n_mc_runs){
        for (i in 1:nrow(controllable_estimates)) {
            #if (estimates$control_status[i] == "controllable") {
            val = decision_vars[i]
            assign(controllable_estimates$variable[i], val, envir=.GlobalEnv)
        }
        for (i in 1:nrow(estimates)) {
            if (estimates$control_status[i] != "controllable") {
                val = mc_draws[mc_i, i]
                assign(estimates$variable[i], val, envir=.GlobalEnv)
            }
        }
        res = school_garden_function()
        
        money = append(money, res$NPV_garden_STEM_public_school)
        biodiversity = append(biodiversity, res$biodiversity)
        health = append(health, res$health_STEM)
    }
    res = c(-mean(money)*0.01, -mean(biodiversity), -mean(health)*0.01)
    
    return(res)
}

fitness(decision_vars)

# Custom function for generating intitial population
my_population <- function(object) {
  print("Create population")
  # Validate input dimensions
  if (length(object@lower) != length(object@upper) || length(object@lower) != length(object@upper)) {
    stop("Lengths of lower bounds, upper bounds, and is_binary must be the same.")
  }
  
  # Number of decision variables
  num_vars <- length(object@lower)
  
  # Initialize population matrix
  population <- matrix(NA, nrow = object@popSize, ncol = num_vars)
  
  # Generate each individual
  for (i in 1:object@popSize) {
    individual <- sapply(1:num_vars, function(j) {
      if (is_binary[j]) {
        # Binary variable: Randomly choose 0 or 1
        sample(c(0, 1), size = 1)
      } else {
        # Continuous variable: Random value between lower and upper bound
        runif(1, min = object@lower[j], max = object@upper[j])
      }
    })
    population[i, ] <- individual
  }
  return(population)
}

# Custom function for mutation operation
binary_mutation_rate = 0.1
continuous_mutation_std = 0.1
my_mutation <- function(object, parent_idx) {
  print("Mutate")
  # Extract attributes
  lower_bounds <- object@lower
  upper_bounds <- object@upper
  population <- object@population  # Population matrix
  num_vars <- length(lower_bounds)
  
  parent <- population[parent_idx, ]
  child = rep(0, num_vars)
  # Mutate each individual
    for (j in 1:num_vars) {
        if (is_binary[j]) {
            if (runif(1) < binary_mutation_rate) {  # Apply mutation with given probability
                # For binary: flip the bit
                child[j] <- ifelse(parent[j] == 0, 1, 0)
            } else {
                child[j] <- parent[j]
            }
        } else {
            # For continuous: generate a new value in bounds
            #child[j] <- runif(1, min = lower_bounds[j], max = upper_bounds[j])

            mutation <- rnorm(1, mean = 0, sd = (upper_bounds[j] - lower_bounds[j]) * continuous_mutation_std)
            child[j] <- pmax(lower_bounds[j], pmin(upper_bounds[j], child[j] + mutation))
        }
    }
  return(child)
}

# Custom function for crossover operation
crossover_rate = 0.8
my_crossover <- function(object, parents) {
  # Extract attributes
  lower_bounds <- object@lower
  upper_bounds <- object@upper
  population <- object@population  # Population matrix
  pop_size <- object@popSize
  num_vars <- length(lower_bounds)
  
  # Crossover: Create a new generation
  children <- matrix(NA, nrow = 2, ncol = num_vars)
   
  parent1 <- population[parents[1], ]
  parent2 <- population[parents[2], ]
  if (runif(1) < crossover_rate) {
    print("Crossover")
    
    # Perform gene-by-gene crossover
    child1 <- numeric(num_vars)
    child2 <- numeric(num_vars)
    
    for (i in 1:num_vars) {
      if (runif(1) < 0.5) {  # 50% chance to take gene from each parent
        child1[i] <- parent1[i]
        child2[i] <- parent2[i]
      } else {
        child1[i] <- parent2[i]
        child2[i] <- parent1[i]
      }
    }
  } else {
    # No crossover, copy parents
    child1 <- parent1
    child2 <- parent2
  }

  children_fitness <- c(NA, NA)

  children[1, ] <- child1
  children[2, ] <- child2
  
  
  return(list(
    children=children,
    fitness=children_fitness
    ))
}

library(rgl)

# Optimizes for three objectives: economic return, biodiversity, and health
# Population-based algorithm with 200 'individuals' over 100 'generations'
result <- rmoo::nsga2(
    type = "real-valued",
    fitness = fitness,
    lower = lower,
    upper = upper,
    selection = rmoo::nsga_tourSelection,
    population = my_population,
    mutation = my_mutation,
    crossover = my_crossover,
    popSize = 200,
    nObj = 3,
    maxiter = 100,
    names = decision_var_names
)

# Takes a very long time to run
# Write all to RData files and load from there

# Extracts Pareto-optimal solutions and scales results for interpretation.
# Filters solutions that cannot be improved in one
# objective without worsening another.

load(file="data/optimization_results/result_nostem_priv_new.RData")
#  loads the previously saved result object from an .RData file. The object
#  contains the results of a multi-objective optimization run, including the
#  fitness values and population of solutions.
rmoo::summary(result_nostem_priv) # displays a summary of the optimization results
mat = result_nostem_priv@fitness 
# contains the fitness values for all solutions in the final 
# generation of the optimization
front1_set = rmoo::non_dominated_fronts(result_nostem_priv)$fit[[1]]
# rmoo:non_dominated_fronts()  to identify which solutions are Pareto-optimal
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
# Filters the rescaled fitness matrix (mat2) to retain only the Pareto-optimal solutions.

# front1_set= indices of Pareto-optimal solutions from mat2 that includes only
# these Pareto-optimal solutions. Example: If mat2 has 200 rows, but front1_set
# contains 24 indices, set1 will be a 24 × 3 24×3 matrix.
set1 = mat2[front1_set, ]

# Repeat for other options
load(file="data/optimization_results/result_stem_priv_new.RData")
rmoo::summary(result_stem_priv)
mat = result_stem_priv@fitness
front1_set = rmoo::non_dominated_fronts(result_stem_priv)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set2_1 = mat2[front1_set, ]
set2 = set2_1[set2_1[, 1]>0, ]

load(file="data/optimization_results/result_nostem_pub_new.RData")
rmoo::summary(result_nostem_pub)
mat = result_nostem_pub@fitness
front1_set = rmoo::non_dominated_fronts(result_nostem_pub)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set3 = mat2[front1_set, ]

load(file="data/optimization_results/result_stem_pub_new.RData")
rmoo::summary(result_stem_pub)
mat = result_stem_pub@fitness
front1_set = rmoo::non_dominated_fronts(result_stem_pub)$fit[[1]]
mat2 = sweep(-mat, 2, c(100, 1, 100) , `*`) # retransform
set4 = mat2[front1_set, ]

# Plot Pareto results ####

library(plotly)

plot_ly() %>%
  add_trace(x = set1[,1], y = set1[,2], z = set1[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'blue', size = 5),
            name = 'private, no STEM') %>%
  add_trace(x = set2[,1], y = set2[,2], z = set2[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'red', size = 5),
            name = 'private, STEM') %>%
  add_trace(x = set3[,1], y = set3[,2], z = set3[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'green', size = 5),
            name = 'public, no STEM') %>%
  add_trace(x = set4[,1], y = set4[,2], z = set4[,3],
            type = "scatter3d", mode = "markers",
            marker = list(color = 'orange', size = 5),
            name = 'public, STEM') %>%
  layout(scene = list(xaxis = list(title = 'economic'),
                      yaxis = list(title = 'biodiversity'),
                      zaxis = list(title = 'health')))

library(ggplot2)
library(cowplot)

# Convert matrices to data frames and add a "Dataset" column
df1 <- as.data.frame(set1); df1$option <- "private, no STEM"
df2 <- as.data.frame(set2); df2$option <- "private, STEM"
df3 <- as.data.frame(set3); df3$option <- "public, no STEM"
df4 <- as.data.frame(set4); df4$option <- "public, STEM"

# Combine all data frames
df_combined <- rbind(df1, df2, df3, df4)

colnames(df_combined) = c("economic", "biodiversity", "health", "option")

# Generate pairwise combinations
pairs <- expand.grid(names(df_combined)[1:3], names(df_combined)[1:3])  # Ignore 'Dataset' column
pairs <- pairs[pairs$Var1 != pairs$Var2, ]              # Remove diagonal pairs
pairs <- unique(t(apply(pairs, 1, sort)))               # Get unique pairs
pairs <- as.data.frame(pairs)
colnames(pairs) <- c("Col1", "Col2")

# Create plots ####

get_legend <- function(my_plot) {
  g <- ggplotGrob(my_plot)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend)
}


# Generate scatterplots without individual legends
plots <- lapply(1:nrow(pairs), function(i) {
  ggplot(df_combined, aes_string(x = pairs$Col1[i], y = pairs$Col2[i], color = "option")) +
    geom_point() +
    #labs(title = paste(pairs$Col1[i], "vs", pairs$Col2[i])) +
    scale_color_manual(values = c("private, no STEM" = "blue", "private, STEM" = "orange",
                                  "public, no STEM" = "darkgreen", "public, STEM" = "red")) +  # Custom colors
    theme_minimal() +
    theme(legend.position = "none")  # Remove individual legends
})

df_combined$Dataset = factor(df_combined$option)


# Combine plots and the single legend ####
combined_plot <- plot_grid(
  plot_grid(plotlist = plots, ncol = 2),  # Arrange plots
  ncol = 1,
  rel_heights = c(4, 0.5)  # Adjust space for the legend
)
ggsave("figures/scatterplot_four_matrices.png", combined_plot, width = 8, height = 6, bg = 'white')


# Write scenarios ####
write.table(set1, file="data/optimization_results/set1.csv")
write.table(set2, file="data/optimization_results/set2.csv")
write.table(set3, file="data/optimization_results/set3.csv")
write.table(set4, file="data/optimization_results/set4.csv")
