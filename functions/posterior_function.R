# Creating posteriors #Bayesian Updating Bayesian updating is a statistical
# technique used to revise existing beliefs (prior data) in light of new
# evidence (observed data). Here’s a step-by-step explanation of how you can
# achieve this:
#
# Express Prior Beliefs:
#
# The prior data consists of 90% confidence intervals. Assuming a normal
# distribution, you can convert these intervals into mean and standard deviation
# values for each variable. Incorporate Observed Data:
#
# Use the mean measured values from your observed data as the new evidence.
# Calculate Posterior Distributions:
#
# Combine the prior distributions (from the confidence intervals) with the
# observed data to get the posterior distributions for each variable. The
# posterior distribution reflects the updated beliefs after considering the new
# evidence. Generate Posterior Values:
#
# From the posterior distributions, derive new mean and standard deviation
# values for each variable. Monte Carlo Simulation:
#
# Use these new posterior values to run a Monte Carlo simulation, which will
# allow you to estimate the uncertainty and variability in the outcomes of
# interest. 

library(dplyr)
library(stats)

generate_posterior <- function(prior_data, observed_data) {
  
  # Calculate Mean and SD for Normal-like distributions
  prior_data <- prior_data %>%
    mutate(
      Mean = ifelse(distribution %in% c("norm", "posnorm", "tnorm_0_1"), (lower + upper) / 2, NA),
      SD = ifelse(distribution %in% c("norm", "posnorm", "tnorm_0_1"), (upper - Mean) / qnorm(0.95), NA)
    )
  # We may expand this to the full list https://cran.r-project.org/web/packages/decisionSupport/decisionSupport.pdf#page=91.00
  # "const" Deterministic case not applicable
  # "norm" Normal calculate, fit
  # "posnorm" Positive normal calculate, fit
  # "tnorm_0_1" 0-1-truncated normal calculate, fit
  # "beta" Beta fit
  # "cauchy" Cauchy fit
  # "logis" Logistic fit
  # "t" Student t fit
  # "chisq" Central Chi-Squared fit
  # "chisqnc" Non-central Chi-Squared fit
  # "exp" Exponential fit
  # "f" Central F fit
  # "gamma" Gamma with scale=1/rate fit
  # "lnorm" Log Normal calculate, fit
  # "unif" Uniform calculate, fit
  # "weibull" Weibull fit
  # "triang" Triangular fit
  # "gompertz" Gompertz fit
  # "pert" (Modified) PERT fit
  
  # Assume a fixed standard deviation for observed data
  observed_data <- observed_data %>%
    mutate(ObservedSD = 1)  # Fixed standard deviation
  # We may adjust this later
  
  # Join Prior Data with Observed Data
  posterior_data <- prior_data %>%
    inner_join(observed_data, by = "variable", relationship = "many-to-many")
  
  # Calculate Posterior Means and Standard Deviations for Normal-like distributions
  posterior_data <- posterior_data %>%
    mutate(
      PosteriorMean = ifelse(distribution.x %in% c("norm", "posnorm", "tnorm_0_1"), (Mean / SD^2 + mean / ObservedSD^2) / (1 / SD^2 + 1 / ObservedSD^2), NA),
      PosteriorSD = ifelse(distribution.x %in% c("norm", "posnorm", "tnorm_0_1"), sqrt(1 / (1 / SD^2 + 1 / ObservedSD^2)), NA)
    )
  
  # Calculate 90% Confidence Intervals for the Posterior Distributions
  z_score <- qnorm(0.95)
  
  posterior_data <- posterior_data %>%
    mutate(
      PosteriorLowerBound = ifelse(distribution.x == "norm", 
                                   PosteriorMean - z_score * PosteriorSD, NA),
      PosteriorUpperBound = ifelse(distribution.x == "norm", 
                                   PosteriorMean + z_score * PosteriorSD, NA),
      PosteriorLowerBound = ifelse(distribution.x == "posnorm", 
                                   pmax(PosteriorMean - z_score * PosteriorSD, 0), 
                                   PosteriorLowerBound),
      PosteriorUpperBound = ifelse(distribution.x == "posnorm", 
                                   PosteriorMean + z_score * PosteriorSD, 
                                   PosteriorUpperBound),
      PosteriorLowerBound = ifelse(distribution.x == "tnorm_0_1", 
                                   pmax(pmin(PosteriorMean - z_score * PosteriorSD, 1), 0), 
                                   PosteriorLowerBound),
      PosteriorUpperBound = ifelse(distribution.x == "tnorm_0_1", 
                                   pmin(pmax(PosteriorMean + z_score * PosteriorSD, 0), 1), 
                                   PosteriorUpperBound)
    )
  
  # Handle Constant distribution separately
  posterior_data <- posterior_data %>%
    mutate(
      PosteriorLowerBound = ifelse(distribution.x == "const", mean, PosteriorLowerBound),
      PosteriorUpperBound = ifelse(distribution.x == "const", mean, PosteriorUpperBound)
    )
  
  # create data in format for decisionSupport functions
  new_posterior_data <- posterior_data %>%
    rename(distribution = distribution.x,  label = label.x, 
           lower = PosteriorLowerBound, upper = PosteriorUpperBound) %>%
    select(variable, lower, upper, distribution, label
    )
  
}

# Test function 
# Load data
prior_data <- read.csv("inputs_school_garden.csv") 
observed_data <- read.csv("inputs_measured_means_school_garden.csv") 

data_post <- generate_posterior(prior_data, observed_data)
