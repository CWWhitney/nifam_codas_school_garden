#' Analyze and Visualize Pareto-Optimal Points for Garden and STEM Options
#'
#' This function identifies Pareto-optimal points for two decision options (STEM and Garden) 
#' based on three objectives: economic return, child health, and biodiversity. It provides 
#' summary statistics, a count of Pareto-optimal points, and comparisons of
#' trade-offs among the objectives for each option.
#'
#' @param economic_return_garden Numeric vector. Economic return values for the Garden option.
#' @param health_garden Numeric vector. Child health values for the Garden option.
#' @param biodiversity_garden Numeric vector. Biodiversity values for the Garden option.
#' @param economic_return_STEM Numeric vector. Economic return values for the STEM option.
#' @param health_STEM Numeric vector. Child health values for the STEM option.
#' @param biodiversity_STEM Numeric vector. Biodiversity values for the STEM option.
#'
#' @return A list with the following elements:
#'   \item{stem_summary}{Summary statistics of the Pareto-optimal points for the STEM option.}
#'   \item{garden_summary}{Summary statistics of the Pareto-optimal points for the Garden option.}
#'
#' @details The function first identifies Pareto-optimal points for each option by maximizing 
#' all three objectives (economic return, child health, and biodiversity) using the `rPref` package. 
#' It then calculates summary statistics for each option's Pareto-optimal points.
#'
#' @examples
#' \dontrun{
#' pareto_posthoc(economic_return_garden, health_garden, biodiversity_garden, 
#'                      economic_return_STEM, health_STEM, biodiversity_STEM)
#' }
#'
#' @import rPref
#' @import reshape2
#' @export
pareto_posthoc <- function(economic_return_garden, health_garden, biodiversity_garden, 
                                 economic_return_STEM, health_STEM, biodiversity_STEM) {
  
  # Combine into data frames for each option
  stem_garden_data <- data.frame(
    economic_return = economic_return_garden,
    biodiversity = health_garden,
    health = biodiversity_garden
  )
  
  garden_data <- data.frame(
    economic_return = economic_return_STEM,
    biodiversity = health_STEM,
    health = biodiversity_STEM
  )
  
  # Identify Pareto-optimal points
  p_stem <- rPref::high(stem_garden_data$biodiversity) * 
    rPref::high(stem_garden_data$health) * 
    rPref::high(stem_garden_data$economic_return)
  pareto_front_stem <- rPref::psel(stem_garden_data, p_stem)
  
  p_garden <- rPref::high(garden_data$biodiversity) * 
    rPref::high(garden_data$health) * 
    rPref::high(garden_data$economic_return)
  pareto_front_garden <- rPref::psel(garden_data, p_garden)
  
  # Summary statistics and range of values for each option
  stem_summary <- summary(pareto_front_stem)
  garden_summary <- summary(pareto_front_garden)
  
  # Count of Pareto-optimal points
  num_pareto_stem <- nrow(pareto_front_stem)
  num_pareto_garden <- nrow(pareto_front_garden)
  
  # Print out details for interpretation
  cat("Number of Pareto-optimal points for STEM option:", num_pareto_stem, "\n")
  cat("Number of Pareto-optimal points for Garden option:", num_pareto_garden, "\n\n")
  cat("Summary of Pareto-optimal points for STEM option:\n")
  print(stem_summary)
  cat("\nSummary of Pareto-optimal points for Garden option:\n")
  print(garden_summary)
  
  # Return a list of outputs
  return(list(
    num_pareto_stem = num_pareto_stem,
    num_pareto_garden = num_pareto_garden,
    stem_summary = stem_summary,
    garden_summary = garden_summary
  ))
}
