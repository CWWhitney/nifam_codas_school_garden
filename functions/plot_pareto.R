# Install necessary packages if not already installed
# install.packages("rPref")
# install.packages("plotly")
# install.packages("akima")

library(akima)
library(plotly)
library(rPref)

#' Generate a 3D Plot of Pareto Front for Decision Options
#'
#' This function calculates and visualizes the Pareto-optimal points for two decision options 
#' (STEM and Garden) based on three objectives: economic return, child health, and biodiversity. 
#' The function returns either a 3D scatter plot or a 3D surface plot, depending on the input.
#'
#' @param economic_return_garden Numeric vector. Economic return values for the Garden option.
#' @param health_garden Numeric vector. Child health values for the Garden option.
#' @param biodiversity_garden Numeric vector. Biodiversity values for the Garden option.
#' @param economic_return_STEM Numeric vector. Economic return values for the STEM option.
#' @param health_STEM Numeric vector. Child health values for the STEM option.
#' @param biodiversity_STEM Numeric vector. Biodiversity values for the STEM option.
#' @param plot_return Character string. Specifies the type of plot to return:
#'   - `"scatter"`: Returns a 3D scatter plot of the Pareto-optimal points.
#'   - `"surface"`: Returns a 3D surface plot of the interpolated Pareto-optimal points.
#'   Defaults to `"scatter"`.
#'
#' @return A `plotly` object representing the Pareto front. If `plot_return` is `"scatter"`, 
#' the function returns a 3D scatter plot. If `plot_return` is `"surface"`, it returns a 3D 
#' surface plot.
#'
#' @details This function identifies Pareto-optimal solutions for two decision options (STEM 
#' and Garden) by maximizing the three objectives: economic return, child health, and biodiversity. 
#' It calculates Pareto-optimal points for each option and visualizes them either as a scatter plot 
#' or as a surface plot. The scatter plot displays the points directly, while the surface plot 
#' uses interpolation to create a smooth surface representing the Pareto front.
#'
#' @examples
#' \dontrun{
#' pareto_front(economic_return_garden, health_garden, biodiversity_garden, 
#'              economic_return_STEM, health_STEM, biodiversity_STEM, plot_return = "scatter")
#'
#' pareto_front(economic_return_garden, health_garden, biodiversity_garden, 
#'              economic_return_STEM, health_STEM, biodiversity_STEM, plot_return = "surface")
#' }
#'
#' @import rPref
#' @import plotly
#' @import akima
#' @export

plot_pareto <- function(economic_return_garden, health_garden, biodiversity_garden, 
                         economic_return_STEM, health_STEM, biodiversity_STEM, plot_return = "scatter") {
  
  # Combine into data frames for each option stem_garden_data has three columns
  # (economic_return, biodiversity, and health) with values representing
  # the STEM option’s Each row in stem_garden_data is a possible solution or
  # outcome, with values for the three objectives.
  
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
  
  # Find Pareto-optimal points for each option
  # p_stem is an object defining preferences for each objective in
  # stem_garden_data. defined using the high() function,
  # which specifies that higher values are preferred for all three objectives
  # (economic return, biodiversity, and child health). The combined preference
  # object p_stem essentially means we are looking for points in
  # stem_garden_data that maximize all three objectives simultaneously.
  
  p_stem <- rPref::high(stem_garden_data$biodiversity) * 
    rPref::high(stem_garden_data$health) * 
    rPref::high(stem_garden_data$economic_return)
  
  # Selecting Pareto-Optimal Points with psel():
  # The psel() function from rPref is used to select Pareto-optimal points from
  # stem_garden_data based on the preferences defined in p_stem.
  # psel(stem_garden_data, p_stem) searches stem_garden_data and finds all
  # points that are non-dominated. A point is non-dominated (or Pareto-optimal)
  # if there is no other point in the data that can improve one objective
  # without worsening at least one other. In simple terms, psel() filters the
  # data to find only the points on the Pareto front—the set of optimal
  # solutions that represent the best trade-offs among the objectives. 
  
  # pareto_front_stem contains only the Pareto-optimal points from
  # stem_garden_data, which are solutions where any improvement in one objective
  # would lead to a decline in at least one other objective. 
  
  # pareto_front_stem <- psel(stem_garden_data, p_stem) extracts the efficient, non-dominated
  # solutions for the STEM option based on the goal of maximizing economic
  # return, biodiversity, and child health.
  pareto_front_stem <- rPref::psel(stem_garden_data, p_stem)
  
  #same for garden
  p_garden <- high(garden_data$biodiversity) * high(garden_data$health) * high(garden_data$economic_return)
  
  pareto_front_garden <- psel(garden_data, p_garden)
  
  # Create a 3D scatter plot for the Pareto front
  if (plot_return == "scatter") {
    plot_scatter <- plot_ly(
      pareto_front_stem,
      x = ~biodiversity,
      y = ~health,
      z = ~economic_return,
      type = 'scatter3d',
      mode = 'markers',
      marker = list(size = 3, color = 'blue'),
      name = 'STEM Option Pareto Front'
    ) %>%
      add_trace(
        data = pareto_front_garden,
        x = ~biodiversity,
        y = ~health,
        z = ~economic_return,
        type = 'scatter3d',
        mode = 'markers',
        marker = list(size = 3, color = 'red'),
        name = 'Garden Option Pareto Front'
      ) %>%
      layout(
        title = "Comparison of Pareto Fronts: STEM vs Garden Option",
        scene = list(
          xaxis = list(title = "Biodiversity"),
          yaxis = list(title = "Child Health"),
          zaxis = list(title = "Economic Return")
        )
      )
    
    return(plot_scatter)
    }
  
  # Create a 3D surface plot for the Pareto front
  if (plot_return == "surface") {
    # Interpolate the surface for both options
    interp_stem <- with(pareto_front_stem, interp(x = biodiversity, y = health, z = economic_return, 
                                                  duplicate = "mean"))
    interp_garden <- with(pareto_front_garden, interp(x = biodiversity, y = health, z = economic_return, 
                                                      duplicate = "mean"))
    
    plot_surface <- plot_ly() %>%
      add_surface(x = interp_stem$x, y = interp_stem$y, z = interp_stem$z, 
                  colorscale = list(c(0, 'blue'), c(1, 'blue')),  # STEM option in blue
                  opacity = 0.7, name = 'STEM Option', showscale = FALSE) %>%
      add_surface(x = interp_garden$x, y = interp_garden$y, z = interp_garden$z, 
                  colorscale = list(c(0, 'red'), c(1, 'red')),  # Garden option in red
                  opacity = 0.7, name = 'Garden Option', showscale = FALSE) %>%
      layout(
        scene = list(
          xaxis = list(title = "Biodiversity"),
          yaxis = list(title = "Child Health"),
          zaxis = list(title = "Economic Return")
        )
      )
  
      return(plot_surface)
  }
  
  # Error message if an incorrect plot_return argument is provided
  stop("Invalid value for plot_return. Use 'scatter' or 'surface'.")
}
