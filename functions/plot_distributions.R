# Function for plotting the distributions
# load the tidyverse library
library(tidyverse)

plot_distributions <- function (mcSimulation_object, vars, method = "smooth_simple_overlay", 
                                bins = 150, old_names = NULL, new_names = NULL, colors = NULL, 
                                outlier_shape = ".", x_axis_name = "Outcome distribution", 
                                y_axis_name = NULL, base_size = 11, ...) 
{
  assertthat::assert_that(class(mcSimulation_object)[[1]] == 
                            "mcSimulation", msg = "mcSimulation_object is not class 'mcSimulation', please provide a valid object. This does not appear to have been generated with the 'mcSimulation' function.")
  data <- data.frame(mcSimulation_object$y, mcSimulation_object$x)
  data <- dplyr::select(data, tidyselect::all_of(vars))
  if (is.null(new_names) | is.null(old_names)) {
    new_names <- names(data)
    old_names <- names(data)
  }
  data <- dplyr::rename_at(data, dplyr::vars(tidyselect::all_of(old_names)), 
                           ~new_names)
  standard_plot_data <- tidyr::pivot_longer(data, tidyselect::all_of(new_names))
  if (is.null(colors)) {
    colors <- c("#009999", "#0000FF", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  }
  if (!is.null(y_axis_name)) 
    y_axis_name <- y_axis_name
  else if (method == "hist_simple_overlay") 
    y_axis_name <- "Number of points in bin"
  else if (method == "boxplot") 
    y_axis_name <- "Decision option"
  else y_axis_name <- "Density estimate"
  standard_plot <- ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                                    group = name, fill = name)) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                              labels = scales::comma) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                    labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
    ggplot2::labs(x = x_axis_name, y = y_axis_name, fill = "Decision\noption") + 
    ggplot2::theme_bw(base_size = base_size)
  if (method == "smooth_simple_overlay") {
    return(standard_plot + ggplot2::geom_density(color = NA, 
                                                 alpha = 0.5) + ggplot2::theme(...))
  }
  if (method == "hist_simple_overlay") {
    return(standard_plot + ggplot2::geom_histogram(color = NA, 
                                                   alpha = 0.5, bins = 150) + ggplot2::theme(...))
  }
  if (method == "boxplot") {
    return(ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                            y = stats::reorder(name, value, FUN = stats::median), 
                                                            fill = name)) + ggplot2::geom_boxplot(outlier.alpha = 0.3) + 
             ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                         labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
             ggplot2::labs(x = x_axis_name, y = y_axis_name, fill = "Decision\noption") + 
             ggplot2::theme_bw(base_size = base_size) + ggplot2::theme(legend.position = "none") + 
             ggplot2::theme(...))
  }
  if (method == "boxplot_density") {
    data <- data.frame(percentage = c(0.1, 0.15, 0.5, 4), 
                       options_difference = c(100, 11229249, 58838895, 507997898))
    regression <- stats::lm(percentage ~ options_difference, 
                            data = data)
    options_difference <- max(standard_plot_data$value) - 
      min(standard_plot_data$value)
    boxploth_width_correction <- stats::coefficients(regression)[[1]] + 
      (stats::coefficients(regression)[[2]] * options_difference)
    return(ggplot2::ggplot(standard_plot_data, ggplot2::aes(x = value, 
                                                            fill = name)) + ggplot2::geom_density(alpha = 0.5, 
                                                                                                  color = NA) + ggstance::geom_boxploth(ggplot2::aes(x = value, 
                                                                                                                                                     y = 0), width = max(stats::density(standard_plot_data$value)$y * 
                                                                                                                                                                           boxploth_width_correction), varwidth = TRUE, alpha = 0.5, 
                                                                                                                                        size = 0.3, outlier.shape = outlier_shape) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                                                 labels = scales::comma) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.01), 
                                                                                                                                                                                                                                                                       labels = scales::comma) + ggplot2::scale_fill_manual(values = colors) + 
             ggplot2::labs(x = x_axis_name, y = y_axis_name) + 
             ggplot2::facet_wrap(. ~ name, scales = "free_y") + 
             ggplot2::theme_bw(base_size = base_size) + ggplot2::theme(strip.background = ggplot2::element_blank(), 
                                                                       legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                                                                     hjust = 1, vjust = 1), ...))
  }
}