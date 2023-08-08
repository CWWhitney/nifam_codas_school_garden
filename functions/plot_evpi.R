# Plot the EVPI
# load the tidyverse library
library(tidyverse)

plot_evpi <- function (EVPIresults, decision_vars, input_table = NULL, new_names = NULL, 
                       unit = NULL, x_axis_name = "Expected Value of Perfect Information", 
                       y_axis_name = NULL, bar_color = "cadetblue", base_size = 11, 
                       ...) 
{
  assertthat::assert_that("EVPI_outputs" %in% class(EVPIresults), 
                          msg = "EVPIresults is not class 'EVPI_outputs', please provide a valid object. This does not appear to have been generated with the 'multi_EVPI' function.")
  if (!is.null(input_table)) 
    assertthat::assert_that(any(class(input_table) %in% c("tbl_df", 
                                                          "tbl", "data.frame")), msg = "The input_table is not a data.frame or tibble (tbl, tbl_df) class, please provide a valid object.")
  full_evpi_data <- NULL
  for (i in 1:length(EVPIresults)) {
    data <- EVPIresults[[i]]
    data["output_variable"] <- names(EVPIresults)[i]
    if (is.null(full_evpi_data)) 
      full_evpi_data <- data
    else full_evpi_data <- dplyr::bind_rows(full_evpi_data, 
                                            data)
  }
  rownames(full_evpi_data) <- NULL
  if (!(is.null(input_table))) 
    combined_table <- dplyr::left_join(full_evpi_data, input_table, 
                                       by = c(variable = "variable"))
  else combined_table <- full_evpi_data
  assertthat::assert_that(any(decision_vars %in% combined_table$output_variable), 
                          msg = "The names provided for decision_vars do not match the names in the EVPIresults. Make sure that they are in the EVPIresults and are spelled correctly.")
  filtered_table <- dplyr::filter(combined_table, EVPI > 0)
  data <- dplyr::filter(filtered_table, output_variable %in% 
                          decision_vars)
  if (nrow(data) == 0) {
    warning("There are no variables with a positive EVPI. You probably do not need a plot for that.", 
            call. = FALSE)
    return(invisible(NULL))
  }
  if (is.null(new_names)) 
    decision_labels <- decision_vars
  else decision_labels <- new_names
  data$output_variable <- factor(data$output_variable, levels = decision_vars, 
                                 labels = decision_labels)
  if (!is.null(input_table)) 
    y_axis <- "label"
  else y_axis <- "variable"
  if (is.null(unit)) 
    unit <- ""
  ggplot2::ggplot(data, ggplot2::aes(x = EVPI, y = stats::reorder(!!ggplot2::ensym(y_axis), 
                                                                  EVPI))) + ggplot2::geom_col(fill = bar_color) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 
                                                                                                                                                                                   0.01)), labels = scales::dollar_format(prefix = unit)) + 
    ggplot2::labs(y = y_axis_name, x = x_axis_name) + ggplot2::facet_wrap(~output_variable, 
                                                                          scales = "free") + ggplot2::theme_bw(base_size = base_size) + 
    ggplot2::theme(strip.background = ggplot2::element_blank()) + 
    ggplot2::theme(...)
}