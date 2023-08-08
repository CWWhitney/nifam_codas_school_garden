# Run the EVPI calculation
# import EVPI from another file
source("functions/empirical_EVPI.R")

multi_EVPI <- function (mc, first_out_var, write_table = FALSE, outfolder = NA) 
{
  if (mean(mc[, 1] == 1:nrow(mc)) == 1) 
    mc <- mc[, 2:ncol(mc)]
  # which and colnames from base R
  outvar1pos <- which(colnames(mc) == first_out_var)
  outvars <- colnames(mc)[outvar1pos:ncol(mc)]
  # print and paste from base R
  print(paste("Processing", length(outvars), "output variables. This can take some time."))
  outputs <- list()
  for (out_var in outvars) {
    # t and sapply from base R 
    results <- t(sapply(colnames(mc)[1:(outvar1pos - 1)], 
    # empirical_EVPI from another script
 function(x) empirical_EVPI(mc, x, out_var)[c("expected_gain", 
                 "EVPI_do", "EVPI_dont")]))
    outs <- data.frame(variable = rownames(results), 
                       expected_gain = unlist(results[, 
                                                                                    "expected_gain"]), EVPI_do = unlist(results[, "EVPI_do"]), 
                       EVPI_dont = unlist(results[, "EVPI_dont"]))
    decision <- ifelse(mean(outs$expected_gain, na.rm = TRUE) > 
                         0, "do", "dont")
    if (decision == "do") 
      res <- outs[, c("EVPI_do")]
    if (decision == "dont") 
      res <- outs[, c("EVPI_dont")]
    names(res) <- outs[, c("variable")]
    outputs[[out_var]] <- outs
    outputs[[out_var]]$EVPI <- res
    outputs[[out_var]]$decision <- decision
    if (write_table) {
      if (is.na(outfolder)) 
        outfolder <- "."
      write.csv(outs, paste(file.path(outfolder, 
                                      paste("EVPI_table_", 
                                      out_var, ".csv", sep = ""))))
    }
    print(paste("Output variable ", which(out_var == outvars), 
                " (", out_var, ") completed.", sep = ""))
  }
  attr(outputs, "class") <- c("EVPI_outputs", "list")
  return(invisible(outputs))
}