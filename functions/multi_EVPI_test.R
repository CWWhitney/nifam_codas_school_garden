library(parallel)

source("functions/empirical_EVPI_test.R")

multi_EVPI_test <- function(mc, first_out_var, write_table = FALSE, outfolder = NA) {
  if (mean(mc[, 1] == 1:nrow(mc)) == 1) {
    mc <- mc[, 2:ncol(mc)]
  }
  
  outvar1pos <- which(colnames(mc) == first_out_var)
  outvars <- colnames(mc)[outvar1pos:ncol(mc)]
  
  print(paste("Processing", length(outvars), "output variables. This can take some time."))
  
  outputs <- list()
  
  for (out_var in outvars) {
    results <- mclapply(colnames(mc)[1:(outvar1pos - 1)], function(x) {
      tryCatch({
        res <- empirical_EVPI_test(mc, x, out_var)[c("expected_gain", "EVPI_do", "EVPI_dont")]
        if (length(res) == 3) {
          return(res)
        } else {
          return(list(expected_gain = NA, EVPI_do = NA, EVPI_dont = NA))
        }
      }, error = function(e) {
        return(list(expected_gain = NA, EVPI_do = NA, EVPI_dont = NA))
      })
    }, mc.cores = detectCores() - 1)
    
    # Convert results to a data frame, handling empty results
    results_df <- do.call(rbind, lapply(results, function(x) {
      if (is.null(x)) {
        return(data.frame(expected_gain = NA, EVPI_do = NA, EVPI_dont = NA))
      } else {
        return(as.data.frame(t(unlist(x))))
      }
    }))
    
    outs <- data.frame(variable = colnames(mc)[1:(outvar1pos - 1)],
                       expected_gain = results_df$expected_gain,
                       EVPI_do = results_df$EVPI_do,
                       EVPI_dont = results_df$EVPI_dont)
    
    # Determine the decision
    decision <- ifelse(mean(outs$expected_gain, na.rm = TRUE) > 0, "do", "dont")
    res <- if (decision == "do") outs$EVPI_do else outs$EVPI_dont
    names(res) <- outs$variable
    
    outputs[[out_var]] <- outs
    outputs[[out_var]]$EVPI <- res
    outputs[[out_var]]$decision <- decision
    
    if (write_table) {
      if (is.na(outfolder)) {
        outfolder <- "."
      }
      write.csv(outs, paste(file.path(outfolder, paste("EVPI_table_", out_var, ".csv", sep = ""))))
    }
    
    print(paste("Output variable ", which(out_var == outvars), " (", out_var, ") completed.", sep = ""))
  }
  
  attr(outputs, "class") <- c("EVPI_outputs", "list")
  return(invisible(outputs))
}
