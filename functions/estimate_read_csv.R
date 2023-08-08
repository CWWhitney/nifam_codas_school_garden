# Read the csv input file

source("functions/as.estimate.R")

estimate_read_csv <- function (fileName, strip.white = TRUE, ...) 
{
  marginal <- NULL
  correlation_matrix <- NULL
  marginalFilename <- fileName
  # read.csv from utils
  marginal <- read.csv(marginalFilename, strip.white = strip.white, 
                       stringsAsFactors = FALSE, ...)
  # subset from base
  marginal <- subset(marginal, variable != "")
  marginal <- data.frame(marginal, row.names = "variable")
  correlationFilename <- gsub(".csv", "_cor.csv", marginalFilename)
  if (file.exists(correlationFilename)) 
    # use data.matrix from base R and read.csv from utils
    correlation_matrix <- data.matrix(read.csv(correlationFilename, 
                                               row.names = 1))
  # as.estimate from another function
  as.estimate(marginal, correlation_matrix = correlation_matrix)
}