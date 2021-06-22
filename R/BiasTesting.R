#' Bias Testing of Measured vs. Expected Results
#'
#' This function calculates the mean error, mean absolute error,
#' RMSE, mean percent standard error, and mean percent error using
#' observed values as denominator and not taking abs values.
#'
#'@param estim Estimated Values
#'@param measu Measured Values
#'
#'@details The length of the estimated and measured values must be equal.
#'
#'@returns "mean error", "mean absolute error", "RMSE", "Mean % SE", "PB"
#'@family Stats Functions
#'
#'@author Aaron Weiskittel
#'
#'@export


Bias <- function(estim, measu) # it can be combined with the previous one
{
  if (length(estim) != length(measu)) warning("length has to be equal")

  bias <- array(dim = 5)
  bias[1] <- sum(measu - estim) / length(estim) # mean error
  bias[2] <- sum(abs(measu - estim)) / length(estim) # mean absolute error (model accuracy)
  bias[3] <- sqrt(sum((measu - estim)^2) / length(estim)) # RMSE (average magnitude of errors)
  # bias[4]=100*sum(abs(measu-estim))/sum(measu)            #different version of Residuals (%)
  bias[4] <- mean(abs((measu - estim) / estim) * 100) # mean percent standard error (see Parresol 1999)
  bias[5] <- mean(abs((measu - estim) / measu) * 100) # mean percent error using observed values as denominator and not taking abs values

  names(bias) <- c("mean error", "mean absolute error", "RMSE", "Mean % SE", "PB")
  return(bias)
}
