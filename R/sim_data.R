#' Simulate data with specified R2
#'
#'
#' @param r2 desired adjusted R2 as numeric vector of length one
#' @param n number of samples (rows)
#' @param p number of dependent variables
#' @param adjusted.r.square is the given \code{r2} value adjusted?
#' (it will then be "unadjusted")
#' @return Object of class "sim_data" wich is basically
#' a data.frame with first column "Y" and preceding
#' columns "Xi" (i = 1, ..., p) where
#' \code{summary(lm(Y ~ ., data = d))$r.squared} is approximately
#' \code{r2} and with some exrta attributes \code{Rsquared} and
#' \code{RMSE} with coresponding values based on the whole data.frame
#' @export
sim_data <- function(r2 = .2, n = 1e6, p = 5, adjusted.r.square = FALSE) {
  lg("sim_data with r2 = ", r2, ", n = ", n, ", p = ", p, " and adjusted.r.square = ", adjusted.r.square)

  # Make symmetric covarianve matrix with ...
  # ... all variance = 1 and covariances = 0 ...
  sig.mat <- diag(p + 1)
  # ... With exception for covariance between outcome and predictors
  # given by r = sqrt(R2 / p)
  if (adjusted.r.square) r2 <- unadj.r2(adj.r2, n, p)
  sig.mat[1, -1] <- sig.mat[-1, 1] <- sqrt(r2 / (p))

  # Data frame with data from multinormal data with desired R2
  d <- as.data.frame(mvtnorm::rmvnorm(n, sigma = sig.mat))
  names(d) <- c("Y", paste0("X", seq_len(p)))

  # We also return R2 and RMSE for this data to use later
  fit      <- lm(Y ~ ., data = d)
  Rsquared <- summary(fit)$r.squared
  RMSE     <- sqrt(mean(residuals(fit) ^ 2))
  structure(d, Rsquared = Rsquared, RMSE = RMSE,
            class = c("sim_data", "data.frame"))
}

