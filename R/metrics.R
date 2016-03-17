
#' Compute R2 and RSME for subsamples of data
#'
#' @param ss object as output from \code{\link{subsamples}}
#' @param method argument passed to \code{\link{model_step}}
#' @param n.sample sample sizes to calculate the metrics for
#' @param N number of subamples of each size. This alternative
#' can be used to reuse a subsamlpe object with larger N set by
#' \code{\link{subsamples}} instead of generating multiple subsamples
#' (this \code{N} can not be larger than the original \code{N})
#' set by \code{\link{subsamples}}).
#' @return object of class "metrics", which is a list of two data.frames,
#' "Rsquared" and "RMSE where each has columns corresponding
#' to sample sizes and rows corresponding to repeated samples.
#' @export
metrics <- function(ss, method = "none", n.sample = seq(50, 500, 50), N = length(ss)) {
  stopifnot(inherits(ss, "subsamples"),
            N <= length(ss),
            all(n.sample <= attr(ss, "n.max"))
            )

  # Capture some attributes before ss is subsetted
  real_Rsquared <- attr(ss, "real_Rsquared")
  real_RMSE     <- attr(ss, "real_RMSE")
  ss <- ss[seq_len(N)]

  metrics  <- function(df) model_step(df, control_method = method)
  # For each subsamlpe in ss ...
  m <- lapply(ss, function(Ni)
    # ... and each subsample size according to n.sample ...
    lapply(n.sample, function(n)
      # ... we calculate the metrics
      metrics(head(Ni, n))))

  m <- lapply(m, function(Ni) t(as.data.frame(Ni)))
  m <- lapply(m, function(Ni) {rownames(Ni) <- NULL; Ni})
  extract_metric <- function(column) {
    x <- t(as.data.frame(lapply(m, function(x) x[, column])))
    colnames(x) <- n.sample
    rownames(x) <- seq_len(N)
    x
  }
  x <- list(Rsquared = extract_metric(1), RMSE = extract_metric(2))
  structure(x,
            n.sample      = n.sample,
            N             = N,
            real_Rsquared = real_Rsquared,
            real_RMSE     = real_RMSE,
            class         = c("metrics", "list"))
}





#' Create model and calculate model metric
#'
#' Call \code{\link{train}} if \code{control_method} is anything else than
#' "none", otherwise call \code{\link{lm}} directly and calculate the metrics.
#'
#' @param d data frame as given by \code{\link{subsamples}}
#' @param control_method argument passed to \code{\link{trainControl}}
#' as its "method" argument
#' @param ... arguments passed to \code{\link{trainControl}}
#' @return Named numeric vector of length two with Rsquared and RMSE
#' @export
model_step <- function(d, control_method) {
  lg(fun = log4r::debug)

  # A simple linear fit is much faster than using train if not needed
  if (control_method == "none") {
    fit <- lm(Y ~ ., data = d)
    c(Rsquared = summary(fit)$r.squared,
      RMSE     = sqrt(mean(residuals(fit) ^ 2)))

  # caret::rtain is usd ffor bootstraping and other validation methods
  } else {
    fit <- caret::train(Y ~ ., data = d, method = "lm",
                      trControl = caret::trainControl(method = control_method))
    c(Rsquared = mean(fit$resample$Rsquared),
      RMSE     = mean(fit$resample$RMSE))
  }
}

