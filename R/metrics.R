
#' Compute R2 and RSME for subsamples of data
#'
#' @param ss object as output from \code{\link{subsamples}}
#' @param method argument passed to \code{\link{caret_step}}
#' @return object of class "metrics", which is a list of two data.frames,
#' "Rsquared" and "RMSE where each has columns corresponding
#' to sample sizes and rows corresponding to repeated samples.
#' @export
metrics <- function(ss, method = "none") {
  stopifnot(inherits(ss, "subsamples"))
  metrics  <- function(df) caret_step(df, control_method = method)
  m <- lapply(ss, function(Ni) lapply(Ni, metrics))
  m <- lapply(m, function(Ni) t(as.data.frame(Ni)))
  m <- lapply(m, function(Ni) {rownames(Ni) <- NULL; Ni})
  extract_metric <- function(column) {
    x <- t(as.data.frame(lapply(m, function(x) x[, column])))
    colnames(x) <- attr(ss, "n.sample")
    rownames(x) <- seq_len(attr(ss, "N"))
    x
  }
  x <- list(Rsquared = extract_metric(1), RMSE = extract_metric(2))
  structure(x,  n.sample = attr(ss, "n.sample"),  N = attr(ss, "N"),
            real_Rsquared = attr(ss, "real_Rsquared"), real_RMSE = attr(ss, "real_RMSE"),
            class = c("metrics", "list"))
}





#' Use caret to calculate model metric
#'
#' @param d data frame as given by \code{\link{subsamples}}
#' @param control_method argument passed to \code{\link{trainControl}}
#' as its "method" argument
#' @param ... arguments passed to \code{\link{trainControl}}
#' @return Named numeric vector of length two with Rsquared and RMSE
#' @export
caret_step <- function(d, control_method){
  lg(fun = log4r::debug)
  x <- caret::train(Y ~ ., data = d, method = "lm",
                    trControl = caret::trainControl(method = control_method,
                        number = switch(control_method, cv = 10, repeatedcv = 10, none = 1, 25))) # dra upp till 1000 för boot!

  if (control_method != "none")
    c(Rsquared = mean(x$resample$Rsquared),
      RMSE     = mean(x$resample$RMSE))
  else
    c(Rsquared = summary(x$finalModel)$r.squared,
      RMSE     = sqrt(mean(residuals(x$finalModel) ^ 2)))
}

