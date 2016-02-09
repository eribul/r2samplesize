
#' Plot method for class "metrics"
#'
#' Boxplots for each samlpe size for Rsquared and RMSE
#'
#' @param x object of class "metrics" (see \code{\link{metrics}})
#' @param ... arguments passed to \code{\link{boxplot}}
#'
#' @return The function is called for its side effects.
#' @export
plot.metrics <- function(x, ...) {
  bp <- function(names_x, ref, ...) {
    boxplot(x[[names_x]], xlab = "sample size", ylab = names_x, ...)
    abline(h = ref, col = "darkblue", lty = 2)
  }
  invisible(mapply(bp, names(x), c(attr(x, "real_Rsquared"),
    attr(x, "real_RMSE")), MoreArgs = list(...)))
}

