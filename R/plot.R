
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
    boxplot(x[[names_x]],
            xlab  = "sample size",
            ylab  = names_x,
            names = attr(x, "n.sample"),
            ...)
    abline(h = ref, col = "darkblue", lty = 2)
  }

  invisible(
    # For both R2 and RMSE
    mapply(bp, names(x),
           c(attr(x, "real_Rsquared"), attr(x, "real_RMSE")),
           MoreArgs = list(...))
    )
}

