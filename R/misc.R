
#' Log messages from other functions within the package
#'
#' This function is for internal use only
#' @param ... arguments pased to \code{\link{paste}} as log message.
#' If no arguments given, \code{\link{sys.call}} will be printed to log.
#' @param fun one of the functions "debug", "info", "warn", "error" or "fatal"
#' from the "log4r" package
#' @return Nothing. The function is called for its side effects.
lg <- function(..., fun = log4r::info) {
  if (getOption("samplemetric.log", FALSE) &&
      suppressWarnings(require("log4r", quietly = TRUE, warn.conflicts = FALSE))) {
    if (!exists(".logger")) .logger <<- log4r::create.logger(logfile = 'sample_metric.log', level = "INFO")
    if (length(list(...))) fun(.logger, paste(...)) else
      fun(.logger, paste("Calling:", deparse(match.call(call = sys.call(sys.parent(2))))))
  }
}



#' Convert between adjusted and non adjusted R2
#' @param r2 (nonadjusted) R2 value
#' @param adj.r2 adjusted R2 value
#' @param n sample size
#' @param p number of parameters in linear model
#' @return numeric vector of length one
#' @seealso \url{https://en.wikipedia.org/wiki/Coefficient_of_determination#Adjusted_R2}
#' @name r2
#' @export
adj.r2 <- function(r2, n, p)
  1 - (1 - r2) * (n - 1) / (n - p - 1)

#' @rdname r2
#' @export
unadj.r2 <- function(adj.r2, n, p)
  1 - (1 - adj.r2) * (n - p - 1) / (n - 1)

