#' Mean and variance of coefficient of determination
#'
#' The distribution of $R^2$ (as an estimate of $\rho^2$, the multiple correlation
#' coefficient in a linear regression model) has been shown to have a
#' quite compicated formula whereas at least its mean and variance can be estimated
#' using a hypergeometric function.
#'
#' @param rho2 value of $\rho^2$ in population
#' @param n sample size
#' @param p number of independent variables used for
#' estimate of multiple correlation
#'
#' @return Mean/variance of the approximated $R^2$ distribution as given in reference.
#' @export
#' @name mean_r2
#' @seealso Algorithms based on (i) and (ii) in:
#' Croux, C., & Dehon, C. (2003). Estimators of the multiple correlation coefficient:
#' Local robustness and confidence intervals. Statistical Papers, 44(3), 315-334.
#'
#' @examples
mean_r2 <- function(rho2, n, p) {
  1 - ((n - p) / (n - 1)) * (1 - rho2) * Re(hypergeo::hypergeo(1, 2, .5 * (n + 1), rho2))
}

#' @rdname mean_r2
#' @export
var_r2 <- function(rho2, n, p) {
  (
    (((n - p) * (n - p + 2)) / ((n - 1) * (n + 1))) *
      (1 - rho2) *
      Re(hypergeo::hypergeo(2, 2, .5 * (n + 3), rho2))
  ) - (1 - mean_r2(rho2, n, p)) ^ 2
}



#' Adjusted $R^2$
#'
#' Adjusted versions of $R^2$ (as an estimate of $\rho^2$, the multiple correlation
#' coefficient in a linear regression model).
#'
#' @section Correction formulas:
#'
#' \describe{
#' \item{smith}{ \bar{R}^2 = 1 - (1-R^2)\frac{n}{n-p}}
#' \item{ezekiel}{ \bar{R}^2 = 1 - (1-R^2)\frac{n-1}{n-p-1}}
#' \item{wherry}{ \bar{R}^2 = 1 - (1-R^2)\frac{n-1}{n-p}}
#' \item{olkin_pratt}{ \bar{R}^2 = 1 - (1-R^2)\frac{n-3}{n-p-1}F(1, 1, \frac{n - p+1}{2}, 1 - R^2)}
#' \item{olkinh_pratt1}{ \bar{R}^2 = 1 - (1-R^2)\frac{n-3}{n-p-1}\left[ 1 +\frac{2(1-R^2)}{n-p-1}\right]}
#' \item{olkinh_pratt2}{  \bar{R}^2 = 1 - (1-R^2)\frac{n-3}{n-p-1}\left[ 1 +\frac{2(1-R^2)}{n-p-1} +
#' \frac{8(1-R^2)^2}{(n-p-1)(N-p+3)}\right]}
#' \item{pratt}{\bar{R}^2 = 1 - (1-R^2)\frac{n-3}{n-p-1}\left[ 1 +\frac{2(1-R^2)}{n-p-2.3}\right]}
#' \item{claudy}{\bar{R}^2 = 1 - (1-R^2)\frac{n-4}{n-p-1}\left[ 1 +\frac{2(1-R^2)}{n-p-1}\right]}
#' }
#'
#' @param r2 estimated multiple correlation coefficient
#' @param n sample size
#' @param p number of independent variables used for
#' estimate of multiple correlation
#' @param adj name of correction formulas as character vector (see section "Correction formulas)
#'
#' @return named vector with adjusted $R^2$
#' @export
#'
#' @examples
adjusted_r2 <- function(r2, n, p, adj = c("smith", "ezekiel", "wherry",
                                          "olkin_pratt", "olkin_pratt1",
                                          "olkin_pratt2", "pratt", "claudy")) {

  smith        <- function() 1 - (n       / (n - p))     * (1 - r2)
  ezekiel      <- function() 1 - ((n - 1) / (n - p - 1)) * (1 - r2)
  wherry       <- function() 1 - ((n - 1) / (n - p    )) * (1 - r2)
  olkin_pratt  <- function() 1 - (((n - 3) * (1 - r2)) / (n - p - 1))
                             * Re(hypergeo::hypergeo(1, 1, (n - p + 1) / 2, 1 - r2))
  olkin_pratt1 <- function() 1 - (((n - 3) * (1 - r2)) / (n - p - 1)) *
                             (1 + (2 * (1 - r2)) / (n - p - 1))
  olkin_pratt2 <- function() 1 - (((n - 3) * (1 - r2)) / (n - p - 1)) *
                             ((1 + (2 * (1 - r2)) / (n - p - 1)) +
                             ((8 * (1 - r2) ^ 2)   / ((n - p - 1) *  (n - p + 3))))
  pratt        <- function() 1 - (((n - 3) * (1 - r2)) / (n - p - 1)) *
                             (1 + (2 * (1 - r2)) / (n - p - 2.3))
  claudy       <- function() 1 - (((n - 4) * (1 - r2)) / (n - p - 1)) *
                             (1 + (2 * (1 - r2)) / (n - p - 1))

  x <- vapply(adj, do.call, numeric(1), list(), envir = environment())
  names(x) <- adj
  x
}



#' Calculate multiple correlation coefficient and related metrics
#'
#' @param x object of class \code{\link{lm}}
#' @param rho2 real $\rho^2$ based on population data or theoretical
#' to calculate \code{\link{mean_r}} and \code{\link{var_r2}}
#' @param ... arguments passed to \code{\link{adjusted_r2}}
#'
#' @return named numeric vector
#' @export
#'
#' @examples
r2 <- function(x, rho2 = NULL, ...) {
  r2 <- summary(x)$r.squared
  n  <- length(residuals(x))
  p  <- length(x$coefficients) - 1
  res <- c(r2 = r2, adjusted_r2(r2, n, p, ...))
  if (!is.null(rho2))
    res <- c(res, mean_r2 = mean_r2(rho2, n, p), var_r2 = var_r2(rho2, n, p))
  res
}
