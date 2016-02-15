
#' Take subsamples from data.frame
#'
#' @param d object of class \code{\link{sim_data}}
#' @param n.max maximum subsample size
#' @param N number of subsamples
#' @return list of length \code{N} with objects of
#' class \code{\link{sim_data}}, each with \code{n.max} rows
#' @export
subsamples <- function(d, n.max = 500, N = 10) {
  stopifnot(inherits(d, "sim_data"))
  structure(replicate(N, dplyr::sample_n(d, n.max), FALSE),
            n.max         = n.max,
            real_Rsquared = attr(d, "Rsquared"),
            real_RMSE     = attr(d, "RMSE"),
            class         = c("subsamples", "list")
  )
}