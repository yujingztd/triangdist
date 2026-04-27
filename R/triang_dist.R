#' Triangular Distribution PDF
#'
#' Density function (dtriang)
#'
#' @param x vector of quantiles.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode mode of the distribution (c).
#' @return A vector of densities.
#' @export
dtriang <- function(x, min, max, mode) {

  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' must be between 'min' and 'max'.")
  }

  d_res <- ifelse(x < min | x > max, 0,
                  ifelse(x < mode,
                         (2 * (x - min)) / ((max - min) * (mode - min)),
                         (2 * (max - x)) / ((max - min) * (max - mode))))

  return(d_res)
}
