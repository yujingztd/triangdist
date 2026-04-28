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


#' Distribution function (ptriang)
#'
#' @param q vector of quantiles.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode mode of the distribution (c).
#' @return A vector of probabilities.
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' must be between 'min' and 'max'.")
  }

  p_res <- ifelse(q <= min, 0,
                  ifelse(q >= max, 1,
                         ifelse(q > mode,
                                ((q - min) ^ 2) / ((max - min) * (mode - min)),
                                1 - (((max - q) ^ 2) / ((max - min) * (max - mode))))))
  return(p_res)
}


#' Quantile function (qtriang)
#'
#' @param p vector of probabilities.
#' @param min lower limit of the distribution (a).
#' @param max upper limit of the distribution (b).
#' @param mode mode of the distribution (c).
#' @return A vector of quantiles.
#' @export
qtriang <- function(p, min, max ,mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be grater tahn 'max'.")
  }
  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' must be between 'min' and 'max'.")
  }
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("Error: 'p' must be between 0 and 1.")
  }

  p_mode <- (mode - min) / (max - min)

  q_res <- ifelse(p <= p_mode,
    min + sqrt(p * (max - min) * (mode - min)),
    max - sqrt((1 - p) * (max - min) * (max - mode)))

  return(q_res)
}










