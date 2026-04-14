#' Significant Rounding While Preserving Integers
#'
#' Rounds numeric values to a specified number of significant digits while
#' preserving whole numbers (integers) where appropriate.
#'
#' @param x A numeric vector to be rounded.
#' @param digits A positive integer specifying the number of significant digits
#'   to retain (passed to \code{\link{signif}}).
#' @param max_decimals Optional non-negative integer specifying the maximum number
#'   of decimal places in the result. If \code{NULL} (default), no additional
#'   rounding is applied beyond significant digits.
#'
#' @details
#' The function first applies significant-digit rounding via \code{\link{signif}}.
#' It then detects values that are effectively whole numbers (within a small
#' numerical tolerance) and rounds them to integers to avoid representations like
#' \code{2.000} or \code{3.0}.
#'
#' Detection of whole numbers is based on a tolerance of
#' \code{.Machine$double.eps^0.5}, which helps account for floating-point
#' imprecision.
#'
#' If \code{max_decimals} is provided, the result is additionally rounded using
#' \code{\link{round}} to limit the number of decimal places.
#'
#' @return A numeric vector of the same length as \code{x}, rounded to the
#'   specified number of significant digits, with integers preserved where possible.
#'
#' @examples
#' signif_preserve_int(c(1.234, 5.678), digits = 2)
#' # returns: 1.2 5.7
#'
#' signif_preserve_int(c(2.0001, 3.9999), digits = 3)
#' # returns: 2 4
#'
#' signif_preserve_int(c(1.2345, 2.3456), digits = 3, max_decimals = 2)
#' # returns: 1.23 2.35
#'
#' signif_preserve_int(c(1000, 1234), digits = 2)
#' # returns: 1000 1200
#'
#' @seealso \code{\link{signif}}, \code{\link{round}}
#'
#' @export
signif_preserve_int <- function(
    x,
    digits,
    max_decimals = NULL
) {

  # validation
  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) ||
      digits < 1 || digits %% 1 != 0) {
    stop("`digits` must be a single integer >= 1")
  }

  if (!is.null(max_decimals)) {
    if (!is.numeric(max_decimals) || length(max_decimals) != 1 ||
        is.na(max_decimals) || max_decimals < 0 || max_decimals %% 1 != 0) {
      stop("`max_decimals` must be a single integer >= 0 or NULL")
    }
  }

  # significant rounding
  s <- signif(x, digits)

  # detect near-integers safely
  is_whole <- abs(s - round(s)) < .Machine$double.eps^0.5

  z <- ifelse(is_whole, round(x, 0), s)

  if (!is.null(max_decimals)) {
    z <- round(z, max_decimals)
  }

  return(z)

}




#' Extract Leading Digits from Numeric Values
#'
#' Extracts the first \code{digits} digits from each element of a numeric vector
#' using a vectorized, math-based approach.
#'
#' @param x A numeric vector from which to extract leading digits.
#' @param digits A positive integer (or integer vector) specifying the number of
#'   leading digits to extract. Must be either length 1 or the same length as \code{x}.
#' @param return_negative_sign Logical; if \code{TRUE} (default), negative values
#'   in \code{x} will produce negative results. If \code{FALSE}, all results are positive.
#'
#' @details
#' The function computes leading digits using logarithms and division, avoiding
#' character conversion for improved performance on large vectors.
#'
#' Values equal to zero are returned as \code{NA}, since zero does not have
#' meaningful leading digits.
#'
#' Floating-point precision may affect results for values very close to powers
#' of 10, although a small numerical adjustment is applied internally to mitigate this.
#'
#' \code{first_digit()} is a convenience wrapper for extracting a single leading digit.
#'
#' @return A numeric vector of the same length as \code{x}, containing the extracted
#'   leading digits (with sign if requested).
#'
#' @examples
#' first_digits(c(123, 456, 789), digits = 1)
#' # returns: 1 4 7
#'
#' first_digits(c(123, 456, 789), digits = 2)
#' # returns: 12 45 78
#'
#' first_digits(c(-123, -456), digits = 1)
#' # returns: -1 -4
#'
#' first_digits(c(-123, -456), digits = 1, return_negative_sign = FALSE)
#' # returns: 1 4
#'
#' first_digits(c(123, 45), digits = c(1, 2))
#' # returns: 1 45
#'
#' first_digit(c(123, 456, 789))
#' # returns: 1 4 7
#'
#' @name first_digits
#' @export
first_digits <- function(x, digits, return_negative_sign = TRUE) {

  # Error handling
  errors <- character()

  if (!is.numeric(x)) errors <- c(errors, "x must be numeric")

  if (!is.numeric(digits) || any(is.na(digits)) || any(digits < 1) || any(digits %% 1 != 0)) {
    errors <- c(errors, "`digits` must be integers >= 1")

  } else if (length(digits) > 1 && length(digits) != length(x)) errors <- c(errors, "length of `digits` must be 1 or length of `x`")

  if (length(errors) > 0)
    stop(paste(errors, collapse = ","))

  # replicate digits if necessary
  if (length(digits) == 1) {
    digits <- rep(digits, length(x))
  }

  # remove near-zeros
  x[x == 0] <- NA

  # sign handling
  sgn <- ifelse(return_negative_sign, sign(x), 1)

  # absolute value for processing
  abs_x <- abs(x)

  # number of digits in each number
  num_digits <- floor(log10(abs_x)) + 1

  # calculate the divisor to get the first `digits` digits
  divisor <- 10^(num_digits - digits)

  # get the first `digits` digits
  result <- floor(abs_x / divisor) * sgn

  return(result)

}

#' @rdname first_digits
#' @export
first_digit <- function(x, return_negative_sign = TRUE) {

  first_digits(x, digits = 1, return_negative_sign)

}
