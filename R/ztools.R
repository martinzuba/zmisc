
#' Round to significant digits without altering integer values
#'
#' Rounds a numeric vector to a specified number of significant digits,
#' but preserves values that would become integers after rounding.
#'
#' Optionally formats the result using locale-specific thousands and decimal separators.
#'
#' @param x Numeric vector
#' @param digits Number of significant digits
#' @param to_char Logical; if TRUE, return formatted character output
#' @param big.mark Thousands separator
#' @param decimal.mark Decimal separator
#' @param max_decimals maximum number of decimals
#' @param ... Passed to [base::prettyNum()]
#'
#' @return Numeric or character vector
#' @export
signif_preserve_int <- function(
    x,
    digits,
    to_char = FALSE,
    big.mark = NULL,
    decimal.mark = NULL,
    max_decimals = NULL,
    ...
) {


  # get separators if needed
  if (to_char && (is.null(big.mark) || is.null(decimal.mark))) {

    reg <- tryCatch(
      utils::readRegistry("Control Panel\\International", "HCU"),
      error = function(e) NULL
    )

    if (is.null(decimal.mark)) {
      decimal.mark <- if (!is.null(reg)) reg$sDecimal else Sys.localeconv()$decimal_point
    }

    if (is.null(big.mark)) {
      big.mark <- if (!is.null(reg)) reg$sThousand else Sys.localeconv()$thousands_sep
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

  if (to_char) {
    prettyNum(z, big.mark = big.mark, decimal.mark = decimal.mark, ...)
  } else {
    z
  }
}
