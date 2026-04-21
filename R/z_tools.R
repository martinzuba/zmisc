#' Copy a data frame to the clipboard for Excel
#'
#' Copy a data frame or matrix to the system clipboard in a format that is
#' optimized for pasting into Microsoft Excel.
#'
#' This function is designed as an **Excel clipboard bridge**, not a general
#' export utility. It formats tabular data in a way that Excel can directly
#' interpret when pasted, including locale-aware decimal handling on Windows.
#'
#' @param x A data frame, matrix, or object coercible to a tabular structure.
#' @param file Destination file. Defaults to `"clipboard"`.
#' @param row.names Logical. Whether to include row names. Default is `FALSE`.
#' @param col.names Logical. Whether to include column names. Default is `TRUE`.
#' @param sep Field separator. Defaults to tab (`"\\t"`), which is required for
#'   reliable Excel clipboard pasting.
#' @param dec Decimal separator. If `NULL` (default), it is determined in the
#'   following order:
#'   \enumerate{
#'     \item `getOption("ztools.dec")`, if set
#'     \item Windows system locale (registry setting, if available)
#'     \item `Sys.localeconv()$decimal_point` as fallback
#'   }
#' @param na Character string used for missing values. Default is `""`.
#' @param ... Additional arguments passed to `utils::write.table()`.
#'
#' @details
#' The function is designed specifically for Excel interoperability via the
#' clipboard. Unlike generic export functions, it prioritizes compatibility
#' with Excel's parsing rules rather than portable file formats.
#'
#' On Windows systems, the decimal separator is inferred from system locale
#' settings when not explicitly provided. This ensures pasted numeric values
#' are correctly interpreted by Excel in most regional configurations.
#'
#' @section Configuration:
#' The default decimal separator can be overridden globally using:
#'
#' \preformatted{
#' options(ztools.dec = ".")
#' }
#'
#' @section Behavior:
#' - Uses tab-separated values for maximum Excel compatibility
#' - Attempts to match Excel's expected decimal separator automatically
#' - Falls back safely to system locale or `"."` if detection fails
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect
#' (copying data to clipboard).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3))
#'
#' # Copy to clipboard for Excel
#' copy(df)
#'
#' # Force decimal separator
#' copy(df, dec = ".")
#'
#' # Set global preference
#' options(ztools.dec = ",")
#' copy(df)
#' }
#'
#' @export
copy <- function(
    x,
    file = "clipboard",
    row.names = FALSE,
    col.names = TRUE,
    sep = "\t",
    dec = NULL,
    na = "",
    ...
) {

  # find default values
  if (is.null(dec)) {

    if (!is.null(getOption("ztools.dec"))) {
      dec <- getOption("ztools.dec")
    } else {
      dec <- tryCatch(
        utils::readRegistry("Control Panel\\International", "HCU")$sDecimal,
        error = function(e) Sys.localeconv()$decimal_point
      )

      # give hint about dec symbol
      if (requireNamespace("cli", quietly = TRUE) & requireNamespace("rlang", quietly = TRUE)) {
        cli::cli_alert_info(
          "Using decimal separator `{dec}`. \nReconfigure using {.emph `dec`} parameter or configure via {.emph options}, e.g.\n{.run [rlang::push_options(\"ztools.dec\" = \".\")](rlang::push_options(\"ztools.dec\" = \".\"))} or {.run [rlang::push_options(\"ztools.dec\" = \",\")](rlang::push_options(\"ztools.dec\" = \",\"))}"
        )
      } else {
        message(
          paste0("Using decimal separator `", dec, "`. \nReconfigure using `dec` parameter or configure via options,\ne.g. `options(\"ztools.dec\" = \".\")` or `options(\"ztools.dec\" = \",\")`}")
        )
      }
    }
  }

  write.table(x = x, file = file, sep = sep, dec = dec, row.names = row.names, col.names = col.names, na = na, ...)

  return(invisible(NULL))

}


