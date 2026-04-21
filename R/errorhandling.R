#' Create a Validator Object for Collecting Errors
#'
#' Creates a simple validator object used internally to accumulate validation
#' errors during function execution without stopping immediately.
#'
#' @return A list with methods:
#' \itemize{
#'   \item \code{check(condition, msg = NULL, msg_cli = NULL)}: Evaluates
#'     \code{condition} and, if \code{FALSE}, records an error message.
#'     The message is chosen in the following order: \code{msg_cli} (when
#'     \code{cli} is available), \code{msg}, or a default message derived
#'     from the unevaluated \code{condition} expression.
#'   \item \code{require_package(pkg, github_source = NULL)}: Checks if
#'     \code{pkg} is installed and, if not, records an error message with
#'     installation instructions. If \code{github_source} is provided, the
#'     instructions point to GitHub via
#'     \code{devtools::install_github()}, otherwise to CRAN via
#'     \code{install.packages()}.
#'   \item \code{errors()}: Returns the character vector of collected error
#'     messages.
#'   \item \code{throw()}: If any errors have been collected, throws them all
#'     at once via \code{cli::cli_abort()} if \code{cli} is available,
#'     otherwise via \code{stop()}. Returns \code{invisible(NULL)} if no
#'     errors are present.
#'   \item \code{clear()}: Resets the collected errors to an empty character
#'     vector.
#' }
#'
#' @details
#' Whether \code{cli} is available is checked once when the validator is
#' created, ensuring that \code{check()}, \code{require_package()}, and
#' \code{throw()} consistently use the same backend for message formatting
#' and error signaling.
#'
#' @export
z_validator <- function() {

  errors <- character()

  use_cli <- requireNamespace("cli", quietly = TRUE)

  check <- function(condition, msg = NULL, msg_cli = NULL) {
    if (!condition) {
      errors <<- c(
        errors,
        if (use_cli && !is.null(msg_cli)) msg_cli else if (!is.null(msg)) msg else paste0("validation failed: ", deparse(substitute(condition))))
    }
  }

  require_package <- function(pkg, github_source = NULL) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (use_cli) {
        errors <<- c(
          errors,
          if (!is.null(github_source)) {
            paste0("Package {.pkg ", pkg, "} is required. ",
                   "{.run [Install from GitHub](devtools::install_github('", github_source, "/", pkg, "'))}")
          } else {
            paste0("Package {.pkg ", pkg, "} is required. ",
                   "{.run [Install from CRAN](devtools::install_cran('", pkg, "'))}")
          }
        )
      } else {
        errors <<- c(
          errors,
          if (!is.null(github_source)) {
            paste0("Package `", pkg, "` is required. Install from GitHub: ",
                   "`devtools::install_github('", github_source, "/", pkg, "')`")
          } else {
            paste0("Package `", pkg, "` is required. Install from CRAN: ",
                   "`install.packages('", pkg, "')`")
          }
        )
      }
    }
  }

  throw <- function(.call = NULL) {
    if (length(errors)) {

      # default `.call`
      if (is.null(.call)) .call <- sys.call(sys.parent(2))

      # prevent debugger
      old <- getOption("error")
      on.exit(options(error = old), add = TRUE)
      options(error = NULL)

      # throw error messages depending on package availability
      if (use_cli) {
        cli::cli_abort(errors, .call = .call)
      } else {
        stop(
          paste0("- ", paste(errors, collapse = "\n- ")),
          call. = .call
        )
      }
    }

    return(invisible(NULL))
  }

  clear <- function() {
    errors <<- character()
  }

  list(
    check = check,
    require_package = require_package,
    errors = function() errors,
    throw = throw,
    clear = clear
  )
}
