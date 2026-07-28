#' Create a Validator Object for Collecting Errors and Warnings
#'
#' Creates a simple validator object used internally to accumulate validation
#' errors and warnings during function execution without stopping immediately.
#' Messages are collected and can be emitted together at the end of a function
#' call using \code{throw()}.
#'
#' @return A list with methods:
#' \itemize{
#'   \item \code{check(condition, msg = NULL, msg_cli = NULL,
#'   type = "error")}: Evaluates \code{condition}. If it evaluates to
#'     \code{FALSE}, records either an error or warning depending on
#'     \code{type}. The message is chosen in the following order:
#'     \code{msg_cli} (when \code{cli} is available), \code{msg}, or a
#'     default message derived from the unevaluated \code{condition}
#'     expression. Invalid conditions (not a single logical value) are
#'     recorded as errors.
#'   \item \code{require_package(pkg, github_source = NULL)}: Checks if
#'     \code{pkg} is installed and, if not, records an error message with
#'     installation instructions. If \code{github_source} is provided, the
#'     instructions point to GitHub via
#'     \code{devtools::install_github()}, otherwise to CRAN via
#'     \code{install.packages()}.
#'   \item \code{errors()}: Returns the character vector of collected error
#'     messages.
#'   \item \code{warnings()}: Returns the character vector of collected
#'     warning messages.
#'   \item \code{throw()}: Emits all collected warnings and errors. Warnings
#'     are emitted first using \code{cli::cli_warn()} if \code{cli} is
#'     available, otherwise via \code{warning()}. Errors are then thrown using
#'     \code{cli::cli_abort()} if \code{cli} is available, otherwise via
#'     \code{stop()}. Returns \code{invisible(NULL)} if no messages have been
#'     collected.
#'   \item \code{clear()}: Resets the collected errors and warnings to empty
#'     character vectors.
#' }
#'
#' @details
#' Whether \code{cli} is available is checked once when the validator is
#' created. This ensures that all subsequent messages use the same backend for
#' formatting and signalling.
#'
#' The validator allows functions to perform several input checks before
#' stopping execution. This enables reporting multiple validation problems at
#' once rather than failing at the first encountered issue.
#'
#' @keywords internal
z_validator <- function() {

  # initialise
  errors <- character()
  warnings <- character()
  use_cli <- requireNamespace("cli", quietly = TRUE)

  # check function: assess conditions
  check <- function(condition, msg = NULL, msg_cli = NULL, type = "error") {

    if (length(condition) != 1 || !is.logical(condition) || is.na(condition)) {
      errors <<- c(errors, paste0("validation failed: condition must be a single logical value (TRUE/FALSE), but got: ", deparse(substitute(condition))))
      return(invisible(NA))
    }

    if (!isTRUE(condition)) {
      if (type == "error") {
        errors <<- c(
          errors,
          if (use_cli && !is.null(msg_cli)) msg_cli else if (!is.null(msg)) msg else paste0("validation failed: ", deparse(substitute(condition)))
        )
      } else if (type == "warning") {
        warnings <<- c(
          warnings,
          if (use_cli && !is.null(msg_cli)) msg_cli else if (!is.null(msg)) msg else paste0("validation warning: ", deparse(substitute(condition)))
        )
      }
      return(invisible(FALSE))
    }
    return(invisible(TRUE))
  }

  # check function: package available
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

  # throw all warnings and errors
  throw <- function(.call = NULL) {

    # default `.call`
    if (is.null(.call)) .call <- sys.call(sys.parent(2))

    # prevent debugger
    old <- getOption("error")
    on.exit(options(error = old), add = TRUE)
    options(error = NULL)

    if (length(warnings)) {

      # throw warnings depending on package availability
      if (use_cli) {
        cli::cli_warn(warnings, .call = .call)
      } else {
        warning(
          paste0("\u2022 ", paste(warnings, collapse = "\n\u2022 ")),
          call. = .call
        )
      }
    }

    if (length(errors)) {

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

  # clear state
  clear <- function() {
    errors <<- character()
    warnings <<- character()
  }

  # return class functions
  list(
    check = check,
    require_package = require_package,
    errors = function() errors,
    warnings = function() warnings,
    throw = throw,
    clear = clear
  )
}

