#' Create a Validator Object for Collecting Errors
#'
#' Creates a simple validator object used internally to accumulate validation
#' errors during function execution without stopping immediately.
#'
#' @return A list with methods:
#' \itemize{
#'   \item \code{check(condition, msg)}: Record an error message if condition is TRUE
#'   \item \code{get()}: Return collected error messages
#'   \item \code{throw()}: Throw all collected errors using \code{z_error()}
#'   \item \code{clear()}: Reset stored errors
#' }
#'
#' @keywords internal
z_validator <- function() {

  errors <- character()

  check <- function(condition, msg) {
    if (condition) {
      errors <<- c(errors, msg)
    }
  }

  get <- function() errors

  throw <- function() {
    if (length(errors)) {
      z_error(errors)
    }
  }

  clear <- function() {
    errors <<- character()
  }

  list(
    check = check,
    get = get,
    throw = throw,
    clear = clear
  )
}


#' Throw Collected Error Messages
#'
#' Throws an error using collected messages, with optional cli formatting
#' if available.
#'
#' @param messages Character vector of error messages.
#'
#' @keywords internal
z_error <- function(messages) {

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_abort(c(messages))
  } else {
    stop(
      paste0("- ", paste(messages, collapse = "\n- ")),
      call. = FALSE
    )
  }
}
