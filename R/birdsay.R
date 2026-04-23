#' Make a Bird Speak Wisdom or Messages
#'
#' `birdsay()` prints an ASCII bird alongside a message, optionally sourced
#' from the \code{fortunes} or \code{statquotes} packages. It supports styled
#' output, multi-line formatting, and automatic citation highlighting.
#'
#' If no message is provided, a random source is selected and a fortune or
#' statistical quote is displayed (if available). If the required package is
#' not installed, the bird will instead prompt the user to install it.
#'
#' @param say Character string or character vector. The message to display.
#'   If \code{NULL}, a random quote is generated from the selected source.
#'
#' @param bird Character vector defining the ASCII bird. If \code{NULL},
#'   a default bird is used, created by jgs.
#'
#' @param width Integer. Maximum width used when formatting fortune/statquote
#'   output. Defaults to 100.
#'
#' @param bird_style Function applied to each line of the ASCII bird.
#'   Defaults to random colours.
#'
#' @param text_style Function applied to normal message text lines.
#'   Defaults to \code{identity}.
#'
#' @param citation_style Function applied to citation lines (e.g. authorship
#'   or source markers). Defaults to italic ANSI styling.
#'
#' @param output_function Function or character string. Determines how the final
#'   output is emitted. Defaults to \code{message}. Can be any function that takes
#'   a single character string (e.g. \code{message}, \code{cat}, \code{print}).
#'   If a character string is provided, it will be resolved via \code{get()}.
#'
#' @param source Character string or \code{NULL}. One of
#'   \code{"fortunes"}, \code{"statquotes"}, or \code{NULL}.
#'   If \code{NULL}, the source is randomly selected.
#'
#' @details
#' Citation rendering rules:
#' \itemize{
#'   \item Lines starting with \code{"   --"} are treated as fortune-style citations.
#'   \item Lines starting with \code{"---"} begin statquote-style citation blocks.
#'   \item Statquote citation blocks may span multiple lines and remain indented.
#' }
#'
#' The function does not depend on any external packages. If \code{fortunes}
#' or \code{statquotes} are unavailable, fallback messages are displayed.
#'
#' The output is constructed as a single character string and passed to
#' \code{output_function}, allowing flexible integration with different
#' output contexts (e.g. console, logs, or UI frameworks).
#'
#' @return Invisibly returns the formatted character string (the rendered bird
#'   and message), and emits it using \code{output_function}.
#'
#' @examples
#' \dontrun{
#' birdsay()
#'
#' birdsay(say = "Hello from the bird!")
#'
#' birdsay(source = "fortunes")
#'
#' birdsay(source = "statquotes")
#'
#' birdsay(
#'   say = "Custom message with styling",
#'   text_style = function(x) paste0("\033[36m", x, "\033[39m")
#' )
#'
#' # Use cat instead of message
#' birdsay(output_function = cat)
#'
#' # Specify output function by name
#' birdsay(output_function = "print")
#' }
#'
#' @importFrom utils capture.output
#' @export
birdsay <- function(
    say = NULL,
    bird = NULL,
    width = 100,
    bird_style = NULL,
    text_style = NULL,
    citation_style = NULL,
    output_function = message,
    source = NULL
) {

  # default bird
  if (is.null(bird)) {
    bird <- c(
      '                      .--.',
      '                    ."  o \\\\__',
      '                 _.-"    ,(  `',
      '             _.-"      ,;;|',
      '        _.-=" _,"    ,,;;;\'',
      '    .-\"`_.-"``-..,,;;;;:\'',
      '    `"\'`          `\\`\\',
      '        jgs        /^\\\\\\'
    )
  }

  bird_width <- max(nchar(bird))

  # default styles

  if (is.null(bird_style)) {
    bird_colour <- sample(c(31:36), size = 1)
    bird_style <- function(x) paste0("\033[", bird_colour, "m", x, "\033[39m")
  }
  if (is.null(text_style)) text_style <- identity
  if (is.null(citation_style)) citation_style <- function(x) paste0("\033[2;3m", x, "\033[0m")


  # default say
  if (is.null(say)) {

    # check available packages
    has_fortunes <- requireNamespace("fortunes", quietly = TRUE)
    has_statquotes <- requireNamespace("statquotes", quietly = TRUE)


    if (is.null(source)) {

      # select source randomly
      source <- sample(c("statquotes", "fortunes"), size = 1)

    } else {
      if (!source %in% c("statquotes", "fortunes")) stop("`source` must be \"statquotes\", \"fortunes\" or `NULL`.")
    }

    if (source == "statquotes" && has_statquotes) {
      say <- paste(
        capture.output(print(statquotes::statquote(), width = width - bird_width)),
        collapse = "\n"
      )
    } else if (source == "fortunes" && has_fortunes) {
      say <- paste(
        capture.output(print(fortunes::fortune(), width = width - bird_width)),
        collapse = "\n"
      )
    } else {
      say <- paste0("A wise bird says: install the `", source, "` package for more wisdom!")
    }
  }

  # Ensure 'say' is a character string before splitting
  say <- as.character(say)
  lines <- strsplit(say, "\n")[[1]]

  # remove first line padding if lines length is larger in relation to bird length
  if (length(lines) >= length(bird) * 0.8) {
    lines <- lines[-1]
  }

  num_bird_lines <- length(bird)
  num_message_lines <- length(lines)
  padding_base <- 3

  # prepare the bird
  the_message <- lapply(
    seq_len(max(num_bird_lines, num_message_lines)),
    function(i) {
      bird_line <- if (i <= num_bird_lines) {
        bird_style(bird[i])
      }  else {
        strrep(" ", bird_width)
      }

      bird_len <- if (i <= num_bird_lines) nchar(bird[i]) else bird_width
      padding_needed <- bird_width - bird_len + padding_base
      padding <- paste(rep(" ", max(0, padding_needed)), collapse = "")

      message_line <- if (i <= num_message_lines) {

        # NOTE: citation detection logic is intentional and order-dependent.
        # Do not refactor without visual regression testing.
        if (any(startsWith(lines[1:i] , "   --")) || startsWith(lines[i], "---")) {
          citation_style(lines[i])
        } else if (any(startsWith(lines[1:(i-1)], "---"))) {
          paste("   ", citation_style(lines[i]))
        } else {
          text_style(lines[i])
        }

      } else ""

      paste(bird_line, padding, message_line, collapse = "")

    }
  )

  # output

  output_fn <- if (is.character(output_function)) get(output_function) else output_function

  output_fn(paste(the_message, collapse = "\n"))

  # output_fn(bird_line, padding, message_line, if (output_function == "cat") "\n")
  #
  # for (i in seq_len(max(num_bird_lines, num_message_lines))) {
  #
  #
  #
  # }

}

