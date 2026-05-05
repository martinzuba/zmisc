 ## gggoeg theme

# goeg colours ----

# new corporate design

goeg_dunkelblau <- "#32466E"
goeg_hellblau <- "#6197B8"
goeg_hellgruen <- "#88A62E"
goeg_dunkelgruen <- "#00573F"
goeg_senfgelb <- "#D7B416"
goeg_terracotta <- "#C36432"

goeg_colours <- c(goeg_dunkelblau, goeg_hellblau, goeg_hellgruen, goeg_dunkelgruen, goeg_senfgelb, goeg_terracotta)

goeg_colour_array <- matrix(
  nrow = 6, ncol = 6,
  data = c(
    "#32466e", "#546586", "#76849e", "#99a3b7", "#bbc1cf", "#dde0e7",
    "#6197b8", "#7ba8c4", "#96bad0", "#b0cbdc", "#cadce7", "#e5eef3",
    "#7a9b06", "#90ac30", "#a6bc59", "#bdcd83", "#d3deac", "#e9eed6",
    "#00573f", "#2b735f", "#558f7f", "#80ab9f", "#aac7bf", "#d5e3df",
    "#d4c315", "#dbcd3c", "#e2d763", "#eae18a", "#f1ebb1", "#f8f5d8",
    "#c36432", "#cd7e54", "#d79876", "#e1b299", "#ebcbbb", "#f5e5dd"
    ),
  byrow = TRUE,
  dimnames = list(
    colour = c("dunkelblau", "hellblau", "hellgrün", "dunkelgrün", "senfgelb", "terracotta"),
    shade = c("standard", "lighter1", "lighter2", "lighter3", "lighter4", "lighter5"))
)

# old corporate design, including inofficials

goeg_rot = "#E53517"
goeg_orange = "#E7750C"
goeg_gelb = "#E9B500"
goeg_gelbgruen = "#B1B50E"
goeg_gruen = "#79B51C"
goeg_cyan = "#39bc9f"
goeg_blau = "#4FA9CB"
goeg_lila = "#887ABC"
goeg_grau = "#A3ABA6"

goeg_rainbow <- c(goeg_rot, goeg_orange, goeg_gelb, goeg_gelbgruen, goeg_gruen, goeg_cyan, goeg_blau, goeg_lila, goeg_grau)

goeg_inofficial_colour_array <- matrix(
  nrow = 9,
  ncol = 6,
  byrow = TRUE,
  data = c(
    "#e53517", "#F18572", "#F5ADA0", "#FAD6CF", "#AC2811", "#721B0C",
    "#E7750C", "#F7AB66", "#FAC798", "#FCE2CB", "#AC5609", "#733906",
    "#e9b500", "#FFDA59", "#FFE791", "#FFF2C8", "#AE8700", "#755A00",
    "#B1B50E", "#EDF150", "#F2F589", "#FAFBC9", "#85880B", "#585A07",
    "#79b51c", "#B1E761", "#CBEF96", "#E7F8C9", "#5B8811", "#3D5A11",
    "#39bc9f", "#85DAC7", "#ADE6DA", "#D6F3EC", "#2C9079", "#1E6253",
    "#4fa9cb", "#95CBDF", "#B9DCEA", "#DCEEF5", "#2D86AC", "#1E5973",
    "#887abc", "#B8AFD7", "#CFCAE4", "#E7E4F2", "#604F9D", "#41366B",
    "#A3ABA6", "#C8CCCA", "#D9DDDB", "#ECEEED", "#77827B", "#4F5752"
  ),
  dimnames = list(
    colour = c("rot", "orange", "gelb", "gelbgrün", "grün", "türkis", "blau", "lila", "grau"),
    shade = c("standard", "lighter1", "lighter2", "lighter3", "darker1", "darker2")
  )
)[, c(4:1, 5:6)]






goeg_colours <- function(
  inofficial = FALSE,
  n = NULL,
  n.colours = NULL,
  n.shades = 1,
  grey = inofficial,
  colour = NULL,
  shade = NULL
) {

  # validate input

  v <- z_validator()
  v$check(is.logical(inofficial), "`cd24` must be `logical`.")
  cd24 <- !inofficial

  v$check(is.logical(grey), "`grey` must be `logical`.")
  v$check(is.null(n) || (is.numeric(n) && n >= 1 && n %% 1 == 0), "`n` must be a positive integer.")
  v$check(is.null(n.colours) || (is.numeric(n.colours) && n.colours >= 1 && n.colours %% 1 == 0), "`n.colours` must be a positive integer.")
  v$check(is.null(n.shades) || (is.numeric(n.shades) && n.shades >= 1 && n.shades %% 1 == 0), "`n.colours` must be a positive integer.")
  v$check(
    is.null(colour) ||
      (cd24 && all(colour %in% dimnames(goeg_colour_array)$colour)) ||
      (!cd24 && all(colour %in% dimnames(goeg_inofficial_colour_array)$colour)),
    paste0("`colour` not found. Available in ",
           ifelse(cd24, "corporate design", "inofficial colours"),
           ": ",
           paste(if (cd24) dimnames(goeg_colour_array)$colour else dimnames(goeg_inofficial_colour_array)$colour, collapse = ", ")
           ))
  v$check(
    is.null(shade) ||
      (cd24 && all(shade %in% dimnames(goeg_colour_array)$shade)) ||
      (!cd24 && all(shade %in% dimnames(goeg_inofficial_colour_array)$shade)),
    paste0("`shade` not found. Available in ",
           ifelse(cd24, "corporate design", "inofficial colours"),
           ": ",
           paste(if (cd24) dimnames(goeg_colour_array)$shade else dimnames(goeg_inofficial_colour_array)$shade, collapse = ", ")
    ))


  v$throw()




}
