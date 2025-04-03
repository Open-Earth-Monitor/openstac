#' @keywords internal
parse_array <- function(x) {
  if (!is.character(x)) return(x)
  strsplit(as.character(x), split = ",")[[1]]
}
#' @keywords internal
parse_int <- function(x) {
  as.integer(parse_array(x))
}
#' @keywords internal
parse_dbl <- function(x) {
  as.numeric(parse_array(x))
}
#' @keywords internal
parse_str <- function(x) {
  as.character(parse_array(x))
}
#' @keywords internal
parse_datetime <- function(x) {
  x <- strsplit(x, "/")[[1]]
  start <- end <- exact <- NULL
  if (length(x) == 1) {
    check_rfc3339(x)
    exact <- as.Date(x)
  } else if (length(x) == 2) {
    if (x[[1]] != "..") {
      check_rfc3339(x[[1]])
      start <- as.Date(x[[1]])
    }
    if (x[[2]] != "..") {
      check_rfc3339(x[[2]])
      end <- as.Date(x[[2]])
    }
    if (is.null(start) && is.null(end)) return(NULL)
  } else {
    return(NA)
  }
  date <- list(start = start, end = end, exact = exact)
  date
}
#' @keywords internal
parse_geojson <- function(x, ...) {
  # TODO: check for geometry conformity
  tryCatch({
    jsonlite::fromJSON(
      txt = x,
      simplifyVector = FALSE,
      auto_unbox = TRUE,
      ...
    )
  }, error = function(e) {
    return(NA)
  })
}
