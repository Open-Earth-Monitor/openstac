parse_array <- function(x) {
  strsplit(as.character(x), split = ",")[[1]]
}

#' @export
parse_str <- function(x) {
  as.character(parse_array(x))
}

#' @export
parse_int <- function(x) {
  as.integer(parse_array(x))
}

#' @export
parse_dbl <- function(x) {
  as.numeric(parse_array(x))
}

#' @export
parse_datetime <- function(x) {
  x <- strsplit(x, "/")[[1]]
  start <- end <- exact <- NULL
  if (length(x) == 1) {
    exact <- as.Date(x)
  } else if (length(x) == 2) {
    if (x[[1]] != "..") start <- as.Date(x[[1]])
    if (x[[2]] != "..") end <- as.Date(x[[2]])
    if (is.null(start) && is.null(end)) return(NULL)
  } else {
    return(NA)
  }
  datetime(start, end, exact)
}

#' @export
parse_json <- function(x, ...) {
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
