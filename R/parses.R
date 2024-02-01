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
  x <- parse_array(x)
  start_date <- end_date <- exact_date <- NULL
  if (length(x) == 1) {
    exact_date <- as.Date(x)
  } else if (length(x) == 2) {
    if (x[[1]] != "..") start_date <- as.Date(x[[1]])
    if (x[[2]] != "..") end_date <- as.Date(x[[2]])
    if (is.null(start_date) && is.null(end_date)) return(NULL)
  } else {
    return(NA)
  }
  datetime(start_date, end_date, exact_date)
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
