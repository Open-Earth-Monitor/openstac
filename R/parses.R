parse_array <- function(value) {
  strsplit(as.character(value), split = ",")[[1]]
}

#' @export
parse_str <- function(value) {
  as.character(parse_array(value))
}

#' @export
parse_int <- function(value) {
  as.integer(parse_array(value))
}

#' @export
parse_dbl <- function(value) {
  as.numeric(parse_array(value))
}

#' @export
parse_datetime <- function(value) {
  value <- parse_array(value)
  start_date <- end_date <- exact_date <- NULL
  if (length(value) == 1) {
    exact_date <- as.Date(value)
  } else if (length(value) == 2) {
    if (value[[1]] != "..") start_date <- as.Date(value[[1]])
    if (value[[2]] != "..") end_date <- as.Date(value[[2]])
    if (is.null(start_date) && is.null(end_date)) return(NULL)
  } else {
    return(NA)
  }
  datetime(start_date, end_date, exact_date)
}

#' @export
parse_json <- function(value, ...) {
  tryCatch({
    jsonlite::fromJSON(
      txt = value,
      simplifyVector = FALSE,
      auto_unbox = TRUE,
      ...
    )
  }, error = function(e) {
    return(NA)
  })
}
