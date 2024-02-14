#' Parse HTTP input parameters strings into R data types.
#'
#' This set of functions convert input strings to a corresponding R data.
#' Note that these functions may introduce `NA` values in case of conversion
#' failure.
#'
#' \itemize{
#'
#' \item `parse_int`: Splits an array-like string into a `integer` vector.
#'   For example, `"1,2,3"` will be parsed into `c(1L, 2L, 3L)`.
#'
#' \item `parse_dbl`: Splits an array-like string into a `numeric` vector.
#'   For example, `"1.5,2,3.5"` will be parsed into `c(1.5, 2, 3.5)`.
#'
#' \item `parse_str`: Splits an array-like string into a `character` vector.
#'   For example, `"apple,banana,orange"` will be parsed into
#'   `c("apple", "banana", "orange")`.
#'
#' \item `parse_datetime`: Parse an interval-like string into a `datetime`
#'   object. For example, `"2020-02-01/2021-06-30"` will be parsed into
#'   `list(start="2020-02-01", end="2021-06-30", exact=NULL)`.
#'   This function also checks if the dates are valid according to
#'   RFC3339 timestamp or time interval. If the datetime parameter
#'   is not valid, it raises an error with the appropriate status code.
#'

#'
#' \item `parse_geojson`: Splits an JSON-like string into a corresponding
#'   `list` vector.
#' }
#'
#' @param x A character vector containing the HTTP input parameter string
#'   to be parsed.
#'
#' @param ... Additional parameter to pass to `jsonlite::fromJSON`.
#'
#' @return A vector containing the parsed elements.
#'
#' @seealso
#' For more detail on `jsonlite::fromJSON`'s parameters: [jsonlite::fromJSON()]
#'
#' @name parse_functions
NULL
#' @keywords internal
parse_array <- function(x) {
  strsplit(as.character(x), split = ",")[[1]]
}
#' @rdname parse_functions
#' @export
parse_int <- function(x) {
  as.integer(parse_array(x))
}
#' @rdname parse_functions
#' @export
parse_dbl <- function(x) {
  as.numeric(parse_array(x))
}
#' @rdname parse_functions
#' @export
parse_str <- function(x) {
  as.character(parse_array(x))
}
#' @rdname parse_functions
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
  date <- datetime(start, end, exact)
  check_datetime(date)
  date
}
#' @rdname parse_functions
#' @export
parse_geojson <- function(x, ...) {
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
