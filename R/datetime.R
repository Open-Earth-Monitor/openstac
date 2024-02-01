datetime <- function(start, end, exact) {
  list(start = start, end = end, exact = exact)
}

datetime_as_str <- function(x) {
  if (is.null(x$start) && is.null(x$end) && is.null(x$exact))
    return(NULL)
  if (is.null(x$start) && is.null(x$end))
    return(as.character(x$exact))
  if (is.null(x$start)) x$start <- ".."
  if (is.null(x$end)) x$end <- ".."
  paste0(x$start, "/", x$end)
}

get_exact_date <- function(x) {
  if (!is.null(x))
    x$exact
}

get_start_date <- function(x) {
  if (!is.null(x))
    x$start
}

get_end_date <- function(x) {
  if (!is.null(x))
    x$end
}
