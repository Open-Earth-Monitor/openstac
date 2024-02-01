datetime <- function(start, end, exact) {
  structure(
    list(start = start, end = end, exact = exact),
    class = "api_datetime"
  )
}

#' @exportS3Method base::as.character
as.character.api_datetime <- function(start, end, exact) {
  if (is.null(start) && is.null(end) && is.null(exact))
    return(NULL)
  if (is.null(start) && is.null(end))
    return(as.character(exact))
  if (is.null(start)) start <- ".."
  if (is.null(end)) end <- ".."
  paste0(start, "/", end)
}

get_exact_date <- function(datetime) {
  if (!is.null(datetime)) {}
}

