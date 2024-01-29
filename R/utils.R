
bbox_as_polygon <- function(x) {
  coords <- matrix(
    unlist(x)[c(1, 3, 3, 1, 1, 2, 2, 4, 4, 2)],
    ncol = 2, byrow = FALSE
  )
  sf::st_sfc(structure(
    list(structure(c(coords), dim = dim(coords))),
    class = c("XY", "POLYGON", "sfg")
  ), crs = 4326)
}

get_datetime <- function(start_date = NULL,
                         end_date = NULL,
                         exact_date = NULL) {
  if (is.null(start_date) && is.null(end_date))
    return(as.character(exact_date))
  if (is.null(start_date)) start_date <- ".."
  if (is.null(end_date)) end_date <- ".."
  paste0(start_date, "/", end_date)
}

get_pages <- function(items, limit) {
  ceiling(length(items$numberMatched) / limit)
}

get_host_name <- function(host) {
  gsub("^.+://", "", gsub(":[0-9]+$", "", host))
}

get_host_port <- function(host) {
  as.integer(gsub("^.*:", "", host))
}
