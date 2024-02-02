
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

get_pages <- function(items, limit) {
  ceiling(items$numberMatched / limit)
}


#' @export
get_host <- function(req) {
  if ("HTTP_HOST" %in% names(req))
    return(paste0(req$rook.url_scheme, "://", req$HTTP_HOST))
  paste0(req$rook.url_scheme, "://", req$SERVER_NAME, req$SERVER_PORT)
}

#' @export
get_method <- function(req) {
  req$REQUEST_METHOD
}
