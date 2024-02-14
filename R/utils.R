get_crs <- function(crs_name) {
  file <- paste0(strsplit(tolower(crs_name), ":")[[1]], collapse = "_")
  file <- system.file("crs", file, package = "openstac")
  stopifnot(nzchar(file))
  wkt <- paste0(readLines(file, encoding = "UTF-8"), collapse = "\n")
  structure(
    list(input = crs_name, wkt = wkt),
    class = "crs"
  )
}

as_bbox <- function(x) {
  x <- unlist(x, TRUE, FALSE)
  stopifnot(length(x) == 4)
  structure(
    c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]]),
    class = "bbox"
  )
}

bbox_as_polygon <- function(bbox) {
  bbox <- as_bbox(bbox)
  coords <- matrix(
    unname(bbox)[c(1, 3, 3, 1, 1, 2, 2, 4, 4, 2)],
    ncol = 2, byrow = FALSE
  )
  structure(
    list(structure(
      list(structure(c(coords), dim = dim(coords))),
      class = c("XY", "POLYGON", "sfg")
    )),
    class = c("sfc_POLYGON", "sfc"),
    precision = 0,
    bbox = bbox,
    crs = get_crs("OGC:CRS84"),
    n_empty = 0L
  )
}

get_pages <- function(items, limit) {
  ceiling(items$numberMatched / limit)
}
