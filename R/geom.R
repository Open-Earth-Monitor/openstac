load_crs <- function(crs_name) {
  file <- paste0(strsplit(tolower(crs_name), ":")[[1]], collapse = "_")
  file <- system.file("crs", file, package = "openstac")
  stopifnot(nzchar(file))
  wkt <- paste0(readLines(file, encoding = "UTF-8"), collapse = "\n")
  structure(
    list(input = crs_name, wkt = wkt),
    class = "crs"
  )
}
geom_types <- c(
  "Point",
  "MultiPoint",
  "LineString",
  "MultiLineString",
  "Polygon",
  "MultiPolygon",
  "GeometryCollection"
)
is_geom <- function(x) {
  if (!is.list(x))
    return(FALSE)
  if (!"type" %in% names(x))
    return(FALSE)
  if (!x$type %in% geom_types)
    return(FALSE)
  if (x$type != "GeometryCollection" && !"coordinates" %in% names(x))
    return(FALSE)
  if (x$type == "GeometryCollection") {
    if (!"geometries" %in% names(x))
      return(FALSE)
    if (!all(vapply(x$geometries, is_geom, logical(1))))
      return(FALSE)
  }
  TRUE
}
geom_type <- function(x) {
  if (!"type" %in% names(x))
    stop("Invalid geometry object")
  x$type
}
geom_switch <- function(x, ...) {
  switch(
    geom_type(x), ...,
    stop("Geometry of type '", geom_type(x), "' is not supported")
  )
}
geom_as_sfg <- function(x) {
  if ("geometry" %in% names(x))
    x <- x$geometry
  geom_switch(
    x,
    Point = point_as_sfg(x),
    MultiPoint = multi_point_as_sfg(x),
    LineString = linestring_as_sfg(x),
    MultiLineString = multi_linestring_as_sfg(x),
    Polygon = polygon_as_sfg(x),
    MultiPolygon = multi_polygon_as_sfg(x),
    GeometryCollection = geom_collection_as_sfg(x)
  )
}
point_as_sfg <- function(x) {
  data <- unlist(x$coordinates)[c(1, 2)]
  structure(data, class = c("XY", "POINT", "sfg"))
}
multi_point_as_sfg <- function(x) {
  data <- matrix(unlist(x$coordinates), ncol = 2, byrow = TRUE)
  structure(c(data), dim = dim(data), class = c("XY", "MULTIPOINT", "sfg"))
}

linestring_as_sfg <- function(x) {
  data <- matrix(unlist(x$coordinates), ncol = 2, byrow = TRUE)
  structure(c(data), dim = dim(data), class = c("XY", "LINESTRING", "sfg"))
}
multi_linestring_as_sfg <- function(x) {
  data <- lapply(x$coordinates, \(lr) {
    data <- matrix(unlist(ls), ncol = 2, byrow = TRUE)
    structure(c(data), dim = dim(data))
  })
  structure(data, class = c("XY", "MULTILINESTRING", "sfg"))
}
polygon_as_sfg <- function(x) {
  data <- lapply(x$coordinates, \(lr) {
    data <- matrix(unlist(lr), ncol = 2, byrow = TRUE)
    structure(c(data), dim = dim(data))
  })
  structure(data, class = c("XY", "POLYGON", "sfg"))
}
multi_polygon_as_sfg <- function(x) {
  data <- lapply(x$coordinates, \(pl) {
    lapply(pl, \(lr) {
      data <- matrix(unlist(lr), ncol = 2, byrow = TRUE)
      structure(c(data), dim = dim(data))
    })
  })
  structure(data, class = c("XY", "MULTIPOLYGON", "sfg"))
}
geom_collection_as_sfg <- function(x) {
  data <- lapply(x$geometries, geom_as_sfg)
  structure(data, class = c("XY", "GEOMETRYCOLLECTION", "sfg"))
}
is_sfg <- function(x) {
  inherits(x, "sfg")
}
sfg_type <- function(x) {
  if (!is_sfg(x))
    stop("Invalid geometry object")
  class(x)[[2]]
}
sfg_switch <- function(x, ...) {
  switch(
    sfg_type(x), ...,
    stop("Geometry of type '", sfg_type(x), "' is not supported")
  )
}
sfg_as_geom <- function(x) {
  geom_switch(
    x,
    POINT = sfg_as_point(x),
    MULTIPOINT = sfg_as_multi_point(x),
    LINESTRING = sfg_as_linestring(x),
    MULTILINESTRING = sfg_as_multi_linestring(x),
    POLYGON = sfg_as_polygon(x),
    MULTIPOLYGON = sfg_as_multi_polygon(x),
    GEOMETRYCOLLECTION = sfg_as_geom_collection(x)
  )
}
sfg_as_point <- function(x) {
  list(
    type = geom_types[[1]],
    coordinates = list(c(x[[1]], x[[2]]))
  )
}
sfg_as_multi_point <- function(x) {
  list(
    type = geom_types[[2]],
    coordinates = apply(x, 1, list, simplify = FALSE)
  )
}
sfg_as_linestring <- function(x) {
  list(
    type = geom_types[[3]],
    coordinates = apply(x, 1, list, simplify = FALSE)
  )
}
sfg_as_multi_linestring <- function(x) {
  list(
    type = geom_types[[4]],
    coordinates = lapply(x, \(ls) apply(ls, 1, list, simplify = FALSE))
  )
}
sfg_as_polygon <- function(x) {
  list(
    type = geom_types[[5]],
    coordinates = lapply(x, \(lr) apply(lr, 1, c, simplify = FALSE))
  )
}
sfg_as_multi_polygon <- function(x) {
  list(
    type = geom_types[[6]],
    coordinates = lapply(x, \(pl) {
      lapply(pl, \(lr) apply(lr, 1, list, simplify = FALSE))
    })
  )
}
sfg_as_geom_collection <- function(x) {
  list(
    type = geom_types[[7]],
    geometries = lapply(x, sfg_as_geom)
  )
}

as_sf_bbox <- function(x) {
  x <- unlist(x, TRUE, FALSE)
  stopifnot(length(x) == 4)
  structure(
    c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]]),
    class = "bbox"
  )
}
bbox_as_sfg <- function(bbox) {
  bbox <- unlist(bbox, TRUE, FALSE)
  stopifnot(length(bbox) == 4)
  coords <- matrix(
    bbox[c(1, 3, 3, 1, 1, 2, 2, 4, 4, 2)],
    ncol = 2, byrow = FALSE
  )
  structure(
    list(structure(c(coords), dim = dim(coords))),
    class = c("XY", "POLYGON", "sfg")
  )
}
bbox_as_geom <- function(bbox) {
  x <- bbox_as_sfg(bbox)
  sfg_as_polygon(x)
}
