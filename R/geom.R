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

get_geom <- function(x) {
  if ("geometry" %in% names(x))
    x <- x$geometry
  geom_switch(
    x,
    Point = point(x),
    MultiPoint = multi_point(x),
    LineString = linestring(x),
    MultiLineString = multi_linestring(x),
    Polygon = polygon(x),
    MultiPolygon = multi_polygon(x),
    GeometryCollection = geom_collection(x)
  )
}

point <- function(x) {
  data <- unlist(x$coordinates)[c(1, 2)]
  structure(data, class = c("XY", "POINT", "sfg"))
}

multi_point <- function(x) {
  data <- matrix(unlist(x$coordinates), ncol = 2, byrow = TRUE)
  structure(c(data), dim = dim(data), class = c("XY", "MULTIPOINT", "sfg"))
}

linestring <- function(x) {
  data <- matrix(unlist(x$coordinates), ncol = 2, byrow = TRUE)
  structure(c(data), dim = dim(data), class = c("XY", "LINESTRING", "sfg"))
}

multi_linestring <- function(x) {
  data <- lapply(x$coordinates, \(ls) {
    data <- matrix(unlist(ls), ncol = 2, byrow = TRUE)
    structure(c(data), dim = dim(data))
  })
  structure(data, class = c("XY", "MULTILINESTRING", "sfg"))
}

polygon <- function(x) {
  data <- lapply(x$coordinates, \(lr) {
    data <- matrix(unlist(lr), ncol = 2, byrow = TRUE)
    structure(c(data), dim = dim(data))
  })
  structure(data, class = c("XY", "POLYGON", "sfg"))
}

multi_polygon <- function(x) {
  data <- lapply(x$coordinates, \(pl) {
    lapply(pl, \(lr) {
      data <- matrix(unlist(lr), ncol = 2, byrow = TRUE)
      structure(c(data), dim = dim(data))
    })
  })
  structure(data, class = c("XY", "MULTIPOLYGON", "sfg"))
}

geom_collection <- function(x) {
  data <- lapply(x$geometries, get_geom)
  structure(data, class = c("XY", "GEOMETRYCOLLECTION", "sfg"))
}
