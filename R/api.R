
#' Handle API requests
#'
#' These are low-level functions responsible for handling requests of the
#' API endpoint. It interfaces HTTP requests from `plumber` and uses the
#' `api`, the `req`, and the `res` objects to prepare a response to the
#' request by dispatching to specific API implementations.
#'
#' Users should parse and validate parameters such as `collection_id`,
#' `bbox`, and `datetime` before calling these functions.
#' `openstac` provides functions like `parse_json()`, `parse_datetime()`,
#' `parse_dbl()`, `parse_int()`, and `parse_str()` to facilitate this process.
#'
#' \itemize{
#'
#' \item `api_landing_page`: handles the STAC `/` endpoint
#'
#' \item `api_conformance`: handles the STAC `/conformance` endpoint
#'
#' \item `api_collections`: handles the STAC `/collections` endpoint
#'
#' \item `api_collection`: handles the STAC
#'   `/collection/\{collection_id\}` endpoint
#'
#' \item `api_items`: handles the STAC
#'   `/collection/\{collection_id\}/items` endpoint
#'
#' \item `api_item`: handles the STAC
#'   `/collection/\{collection_id\}/item/\{item_id\}` endpoint
#'
#' \item `api_search`: handles the STAC `/search` endpoint
#'
#' }
#'
#' @param api An object representing the API. This object is typically
#'   created using either the `create_stac` or `create_ogcapi`
#'
#' @param req The request object from the `plumber` package, containing
#'   information about the HTTP request made to the API endpoint.
#'
#' @param res The response object from the `plumber` package, used to
#'   construct and send the HTTP response back to the client making
#'   the request.
#'
#' @param collection_id The identifier of the collection. This parameter
#'   specifies which collection the request is targeting.
#'
#' @param limit The maximum number of items to return. If not specified,
#'   the default value is used.
#'
#' @param bbox The bounding box for spatial filtering, specified as a
#'   numeric vector of four coordinates
#'   (`minlon`, `minlat`, `maxlon`, `maxlat`). Use `parse_dbl()` to
#'   convert comma-separated string to numeric vector.
#'
#' @param datetime The temporal filter for items. It must be specified
#'   as a `list(start = start_date, end = end_date, exact = exact_date)`
#'   object. Use `parse_datetime()` function to convert STAC datetime
#'   string to this object.
#'
#' @param intersects The spatial filter for items, specified as a GeoJSON
#'   geometry object representing the area of interest. Use `parse_json()`
#'   function to convert strings of GeoJSON geometries into an equivalent
#'   `list()` object.
#'
#' @param ids A list of item identifiers to filter the search results.
#'   Use `parse_str()` to convert a comma-separated string to character
#'   vector.
#' @param collections A list of collection identifiers to filter the search results.
#' @param page The page number of the results when paginating.
#' @param ... Additional arguments to be passed to the method-specific
#'   functions.
#'
#' @seealso \code{\link{create_stac}}, \code{\link{create_ogcapi}}
#'   Functions for creating STAC and OGC API objects, respectively.
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' @name api_handling
#'
NULL
#' @keywords internal
create_api <- function(api_class, title, description, conforms_to, ...) {
  structure(
    list(
      title = title,
      description = description,
      conforms_to = conforms_to, ...
    ),
    class = api_class,
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}
#' @keywords internal
api_env <- function(api) {
  attr(api, "env")
}
#' @keywords internal
api_attr <- function(api, name) {
  if (exists(name, envir = api_env(api)))
    get(name, envir = api_env(api), inherits = FALSE)
}
#' @keywords internal
`api_attr<-` <- function(api, name, value) {
  assign(name, value, envir = api_env(api), inherits = FALSE)
  api
}
#' @rdname api_handling
#' @export
api_landing_page <- function(api, req, res, ...) {
  UseMethod("api_landing_page", api)
}
#' @rdname api_handling
#' @export
api_conformance <- function(api, req, res, ...) {
  UseMethod("api_conformance", api)
}
#' @rdname api_handling
#' @export
api_collections <- function(api, req, res, ...) {
  UseMethod("api_collections", api)
}
#' @rdname api_handling
#' @export
api_collection <- function(api, req, res, collection_id, ...) {
  UseMethod("api_collection", api)
}
#' @rdname api_handling
#' @export
api_items <- function(api,
                      req,
                      res,
                      collection_id,
                      limit,
                      bbox,
                      datetime,
                      page, ...) {
  UseMethod("api_items", api)
}
#' @rdname api_handling
#' @export
api_item <- function(api, req, res, collection_id, item_id, ...) {
  UseMethod("api_item", api)
}
#' @rdname api_handling
#' @export
api_search <- function(api,
                       req,
                       res,
                       limit,
                       bbox,
                       datetime,
                       intersects,
                       ids,
                       collections,
                       page, ...) {
  UseMethod("api_search", api)
}

# Based on https://github.com/rstudio/plumber/issues/66#issuecomment-418660334
# HTTP CORS support
#' @rdname api_helpers
#' @export
api_cors_handler <- function(req, res, origin = "*", methods = "*") {
  res$setHeader("Access-Control-Allow-Origin", origin)
  if (req$REQUEST_METHOD != "OPTIONS") {
    plumber::forward()
  } else {
    res$setHeader("Access-Control-Allow-Methods", methods)
    res$setHeader("Access-Control-Allow-Headers",
                  req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  }
}
#' @rdname api_helpers
#' @export
api_error_handler <- function(req, res, err) {
  if (is.null(err$status)) err$status <- 500
  if (is.null(err$message)) err$message <- "Internal server error"
  res$status <- err$status
  list(code = err$status, message = paste("Error:", err$message))
}
#' @rdname api_helpers
#' @export
api_stop <- function(status, ...) {
  stop(errorCondition(paste0(...), status = status))
}
#' @rdname api_helpers
#' @export
api_stopifnot <- function(expr, status, ...) {
  message <- paste0(...)
  if (!nzchar(message))
    message <- paste(deparse(substitute(expr)), "is not TRUE")
  if (!expr) api_stop(status, message)
}
