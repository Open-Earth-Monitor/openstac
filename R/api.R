#' Handle API requests
#'
#' These are functions responsible for handling requests of the
#' API endpoint. It interfaces HTTP requests from `plumber` and uses the
#' `api` and the `req` objects to prepare a response to the request by
#' dispatching to specific API implementations. HTTP input parameters are
#' parsed internally.
#'
#' \itemize{
#'
#' \item `create_api`: Creates an API object. It allows users setup
#'   custom API classes to create response documents.
#'
#' \item `create_oafeat`: Creates an API object for OGC API Features.
#'
#' \item `create_stac`: Creates an API object for STAC.
#'
#' \item `setup_plumber`: Register the Plumber router in the API server.
#'   It also can enable the Plumber documentation and set the handler
#'   of errors in the API.
#'
#' \item `api_spec`: Generates the OpenAPI specification for the API server.
#'
#' \item `api_landing_page`: Handles the STAC `/` endpoint.
#'
#' \item `api_conformance`: Handles the STAC `/conformance` endpoint.
#'
#' \item `api_collections`: Handles the STAC `/collections` endpoint.
#'
#' \item `api_collection`: Handles the STAC
#'   `/collection/{collection_id}` endpoint.
#'
#' \item `api_items`: Handles the STAC
#'   `/collection/{collection_id}/items` endpoint.
#'
#' \item `api_item`: Handles the STAC
#'   `/collection/{collection_id}/item/{item_id}` endpoint.
#'
#' \item `api_search`: Handles the STAC `/search` endpoint.
#'
#' }
#'
#' @param api_class A character string specifying the custom S3 class
#'   of the API. It allows advanced users setup new classes to handle
#'   response documents. Currently, `openstac` supports `oafeat` and
#'   `stac` S3 classes. To implement a new set of response document
#'   handlers, users must implement for their new class all generic
#'   functions declared in `R/doc.R`. For more details, see the
#'   `github` page of the project.
#'
#' @param id A character string specifying the id of the API.
#'
#' @param title A character string specifying the title of the API.
#'
#' @param description A character string describing the API.
#'
#' @param conforms_to A character vector specifying the conformance
#'   standards adhered to by the API. This parameter can be NULL or
#'   contain additional conformance standards to add to the defaults.
#'
#' @param pr The Plumber router object to be associated with the API server.
#'   For annotated API definition, users can capture the current Plumber
#'   object by annotating `@plumber` keyword in comment block. See
#'   references below for more details.
#'
#' @param spec_endpoint The endpoint where the API specification
#'   (OpenAPI) will be available. An `NULL` value disable this feature.
#'
#' @param docs_endpoint The endpoint where the API documentation
#'   (swagger) will be available. An `NULL` value disable this feature.
#'
#' @param handle_errors A logical value indicating whether to handle
#'   errors using the `openstac` default error handler. Default is `TRUE`.
#'
#' @param api_base_url A character string specifying the base URL of the API.
#'
#' @param api An object representing the API. This object is typically
#'   created using either the `create_stac` or `create_ogcapi`
#'
#' @param collection_id The identifier of the collection. This parameter
#'   specifies which collection the request is targeting.
#'
#' @param item_id The identifier of the item within the specified collection.
#'   This parameter specifies which item the request is targeting.
#'
#' @param limit The maximum number of items to return. If not specified,
#'   the default value is used.
#'
#' @param bbox The bounding box for spatial filtering, specified as a
#'   comma-separated string of four coordinates
#'   (`long_min`,`lat_min`,`long_max`,`lat_max`).
#'
#' @param datetime The temporal filter for items. It must be specified
#'   as a STAC datetime interval or timestamp string
#'   (e.g. `2020-03-29/2021-12-31`).
#'
#' @param intersects The spatial filter for items, specified as a GeoJSON
#'   geometry object representing the area of interest. The data comes
#'   as string representing a GeoJSON geometry.
#'
#' @param ids A comma-separated string of item identifiers to filter
#'   the search results.
#'
#' @param collections A comma-separated string of collection identifiers
#'   to filter the search results.
#'
#' @param page The page number of the results when paginating.
#'
#' @param ... Additional arguments to be passed to the method-specific
#'   functions.
#'
#' @return For API creation functions, returns a api object. For API
#'   handling functions, returns the document to return as response.
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#'
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' For more information about annotated Plumber API definition, see:
#' \url{https://www.rplumber.io/articles/annotations.html}
#'
#' @name api_handling
#'
NULL
#' @rdname api_handling
#' @export
create_api <- function(api_class,
                       title,
                       description,
                       conforms_to, ...) {
  env <- list2env(
    x = list(
      title = title,
      description = description,
      conforms_to = conforms_to, ...
    ),
    parent = emptyenv(),
    hash = TRUE
  )
  structure(
    list(
      title = \() env$title,
      description = \() env$description,
      conforms_to = \() env$conforms_to,
      as_list = \() as.list(env),
      get = \(name, default = NULL) get0(name, env, ifnotfound = default),
      set = \(name, value) assign(name, value, envir = env),
      has = \(name) exists(name, envir = env),
      env = \() env
    ),
    class = api_class
  )
}
#' @rdname api_handling
#' @export
create_oafeat <- function(title,
                          description,
                          conforms_to = NULL, ...) {
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  ogcapi_conforms_to <- c(
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
  )
  api <- create_api(
    api_class = "oafeat",
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to), ...
  )
  api
}
#' @rdname api_handling
#' @export
create_stac <- function(id,
                        title,
                        description,
                        conforms_to = NULL, ...) {
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  ogcapi_conforms_to <- c(
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson",
    "https://api.stacspec.org/v1.0.0/core",
    "https://api.stacspec.org/v1.0.0/collections",
    "https://api.stacspec.org/v1.0.0/item-search",
    "https://api.stacspec.org/v1.0.0/ogcapi-features"
  )
  api <- create_api(
    api_class = c("stac", "oafeat"),
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to),
    stac_version = "1.0.0",
    id = id, ...
  )
  api
}
#' @rdname api_handling
#' @export
setup_plumber <- function(api,
                          pr, ...,
                          handle_errors = TRUE,
                          api_base_url = NULL,
                          spec_endpoint = "/api",
                          docs_endpoint = "/docs") {
  stopifnot(is_absolute_url(api_base_url))
  api_attr(api, "plumber") <- pr
  if (handle_errors) {
    plumber::pr_set_error(pr, api_error_handler)
  }
  api_attr(api, "api_base_url") <- api_base_url
  if (!is.null(spec_endpoint)) {
    setup_plumber_spec(api, pr, spec_endpoint)
    if (!is.null(docs_endpoint)) {
      setup_plumber_docs(api, pr, docs_endpoint, spec_endpoint)
    }
  }
}
#' @rdname api_handling
#' @export
api_landing_page <- function(api, ...) {
  UseMethod("api_landing_page", api)
}
#' @rdname api_handling
#' @export
api_conformance <- function(api, ...) {
  UseMethod("api_conformance", api)
}
#' @rdname api_handling
#' @export
api_collections <- function(api, ...) {
  UseMethod("api_collections", api)
}
#' @rdname api_handling
#' @export
api_collection <- function(api, collection_id, ...) {
  UseMethod("api_collection", api)
}
#' @rdname api_handling
#' @export
api_items <- function(api,
                      collection_id,
                      limit,
                      bbox,
                      datetime,
                      page, ...) {
  UseMethod("api_items", api)
}
#' @rdname api_handling
#' @export
api_item <- function(api, collection_id, item_id, ...) {
  UseMethod("api_item", api)
}
#' @rdname api_handling
#' @export
api_search <- function(api,
                       limit,
                       bbox,
                       datetime,
                       intersects,
                       ids,
                       collections,
                       page, ...) {
  UseMethod("api_search", api)
}
#' @keywords internal
map_collections <- function(doc, fn, ...) {
  doc$collections <- lapply(doc$collections, fn, ...)
  doc
}
#' @keywords internal
map_features <- function(doc, fn, ...) {
  doc$features <- lapply(doc$features, fn, ...)
  doc
}
