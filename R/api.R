# A list of all conformance classes specified in a standard that the
# server conforms to.
.conforms_to <- c(
  "https://api.stacspec.org/v1.0.0/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

.stac_version <- "v1.0.0"

create_api_stac <- function(title, description, conforms_to = NULL, ...) {
  structure(
    list(
      stac_version = .stac_version,
      title = title,
      description = description,
      conforms_to = c(.conforms_to, conforms_to),
      ...
    ),
    class = "stac",
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

# create_api_oafeat <- function(title, description, conforms_to = NULL, ...) {
#   structure(
#     list(
#       stac_version = .stac_version,
#       title = title,
#       description = description,
#       conforms_to = c(.conforms_to, conforms_to),
#       ...
#     ),
#     class = "oafeat",
#     env = new.env(hash = TRUE, parent = parent.frame())
#   )
# }

api_env <- function(api) {
  attr(api, "env")
}

api_attr <- function(api, name) {
  if (exists(name, envir = api_env(api)))
    get(name, envir = api_env(api), inherits = FALSE)
}

api_set_attr <- function(api, name, value) {
  assign(name, value, envir = api_env(api), inherits = FALSE)
  api
}

#' @export
set_db <- function(api, driver, ...) {
  db <- new_db(driver, ...)
  api_set_attr(api, "db", db)
}

#' @export
api_db <- function(api) {
  api_attr(api, "db")
}

#' @export
api_landing_page <- function(api, ...) {
  UseMethod("api_landing_page", api)
}

#' @export
api_conformance <- function(api, ...) {
  UseMethod("api_conformance", api)
}

#' @export
api_collections <- function(api, ...) {
  UseMethod("api_collections", api)
}

#' @export
api_collection <- function(api, collection_id, ...) {
  UseMethod("api_collection", api)
}

#' @export
api_items <- function(api, collection_id, limit, bbox, datetime, page, ...) {
  UseMethod("api_items", api)
}

#' @export
api_item <- function(api, collection_id, item_id, ...) {
  UseMethod("api_item", api)
}

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

#' @export
api_error_handler <- function(req, res, err) {
  if (is.null(err$status)) err$status <- 500
  if (is.null(err$message)) err$message <- "Internal server error"
  res$status <- err$status
  list(code = err$status, message = paste("Error:", err$message))
}

#' @export
api_stop <- function(status, ...) {
  stop(errorCondition(paste0(...), code = status))
}

#' @export
api_stopifnot <- function(expr, status, ...) {
  message <- paste0(...)
  if (!nzchar(message))
    message <- paste(deparse(substitute(expr)), "is not TRUE")
  if (!expr) api_stop(status, message)
}
