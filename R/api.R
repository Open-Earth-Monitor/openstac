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
api_landing_page <- function(api, req, res, ...) {
  UseMethod("api_landing_page", api)
}

#' @export
api_conformance <- function(api, req, res, ...) {
  UseMethod("api_conformance", api)
}

#' @export
api_collections <- function(api, req, res, ...) {
  UseMethod("api_collections", api)
}

#' @export
api_collection <- function(api, req, res, collection_id, ...) {
  UseMethod("api_collection", api)
}

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

#' @export
api_item <- function(api, req, res, collection_id, item_id, ...) {
  UseMethod("api_item", api)
}

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

#' @export
api_error_handler <- function(req, res, err) {
  if (is.null(err$status)) err$status <- 500
  if (is.null(err$message)) err$message <- "Internal server error"
  res$status <- err$status
  list(code = err$status, message = paste("Error:", err$message))
}

#' @export
api_stop <- function(status, ...) {
  stop(errorCondition(paste0(...), status = status))
}

#' @export
api_stopifnot <- function(expr, status, ...) {
  message <- paste0(...)
  if (!nzchar(message))
    message <- paste(deparse(substitute(expr)), "is not TRUE")
  if (!expr) api_stop(status, message)
}
