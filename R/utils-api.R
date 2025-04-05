#' API helper functions
#'
#' This set of functions provides HTTP CORS (Cross-Origin Resource Sharing)
#' support and error handling for the API.
#'
#' \itemize{
#'
#' \item `api_cors_handler`: HTTP CORS support. Typically called from a
#'   `plumber` filter to manage CORS.
#'
#' \item `api_error_handler`: Error handling function to be provided to
#'   `plumber` router to construct HTTP errors in a standardized way.
#'   Use `plumber::pr_set_error()` function to set the error handler
#'   in `plumber`.
#'
#' \item `api_stop`: Throws an error and set the HTTP status code and
#'   error message to be returned to the user by the `api_error_handler`
#'   function.
#'
#' \item `api_stopifnot`: Throws an error if the provided expression
#'   is evaluated as `FALSE`.
#'
#' \item `get_host`: Get the API host address from an `req` object.
#'
#' \item `get_path`: Get the path from an `req` object.
#'
#' \item `get_method`: Get the HTTP method from an `req` object.
#'
#' }
#'
#' @param req The request object from the `plumber` package, containing
#'   information about the HTTP request made to the API endpoint.
#'
#' @param res The response object from the `plumber` package, used to
#'   construct and send the HTTP response back to the client making
#'   the request.
#'
#' @param origin The value to set for the 'Access-Control-Allow-Origin'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param methods The value to set for the 'Access-Control-Allow-Methods'
#'   header in CORS requests. Defaults to '*'.
#'
#' @param err The error object containing information about the
#'   encountered error. If the error is thrown by `api_stopifnot` or
#'   `api_stop` functions, this object has 'status' and 'message' fields
#'   that are used to produce the HTTP response error.
#'
#' @param status The HTTP status code to set for the response. This just
#'   works if the `api_error_handler` function is handling errors in
#'   `plumber`.
#'
#' @param api The API object to be associated with the `plumber` router.
#'   This object is used to store additional information about the API
#'   To create an API object, use the `create_api` function.
#'
#' @param conforms_to A list with the conformance standards adhered to
#'   by the API. This parameter can be NULL or contain additional
#'   conformance standards to add to the defaults.
#'
#' @param expr The expression to evaluate. If the expression evaluates
#'   to FALSE, an error will be raised.
#'
#' @param ... Additional arguments to be passed to error handling functions.
#'
#' @seealso
#' [plumber::pr_set_error()]: Function to set error handler in `plumber`.
#'
#' @references
#' The code for `api_cors_handler` was based on a issue discussion post
#' on CORS support in plumber at
#' `https://github.com/rstudio/plumber/issues/66#issuecomment-418660334`
#'
#' @name api_helpers
NULL
#' @rdname api_helpers
#' @export
api_cors_handler <- function(req, res, origin = "*", methods = "*") {
  res$setHeader("Access-Control-Allow-Origin", origin)
  if (req$REQUEST_METHOD != "OPTIONS") {
    plumber::forward()
  } else {
    res$setHeader("Access-Control-Allow-Methods", methods)
    res$setHeader(
      "Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS
    )
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
  if (length(message) == 0 || !nzchar(message)) {
    message <- paste(deparse(substitute(expr)), "is not TRUE")
  }
  if (!expr) api_stop(status, message)
}
#' @rdname api_helpers
#' @export
get_host <- function(api, req) {
  base_url <- api_attr(api, "api_base_url")
  if (is_absolute_url(base_url)) {
    return(base_url)
  }
  if (!length(base_url)) {
    base_url <- ""
  }
  if ("HTTP_HOST" %in% names(req)) {
    base_url <- paste0(req$rook.url_scheme, "://", req$HTTP_HOST)
    return(base_url)
  }
  if (length(req$SERVER_PORT) && nzchar(req$SERVER_PORT) &&
    req$SERVER_PORT != "80") {
    base_url <- paste0(
      req$rook.url_scheme, "://", req$SERVER_NAME, ":", req$SERVER_PORT,
      base_url
    )
    return(base_url)
  }
  base_url <- paste0(req$rook.url_scheme, "://", req$SERVER_NAME, base_url)
  base_url
}
#' @rdname api_helpers
#' @export
api_add_conforms_to <- function(api, conforms_to) {
  current <- api_attr(api, "conforms_to")
  if (!is.null(current)) {
    conforms_to <- unique(c(current, conforms_to))
  }
  api_attr(api, "conforms_to") <- conforms_to
}
#' @keywords internal
get_path <- function(req) {
  req$PATH_INFO
}
#' @keywords internal
get_method <- function(req) {
  req$REQUEST_METHOD
}
#' @keywords internal
get_querystr <- function(req) {
  req$QUERY_STRING
}
#' @keywords internal
api_attr <- function(api, name) {
  api$get(name)
}
#' @keywords internal
`api_attr<-` <- function(api, name, value) {
  api$set(name, value)
  api
}
#' @keywords internal
get_plumber <- function(api) {
  api_attr(api, "plumber")
}
#' @keywords internal
setup_plumber_spec <- function(api, pr, spec_endpoint) {
  spec_handler <- function(req, res, ...) {
    # TODO: add models
    utils::modifyList(
      list(servers = list(list(
        url = make_url(get_host(api, req))
      ))),
      pr$getApiSpec()
    )
  }
  api_add_conforms_to(
    api, "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30"
  )
  api_attr(api, "spec_endpoint") <- spec_endpoint
  plumber::pr_set_docs(pr, FALSE)
  plumber::pr_get(
    pr = pr,
    path = spec_endpoint,
    handler = spec_handler,
    serializer = plumber::serializer_unboxed_json(),
    tag = "API"
  )
}
#' @keywords internal
setup_plumber_docs <- function(api, pr, docs_endpoint, spec_endpoint) {
  # remove leading slash
  spec_endpoint <- gsub("^/", "", spec_endpoint)

  docs_handler <- function(req, res, ...) {
    html_lines <- swagger::swagger_spec(
      api_path = paste0(
        '"',
        make_url(get_host(api, req), spec_endpoint, ...),
        '"'
      )
    )
    html_lines <- strsplit(html_lines, "\n")[[1]]
    # Inject <base href="/docs/"> into the <head> section
    head_index <- grep("<head.*?>", html_lines, ignore.case = TRUE)
    if (length(head_index) > 0) {
      html_lines <- append(
        html_lines,
        values = '  <base href="/docs/">',
        after = head_index[1]
      )
    }
    paste(html_lines, collapse = "\n")
  }

  api_attr(api, "docs_endpoint") <- docs_endpoint
  plumber::pr_static(
    pr = pr,
    path = paste0(docs_endpoint, "/"),
    direc = swagger::swagger_path()
  )
  plumber::pr_get(
    pr = pr,
    path = docs_endpoint,
    handler = docs_handler,
    serializer = plumber::serializer_html(),
    tag = "API"
  )
  plumber::pr_get(
    pr = pr,
    path = paste0(docs_endpoint, "/index.html"),
    handler = docs_handler,
    serializer = plumber::serializer_html(),
    tag = "API"
  )
  # plumber::pr_static(
  #   pr = pr,
  #   path = docs_endpoint,
  #   direc = swagger::swagger_path()
  # )
  # plumber::pr_get(
  #   pr = pr,
  #   path = paste0(docs_endpoint, "/index.html"),
  #   handler = docs_handler,
  #   serializer = plumber::serializer_html(),
  #   tag = "API"
  # )
  # plumber::pr_get(
  #   pr = pr,
  #   path = paste0(docs_endpoint, "/"),
  #   handler = docs_handler,
  #   serializer = plumber::serializer_html(),
  #   tag = "API"
  # )
}
#' @keywords internal
local_req <- function(path = "/", body = list()) {
  req <- list(
    REQUEST_METHOD = "GET",
    HTTP_ACCESS_CONTROL_REQUEST_HEADERS = "*",
    HTTP_HOST = "0.0.0.0",
    HTTP_ACCEPT = "*/*",
    SERVER_NAME = "0.0.0.0",
    SERVER_PORT = "8080",
    PATH_INFO = "",
    HTTP_AUTHORIZATION = "0123456789",
    rook.url_scheme = "http",
    body = body
  )
  req
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
