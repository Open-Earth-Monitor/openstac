format_endpoint <- function(x) {
  x <- gsub("<([^:]+):[^>]+>", "<\\1>", x)
  gsub("<([^>]+)>", "{\\1}", x)
}

list_endpoints <- function(api) {
  plumb <- get_plumb(api)
  lapply(plumb$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
}

get_endpoint <- function(...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  host <- paste0(get_scheme(api), "://", get_host(api), ":", get_port(api))
  path <- paste0(segments, collapse = "/")
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  href <- paste0(host, path)
  if (query != "") href <- paste0(href, "?", query)
  href
}
