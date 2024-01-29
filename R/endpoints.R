format_endpoint <- function(x) {
  x <- gsub("<([^:]+):[^>]+>", "<\\1>", x)
  gsub("<([^>]+)>", "{\\1}", x)
}

list_endpoints <- function(api) {
  plumb <- api_plumb(api)
  lapply(plumb$endpoints$`__no-preempt__`, function(x) {
    list(path = format_endpoint(x$path), methods = list(x$verbs))
  })
}

get_endpoint <- function(host, ..., method = "GET") {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  href <- paste0(host, path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") href <- paste0(href, "?", query)
  href
}
