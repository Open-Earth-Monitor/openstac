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

get_endpoint <- function(api, ...) {
  paste0(get_scheme(api), "://", get_host(api), ":", get_port(api), ...)
}
