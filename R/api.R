api <- NULL

create_api <- function(id, title, description) {
  structure(
    list(
      id = id,
      title = title,
      description = description,
      stac_version = stac_version
    ),
    class = c(id, "stac_api"),
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}

get_env <- function(api) {
  attr(api, "env")
}

get_attr <- function(api, name) {
  if (exists(name, envir = get_env(api)))
    get(name, envir = get_env(api), inherits = FALSE)
}

set_attr <- function(api, name, value) {
  assign(name, value, envir = get_env(api), inherits = FALSE)
  api
}

get_plumb <- function(api) {
  get_attr(api, "plumb")
}

set_plumb <- function(api, plumb) {
  set_attr(api, "plumb", plumb)
}

get_scheme <- function(api) {
  get("scheme", envir = get_env(api), inherits = FALSE)
}

get_host <- function(api) {
  get("host", envir = get_env(api), inherits = FALSE)
}

get_port <- function(api) {
  get("port", envir = get_env(api), inherits = FALSE)
}

start_api <- function(api, envir) {
  api_file <- system.file("R/stac-api.R", package = "stacserver")
  plumb <- plumber::pr(api_file, envir = envir)
  set_plumb(api, plumb)
  plumber::pr_run(plumb, host = get_host(api), port = get_port(api))
}

run_api <- function(api, host = "http://127.0.0.1", port = 8000) {
  stopifnot(grepl("^.+://", host))
  set_attr(api, "scheme", gsub("://.*$", "", host))
  set_attr(api, "host", gsub("^.+://", "", host))
  set_attr(api, "port", port)
  start_api(api, envir = environment())
}

get_landing_page <- function() {
  data <- list(
    type = "Catalog",
    conformsTo = conforms_to
  )
  data <- c(api, data)
  update_catalog_links(data)
}
