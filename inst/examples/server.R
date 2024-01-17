library(stacserver)

# Create API object
api <- create_api(
  id = "stac-api",
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Load collections
local_file <- "collections.rds"
set_db(api, driver = "local", file = local_file)


new_collection.local <- function(api, collection) {
  data <- get_attr(api, "collections")
  if (is.null(data)) data <- list()
  data[[collection$id]] <- collection
  set_attr(api, "collections", data)
}

save_collections.local <- function(api) {
  connection <- get_db(api)
  file <- connection$file
  stopifnot(!is.null(file))
  data <- get_attr(api, "collections")
  stopifnot(!is.null(data))
  saveRDS(data, file)
}
