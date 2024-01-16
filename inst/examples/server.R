library(stacserver)

# Create API object
api <- create_api(
  id = "stac-api",
  title = "R STAC API server",
  description = "This is a compliant STAC API 1.0.0 R backend."
)

# Load collections
collections <- jsonlite::read_json(
  path = system.file("sits/collections.json", package = "openeocraft"),
  auto_unbox = TRUE
)
load_collections(api, collections = collections)
