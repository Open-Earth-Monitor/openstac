library(stacserver)

# Create API object
api <- create_api(
  id = "stac-api",
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Set API database
db_file <- system.file("db/openlandmap.rds", package = "stacserver")
set_db(api, driver = "local", file = db_file)

# Run API
run_api(api)
