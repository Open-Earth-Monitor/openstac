library(stacserver)

# Create API object
api <- create_api(
  id = "stac-api",
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Load collections
local_file <- "~/stac-local.rds"
set_db(api, driver = "local", file = local_file)

run_api(api)
