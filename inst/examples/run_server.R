library(openstac)

# Create API object
api <- openstac::create_api(
  id = "stac-api",
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Set API database
db_file <- system.file("db/openlandmap.rds", package = "openstac")
openstac::set_db(api, driver = "local", file = db_file)

# Run API
openstac::run_api(api)
