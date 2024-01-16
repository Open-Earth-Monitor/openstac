run <- function(host = "127.0.0.1", port = 8000) {
  api_file <- system.file("R/stac-api.R", package = "stacserver")
  api <- plumber::plumb(api_file)
  api$run(host = host, port = port)
}
