get_pages <- function(items, limit) {
  ceiling(items$numberMatched / limit)
}
is_absolute_url <- function(url) {
  grepl("^.+://.+$", url)
}
escape_url <- function(url) {
  gsub("/", "%2F", url)
}
as_item <- function(item) {
  structure(item, class = c("doc_item", "rstac_doc","list"))
}
create_items <- function(features, ...) {
  features <- lapply(features, as_item)
  structure(
    list(type = "FeatureCollection", features = features, ...),
    class = c("doc_items", "rstac_doc","list")
  )
}
as_collection <- function(collection) {
  structure(collection, class = c("doc_collection", "rstac_doc","list"))
}
create_collections <- function(collections, ...) {
  collections <- lapply(collections, as_collection)
  structure(
    list(collections = collections, ...),
    class = c("doc_collections", "rstac_doc","list")
  )
}
possibly <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}
as_datetime <- function(datetime) {
  format(
    x = as.POSIXct(
      x = datetime,
      tz = "GMT",
      tryFormats = c("%Y-%m-%dT%H:%M:%OSZ",
                     "%Y-%m-%d %H:%M:%OSZ",
                     "%Y-%m-%dT%H:%M:%OSz",
                     "%Y-%m-%d %H:%M:%OSz",
                     "%Y-%m-%dT%H:%M:%OS",
                     "%Y-%m-%d %H:%M:%OS",
                     "%Y-%m-%dT%H:%M",
                     "%Y-%m-%d %H:%M",
                     "%Y-%m-%d")),
    format = "%Y-%m-%dT%H:%M:%OSZ",
    usetz = FALSE
  )
}
