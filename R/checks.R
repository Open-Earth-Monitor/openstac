#' Validate HTTP input parameters for STAC and OGC API requests.
#'
#' This set of functions provides validation for various HTTP input
#' parameters commonly used in STAC and OGC API requests. If the
#' validation fails, these functions raises an API error with the
#' appropriate status code and error message. If the function
#' `api_error_handler` is set to handle erros in the API, it will
#' use these parameters to produce the error back to the user.
#'
#' \itemize{
#'
#' \item `check_limit`: Checks if the limit parameter is within the
#'   specified range. If the limit is not an integer or is outside the
#'   specified range, it raises an API error with the appropriate status
#'   code.
#'
#' \item `check_bbox`: Checks if the bbox parameter contains numeric
#'   coordinates and has exactly four numbers. If the bbox parameter
#'   is not valid, it raises an error with the appropriate status code.
#'
#' \item `check_page`: Checks if the page parameter is a positive integer.
#'   If the page parameter is not a positive integer, it raises an error
#'   with the appropriate status code.
#'
#' \item `check_intersects`: Checks if the intersects parameter is a
#'   valid geometry. If the intersects parameter is not a valid GeoJSON
#'   geometry, it raises an error with the appropriate status code.
#'
#' \item `check_collections`: Checks if at least one collection is
#'   provided. If no collections are provided, it raises an error with
#'   the appropriate status code.
#'
#' \item `check_collection_in_db`: Checks if the specified collection
#'   exists in the database. If the collection does not exist, it raises
#'   an error with the appropriate status code.
#'
#' \item `check_item_in_db`: Checks if the specified item exists in
#'   the database. If the item does not exist, it raises an error with
#'   the appropriate status code.
#'
#' }
#'
#' @param min The minimum allowed value for the limit parameter.
#'
#' @param max The maximum allowed value for the limit parameter.
#'
#' @param limit The limit parameter to be checked.
#'
#' @param bbox The bbox parameter to be checked.
#'
#' @param datetime The datetime parameter to be checked.
#'
#' @param page The page parameter to be checked.
#'
#' @param intersects The intersects parameter to be checked.
#'
#' @param collections The collections parameter to be checked.
#'
#' @param collection_id The identifier of the collection. This parameter
#'   specifies which collections will be checked.
#'
#' @param item_id The identifier of the item within the specified collection.
#'   This parameter specifies which items will be checked.
#'
#' @seealso
#' For more details on error handler: [api_error_handler()]
#'
#' @name validate_functions
NULL

#' @keywords internal
check_rfc3339 <- function(datetime) {
  api_stopifnot(
    !is.na(datetime),
    status = 400,
    "datetime is not a valid time stamp or time interval"
  )
}
#' @keywords internal
check_datetime <- function(datetime) {
  start_date <- get_datetime_start(datetime)
  end_date <- get_datetime_end(datetime)
  exact_date <- get_datetime_exact(datetime)
  if (!is.null(start_date)) check_rfc3339(start_date)
  if (!is.null(end_date)) check_rfc3339(end_date)
  if (!is.null(exact_date)) check_rfc3339(exact_date)
}
#' @rdname validate_functions
#' @export
check_limit <- function(limit, min, max) {
  api_stopifnot(
    !is.na(limit),
    status = 400,
    "limit is not an integer"
  )
  api_stopifnot(
    limit >= min && limit <= max,
    status = 400,
    "limit not between ", min, " and ", max
  )
}
#' @rdname validate_functions
#' @export
check_bbox <- function(bbox) {
  api_stopifnot(
    all(!is.na(bbox)),
    status = 400,
    "bbox coordinates are not numeric"
  )
  api_stopifnot(
    length(bbox) == 4,
    status = 400,
    "bbox does not have 4 numbers"
  )
}
#' @rdname validate_functions
#' @export
check_page <- function(page) {
  api_stopifnot(
    !is.na(page),
    status = 400,
    "page is not an integer"
  )
  api_stopifnot(
    page >= 1,
    status = 400,
    "page not greater than or equal to 1"
  )
}
#' @rdname validate_functions
#' @export
check_intersects <- function(intersects) {
  api_stopifnot(
    is_geom(intersects),
    status = 400,
    "intersects is not a valid geometry"
  )
}
#' @rdname validate_functions
#' @export
check_collections <- function(collections) {
  api_stopifnot(
    length(collections) >= 1,
    status = 400,
    "at least one collection must be provided"
  )
}
#' @rdname validate_functions
#' @export
check_collection_in_db <- function(db, collection_id) {
  api_stopifnot(
    all(db_collections_id_exist(db, collection_id)),
    status = 404,
    "collection not found on the server"
  )
}
#' @rdname validate_functions
#' @export
check_item_in_db <- function(db, collection_id, item_id) {
  api_stopifnot(
    all(db_items_id_exist(db, collection_id, item_id)),
    status = 404,
    "item not found on the server"
  )
}
