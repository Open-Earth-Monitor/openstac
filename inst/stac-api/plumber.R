#* @apiTitle STAC API
#* @apiDescription R STAC API server.
#* @apiVersion 1.0.0
stac_version <- "1.0.0"

# A list of all conformance classes specified in a standard that the
# server conforms to.
# TODO: update dynamically using extensions
conforms_to <- c(
  "https://api.stacspec.org/v1.0.0/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

get_api <- function(req, res) {

}

#* Landing page
#* @get /
#* @serializer unboxedJSON
function(req, res) {
  stacserver::api_landing_page(api)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
function(req, res) {
  stacserver::api_conformance(api)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function(req, res) {
  stacserver::api_collections(api)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id The ID of the collection
#* @serializer unboxedJSON
function(req, res, collection_id) {
  stacserver::api_collection(api, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id The ID of the collection
#* @param limit Maximum number of features to return (default: 10)
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @param page Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(req,
         res,
         collection_id,
         limit = 10,
         bbox,
         datetime,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- stacserver::parse_int(limit[[1]])
    stacserver::check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- stacserver::parse_dbl(bbox)
    stacserver::check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
    stacserver::check_datetime(datetime)
  }
  if (!is.null(page)) {
    page <- stacserver::parse_page(page)
    stacserver::check_page(page)
  }
  # call api items
  stacserver::api_items(
    api = api,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id The ID of the collection
#* @param item_id The ID of the item
#* @serializer unboxedJSON
function(req, res, collection_id, item_id) {
  stacserver::api_item(api, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @post /search
#* @param limit Maximum number of features to return (default: 10)
#* @param bbox Bounding box (minx,miny,maxx,maxy)
#* @param datetime Datetime filter
#* @param intersects GeoJSON geometry to do spatial search
#* @param ids Array of items ID to return
#* @param collections Array of collection ID
#* @param page Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(req,
         res,
         limit = 10,
         bbox,
         datetime,
         intersects,
         ids,
         collections,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- stacserver::parse_int(limit[[1]])
    stacserver::check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  stacserver::api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- stacserver::parse_dbl(bbox)
    stacserver::check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- stacserver::parse_datetime(datetime[[1]])
    stacserver::check_datetime(datetime)
  }
  method <- req$REQUEST_METHOD
  if (!is.null(intersects)) {
    stacserver::api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- stacserver::parse_json(intersects)
    stacserver::check_intersects_param(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- stacserver::parse_str(ids)
  if (missing(collections)) collections <- NULL
  if (!is.null(collections)) {
    collections <- stacserver::parse_str(collections)
    stacserver::check_collections(collections)
  }
  if (!is.null(page)) {
    page <- stacserver::parse_int(page[[1]])
    stacserver::check_page(page)
  }
  # call api search
  stacserver::api_search(
    api = api,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page,
    method = method
  )
}
