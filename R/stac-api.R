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

#* Landing page
#* @get /
#* @serializer unboxedJSON
function() {
  api_landing_page(api)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
function() {
  api_conformance(api)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
function() {
  api_collections(api)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
function(collection_id) {
  api_collection(api, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
function(collection_id,
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
    page <- as.integer(page)
    check_page(page)
  }
  # call api items
  api_items(
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
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
function(collection_id, item_id) {
  api_item(api, collection_id, item_id)
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
         limit = 10,
         bbox,
         datetime,
         intersects,
         ids,
         collections,
         page = 1) {
  method <- req$REQUEST_METHOD
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  stacserver::api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  exact_date <- NULL
  start_date <- NULL
  end_date <- NULL
  if (!is.null(datetime)) {
    datetime <- strsplit(datetime, "/")[[1]]
    check_datetime(datetime)
    if (length(datetime) == 1) {
      exact_date <- as.Date(datetime)
    } else {
      if (datetime[[1]] != "..") {
        start_date <- as.Date(datetime[[1]])
      }
      if (datetime[[2]] != "..") {
        end_date <- as.Date(datetime[[2]])
      }
    }
  }
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    check_intersects_param(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- split_str(ids)
  if (missing(collections)) collections <- NULL
  if (!is.null(collections)) {
    collections <- split_str(collections)
    check_collections_param(collections)
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    check_page(page)
  }
  # call api search
  api_search(
    api = api,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page,
    method = method
  )
}
