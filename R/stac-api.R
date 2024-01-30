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
    limit <- as.integer(limit)
    check_limit(limit)
  }
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- as.numeric(bbox)
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
  if (!is.null(page)) {
    page <- as.integer(page)
    check_page_param(page)
  }
  # call api items
  api_items(
    api = api,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    exact_date = exact_date,
    start_date = start_date,
    end_date = end_date,
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
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:[int] Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:list GeoJSON geometry to do spatial search
#* @param ids:[str] Array of items ID to return
#* @param collections:[str] Array of collection ID
#* @param page:int Pagination parameter (default: 1)
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
    limit <- as.integer(limit)
    check_limit(limit)
  }
  api_stopifnot(
    value = is.null(bbox) || is.null(intersects),
    code = 400,
    message = "only one of either intersects or bbox may be provided"
  )
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- as.numeric(bbox)
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
  if (missing(intersects)) intersects <- NULL
  if (!is.null(intersects)) {
    api_stopifnot(
      value = method == "POST",
      code = 405,
      message = "the request method is not supported"
    )
    check_intersects_param(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- as.character(ids)
  if (missing(collections)) collections <- NULL
  if (!is.null(collections)) {
    collections <- as.character(collections)
    check_collections_param(collections)
  }
  if (!is.null(page)) {
    page <- as.integer(page)
    check_page_param(page)
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
