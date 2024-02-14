#* @apiTitle OpenLandMap STAC API
#* @apiDescription Spatio-Temporal Asset Catalog for global layers provided by OpenLandMap and maintaned by OpenGeoHub Foundation
#* @apiVersion 1.0.0
#* @apiBasePath /

# Install openstac
remotes::install_github("rolfsimoes/openstac", quiet = TRUE)

# Load libraries
library(openstac)
library(plumber)
library(promises)
library(future)

# Set number of processes to serve the API
future::plan(future::multisession(workers = 2))

# A list of all conformance classes specified in a standard that the
# server conforms to.
conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson",
  "https://api.stacspec.org/v1.0.0/core",
  "https://api.stacspec.org/v1.0.0/collections",
  "https://api.stacspec.org/v1.0.0/item-search",
  "https://api.stacspec.org/v1.0.0/ogcapi-features"
)

# Create an STAC server API object
api <- create_stac(
  id = "openlandmap",
  title = "OpenLandMap STAC API",
  description = "Spatio-Temporal Asset Catalog for global layers provided by OpenLandMap and maintaned by OpenGeoHub Foundation",
  conforms_to = conforms_to
)

# Set API database
api <- set_db(
  api, driver = "mongodb",
  db = "openlandmap",
  url = "mongodb://localhost:23874" # the same as the container port
)

#* Custom error handling
#* @plumber
function(pr) {
  pr_set_error(pr, api_error_handler)
}

#* Enable Cross-origin Resource Sharing
#* @filter cors
function(req, res) {
  api_cors_handler(req, res, origin = "*", methods = "*")
}

#* Landing page
#* @get /
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_landing_page(api, req, res)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_conformance(api, req, res)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_collections(api, req, res)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id) {
  api_collection(api, req, res, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req,
         res,
         collection_id,
         limit = 10,
         bbox,
         datetime,
         page = 1) {
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api items in asynchronously
  promises::future_promise({
    api_items(
      api = api,
      req = req,
      res = res,
      collection_id = collection_id,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      page = page
    )
  })
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id, item_id) {
  api_item(api, req, res, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:str GeoJSON geometry to do spatial search
#* @param ids:str Array of items ID to return
#* @param collections:str Array of collection ID
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
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
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  method <- get_method(req)
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- parse_geojson(intersects)
    check_intersects(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- parse_str(ids)
  api_stopifnot(
    !missing(collections),
    status = 400,
    "collections parameter must be provided"
  )
  if (!is.null(collections)) {
    collections <- parse_str(collections)
    check_collections(collections)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api search asynchronously
  promises::future_promise({
    api_search(
      api = api,
      req = req,
      res = res,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      intersects = intersects,
      ids = ids,
      collections = collections,
      page = page
    )
  })
}

#* Search endpoint
#* @post /search
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  # get POST parameters
  limit <- req$argsBody$limit
  bbox <- req$argsBody$bbox
  datetime <- req$argsBody$datetime
  intersects <- req$argsBody$intersects
  ids <- req$argsBody$ids
  collections <- req$argsBody$collections
  # set defaults
  if (is.null(limit)) limit <- 10
  if (is.null(page)) page <- 1
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  method <- get_method(req)
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- parse_geojson(intersects)
    check_intersects(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- parse_str(ids)
  if (!is.null(collections)) {
    collections <- parse_str(collections)
    check_collections(collections)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api search asynchronously
  promises::future_promise({
    api_search(
      api = api,
      req = req,
      res = res,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      intersects = intersects,
      ids = ids,
      collections = collections,
      page = page
    )
  })
}
