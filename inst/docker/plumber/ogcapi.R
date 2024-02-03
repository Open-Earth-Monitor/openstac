#* @apiTitle OpenLandMap OGC API Features
#* @apiDescription Searchable spatiotemporal assets of OpenLandMap hosted by OpenGeoHub
#* @apiVersion 1.0.0
#* @apiBasePath /ogcapi

# Install openstac
remotes::install_github("rolfsimoes/openstac")

# Load libraries
library(openstac)
library(plumber)
library(promises)

# Set number of parallel processes to serve the API
future::plan(future::multisession(workers = 2))

# Create an OGC server API object
api <- create_ogcapi(
  title = "OpenLandMap OGC API",
  description = "Searchable spatiotemporal assets of OpenLandMap hosted by OpenGeoHub"
)

# Set API database to OpenLandMap
db_file <- system.file("db/openlandmap.rds", package = "openstac")
api <- set_db(api, driver = "local", file = db_file)

#* Custom error handling
#* @plumber
function(pr) {
  pr_set_error(pr, api_error_handler)
}

#* Enable Cross-origin Resource Sharing
#* Based on https://github.com/rstudio/plumber/issues/66#issuecomment-418660334
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  if (req$REQUEST_METHOD != "OPTIONS") plumber::forward()
  res$setHeader("Access-Control-Allow-Methods", "*")
  res$setHeader("Access-Control-Allow-Headers",
                req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
  res$status <- 200
  return(list())
}

#* Landing page
#* @get /
#* @serializer unboxedJSON
#* @tag 'OGC API Features Core v1.0.0'
function(req, res) {
  api_landing_page(api, req, res)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
#* @tag 'OGC API Features Core v1.0.0'
function(req, res) {
  api_conformance(api, req, res)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
#* @tag 'OGC API Features Core v1.0.0'
function(req, res) {
  api_collections(api, req, res)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
#* @tag 'OGC API Features Core v1.0.0'
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
#* @tag 'OGC API Features Core v1.0.0'
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
    check_datetime(datetime)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # call api items
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
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
#* @tag 'OGC API Features Core v1.0.0'
function(req, res, collection_id, item_id) {
  api_item(api, req, res, collection_id, item_id)
}
