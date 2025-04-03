#* @apiTitle OpenLandMap STAC API Example
#* @apiDescription Example of Spatio-Temporal Asset Catalog for global
#*   layers provided by OpenLandMap and maintaned by OpenGeoHub Foundation
#* @apiVersion 1.0.0
#* @apiBasePath /v1

# Load libraries
library(openstac)

# Create an STAC server API object
api <- create_stac(
  id = "openlandmap",
  title = "OpenLandMap STAC API Example",
  description = paste(
    "Example of Spatio-Temporal Asset Catalog for",
    "global layers provided by OpenLandMap and",
    "maintaned by OpenGeoHub Foundation"
  )
)

# Set API database
# db_file <- system.file("db/olm-example.rds", package = "openstac")
# api <- set_db(api, driver = "local", file = db_file)
api <- set_db(
  api = api,
  driver = "mongodb",
  db = "openlandmap",
  url = "mongodb://127.0.0.1:27017"
)
#
# api <- set_queryables(
#   api = api,
#   collection_id = "CulturePnt",
#   title = "Cultural (Points)",
#   description = "Cultural: Information about features on the landscape with a point geometry that have been constructed by man",
#   F_CODE = query_str(
#     title = "Feature type",
#     enum = c("AK121", "AL012", "AL030", "AL130", "BH075")
#   ),
#   ZI001_SDV = query_datetime(
#     title = "Last Change"
#   ),
#   ZI037_REL = query_int(
#     title = "Religious Designation",
#     enum = c(-999999, 1:14)
#   ),
#   geometry = query_geom_point()
# )

#* Setup plumber router
#* @plumber
function(pr) {
  setup_plumber(
    api = api,
    pr = pr,
    handle_errors = TRUE,
    spec_endpoint = "/api",
    docs_endpoint = "/docs"
  )
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
function(collection_id, req, res) {
  api_collection(api, collection_id, req, res)
}

#* Collections endpoint
#* @get /queryables
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_queryables(api, req, res)
}

#* Collection endpoint
#* @get /collections/<collection_id>/queryables
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(collection_id, req, res) {
  api_queryables(api, collection_id, req, res)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box in OGC:CRS84 (long_min,lat_min,long_max,lat_max)
#* @param datetime:str Datetime filter (YYYY-MM-DD/YYYY-MM-DD)
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
  api_items(
    api = api,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page,
    req,
    res
  )
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id, item_id) {
  api_item(api, collection_id, item_id, req, res)
}

#* Search endpoint
#* @get /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box in OGC:CRS84 (long_min,lat_min,long_max,lat_max)
#* @param datetime:str Datetime filter (YYYY-MM-DD/YYYY-MM-DD)
#* @param intersects:str GeoJSON geometry to do spatial search
#* @param ids:str Array of items ID to return
#* @param collections:str Array of collection ID
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req,
         res,
         limit = 10,
         bbox = "",
         datetime,
         intersects = "",
         ids,
         collections,
         page = 1) {
  api_search(
    api = api,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page,
    req,
    res
  )
}
