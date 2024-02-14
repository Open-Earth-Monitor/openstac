
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openstac

<!-- badges: start -->
<!-- badges: end -->

## Introduction

`openstac` is an R package designed to facilitate the creation of a STAC
(SpatioTemporal Asset Catalog) API server. This package leverages the
`plumber` package to create a RESTful API compliant with the STAC 1.0.0
specification. With `openstac`, you can easily expose spatial and
temporal data as STAC-compliant services, enabling seamless integration
with other geospatial tools and platforms.

## Features

- Quickly set up a STAC API server using R
- Expose collections of geospatial data
- Retrieve individual items from collections
- Perform spatial and temporal searches
- Easily customize endpoints and responses

## Installation

You can install `openstac` from
[GitHub](https://github.com/rolfsimoes/openstac) using the `remotes`
package:

``` r
remotes::install_github("Open-Earth-Monitor/openstac")
```

## Usage

To use `openstac`, follow these steps:

1.  Import the `openstac` library: `library(openstac)`.
2.  Create a new STAC API server using `create_api_stac()`.
3.  Configure your API endpoints for STAC API using plumber.
4.  Run your API using plumber.

## Example

This is a basic example:

``` r

#* @apiTitle STAC API
#* @apiDescription R STAC API server.
#* @apiVersion 1.0.0

library(openstac)

# Create an STAC server API object
api <- create_stac(
  id = "my_stac",
  title = "R STAC API server",
  description = "This is a STAC API 1.0.0 compliant R backend."
)

# Set API database
db_file <- system.file("db/olm-example.rds", package = "openstac")
api <- set_db(api, driver = "local", file = db_file)

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
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id, item_id) {
  api_item(api, req, res, collection_id, item_id)
}
```

To run this example, save this code in a file, e.g. `my_api.R` and run:

``` r

library(plumber)

plumber::plumb("my_api.R") |> 
  plumber::run(host = "127.0.0.1", port = 8888)
```

## Documentation

For detailed usage and additional examples, please refer to the
`openstac`
[documentation](https://github.com/Open-Earth-Monitor/openstac).

## Contributing

This package is under early development. We are going to add the
contributing guidelines soon.

## License

© OpenGeoHub Foundation, 2024. Licensed under the [MIT
License](LICENSE).

## Acknowledgements & Funding

This work is supported by [OpenGeoHub
Foundation](https://opengeohub.org/) and has received funding from the
European Commission (EC) through the projects:

- [Open-Earth-Monitor Cyberinfrastructure](https://earthmonitor.org/):
  Environmental information to support EU’s Green Deal (1 Jun. 2022 – 31
  May 2026 - [101059548](https://cordis.europa.eu/project/id/101059548))
- [AI4SoilHealth](https://ai4soilhealth.eu/): Accelerating collection
  and use of soil health information using AI technology to support the
  Soil Deal for Europe and EU Soil Observatory (1 Jan. 2023 – 31
  Dec. 2026 -
  [101086179](https://cordis.europa.eu/project/id/101086179))
