#' sf to GeoJSON for OpenEO
#'
#' @param sf sf object
#' @return geojson formats that works with OpenEO
#' @export
#' @importFrom sf st_geometry_type st_coordinates st_crs
sf_to_geojson <- function(sf) {
  geom <- sf::st_geometry_type(sf)

  geom_type <- if(all(tolower(geom) == "point")) {
    "Point"
  } else if (all(tolower(geom) == "polygon")) {
    "Polygon"
  } else {
    stop("undefined geom_type")
  }

  coords <- sf::st_coordinates(sf)

  geojson <- list(
    type = geom_type,
    coordinates = if(geom_type == "Polygon") {
      list(matrix(coords[, c("X", "Y")], ncol = 2))
    } else {
      as.numeric(coords)
    },
    crs = list(
      type = "name",
      properties = list(name = sf::st_crs(sf)$proj4string)
    )
  )
  return(geojson)
}

