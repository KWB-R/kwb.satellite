if(FALSE) {

  url <- "https://fbinter.stadt-berlin.de/fb/atom/Gewaesserkarte/Gewaesserkarte.zip"
  tfile <- basename(url)

  download.file(url, destfile = basename(url))

  unzip(zipfile = tfile,
        exdir = "gewaesser_berlin")

  gewaesser_flaechen <- sf::read_sf("gewaesser_berlin/Gewaesser_Berlin_Flaechen.shp",
                                    options = "ENCODING=WINDOWS-1252")

  gewaesser_flaechen@data <- declare_character_column_encoding(gewaesser_flaechen@data, encoding = "latin1")
  gewaesser_flaechen$area <- sf::st_area(gewaesser_flaechen)

  View(gewaesser_flaechen@data)

  gewaesser_flaechen_klein <- gewaesser_flaechen %>%
    dplyr::filter(stringr::str_starts(GEWART, pattern = "Stehendes")) %>%
    dplyr::arrange(dplyr::desc(area))

  sum(gewaesser_flaechen_klein$area)/1000000


  # Define an area of interest.
  lake <- gewaesser_flaechen_klein[1,]

  geometry <- ee$Geometry$Polygon(
    coords = sf::st_coordinates(lake),
    proj = sf::st_crs(lake),
    geodesic = FALSE
  )


}


