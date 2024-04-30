if(FALSE) {
archive::archive_extract("https://data.geobasis-bb.de/geofachdaten/Wasser/Gewaesserguete/klarwasserseen.zip")

lakes_bb_clear <- sf::read_sf("Klarwasserseen.shp")

archive::archive_extract("https://data.geobasis-bb.de/geofachdaten/Wasser/Hydrologie/seen25.zip",
                         dir = "lakes_bb")
lakes_bb <- sf::read_sf("lakes_bb/Seen25_20211105/seen25.shp")

baalsee <- lakes_bb[lakes_bb$SEE_NAME == "Großer Baalsee",]

reticulate::use_condaenv("ad4gd")
rgee::ee_Initialize()

library(magrittr)

baalsee_data_poly <- get_data_for_years(years = 2018:2024,
                                        lakes = baalsee,
                                        col_lakename = "SEE_NAME")

baalsee_data_centroid <- get_data_for_years(years = 2018:2024,
                                            lakes = baalsee,
                                            col_lakename = "SEE_NAME",
                                            centroid = TRUE)


}
