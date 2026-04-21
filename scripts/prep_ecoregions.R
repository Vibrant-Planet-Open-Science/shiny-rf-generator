# prep_ecoregions.R
# Downloads RESOLVE Ecoregions 2017, filters to western US, simplifies,
# and saves as www/ecoregions_western.geojson for the RF-Generator map.
#
# Source: RESOLVE Ecoregions 2017 (CC-BY 4.0)
# Run once from project root: Rscript scripts/prep_ecoregions.R

library(sf)
library(dplyr)
library(rmapshaper)

sf::sf_use_s2(FALSE)

# Western US state FIPS codes (for filtering via counties shapefile)
WESTERN_STATES <- c("WA", "OR", "CA", "NV", "ID", "MT", "WY", "CO", "UT", "AZ", "NM")

# ── Download ─────────────────────────────────────────────────────────────────
zip_path <- tempfile(fileext = ".zip")
url <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"
message("Downloading RESOLVE Ecoregions 2017...")
download.file(url, zip_path, mode = "wb")

exdir <- tempdir()
unzip(zip_path, exdir = exdir)

# Find the shapefile inside the extracted directory
shp_file <- list.files(exdir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
if (length(shp_file) == 0) stop("No .shp found in zip")
message("Reading: ", shp_file[1])

eco <- sf::st_read(shp_file[1], quiet = TRUE) %>%
  sf::st_make_valid()

# ── Filter to western US ─────────────────────────────────────────────────────
# Use the project's western counties shapefile as the clip boundary
counties <- sf::st_read(here::here("data", "tl_2024_western_counties.gpkg"), quiet = TRUE)
western_bbox <- sf::st_as_sfc(sf::st_bbox(counties))
western_bbox <- sf::st_transform(western_bbox, sf::st_crs(eco))

message("Clipping to western US extent...")
eco_western <- sf::st_intersection(eco, western_bbox) %>%
  sf::st_make_valid()

# Drop ecoregions with negligible area in western US
eco_western <- eco_western %>%
  dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
  dplyr::filter(area > 1e6) %>%
  dplyr::select(ECO_NAME, ECO_ID, BIOME_NAME, REALM, NNH, COLOR, COLOR_BIO, COLOR_NNH)

# ── Simplify ─────────────────────────────────────────────────────────────────
message("Simplifying polygons (keep = 0.05)...")
eco_simple <- rmapshaper::ms_simplify(eco_western, keep = 0.05, keep_shapes = TRUE)

# Transform to WGS84 for leaflet
eco_simple <- sf::st_transform(eco_simple, crs = 4326)

# ── Save ─────────────────────────────────────────────────────────────────────
out_path <- here::here("www", "ecoregions_western.geojson")
sf::st_write(eco_simple, out_path, delete_dsn = TRUE)
message("Saved: ", out_path)
message("File size: ", round(file.size(out_path) / 1024 / 1024, 1), " MB")
