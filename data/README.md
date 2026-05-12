# Bundled data

Files in this directory are shipped with the app and read at runtime via
`here::here("data", "<file>")` from `R/app.R` and `R/functions.R`.

## all_vars_codes.rds

Small lookup table (~2 KB) of forest-type and structure-class codes.
Bundled locally to eliminate a blocking S3 read at app startup.

**Source:**
`s3://vp-open-science/biodiversity/habitat-suitability/response_function_data/conus/v0.0.0/rshiny-spatial-data/all_vars_codes.rds`

**Refresh:**
```bash
aws s3 cp \
  s3://vp-open-science/biodiversity/habitat-suitability/response_function_data/conus/v0.0.0/rshiny-spatial-data/all_vars_codes.rds \
  data/all_vars_codes.rds \
  --profile vpdc
```

## ca_counties.rds

California county GEOIDs — used for variant detection (CA vs CR) based on
AOI / county overlap. Read by `R/app.R` at startup and by
`get_tm_ids()` in `R/functions.R`.

## tl_2024_western_counties.gpkg

Western US counties shapefile — used to intersect ecoregion / AOI
selections with county boundaries, then look up per-county TreeMap
stand IDs from S3.

## unique_stand_data.rds

Stand metadata lookup — maps `StandID + Variant` → `Forest_Type`,
`Structure_Class`, etc. Joined with TreeMap IDs in `get_tm_ids()`.
