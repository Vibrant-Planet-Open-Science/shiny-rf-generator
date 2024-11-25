#' Get TM IDs from an AOI and County Data
#'
#' This function retrieves TreeMap (TM) IDs by intersecting an Area of Interest (AOI) with county data.
#' It also determines the variant type (CA or CO) based on the overlap percentage and retrieves TM IDs from
#' filtered county data in an S3 bucket.
#'
#' @param aoi_path Path to the AOI file (GeoPackage or TIFF).
#' @param filetype File type of the AOI ("gpkg" or "tif").
#' @param unique_ids A data frame containing unique IDs for stands and variants.
#'
#' @return A data frame with TM IDs, their counts, and associated variant information.
#' @importFrom sf st_read st_transform st_crs st_intersection st_bbox
#' @importFrom aws.s3 s3readRDS get_bucket
#' @importFrom dplyr select mutate filter left_join if_else full_join bind_cols rename
#' @importFrom purrr map_df
#' @importFrom magrittr %>%
#' @importFrom timeDate timeDate
#' @examples
#' # Example usage
#' result <- get_tm_ids("path/to/aoi.gpkg", "gpkg", unique_ids_df)
#' 
get_tm_ids <- function(aoi_path, filetype, unique_ids){
  
  county <- sf::read_sf(here::here("data", "tl_2024_western_counties.gpkg"))
  
  # Load and process AOI data
    aoi <- sf::read_sf(aoi_path) %>%
      sf::st_transform(sf::st_crs(county))
    aoi_counties <- sf::st_intersection(county, aoi)
  
  # Load California counties for GEOID comparison
  ca_counties <- readRDS(here::here("data","ca_counties.rds")) %>%
    dplyr::select(GEOID)
  
  # Determine variant type based on overlap percentage
  variant_percent <- length(which(aoi_counties$GEOID %in% ca_counties$GEOID)) / length(aoi_counties$GEOID)
  variant <- ifelse(variant_percent >= 0.5, "CA", "CO")
  
  
  # Fetch TM IDs from the S3 bucket
  county_tmids <- aws.s3::get_bucket(
    bucket = 'vp-open-science',
    prefix = 'rf-generator-data/rshiny-spatial-data/filtered_tmids_by_county'
  )
  
  # Parse file paths from the bucket
  files <- county_tmids %>%
    list()|>
    purrr::map_df(~as.data.frame(.)) %>%
    dplyr::select(Key) #|>

  files=files$Key
  
  # Extract GEOIDs from filenames
  ids <- sapply(files, function(files) {
    locs <- unlist(gregexpr("_", files))
    substring(files, locs[4] + 1, locs[5] - 1)
  })
  
  # Filter filenames by AOI counties
  counts <- files[ids %in% aoi_counties$GEOID]
  
  # Process each filtered filename
  tm_out <- NULL
  for (i in seq_along(counts)) {
    tm_ids <- aws.s3::s3readRDS(
      bucket = 'vp-open-science',
      object = counts[i]
    )
    
    # Filter TM IDs within AOI bounds
    dims <- sf::st_bbox(aoi)
    colnames(tm_ids)=c('lat', 'lon', 'tm_id')
    tm_ids <- tm_ids %>%
      dplyr::filter(
        lat >= dims["xmin"] & lat <= dims["xmax"],
        lon >= dims["ymin"] & lon <= dims["ymax"],
        !is.na(tm_id)
      ) %>%
      dplyr::select(tm_id) %>%
      table()
    
    # Format TM IDs as a data frame
    tm_ids <- data.frame(
      StandID = names(tm_ids),
      count = as.numeric(tm_ids)
    )
    
    # Combine TM ID data across iterations
    if (is.null(tm_out)) {
      tm_out <- tm_ids
    } else {
      tm_out <- tm_ids %>%
        dplyr::rename(count2 = count) %>%
        dplyr::full_join(tm_out, by = "StandID") %>%
        dplyr::mutate(
          count = dplyr::if_else(is.na(count), 0, count),
          count2 = dplyr::if_else(is.na(count2), 0, count2),
          count = count + count2
        ) %>%
        dplyr::select(-count2)
    }
  }
  
  # Add variant information and join with unique IDs
  tm_out <- tm_out %>%
    dplyr::mutate(Variant = variant) %>%
    dplyr::left_join(unique_ids, by = c("StandID", "Variant"))
  
  return(tm_out)
}


#' Response Spacer Function
#'
#' This function calculates the relative response functions (RF) for a specified variable of interest
#' by management ID (MgmtID). It calculates RF as the relative difference from the base scenario.
#'
#' @param df A data frame containing forest management data.
#' @param variable_of_interest The name of the variable of interest to compute response functions.
#' @param mgmtID A character vector specifying management IDs to filter the data.
#' @return A data frame containing the calculated response functions for the variable of interest.
#' @import dplyr
#' @export
response_spacer <- function(df, variable_of_interest, mgmtID) {
  # Filter data for the given management IDs
  ids <- df %>%
    dplyr::filter(MgmtID %in% mgmtID) %>%
    dplyr::select(StandID) %>%
    dplyr::distinct()
  
  df2 <- df %>%
    dplyr::filter(
      StandID %in% ids$StandID,
      MgmtID %in% c(mgmtID, "BASE")
    )
  
  # Calculate relative response functions
  out <- df2 %>%
    dplyr::select(MgmtID, StandID, rel.time, percent_influence, dplyr::all_of(variable_of_interest)) %>%
    dplyr::rename(variable = 5) %>%
    dplyr::filter(rel.time > -2) %>%
    dplyr::mutate(variable = variable * percent_influence) %>%
    dplyr::group_by(MgmtID, rel.time, StandID) %>%
    dplyr::reframe(variable = sum(variable)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rel.time) %>%
    dplyr::arrange(MgmtID)
  
  # Extract baseline data
  base <- out %>%
    dplyr::filter(MgmtID == "BASE") %>%
    dplyr::rename(base = variable) %>%
    dplyr::select(StandID, rel.time, base)
  
  # Join with baseline data and calculate response functions
  out <- out %>%
    dplyr::left_join(base, by = c("StandID", "rel.time")) %>%
    dplyr::mutate(
      rf = (variable - base) / base,
      rf = round(rf, 2)
    ) %>%
    dplyr::filter(MgmtID != "BASE") %>%
    dplyr::select(MgmtID, rel.time, variable_of_interest = rf, StandID)
  
  return(out)
}

#' Calculate Maximum Value (Ignoring NA)
#'
#' This function calculates the maximum value of a numeric vector, ignoring NA values.
#'
#' @param val A numeric vector.
#' @return A single numeric value representing the maximum value.
#' @export
max_no_na <- function(val) {
  max(val, na.rm = TRUE)
}

#' Clean Data Frame
#'
#' This function removes columns with extreme low maximum values (indicative of invalid data).
#'
#' @param df A data frame to be cleaned.
#' @return A cleaned data frame with invalid columns removed.
#' @import dplyr
#' @export
cleanDF <- function(df) {
  options(warn = 1)
  df2 <- df %>%
    dplyr::select(-c(
      CaseID, StandID, MgmtID, RunTitle, Variant, rel.time, Year, Number_of_Strata, 
      Structure_Class, ForTyp, SizeCls, StkCls, Stratum_1_Status_Code, Stratum_1_SpeciesFIA_1,
      Stratum_1_SpeciesFIA_2, Stratum_2_Status_Code, Stratum_2_SpeciesFIA_1, Stratum_2_SpeciesFIA_2,
      percent_influence
    ))
  
  out <- suppressWarnings(apply(df2, 2, max_no_na)) %>%
    t() %>%
    data.frame()
  remove <- out[, out < -10000] %>% colnames()
  cleaned <- df %>% dplyr::select(-all_of(remove))
  return(cleaned)
}

#' Get Filtered Stand Data
#'
#' Filters stand-level data based on IDs and appends influence percentages.
#'
#' @param stand_data_frame A data frame of stand-level data.
#' @param ids A data frame of StandID and count information.
#' @return A filtered and annotated data frame of stand-level data.
#' @import dplyr
#' @export
get_filtered_stand_data <- function(stand_data_frame, ids) {
  ids <- ids %>%
    dplyr::select(StandID, count) %>%
    dplyr::mutate(
      StandID = as.character(StandID),
      percent_influence = count / sum(count, na.rm = TRUE)
    )
  
  filtered_data <- stand_data_frame %>%
    dplyr::mutate(StandID = as.character(StandID)) %>%
    dplyr::filter(StandID %in% ids$StandID) %>%
    dplyr::left_join(ids, by = "StandID") %>%
    dplyr::select(
      CaseID, StandID, MgmtID, RunTitle, Variant, Year, Number_of_Strata,
      Structure_Class, ForTyp, SizeCls, StkCls, percent_influence,
      Tpa, BA, SDI, QMD, MCuFt, Acc, Mort,
      Surface_Litter, Surface_Duff, Surface_Herb, Surface_Shrub,
      Standing_Snag_lt3, Standing_Snag_ge3, Hard_snags_total, Total_Cover
    ) %>%
    dplyr::distinct()
  
  return(filtered_data)
}

#' Get Potential Variable Names
#'
#' Extracts column names for potential response variables.
#'
#' @param stand_dataframe A data frame of stand data.
#' @return A character vector of column names.
#' @export
get_potential_variable_names <- function(stand_dataframe) {
  variable_names <- stand_dataframe %>%
    dplyr::select(-c(
      CaseID, StandID, MgmtID, RunTitle, Variant, rel.time, Year, Number_of_Strata,
      Structure_Class, ForTyp, SizeCls, StkCls, percent_influence
    )) %>%
    colnames()
  return(variable_names)
}