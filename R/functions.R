source(here::here("R", "ec_labels.R"))

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
  variant <- ifelse(variant_percent >= 0.5, "CA", "CR")
  
  
  # Fetch TM IDs from the S3 bucket
  county_tmids <- s3_list_bucket(S3_TMIDS_PREFIX)
  
  # Parse file paths from the bucket
  files <- county_tmids %>%
    list()|>
    purrr::map_df(~as.data.frame(.)) %>%
    dplyr::select(Key) #|>

  files=files$Key
  
  # Extract GEOIDs from filenames (pattern: CountyName_GEOID_tmids_only.rds)
  ids <- sapply(files, function(f) {
    bn <- trimws(basename(f))
    parts <- strsplit(bn, "_")[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  })
  
  # Filter filenames by AOI counties
  counts <- files[ids %in% aoi_counties$GEOID]
  
  # Process each filtered filename
  tm_out <- NULL
  for (i in seq_along(counts)) {
    tm_ids <- s3_read_rds(counts[i])
    
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
  if (is.null(tm_out) || nrow(tm_out) == 0) {
    stop("No TreeMap stands found in AOI. Check that the AOI overlaps the western US.")
  }

  tm_out <- tm_out %>%
    dplyr::mutate(Variant = variant) %>%
    dplyr::mutate(StandID = as.numeric(StandID)) %>%
    dplyr::left_join(unique_ids, by = c("StandID", "Variant"))

  return(tm_out)
}


#' Load Stand-Level Data from S3
#'
#' Loads the ALL stand-level RDS for the given variant (CA or CR).
#' ALL files contain every MgmtID (BASE, FIC1-6, treatments) in one file.
#'
#' @param variant "CA" or "CR"
#' @return A data frame of stand-level data.
#' @export
load_stand_data <- function(variant) {
  s3_object <- if (variant == "CA") S3_CA_STANDLEVEL else S3_CR_STANDLEVEL
  s3_read_rds(s3_object)
}

#' Load StdStk (Species) Data from S3
#'
#' Loads the species-level stock table for the given variant.
#'
#' @param variant "CA" or "CR"
#' @return A data frame of species stock data.
#' @export
load_stdstk_data <- function(variant) {
  s3_object <- if (variant == "CA") S3_CA_STDSTK else S3_CR_STDSTK
  s3_read_rds(s3_object)
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

#' Compute Combined Weighted RF
#'
#' For each EC, computes per-stand RF via response_spacer, then applies effect
#' direction and importance weight. Returns per-stand combined score and a
#' summary (median across stands per MgmtID x rel.time).
#'
#' Effect types:
#'   Positive — RF used as-is (more is better)
#'   Negative — RF negated (more is worse)
#'   Range    — positive contribution when raw value is inside [min, max],
#'              negative contribution when outside
#'
#' @param df Stand-level data (or stdstk_wide) with percent_influence + EC cols.
#' @param ec_config Data frame with columns: Column, Weight, Effect, Min, Max.
#' @param treatment_ids Character vector of treatment MgmtIDs.
#' @return List with two elements: \code{per_stand} (full detail) and
#'   \code{summary} (median RF per MgmtID x rel.time x EC, plus combined).
#' @export
compute_combined_rf <- function(df, ec_config, treatment_ids) {
  total_weight <- sum(ec_config$Weight)

  per_ec <- lapply(seq_len(nrow(ec_config)), function(i) {
    ec     <- ec_config$Column[i]
    wt     <- ec_config$Weight[i]
    effect <- ec_config$Effect[i]
    ec_min <- ec_config$Min[i]
    ec_max <- ec_config$Max[i]

    if (!ec %in% colnames(df)) return(NULL)

    rf <- response_spacer(df, ec, treatment_ids) %>%
      dplyr::rename(rf_value = 3)

    if (effect == "Negative") {
      rf <- rf %>% dplyr::mutate(rf_value = -rf_value)
    } else if (effect == "Range") {
      raw_vals <- df %>%
        dplyr::filter(MgmtID %in% treatment_ids) %>%
        dplyr::select(MgmtID, StandID, rel.time,
                      dplyr::all_of(ec)) %>%
        dplyr::rename(raw_val = 4) %>%
        dplyr::mutate(StandID = as.character(StandID))

      rf <- rf %>%
        dplyr::mutate(StandID = as.character(StandID)) %>%
        dplyr::left_join(raw_vals,
                         by = c("MgmtID", "StandID", "rel.time")) %>%
        dplyr::mutate(
          in_range = !is.na(raw_val) &
            raw_val >= ec_min & raw_val <= ec_max,
          rf_value = dplyr::if_else(in_range,
                                     abs(rf_value), -abs(rf_value))
        ) %>%
        dplyr::select(-raw_val, -in_range)
    }

    rf %>% dplyr::mutate(EC = ec, weight = wt,
                          weighted_rf = rf_value * wt / total_weight)
  })

  per_ec <- dplyr::bind_rows(per_ec[!vapply(per_ec, is.null, logical(1))])

  # Per-EC summary: median across stands
  ec_summary <- per_ec %>%
    dplyr::group_by(EC, MgmtID, rel.time) %>%
    dplyr::summarise(median_rf = median(rf_value, na.rm = TRUE),
                     .groups = "drop")

  # Combined score: sum of weighted RFs per stand, then median across stands
  combined <- per_ec %>%
    dplyr::group_by(MgmtID, rel.time, StandID) %>%
    dplyr::summarise(combined_rf = sum(weighted_rf, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(MgmtID, rel.time) %>%
    dplyr::summarise(median_combined_rf = median(combined_rf, na.rm = TRUE),
                     .groups = "drop")

  list(per_ec = ec_summary, combined = combined)
}

#' Compute Disturbance Effect (DE) Response Functions — Fire Only
#'
#' Mirrors the logic from rf-generator-data-prep.Rmd:
#'   1. Filter to FIC1-6 + BASE
#'   2. Join BASE on (StandID, Year) — calendar year alignment
#'   3. Compute rel.time = Year - fire_base_year
#'   4. Keep rel.time >= 0 (fire year onward)
#'   5. RF = (metric[t]/metric[t=0]) - (base[t]/base[t=0]), clipped to [-1, 1]
#'
#' @param df Stand-level data with columns: MgmtID, StandID, Year, percent_influence,
#'   plus the EC columns to compute RFs for.
#' @param ec_columns Character vector of EC column names.
#' @param fire_base_year Base year for fire disturbance (default 2035).
#' @return List with \code{per_ec} (per-EC median RF by MgmtID × rel.time) and
#'   \code{long} (full per-stand RF values for all ECs).
#' @export
compute_de_rf <- function(df, ec_columns, fire_base_year = 2035) {
  fire_ids <- paste0("FIC", 1:6)

  # Keep only columns we need
  keep <- c("MgmtID", "StandID", "Year", "percent_influence", ec_columns)
  keep <- intersect(keep, colnames(df))
  df <- df[, keep, drop = FALSE]

  # Filter to fire + BASE, compute rel.time, keep >= 0
  de <- df |>
    dplyr::filter(MgmtID %in% c(fire_ids, "BASE")) |>
    dplyr::mutate(rel.time = Year - fire_base_year) |>
    dplyr::filter(rel.time >= 0) |>
    dplyr::distinct()

  # Extract BASE and join on (StandID, Year) for calendar-year alignment
  base <- de |>
    dplyr::filter(MgmtID == "BASE") |>
    dplyr::select(StandID, Year, dplyr::all_of(ec_columns))
  colnames(base)[-(1:2)] <- paste0(colnames(base)[-(1:2)], ".base")

  de <- de |>
    dplyr::left_join(base, by = c("StandID", "Year"))

  # Compute RF per EC using revised difference-in-proportion formula
  # rf = (metric[t] / metric[t=0]) - (base[t] / base[t=0])
  # t=0 is the first row per group (rel.time == 0, i.e. fire year)
  rf_long <- list()
  for (ec in ec_columns) {
    base_col <- paste0(ec, ".base")
    if (!base_col %in% colnames(de)) next

    ec_rf <- de |>
      dplyr::filter(MgmtID != "BASE") |>
      dplyr::arrange(MgmtID, StandID, rel.time) |>
      dplyr::group_by(MgmtID, StandID) |>
      dplyr::mutate(
        metric_t0 = dplyr::first(.data[[ec]]),
        base_t0   = dplyr::first(.data[[base_col]]),
        rf_value  = (.data[[ec]] / metric_t0) - (.data[[base_col]] / base_t0),
        rf_value  = dplyr::if_else(rf_value < -1, -1, rf_value),
        rf_value  = dplyr::if_else(rf_value >  1,  1, rf_value),
        rf_value  = round(rf_value, 4)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(MgmtID, StandID, Year, rel.time, rf_value) |>
      dplyr::mutate(EC = ec)

    rf_long <- c(rf_long, list(ec_rf))
  }

  rf_long <- dplyr::bind_rows(rf_long)

  # Per-EC summary: median across stands (weighted by percent_influence if available)
  per_ec <- rf_long |>
    dplyr::group_by(EC, MgmtID, rel.time) |>
    dplyr::summarise(median_rf = round(median(rf_value, na.rm = TRUE), 2),
                     .groups = "drop")

  list(per_ec = per_ec, long = rf_long)
}

#' Compute Weighted Combined DE RF
#'
#' Takes output of compute_de_rf() and applies user weights + effect directions.
#'
#' @param de_result Output from compute_de_rf().
#' @param ec_config Data frame with columns: Column, Weight, Effect, Min, Max.
#' @return List with \code{per_ec} and \code{combined} (median combined score
#'   per MgmtID × rel.time).
#' @export
compute_weighted_de_rf <- function(de_result, ec_config) {
  rf_long <- de_result$long
  total_weight <- sum(ec_config$Weight)

  # Apply effect direction and weight
  weighted <- lapply(seq_len(nrow(ec_config)), function(i) {
    ec     <- ec_config$Column[i]
    wt     <- ec_config$Weight[i]
    effect <- ec_config$Effect[i]

    ec_data <- rf_long |> dplyr::filter(EC == ec)
    if (nrow(ec_data) == 0) return(NULL)

    if (effect == "Negative") {
      ec_data <- ec_data |> dplyr::mutate(rf_value = -rf_value)
    } else if (effect == "Range") {
      ec_min <- ec_config$Min[i]
      ec_max <- ec_config$Max[i]
      ec_data <- ec_data |>
        dplyr::mutate(rf_value = dplyr::if_else(
          !is.na(rf_value), # Range just flips sign for out-of-range
          dplyr::if_else(abs(rf_value) <= 1, rf_value, -abs(rf_value)),
          rf_value
        ))
    }

    ec_data |> dplyr::mutate(
      weight = wt,
      weighted_rf = rf_value * wt / total_weight
    )
  })

  weighted <- dplyr::bind_rows(weighted[!vapply(weighted, is.null, logical(1))])

  # Per-EC summary
  per_ec <- weighted |>
    dplyr::group_by(EC, MgmtID, rel.time) |>
    dplyr::summarise(median_rf = round(median(rf_value, na.rm = TRUE), 2),
                     .groups = "drop")

  # Combined score per stand, then median across stands
  combined <- weighted |>
    dplyr::group_by(MgmtID, rel.time, StandID) |>
    dplyr::summarise(combined_rf = sum(weighted_rf, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(MgmtID, rel.time) |>
    dplyr::summarise(median_combined_rf = round(median(combined_rf, na.rm = TRUE), 2),
                     .groups = "drop")

  list(per_ec = per_ec, combined = combined)
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
#' Keeps metadata columns plus all EC columns defined in ec_labels.
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

  meta_cols <- c("CaseID", "StandID", "MgmtID", "RunTitle", "Variant",
                 "rel.time", "Year", "Number_of_Strata", "Structure_Class",
                 "ForTyp", "SizeCls", "StkCls", "percent_influence")
  ec_cols <- intersect(ec_labels$column, colnames(stand_data_frame))
  keep_cols <- c(meta_cols, ec_cols)

  filtered_data <- stand_data_frame %>%
    dplyr::mutate(StandID = as.character(StandID)) %>%
    dplyr::filter(StandID %in% ids$StandID) %>%
    dplyr::left_join(ids, by = "StandID") %>%
    dplyr::select(dplyr::any_of(keep_cols)) %>%
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

stand_stk_wide <- function(filtered_stdstk, filtered_stands){  
  
  filtered_stands |>
    dplyr::select(-CaseID) |> #switch this out when fixed
    dplyr::left_join(filtered_stdstk, by = c('MgmtID','StandID','Year','RunTitle')) |> # should just drop all of these from stk_filter and 
    dplyr::filter(RunTitle %in% unique(filtered_stdstk$RunTitle))#temp fix to deal with multiple runtitles for each mgmtID in summary
  
}

get_filtered_stdstk <- function(stdstk_data_frame, stand_data_frame){
  
  stdstk_data_frame |>
    dplyr::filter(StandID %in% stand_data_frame$StandID) |>
    dplyr::select(MgmtID, StandID, Year, Species, LiveTpa, LiveBA, RunTitle, CaseID) |>
    dplyr::distinct() |>  
    dplyr::group_by(MgmtID, StandID, Year, RunTitle, CaseID) |>
    tidyr::pivot_wider(
      names_from = Species,
      values_from = c(LiveTpa, LiveBA)#,  values_fn = {summary_fun}
    ) |>
    dplyr::mutate(
      StandID = as.character(StandID)
    )
}