


response_spacer <- function(df,variable_of_interest, mgmtID){

  ids <- 
    df |> 
    dplyr::filter(MgmtID %in% mgmtID) |>
    dplyr::select(StandID) |>
    dplyr::distinct()
  
  df2 <- 
    df |> 
    dplyr::filter(StandID %in% ids$StandID,
                  MgmtID %in% c(mgmtID, 'BASE'))
   
  out <-
    df2 |>
    dplyr::select(MgmtID, StandID, rel.time, percent_influence, dplyr::all_of(variable_of_interest)) |>
    dplyr::rename(variable = 5) |>
  #  dplyr::filter(variable <= quantile(variable, 0.95, na.rm = T)) |>
    dplyr::filter(rel.time > -2) |>
    dplyr::mutate(
      variable = variable * percent_influence
    ) |>
    dplyr::group_by(MgmtID,rel.time,StandID) |>
    dplyr::reframe(
      variable = sum(variable)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(rel.time) |>
    dplyr::arrange(MgmtID) 
  
  base <-
    out |>
    dplyr::filter(MgmtID %in% 'BASE') |>
    dplyr::rename(base = variable) |>
    dplyr::select(StandID, rel.time, base) 
  
  out <-
    out |>
    #added standid below @ 11pm
    dplyr::left_join(base, by = c('StandID','rel.time')) |>
    dplyr::mutate(
      rf = (variable - base) / base,
      rf = round(rf, digits = 2)
    ) |> 
    dplyr::filter(MgmtID != 'BASE') |>
    dplyr::select(c(1,2,3,6)) |>
    data.frame()
  
  
  colnames(out)[3] <- variable_of_interest
  
  out
}

max_no_na <- function(val){max(val,na.rm = T)}

cleanDF <- function(df){
  options(warn = 1)
  df2 <- 
    df |>  
    dplyr::select(-c(CaseID, StandID, MgmtID, RunTitle, Variant, rel.time, Year, Number_of_Strata, 
                     Structure_Class,ForTyp,SizeCls,StkCls,Stratum_1_Status_Code,Stratum_1_SpeciesFIA_1,
                     Stratum_1_SpeciesFIA_2,Stratum_2_Status_Code,Stratum_2_SpeciesFIA_1,
                     Stratum_2_SpeciesFIA_2, percent_influence)) 
  
  out <- suppressWarnings(apply(df2,2,max_no_na)) |> t() |> data.frame()
  remove <- out[,out < -10000] |> colnames() # this removes variables that have a max value of negative infinity and is set super high to not accidental remove real numbers
  df |> dplyr::select(-all_of(remove))
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
stand_stk_wide <- function(filtered_stdstk, filtered_stands){  
  
  filtered_stands |>
    dplyr::select(-CaseID) |> #switch this out when fixed
    dplyr::left_join(filtered_stdstk, by = c('MgmtID','StandID','Year','RunTitle')) |> # should just drop all of these from stk_filter and 
    dplyr::filter(RunTitle %in% unique(filtered_stdstk$RunTitle))#temp fix to deal with multiple runtitles for each mgmtID in summary
  
}



get_filtered_stand_data <- function(stand_data_frame, ids){
  ids <- 
    ids |>
    dplyr::select(StandID, count) |>
    # dplyr::rename(
    #   StandID = tm_id
    # )|>
    dplyr::mutate(
      StandID = as.character(StandID),
      percent_influence = count / sum(count, na.rm = T)
      )
  
  stand_data_frame |>
    dplyr::mutate(
      StandID = as.character(StandID)
    ) |>
    dplyr::filter(StandID %in% ids$StandID) |>
    dplyr::left_join(ids, by = 'StandID') |>
    dplyr::select(
      #filtering variables
      CaseID, StandID, MgmtID, 
      RunTitle, Variant, 
      Year,Number_of_Strata, Structure_Class,
      ForTyp, SizeCls, StkCls,
      Stratum_1_Status_Code, Stratum_1_SpeciesFIA_1,
      Stratum_1_SpeciesFIA_2, Stratum_2_Status_Code, 
      Stratum_2_SpeciesFIA_1,Stratum_2_SpeciesFIA_2,
      percent_influence,
      #stand-level attributes (removals (1 year of data) and predictions (generally equal to model output) removed)
      Tpa, BA, SDI, QMD,MCuFt,Acc,Mort,
      #surface fuels
      Surface_Litter,Surface_Duff, Surface_Herb,Surface_Shrub,
      #snag & cover information
      Standing_Snag_lt3,Standing_Snag_ge3, Hard_snags_total, Total_Cover, 
      #stratum 1 (overstory)
      Stratum_1_DBH,Stratum_1_Nom_Ht, Stratum_1_Crown_Base,Stratum_1_Crown_Cover,#Stratum_1_Lg_Ht,Stratum_1_Sm_Ht,
      #stratum 2 (under/midstory)
      Stratum_2_DBH,Stratum_2_Nom_Ht, Stratum_2_Crown_Base,Stratum_2_Crown_Cover,#Stratum_2_Lg_Ht,Stratum_2_Sm_Ht, 
    ) |> 
    data.frame()  |> 
    dplyr::distinct()
  
}

get_potential_variable_names <- function(stand_dataframe){
  stand_dataframe |>
    dplyr::select(-c(CaseID, StandID, MgmtID, RunTitle, Variant, rel.time, Year, Number_of_Strata, Structure_Class,ForTyp,
                     SizeCls,StkCls,Stratum_1_Status_Code,Stratum_1_SpeciesFIA_1,Stratum_1_SpeciesFIA_2,
                     Stratum_2_Status_Code,Stratum_2_SpeciesFIA_1,Stratum_2_SpeciesFIA_2, percent_influence)) |>
    colnames()
}




### 2 : Function ###
#library(tidyverse)
get_tm_ids <- function(aoi_path, filetype, unique_ids){
  
  county <- 
    '/Users/eyackulic/Downloads/tl_2024_us_county/tl_2024_western_counties.gpkg' |>
    sf::read_sf()
  
  if(filetype %in% 'pkg'){ #fix this line of code to make sense
  
  aoi <- sf::read_sf(aoi_path) |>
    sf::st_transform(sf::st_crs(county))
  
  aoi_counties <- sf::st_intersection(county, aoi)
  
  } else if(filetype %in% 'tif'){
    
  }
  #last rds to read in
  ca_counties <- 
    aws.s3::s3readRDS(
      bucket = 'vp-open-science',
      object = 'rf-generator-data/rshiny-spatial-data/ca_counties.rds') |>
    dplyr::select(GEOID)
  #  
  #  readRDS('/Users/eyackulic/Desktop/ca_counties.rds')
  variant_percent <- length(which(aoi_counties$GEOID %in% ca_counties$GEOID))/length(aoi_counties$GEOID)
  
  if(variant_percent >= .5){
    variant = 'CA'
  }else{variant = 'CO'}
  
 # filenames <-  '/Users/eyackulic/Desktop/western_counties/filtered' |> list.files(full.names = T)
  county_tmids <-   
    aws.s3::get_bucket(bucket = 'vp-open-science',
                       prefix = 'rf-generator-data/rshiny-spatial-data/filtered_tmids_by_county')
#need to map filenames to non-list and make it work with below :
  files <- 
    county_tmids  |> 
    list() |>
  purrr::map_df(~as.data.frame((.))) |>
    dplyr::select(Key)
  
  filenames <- paste0(#'/vsis3/vp-open-science/', 
    files$Key)
  
  ids <- vector()
  
  for(i in 1:length(filenames)){
    locs <- unlist(gregexpr('_',filenames[i]))
    #ids[i] <- substring(filenames[i],locs[2]+1,locs[3]-1)
    ids[i] <- substring(filenames[i],locs[4]+1,locs[5]-1)
  }
  
  counts <- filenames[which(ids %in% aoi_counties$GEOID)]
  
  for(i in 1:length(counts)){
    t1 <- substring(timeDate::timeDate(), 18, stringr::str_length(timeDate::timeDate())) |> as.numeric()
    
    tm_ids <- 
      aws.s3::s3readRDS(bucket = 'vp-open-science',
                        object = counts[i])
#      counts[i] |> readRDS()
    
    colnames(tm_ids) <- c('lat','lon','tm_id')
                          #,'state','county','id')
    dims <- sf::st_bbox(aoi)
    tm_ids <- 
      tm_ids |> dplyr::filter(
        lat >= dims[1] & 
          lat <= dims[3] &
          lon >= dims[2] & 
          lon <= dims[4],
        !is.na(tm_id)
      ) |>
      dplyr::select(tm_id) |>
      table()
    
    tm_ids <-
      tm_ids |>
      names() |>
      dplyr::bind_cols(as.numeric(tm_ids)) |> 
      data.frame() |> 
      magrittr::set_colnames( c('StandID', 'count'))
    
    if(i == 1){
      tm_out <- tm_ids
      #      print(substring(timeDate::timeDate(), 18, stringr::str_length(timeDate::timeDate())) |> as.numeric() - t1)
    }else{
      tm_out <- 
        tm_ids |>
        dplyr::mutate(
          count2 = count
        ) |> 
        dplyr::select( -count) |>
        dplyr::full_join(tm_out, by = 'StandID') |>
        dplyr::mutate(
          count = dplyr::if_else(
            is.na(count), 0, count),
          count2 = dplyr::if_else(
            is.na(count2), 0, count2),
          count3 = count + count2
        ) |> 
        dplyr::select(-c(count, count2)) |> 
        dplyr::rename(count = count3) |>
        dplyr::mutate(StandID = as.character(StandID))
      #    print(substring(timeDate::timeDate(), 18, stringr::str_length(timeDate::timeDate())) |> as.numeric() - t1)
    }
  }
  tm_out$Variant <- variant
  ##NEED TO MAP OUT VARIANT IN GPKG PART
  dplyr::left_join(tm_out, unique_ids, by = c('StandID','Variant')) 
}



get_rf_variables <- function(stand_level_path, stk_path, aoi_path, filetype){
  
  ids = get_tm_ids(aoi_path, filetype)
#  stand_level_path <- '/Users/eyackulic/Downloads/CA-TRT-StandLevel_2024-10-10.rds'
#  stk_path <- '/Users/eyackulic/Downloads/CA-TRT-StdStk_2024-10-10.rds'
  
  stand_data <- 
    stand_level_path |>
    readRDS() |> 
    dplyr::mutate(
      rel.time = Year - 2030  
    ) |>
    get_filtered_stand_data(ids) 
  
  all_variables <- 
    stk_path |>
    readRDS() |> 
    dplyr::filter(!Species %in% 'All') |>
    get_filtered_stdstk(stand_data_frame = stand_data) |>
    stand_stk_wide(filtered_stands = stand_data) |>
    cleanDF()
  
  all_variable_names <- 
    all_variables |> 
    get_potential_variable_names()
  
  mgmtIDs <- 
    stand_data |> 
    dplyr::select(MgmtID) |> 
    dplyr::distinct() |> 
    dplyr::filter(MgmtID != 'BASE') |> 
    unlist() 
  
  #another option is creating a list of standids in each mgmtid. 
  #should be able to remove a loop this way
  others <- all_variables |> select(-all_of(all_variable_names))
  
  for(i in 1:length(all_variable_names)){
    for(j in 1:length(mgmtIDs)){
      mgmt_out <-
        all_variables |>
        response_spacer(variable_of_interest = all_variable_names[i],mgmtID = mgmtIDs[j])
      if(j == 1){
        out <- mgmt_out
      }else{
        out <- dplyr::bind_rows(out, mgmt_out)
      }
    }
    if( i == 1){
      final_out <- out
    }else{
      final_out <- dplyr::left_join(out, final_out, by = c('MgmtID', 'rel.time','StandID'))
    }
  }
  
  final_out
}
