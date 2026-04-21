# EC Label Crosswalk
# Maps raw FVS column names → user-facing labels, subcategory, unit, and visibility
#
# show_default: TRUE = visible in standard EC picker
#               FALSE = hidden behind "Show advanced ECs" toggle
#
# subcategory: used for grouping in the filterable list

ec_labels <- tibble::tribble(
  ~column,                    ~label,                        ~subcategory,     ~unit,          ~show_default,

  # ── Canopy ──────────────────────────────────────────────────────────────────
  "Stratum_1_Crown_Cover",    "Canopy cover",                "Canopy",         "% cover",      TRUE,
  "Stratum_2_Crown_Cover",    "Understory canopy cover",     "Canopy",         "% cover",      TRUE,
  "Stratum_3_Crown_Cover",    "Lower stratum canopy cover",  "Canopy",         "% cover",      FALSE,
  "Total_Cover",              "Total cover",                 "Canopy",         "% cover",      TRUE,
  "CCF",                      "Crown competition factor",    "Canopy",         "",             FALSE,

  # ── Tree size / structure ────────────────────────────────────────────────────
  "QMD",                      "Quadratic mean diameter",     "Tree size",      "cm",           TRUE,
  "BA",                       "Basal area",                  "Tree size",      "m²/ha",        TRUE,
  "Tpa",                      "Trees per acre",              "Tree size",      "trees/acre",   TRUE,
  "TopHt",                    "Top height",                  "Tree size",      "ft",           TRUE,
  "SDI",                      "Stand density index",         "Tree size",      "",             FALSE,
  "MCuFt",                    "Merchantable cubic feet",     "Tree size",      "ft³",          FALSE,

  # ── Understory ───────────────────────────────────────────────────────────────
  "Surface_Shrub",            "Surface shrub cover",         "Understory",     "% cover",      TRUE,
  "Surface_Herb",             "Herbaceous cover",            "Understory",     "% cover",      TRUE,

  # ── Fuels ────────────────────────────────────────────────────────────────────
  "Forest_Down_Dead_Wood",    "Down dead wood",              "Fuels",          "tons/acre",    TRUE,
  "Surface_Litter",           "Surface litter",              "Fuels",          "tons/acre",    TRUE,
  "Surface_Duff",             "Surface duff",                "Fuels",          "tons/acre",    TRUE,
  "Surface_lt3",              "Downed wood <3\"",            "Fuels",          "tons/acre",    FALSE,
  "Surface_ge3",              "Downed wood ≥3\"",            "Fuels",          "tons/acre",    TRUE,
  "Surface_Total",            "Total surface fuels",         "Fuels",          "tons/acre",    FALSE,

  # ── Snags ─────────────────────────────────────────────────────────────────────
  "Hard_snags_total",         "Hard snags",                  "Wildlife habitat","snags/acre",  TRUE,
  "Soft_snags_total",         "Soft snags",                  "Wildlife habitat","snags/acre",  TRUE,
  "Hard_soft_snags_total",    "All snags",                   "Wildlife habitat","snags/acre",  FALSE,

  # ── Carbon ───────────────────────────────────────────────────────────────────
  "Total_Stand_Carbon",       "Total stand carbon",          "Carbon",         "tons C/acre",  FALSE,
  "Aboveground_Total_Live",   "Aboveground live biomass",    "Carbon",         "tons/acre",    FALSE,
  "Forest_Floor",             "Forest floor carbon",         "Carbon",         "tons C/acre",  FALSE,

  # ── Growth ───────────────────────────────────────────────────────────────────
  "Acc",                      "Periodic annual increment",   "Growth",         "ft³/acre/yr",  FALSE,
  "Mort",                     "Mortality",                   "Growth",         "trees/acre",   FALSE,
)

# MgmtID crosswalk — BASE + fire + 3 initial treatments
# Remaining treatment codes available to add later: CMCC, CMUR, HCTA, HERB,
#   MTIR, MTUR, REVA, RMMA, RMTF, RXAI, RXGF
mgmt_labels <- tibble::tribble(
  ~mgmt_id,   ~label,                                    ~type,       ~method,
  "BASE",     "No disturbance (baseline)",               "baseline",  NA,
  "FIC1",     "Wildfire — FIC 1 (<2 ft flame length)",  "fire",      NA,
  "FIC2",     "Wildfire — FIC 2 (2–4 ft flame length)", "fire",      NA,
  "FIC3",     "Wildfire — FIC 3 (4–6 ft flame length)", "fire",      NA,
  "FIC4",     "Wildfire — FIC 4 (6–8 ft flame length)", "fire",      NA,
  "FIC5",     "Wildfire — FIC 5 (8–12 ft flame length)","fire",      NA,
  "FIC6",     "Wildfire — FIC 6 (>12 ft flame length)", "fire",      NA,
  "MRCC",     "Ground-based mechanical clearcut",        "treatment", "Mechanical removal",
  "MTTH",     "Manual thinning",                         "treatment", "Manual",
  "RMGP",     "Grapple pile burn",                       "treatment", "Mechanical rearrangement",
)

# S3 paths — centralised here so app.R only references these constants
S3_BUCKET <- "vp-open-science"
S3_PREFIX <- "biodiversity/habitat-suitability/response_function_data/conus/v0.0.0"

s3_path <- function(...) {
  paste(S3_PREFIX, ..., sep = "/")
}

S3_CA_STANDLEVEL  <- s3_path("raw-data", "CA-ALL-StandLevel_2024-12-18.rds")
S3_CR_STANDLEVEL  <- s3_path("raw-data", "CR-ALL-StandLevel_2024-12-18.rds")
S3_CA_STDSTK      <- s3_path("raw-data", "CA-FIC-StdStk_2024-09-25.rds")
S3_CR_STDSTK      <- s3_path("raw-data", "CR-TRT-StdStk_2024-10-10.rds")
S3_CA_STANDFILTER <- s3_path("raw-data", "CA-TRT-StandFilter_2024-10-10.rds")
S3_CR_STANDFILTER <- s3_path("raw-data", "CR-TRT-StandFilter_2024-10-10.rds")
S3_ALL_VARS       <- s3_path("rshiny-spatial-data", "all_vars_codes.rds")
S3_UNIQUE_STANDS  <- s3_path("rshiny-spatial-data", "unique_stands_western.rds")
S3_COUNTIES_GPKG  <- s3_path("rshiny-spatial-data", "tl_2024_western_counties.gpkg")
S3_TMIDS_PREFIX   <- s3_path("rshiny-spatial-data", "filtered_tmids_by_county")
