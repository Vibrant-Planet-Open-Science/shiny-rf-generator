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

  # ── Stand metrics ─────────────────────────────────────────────────────────────
  "QMD",                      "Quadratic mean diameter",     "Stand metrics",      "cm",           TRUE,
  "BA",                       "Basal area",                  "Stand metrics",      "m²/ha",        TRUE,
  "Tpa",                      "Trees per acre",              "Stand metrics",      "trees/acre",   TRUE,
  "TopHt",                    "Top height",                  "Stand metrics",      "ft",           TRUE,
  "SDI",                      "Stand density index",         "Stand metrics",      "",             FALSE,
  "MCuFt",                    "Merchantable cubic feet",     "Stand metrics",      "ft³",          FALSE,

  # ── Understory ───────────────────────────────────────────────────────────────
  "Surface_Shrub",            "Surface shrub cover",         "Understory",     "tons/acre",    TRUE,
  "Surface_Herb",             "Herbaceous cover",            "Understory",     "tons/acre",    TRUE,

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
  "MRCT",     "Ground-based mechanical thin",             "treatment", "Mechanical removal",
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

# ── Anonymous S3 readers ─────────────────────────────────────────────────
# vp-open-science is a public bucket. Passing empty key/secret tells aws.s3
# to skip request signing, so co-authors don't need AWS credentials.
S3_REGION <- "us-west-2"

# Empty key/secret tells aws.s3 to skip AWS Signature V4 signing entirely
# and make the request as anonymous. vp-open-science is a public bucket;
# any signed request will be rejected with 403 if the credentials in the
# default chain (env vars / ~/.aws/credentials / IAM role) don't have
# explicit access to this bucket. Most contributors won't.
S3_KEY    <- ""
S3_SECRET <- ""

# Both helpers wrap the aws.s3 call in a small retry loop. Why: even on a
# healthy network, individual IPs in the AWS S3 us-west-2 pool can be
# transiently unreachable from residential ISPs. Retrying triggers a fresh
# DNS lookup which usually returns a different IP. 3 attempts handles the
# typical case; cost when AWS is healthy is zero (first try succeeds).
#
# Note: a dual-stack base_url override (s3.dualstack.us-west-2.amazonaws.com)
# was tested as a workaround for an IPv4 routing outage on 2026-05-11, but
# turned out to interact badly with aws.s3's URL construction for unsigned
# requests. Removed; if IPv4 routing degrades again, prefer fixing the
# network (router reboot, DNS flush) over re-introducing base_url here.
S3_MAX_TRIES <- 3

s3_read_rds <- function(object) {
  last_err <- NULL
  for (i in seq_len(S3_MAX_TRIES)) {
    result <- tryCatch(
      aws.s3::s3readRDS(
        object = object,
        bucket = S3_BUCKET,
        region = S3_REGION,
        key    = S3_KEY,
        secret = S3_SECRET
      ),
      error = function(e) e
    )
    if (!inherits(result, "error")) return(result)
    last_err <- result
    if (i < S3_MAX_TRIES) {
      message(sprintf("S3 read failed (attempt %d/%d): %s -- retrying...",
                      i, S3_MAX_TRIES, conditionMessage(result)))
      Sys.sleep(1)
    }
  }
  stop(last_err)
}

s3_list_bucket <- function(prefix) {
  last_err <- NULL
  for (i in seq_len(S3_MAX_TRIES)) {
    result <- tryCatch(
      aws.s3::get_bucket(
        bucket = S3_BUCKET,
        prefix = prefix,
        region = S3_REGION,
        key    = S3_KEY,
        secret = S3_SECRET
      ),
      error = function(e) e
    )
    if (!inherits(result, "error")) return(result)
    last_err <- result
    if (i < S3_MAX_TRIES) {
      message(sprintf("S3 list failed (attempt %d/%d): %s -- retrying...",
                      i, S3_MAX_TRIES, conditionMessage(result)))
      Sys.sleep(1)
    }
  }
  stop(last_err)
}
