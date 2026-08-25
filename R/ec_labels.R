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
# Data for this application is provided in a public bucket, so these reads must be UNSIGNED.
#
# We deliberately do NOT use aws.s3 here. aws.s3 signs a request whenever it
# can *locate* credentials, walking: AWS_* env vars -> ./.aws/credentials ->
# ~/.aws/credentials -> EC2 metadata. Meaning that if a user has stale/invalid 
# credentials anywhere in that chain (e.g. static keys left in ~/.aws/credentials 
# after an SSO migration), aws.s3 signs with them and S3 returns HTTP 403 — which 
# will make the app die at launch for internal and external users.
# Empty key/secret arguments do NOT prevent this, and scrubbing the AWS_* env
# vars is insufficient because ~/.aws/credentials is read unconditionally.
#
# Instead we fetch over plain anonymous HTTPS, which never enters the AWS
# credential/signing path at all and therefore cannot be broken by any ambient
# credential state (stale env vars, revoked static keys in ~/.aws/credentials,
# EC2 metadata, etc.). httr and xml2 are already dependencies (via aws.s3) and
# pinned in renv.lock.
S3_REGION <- "us-west-2"

# Shared curl settings for the anonymous GETs below. We deliberately do NOT use
# httr::timeout(), which caps TOTAL request time and would kill legitimate large
# downloads (the StandLevel .rds files are ~750 MB). Instead: connecttimeout
# aborts a hung connect, and the low-speed pair aborts a transfer that stalls
# below 1 byte/s for 60s — neither penalises a healthy large download. Six of the
# seven S3 call sites are reactive, so a hung request would otherwise freeze the
# Shiny session with no recovery.
S3_HTTP_CFG <- httr::config(connecttimeout = 30, low_speed_limit = 1, low_speed_time = 60)

# Virtual-hosted HTTPS URL for a public object key. Each path segment is
# percent-encoded, but the "/" separators are preserved.
s3_public_url <- function(key, bucket = S3_BUCKET, region = S3_REGION) {
  segs <- vapply(strsplit(key, "/", fixed = TRUE)[[1]],
                 function(s) utils::URLencode(s, reserved = TRUE), character(1))
  sprintf("https://%s.s3.%s.amazonaws.com/%s", bucket, region, paste(segs, collapse = "/"))
}

# Read an .rds object from the public bucket, unsigned. Downloads to a temp file
# so readRDS() auto-detects the compression, then removes it.
s3_read_rds <- function(object) {
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  resp <- httr::GET(s3_public_url(object), httr::write_disk(tf, overwrite = TRUE), S3_HTTP_CFG)
  httr::stop_for_status(resp, task = paste("read s3://", S3_BUCKET, "/", object, sep = ""))
  readRDS(tf)
}

# List object keys under a public prefix, unsigned, via paginated ListObjectsV2.
# Returns an "s3_keys" object: a list of one-field entries (each with $Key), plus
# an as.data.frame method — this mirrors the shape of aws.s3::get_bucket() closely
# enough that existing call sites (which read $Key, or coerce to a data frame and
# select Key) keep working unchanged.
s3_list_bucket <- function(prefix) {
  base_url <- sprintf("https://%s.s3.%s.amazonaws.com", S3_BUCKET, S3_REGION)
  keys <- character(0)
  token <- NULL
  repeat {
    query <- list(`list-type` = "2", prefix = prefix, `max-keys` = "1000")
    if (!is.null(token)) query[["continuation-token"]] <- token
    resp <- httr::GET(base_url, query = query, S3_HTTP_CFG)
    httr::stop_for_status(resp, task = paste("list s3://", S3_BUCKET, "/", prefix, sep = ""))
    # Parse with xml2, not regex: xml_text() entity-decodes keys containing
    # &/</>, and xml_find_first() returns NA for a missing node — so a truncated
    # response with no NextContinuationToken breaks the loop instead of sending
    # the whole XML body back as the next token.
    doc <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))
    xml2::xml_ns_strip(doc)  # ListObjectsV2 has a default namespace that breaks bare xpath
    keys  <- c(keys, xml2::xml_text(xml2::xml_find_all(doc, "//Key")))
    trunc <- xml2::xml_text(xml2::xml_find_first(doc, "//IsTruncated"))
    token <- xml2::xml_text(xml2::xml_find_first(doc, "//NextContinuationToken"))
    if (!identical(trunc, "true") || is.na(token) || !nzchar(token)) break
  }
  structure(lapply(keys, function(k) list(Key = k)), class = "s3_keys")
}

# as.data.frame method so a returned s3_keys object coerces to a one-column
# (Key) data frame, matching how aws.s3::get_bucket() results were consumed.
as.data.frame.s3_keys <- function(x, ...) {
  data.frame(Key = vapply(x, `[[`, character(1), "Key"), stringsAsFactors = FALSE)
}
