#!/usr/bin/env Rscript
# scripts/qa_fvs_s3.R
#
# Manual QA/QC against the FVS data files on S3 (vp-open-science).
# Standalone — not invoked by the app. Run when you want a fresh report.
#
# Usage (from project root):
#   Rscript scripts/qa_fvs_s3.R
#
# Outputs:
#   qa_reports/qa_fvs_s3_<timestamp>.md
#   qa_reports/figures/<file>_median_ts.png   (BASE + FIC1-6 median time series)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(here)
})

# Reuse only the S3 path constants + reader and the existing timing checks.
source(here::here("R", "ec_labels.R"))
source(here::here("R", "check_timing_invariants.R"))

# ── Config ──────────────────────────────────────────────────────────────────

EXPECTED_YEARS <- c(2025, 2030, 2031, 2034, 2035, 2040, 2045, 2050, 2055, 2060, 2065)

# Conservative physical-plausibility bounds. NA = unbounded on that side.
# Negatives flag impossible values; upper bounds catch obvious unit/scale bugs.
EC_RANGES <- list(
  BA                       = c(0,  600),
  Tpa                      = c(0, 5000),
  QMD                      = c(0,  100),
  TopHt                    = c(0,  300),
  Stratum_1_Crown_Cover    = c(0,  100),
  Stratum_2_Crown_Cover    = c(0,  100),
  Stratum_3_Crown_Cover    = c(0,  100),
  Total_Cover              = c(0,  100),
  CCF                      = c(0, 1500),
  SDI                      = c(0, 2000),
  MCuFt                    = c(0,   NA),
  Surface_Shrub            = c(0,   NA),
  Surface_Herb             = c(0,   NA),
  Surface_Litter           = c(0,   NA),
  Surface_Duff             = c(0,   NA),
  Surface_lt3              = c(0,   NA),
  Surface_ge3              = c(0,   NA),
  Surface_Total            = c(0,   NA),
  Forest_Down_Dead_Wood    = c(0,   NA),
  Hard_snags_total         = c(0,   NA),
  Soft_snags_total         = c(0,   NA),
  Hard_soft_snags_total    = c(0,   NA),
  Total_Stand_Carbon       = c(0,   NA),
  Aboveground_Total_Live   = c(0,   NA),
  Forest_Floor             = c(0,   NA),
  Acc                      = c(NA,  NA),  # PAI can technically be negative
  Mort                     = c(0,   NA),
  LiveTpa                  = c(0,   NA),
  LiveBA                   = c(0,   NA)
)

KNOWN_MGMT_IDS <- mgmt_labels$mgmt_id  # from ec_labels.R

REQUIRED_STAND_COLS  <- c("MgmtID", "StandID", "Year")
REQUIRED_STDSTK_COLS <- c("MgmtID", "StandID", "Year", "Species", "LiveTpa", "LiveBA")

S3_FILES <- list(
  list(label = "CA-ALL-StandLevel", path = S3_CA_STANDLEVEL, kind = "stand",  variant = "CA"),
  list(label = "CR-ALL-StandLevel", path = S3_CR_STANDLEVEL, kind = "stand",  variant = "CR"),
  list(label = "CA-FIC-StdStk",     path = S3_CA_STDSTK,     kind = "stdstk", variant = "CA"),
  list(label = "CR-TRT-StdStk",     path = S3_CR_STDSTK,     kind = "stdstk", variant = "CR")
)

TS         <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
OUT_DIR    <- here::here("qa_reports")
FIG_DIR    <- file.path(OUT_DIR, "figures")
REPORT_PATH <- file.path(OUT_DIR, sprintf("qa_fvs_s3_%s.md", TS))
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Check helpers ───────────────────────────────────────────────────────────

qa_schema <- function(df, required) {
  missing <- setdiff(required, names(df))
  list(pass = length(missing) == 0, missing = missing, n_cols = ncol(df))
}

qa_mgmt_enum <- function(df, known) {
  observed <- sort(unique(as.character(df$MgmtID)))
  list(observed = observed, unknown = setdiff(observed, known))
}

qa_year_coverage <- function(df, expected) {
  observed <- sort(unique(df$Year))
  list(observed = observed, missing = setdiff(expected, observed))
}

qa_na_audit <- function(df) {
  na_frac <- vapply(df, function(x) mean(is.na(x)), numeric(1))
  na_frac <- na_frac[na_frac > 0]
  if (length(na_frac) == 0) {
    return(tibble(column = character(), na_frac = numeric(), n_na = integer()))
  }
  tibble(
    column  = names(na_frac),
    na_frac = unname(na_frac),
    n_na    = as.integer(round(unname(na_frac) * nrow(df)))
  ) |> arrange(desc(na_frac))
}

qa_range <- function(df, ranges) {
  out <- list()
  for (col in intersect(names(ranges), names(df))) {
    rng <- ranges[[col]]; lo <- rng[1]; hi <- rng[2]
    vals <- df[[col]]
    if (!is.numeric(vals)) next
    n_below <- if (!is.na(lo)) sum(vals < lo, na.rm = TRUE) else 0L
    n_above <- if (!is.na(hi)) sum(vals > hi, na.rm = TRUE) else 0L
    if (n_below + n_above > 0) {
      out[[col]] <- tibble(
        column  = col,
        lo      = lo,
        hi      = hi,
        n_below = n_below,
        n_above = n_above,
        min_val = suppressWarnings(min(vals, na.rm = TRUE)),
        max_val = suppressWarnings(max(vals, na.rm = TRUE))
      )
    }
  }
  if (length(out) == 0) tibble() else bind_rows(out)
}

qa_duplicates <- function(df, keys) {
  if (!all(keys %in% names(df))) return(tibble())
  df |>
    group_by(across(all_of(keys))) |>
    summarise(n = n(), .groups = "drop") |>
    filter(n > 1)
}

# Port of old check_tpa_match_2025: at the Tpa baseline year, every MgmtID's
# Tpa for a given StandID should equal BASE's Tpa (no treatment applied yet).
qa_tpa_baseline <- function(df, year = 2025, tol = 1e-2) {
  if (!"Tpa" %in% names(df)) return(tibble())
  base <- df |>
    filter(MgmtID == "BASE", Year == year) |>
    select(StandID, base_Tpa = Tpa) |>
    distinct(StandID, .keep_all = TRUE)
  others <- df |>
    filter(MgmtID != "BASE", Year == year) |>
    select(MgmtID, StandID, Tpa)
  others |>
    inner_join(base, by = "StandID") |>
    mutate(diff = abs(Tpa - base_Tpa)) |>
    filter(diff > tol) |>
    arrange(desc(diff))
}

qa_stand_count_drift <- function(df) {
  df |>
    group_by(MgmtID, Year) |>
    summarise(n_stands = n_distinct(StandID), .groups = "drop") |>
    group_by(MgmtID) |>
    summarise(
      min_stands = min(n_stands),
      max_stands = max(n_stands),
      .groups   = "drop"
    ) |>
    filter(min_stands != max_stands)
}

# ── Plot: median EC time series for BASE + FIC1-6 ───────────────────────────

plot_median_timeseries <- function(df, label, out_path) {
  fic_ids <- paste0("FIC", 1:6)
  keep    <- c("BASE", fic_ids)
  d <- df |> filter(MgmtID %in% keep)
  if (nrow(d) == 0) {
    message("  [plot] no BASE/FIC rows in ", label, " — skipping")
    return(NULL)
  }

  ec_cols <- intersect(names(EC_RANGES), names(d))
  ec_cols <- ec_cols[vapply(d[ec_cols], is.numeric, logical(1))]
  if (length(ec_cols) == 0) {
    message("  [plot] no numeric EC columns in ", label, " — skipping")
    return(NULL)
  }

  long <- d |>
    select(MgmtID, Year, all_of(ec_cols)) |>
    pivot_longer(all_of(ec_cols), names_to = "ec", values_to = "value") |>
    group_by(MgmtID, Year, ec) |>
    summarise(median_val = median(value, na.rm = TRUE), .groups = "drop") |>
    mutate(MgmtID = factor(MgmtID, levels = keep))

  fic_pal <- colorRampPalette(c("#FED976", "#FC4E2A", "#800026"))(6)
  pal <- c("BASE" = "#000000", setNames(fic_pal, fic_ids))

  n_panels <- length(ec_cols)
  ncol <- min(4, n_panels)
  nrow <- ceiling(n_panels / ncol)

  p <- ggplot(long, aes(Year, median_val, color = MgmtID)) +
    geom_line(linewidth = 0.55) +
    facet_wrap(~ ec, scales = "free_y", ncol = ncol) +
    scale_color_manual(values = pal) +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom", strip.text = element_text(size = 8)) +
    labs(title = paste("Median EC time series —", label),
         subtitle = "BASE + FIC1–6, median across stands",
         x = "Year", y = NULL, color = NULL)

  ggsave(out_path, p,
         width  = max(8, 2.6 * ncol),
         height = max(5, 2 * nrow),
         dpi    = 130)
  out_path
}

# ── Per-file driver ─────────────────────────────────────────────────────────

run_qa_for_file <- function(spec) {
  message("── ", spec$label, " ──")
  t0 <- Sys.time()
  message("  loading: ", spec$path)
  df <- s3_read_rds(spec$path)
  load_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  loaded %s rows in %.1fs", format(nrow(df), big.mark = ","), load_secs))

  required <- if (spec$kind == "stand") REQUIRED_STAND_COLS else REQUIRED_STDSTK_COLS
  dup_keys <- if (spec$kind == "stand") c("MgmtID", "StandID", "Year")
              else                        c("MgmtID", "StandID", "Year", "Species")

  results <- list(
    spec        = spec,
    n_rows      = nrow(df),
    n_stands    = if ("StandID" %in% names(df)) dplyr::n_distinct(df$StandID) else NA_integer_,
    year_range  = if ("Year"    %in% names(df)) range(df$Year, na.rm = TRUE)  else c(NA, NA),
    load_secs   = load_secs,
    schema      = qa_schema(df, required),
    mgmt        = qa_mgmt_enum(df, KNOWN_MGMT_IDS),
    year_cov    = qa_year_coverage(df, EXPECTED_YEARS),
    na_audit    = qa_na_audit(df),
    range_viol  = qa_range(df, EC_RANGES),
    duplicates  = qa_duplicates(df, dup_keys),
    tpa_base    = if (spec$kind == "stand") qa_tpa_baseline(df, 2025) else tibble(),
    stand_drift = if (spec$kind == "stand") qa_stand_count_drift(df) else tibble(),
    pre_trt     = if (spec$kind == "stand") check_pre_treatment_invariant(df) else tibble(),
    pre_fire    = if (spec$kind == "stand") check_pre_fire_invariant(df)      else tibble(),
    plot_path   = NULL
  )

  fig_path <- file.path(FIG_DIR, paste0(spec$label, "_median_ts.png"))
  results$plot_path <- plot_median_timeseries(df, spec$label, fig_path)

  rm(df); gc(verbose = FALSE)
  results
}

# ── Markdown report ─────────────────────────────────────────────────────────

mark <- function(ok) if (isTRUE(ok)) "✓" else "⚠"

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)

table_md <- function(df, max_rows = 15) {
  if (nrow(df) == 0) return("(none)")
  shown <- head(df, max_rows)
  hdr <- paste(names(shown), collapse = " | ")
  sep <- paste(rep("---", ncol(shown)), collapse = " | ")
  rows <- apply(shown, 1, function(r) paste(r, collapse = " | "))
  more <- if (nrow(df) > max_rows) sprintf("\n_…and %d more rows_", nrow(df) - max_rows) else ""
  paste0("| ", hdr, " |\n| ", sep, " |\n",
         paste("| ", rows, " |", collapse = "\n"), more)
}

render_file_section <- function(r) {
  spec <- r$spec
  lines <- c()
  add <- function(...) lines <<- c(lines, paste0(...))

  add("## ", spec$label, "  \n")
  add(sprintf("**Variant:** %s · **Kind:** %s · **Path:** `%s`  ",
              spec$variant, spec$kind, spec$path))
  add(sprintf("**Rows:** %s · **Distinct StandIDs:** %s · **Year range:** %d–%d · **Load time:** %.1fs",
              fmt_int(r$n_rows),
              if (is.na(r$n_stands)) "n/a" else fmt_int(r$n_stands),
              r$year_range[1], r$year_range[2], r$load_secs))
  add("")

  # Tier 1
  add("### Tier 1 — schema, enumeration, timing\n")
  add(sprintf("- %s **Schema** — %s",
              mark(r$schema$pass),
              if (r$schema$pass) sprintf("all required columns present (%d total)", r$schema$n_cols)
              else sprintf("MISSING: %s", paste(r$schema$missing, collapse = ", "))))
  add(sprintf("- %s **MgmtID enumeration** — %d observed; %d unknown to `mgmt_labels`%s",
              mark(length(r$mgmt$unknown) == 0),
              length(r$mgmt$observed),
              length(r$mgmt$unknown),
              if (length(r$mgmt$unknown) > 0) sprintf(": %s", paste(r$mgmt$unknown, collapse = ", ")) else ""))
  add(sprintf("  - Observed: %s", paste(r$mgmt$observed, collapse = ", ")))
  add(sprintf("- %s **Year coverage** — %d expected years; %d missing%s",
              mark(length(r$year_cov$missing) == 0),
              length(EXPECTED_YEARS),
              length(r$year_cov$missing),
              if (length(r$year_cov$missing) > 0) sprintf(": %s", paste(r$year_cov$missing, collapse = ", ")) else ""))
  if (spec$kind == "stand") {
    add(sprintf("- %s **Pre-treatment invariant (2029)** — %d mismatches",
                mark(nrow(r$pre_trt) == 0), nrow(r$pre_trt)))
    add(sprintf("- %s **Pre-fire invariant (2034)** — %d mismatches",
                mark(nrow(r$pre_fire) == 0), nrow(r$pre_fire)))
  }
  add("")

  # Tier 2
  add("### Tier 2 — ranges, NAs, baselines, duplicates\n")
  add(sprintf("- %s **Range plausibility** — %d columns with violations",
              mark(nrow(r$range_viol) == 0), nrow(r$range_viol)))
  if (nrow(r$range_viol) > 0) {
    add("\n", table_md(r$range_viol), "\n")
  }
  add(sprintf("- %s **NA audit** — %d columns contain any NAs",
              mark(nrow(r$na_audit) == 0), nrow(r$na_audit)))
  if (nrow(r$na_audit) > 0) {
    na_show <- r$na_audit |> mutate(na_frac = sprintf("%.3f%%", 100 * na_frac))
    add("\n", table_md(na_show), "\n")
  }
  add(sprintf("- %s **Duplicate rows** — %d duplicate key groups",
              mark(nrow(r$duplicates) == 0), nrow(r$duplicates)))
  if (spec$kind == "stand") {
    add(sprintf("- %s **Tpa baseline at 2025** — %d (StandID, MgmtID) mismatches vs BASE",
                mark(nrow(r$tpa_base) == 0), nrow(r$tpa_base)))
    if (nrow(r$tpa_base) > 0) {
      add("\n", table_md(r$tpa_base), "\n")
    }
    add(sprintf("- %s **Stand count drift** — %d MgmtIDs with stand count varying across years",
                mark(nrow(r$stand_drift) == 0), nrow(r$stand_drift)))
    if (nrow(r$stand_drift) > 0) {
      add("\n", table_md(r$stand_drift), "\n")
    }
  }
  add("")

  # Tier 3 / plot
  add("### Median time series — BASE + FIC1–6\n")
  if (!is.null(r$plot_path)) {
    rel <- sub(paste0("^", normalizePath(OUT_DIR, mustWork = FALSE), "/?"), "",
               normalizePath(r$plot_path, mustWork = FALSE))
    add(sprintf("![](%s)", rel))
  } else {
    add("_no plot generated_")
  }
  add("\n---\n")
  paste(lines, collapse = "\n")
}

render_summary <- function(all_results) {
  rows <- lapply(all_results, function(r) {
    tier1_fails <- sum(
      !r$schema$pass,
      length(r$mgmt$unknown) > 0,
      length(r$year_cov$missing) > 0,
      nrow(r$pre_trt)  > 0,
      nrow(r$pre_fire) > 0
    )
    tier2_warns <- sum(
      nrow(r$range_viol)  > 0,
      nrow(r$na_audit)    > 0,
      nrow(r$duplicates)  > 0,
      nrow(r$tpa_base)    > 0,
      nrow(r$stand_drift) > 0
    )
    tibble(
      File         = r$spec$label,
      Rows         = fmt_int(r$n_rows),
      Stands       = if (is.na(r$n_stands)) "n/a" else fmt_int(r$n_stands),
      MgmtIDs      = length(r$mgmt$observed),
      Years        = sprintf("%d–%d", r$year_range[1], r$year_range[2]),
      `Tier-1 fails` = tier1_fails,
      `Tier-2 warns` = tier2_warns
    )
  })
  table_md(bind_rows(rows), max_rows = 100)
}

# ── Main ────────────────────────────────────────────────────────────────────

main <- function() {
  message("\nFVS S3 QA/QC — ", TS, "\n")
  results <- lapply(S3_FILES, run_qa_for_file)

  header <- c(
    "# FVS S3 QA/QC Report",
    "",
    sprintf("**Generated:** %s  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("**Bucket:** `%s`  ", S3_BUCKET),
    sprintf("**Prefix:** `%s`", S3_PREFIX),
    "",
    "## Summary",
    "",
    render_summary(results),
    "",
    "**Legend:** ✓ = clean · ⚠ = needs review. Tier-1 issues block the data from being trusted; Tier-2 are quality warnings worth eyeballing.",
    "",
    "---",
    ""
  )
  body <- vapply(results, render_file_section, character(1))
  writeLines(c(header, body), REPORT_PATH)

  message("\nReport written: ", REPORT_PATH)
  message("Figures:        ", FIG_DIR)
  invisible(results)
}

if (!interactive()) {
  main()
}
