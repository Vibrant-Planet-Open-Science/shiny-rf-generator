#' Check Pre-Treatment Timing Invariant
#'
#' At the pre-treatment year (default 2029), ALL MgmtIDs should have identical
#' stand values to BASE — no treatment or disturbance has happened yet.
#'
#' @param df Stand-level data with columns: MgmtID, StandID, Year, plus numerics.
#' @param year Pre-treatment year to check (default 2029).
#' @param tol Floating-point tolerance (default 1e-6).
#' @return Tibble of mismatches (empty if invariant holds).
check_pre_treatment_invariant <- function(df, year = 2029, tol = 1e-6) {
  num_cols <- df |>
    dplyr::select(where(is.numeric)) |>
    dplyr::select(-dplyr::any_of(c("StandID", "Year"))) |>
    colnames()

  base <- df |>
    dplyr::filter(MgmtID == "BASE", Year == year) |>
    dplyr::select(StandID, dplyr::all_of(num_cols))

  others <- df |>
    dplyr::filter(MgmtID != "BASE", Year == year) |>
    dplyr::select(MgmtID, StandID, dplyr::all_of(num_cols))

  joined <- others |>
    dplyr::inner_join(base, by = "StandID", suffix = c("", "_base"))

  mismatches <- list()
  for (col in num_cols) {
    base_col <- paste0(col, "_base")
    if (!base_col %in% colnames(joined)) next
    diffs <- joined |>
      dplyr::mutate(
        .val = .data[[col]],
        .base = .data[[base_col]],
        .diff = abs(.val - .base)
      ) |>
      dplyr::filter(.diff > tol) |>
      dplyr::select(MgmtID, StandID, column = dplyr::all_of(col),
                     value = .val, base_value = .base, abs_diff = .diff) |>
      dplyr::mutate(column = col)
    if (nrow(diffs) > 0) mismatches <- c(mismatches, list(diffs))
  }

  if (length(mismatches) == 0) {
    tibble::tibble(
      MgmtID = character(), StandID = numeric(), column = character(),
      value = numeric(), base_value = numeric(), abs_diff = numeric()
    )
  } else {
    dplyr::bind_rows(mismatches)
  }
}

#' Check Pre-Fire Timing Invariant
#'
#' At the pre-fire year (default 2034), BASE and FIC1-6 should have identical
#' stand values — fire hasn't happened yet. Treatment MgmtIDs are excluded
#' because their treatment already occurred in 2030.
#'
#' @param df Stand-level data.
#' @param year Pre-fire year to check (default 2034).
#' @param tol Floating-point tolerance (default 1e-6).
#' @return Tibble of mismatches (empty if invariant holds).
check_pre_fire_invariant <- function(df, year = 2034, tol = 1e-6) {
  fire_ids <- paste0("FIC", 1:6)
  fire_df <- df |> dplyr::filter(MgmtID %in% c("BASE", fire_ids))
  check_pre_treatment_invariant(fire_df, year = year, tol = tol)
}
