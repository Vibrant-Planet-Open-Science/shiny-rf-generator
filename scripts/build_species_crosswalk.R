# Builds data/species_crosswalk.csv from the FVS_SpeciesCrosswalk.xlsx workbook.
# Source: USDA Forest Service FVS Species Crosswalk (Excel original from Kat).
# Sheet:  WesternSpeciesTranslator (CA + CR variants live in this set).
# Output: code (USDA PLANTS SYMBOL), common_name, scientific_name.
#
# Run from project root with:  Rscript scripts/build_species_crosswalk.R

required <- c("readxl", "dplyr", "readr", "here")
missing  <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: renv::install(c('",
       paste(missing, collapse = "', '"), "'))")
}

library(readxl)
library(dplyr)
library(readr)
library(here)

xls_path <- here("data", "FVS_SpeciesCrosswalk.xls")
out_path  <- here("data", "species_crosswalk.csv")

species_labels <- read_excel(xls_path, sheet = "WesternSpeciesTranslator") |>
  select(
    code            = `USDA PLANTS SYMBOL`,
    common_name     = `COMMON NAME`,
    scientific_name = `SCIENTIFIC NAME`
  ) |>
  filter(!is.na(code), nzchar(code)) |>
  # Drop deprecated single-letter "old code" rows.
  filter(!code %in% c("C", "J", "L", "M", "S")) |>
  mutate(
    # Source data uses lowercase first letter for non-proper common names
    # (e.g., "weeping willow") but keeps proper nouns capitalized
    # (e.g., "Sitka willow", "Port Orford cedar"). Normalize so every name
    # starts with a capital while preserving internal capitalization.
    common_name = sub("^(.)", "\\U\\1", common_name, perl = TRUE)
  ) |>
  distinct(code, .keep_all = TRUE) |>
  arrange(scientific_name, common_name)

write_csv(species_labels, out_path)

cat(sprintf("Wrote %d rows to %s\n", nrow(species_labels), out_path))

# Sanity check: known species from prior AOI screenshots.
checks <- c("PSME", "TABR2", "ABCO", "QUKE", "PIPO", "ARME")
cat("\nSanity check (these should all show up):\n")
print(species_labels |> filter(code %in% checks))
