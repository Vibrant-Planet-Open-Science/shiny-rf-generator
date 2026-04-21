# RF Generator (v2.0)

An RShiny application for generating **Response Functions (RFs)** that describe how wildfire and forest management treatments affect habitat components for wildlife species. Built by [Vibrant Planet Open Science](https://github.com/Vibrant-Planet-Open-Science).

> ⚠️ **Status: Active development.** This is v2.0, currently being rebuilt from v1. The UI skeleton is working, real data wiring and RF computation are in progress. Not production-ready — see [Current scope & limitations](#current-scope--limitations).

---

## What it does

RF Generator lets a user define an area of interest, pick the ecosystem components (ECs) that matter for a given species or habitat concern, set importance weights and effect types, and get back a weighted Response Function describing how different wildfire intensities and management treatments affect habitat suitability over time.

Outputs feed into:
- Quantitative Wildfire Risk Assessments (QWRA)
- NEPA analyses
- Species at Risk Assessment workshops

v2.0 replaces the v1 fixed-slider approach with the manuscript-aligned method: separated importance weights (1–5), explicit effect types (positive / negative / range), and a richer set of exposed ECs.

## Who it's for

Wildlife biologists, fire ecologists, HVRA workshop participants, and analysts working on forest management and wildfire risk planning. Some familiarity with the Forest Vegetation Simulator (FVS) outputs is helpful but not required.

## Current scope & limitations

**In scope (v2.0 initial release):**
- Western US, California (CA) and Central Rockies (CR) FVS variants
- Fire intensity classes FIC1–6
- Treatments: MRCC (ground-based mechanical thinning), MTTH (manual thinning), RMGP (grapple pile burn)
- Stand-level and species-specific RFs

**Not yet wired (in progress):**
- Real S3 data loads (currently placeholder values in some screens)
- Full weights UI (1–5 stepper + effect type pills + range inputs)
- `response_spacer()` integration for real RF computation
- Download handlers (RF outputs, EC config, fact sheet)

**Out of scope this release:**
- Additional treatments (CMCC, CMUR, HCTA, HERB, MRCT, MTIR, MTUR, REVA, RMMA, RMTF, RXAI, RXGF)
- Modular `ui.R` / `server.R` / `global.R` restructure
- Arrow/Parquet conversion of the large RDS files
- Library submission flow

**Known issues:**
- Large AOIs (multi-state) may time out; ecoregion-scale AOIs are the expected upper bound

## Quick start

### Prerequisites
- R ≥ 4.3
- AWS credentials with read access to the `vp-open-science` bucket (set in `~/.aws/credentials` or via environment variables)
- Required R packages: `shiny`, `leaflet`, `sf`, `dplyr`, `tibble`, `DT`, `aws.s3`

### Run
```r
# From the repo root
shiny::runApp("R")
```

The app opens in your default browser at `http://127.0.0.1:NNNN`.

## Repository layout

| Path | Purpose |
|---|---|
| `R/app.R` | Main Shiny app — 8-screen wizard |
| `R/ec_labels.R` | EC crosswalk, MgmtID labels, S3 path constants |
| `R/functions.R` | Data pipeline helpers (`get_tm_ids`, `response_spacer`, `cleanDF`) |
| `R/check_timing_invariants.R` | Pre-disturbance timing sanity checks |
| `app/rf-generator-v1/` | v1 app, kept for reference |
| `data/` | Small local data files (county shapefiles, etc.) |
| `www/ecoregions_western.geojson` | RESOLVE 2017 ecoregion polygons for the map |
| `docs/timing_assumptions.md` | Audit of t=0 conventions for fire vs treatment |
| `README.md` | This file |

## Data

The app pulls precomputed FVS outputs from S3:

```
s3://vp-open-science/biodiversity/habitat-suitability/response_function_data/conus/v0.0.0/
```

Primary files used:
- `CA-ALL-StandLevel_2024-12-18.rds` — California variant, all scenarios
- `CR-ALL-StandLevel_2024-12-18.rds` — Central Rockies variant, all scenarios
- `CA-FIC-StdStk_*.rds`, `CR-TRT-StdStk_*.rds` — species-level stock tables
- `rshiny-spatial-data/` — county-level TreeMap ID lookups, western counties gpkg


## Wizard flow

1. **AOI** — upload a `.gpkg` boundary file or select ecoregions on the map
2. **Filters** — narrow stands by forest type and structural class (O'Hara 1996)
3. **Review** — confirm AOI, variant, and matching stand count before loading
4. **RF type** — pick stand-characteristics path or tree-species path
5. **Species** *(species path only)* — select target tree species
6. **ECs** — pick ecosystem components grouped by subcategory (canopy, tree size, fuels, wildlife habitat, carbon, growth)
7. **Weights** — set importance (1–5) and effect type (positive / negative / range) per EC
8. **Download** — review RF outputs, EC config, and fact sheet; download as CSV + markdown bundle

## Development

### Branching
- `main` — most recent working state
- `v2-wiring` — active development branch for v2.0
- Feature branches off `v2-wiring` for larger changes


### Related documentation
- Manuscript describing the RF methodology (in prep)

## Credits

Developed by **Katharyn Duffy** (lead) with contributions from **Sophie Gilbert** (PERC), **Mike Koontz**, **Tyler Hoecker**, and **Ethan Yackulic**.

Built on simulations from the Forest Vegetation Simulator (FVS) by the USDA Forest Service.
Ecoregion boundaries from [RESOLVE Ecoregions 2017](https://ecoregions.appspot.com/) (CC-BY 4.0).

## License

MIT — see [LICENSE](LICENSE).
