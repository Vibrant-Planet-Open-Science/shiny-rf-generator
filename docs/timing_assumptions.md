# Timing Assumptions — RF Generator v2

## FVS Simulation Timeline

| Year | Event |
|------|-------|
| 2025 | Simulation start (all scenarios) |
| 2029 | Last pre-treatment reporting year |
| 2030 | Treatment year (MRCT, MTTH, RMGP, etc.) |
| 2031 | First post-treatment reporting year |
| 2034 | Last pre-fire reporting year (FIC only) |
| 2035 | Fire/disturbance year (FIC1-6) |
| 2036 | First post-fire reporting year |
| 2040-2065 | Recovery period (5-year intervals) |

## Two Processing Streams

### Treatment Effect (TE) — not yet in scope
- **MgmtIDs:** all non-FIC treatments + BASE
- **Base year:** 2030 (treatment year)
- **rel.time:** `Year - 2030`
- **Filter:** `rel.time >= 0` (year 2030+)

### Disturbance Effect (DE) — current scope (fire only)
- **MgmtIDs:** FIC1-6 + BASE
- **Base year:** 2035 (fire year)
- **rel.time:** `Year - 2035`
- **Filter:** `rel.time >= 0` (year 2035+)

## RF Formula

"Revised difference in proportion" from rf-generator-data-prep.Rmd:

```
rf = (metric[t] / metric[t=0]) - (base[t] / base[t=0])
```

- `metric[t]` = EC value for this MgmtID at time t
- `metric[t=0]` = EC value for this MgmtID at fire year (2035)
- `base[t]` = BASE EC value at the same calendar year
- `base[t=0]` = BASE EC value at fire year (2035)
- Clipped to [-1, 1]
- RF = 0 at t=0 by construction

## Calendar Year Alignment

BASE is joined to FIC scenarios on `(StandID, Year)` — same calendar year, NOT same rel.time. This is critical because:

- BASE rel.time is anchored to 2030 (treatment year)
- FIC rel.time is anchored to 2035 (fire year)
- Joining on rel.time would compare different calendar years (5 years apart)

## Pre-disturbance Invariant Checks

### Existing code (from RF-generator/R/)

`check_tpa_match.R`: Checks Tpa matches at year 2025 (pre-treatment) or 2030 (pre-fire). Uses `round(Tpa, 2) == round(baseline_Tpa, 2)`.

`apply_trt_thresholds.R`: Validates treatment effect at year 2031. Thresholds: -0.7 for MRCC/CMCC, -0.01 for others. Skips: MTIR, BASE, HERB, RMTF, REVA, RXGF, RXAI, RMMA, HTCA.

### Invariant 1 — Pre-treatment (Year 2029)

**Expected:** All MgmtIDs have identical stand values at year 2029.

**Actual (CA-ALL):** FAILS for FIC1-6. The FIC scenarios were re-run with different CaseIDs ("FIC1 re-run", etc.) producing small growth divergence (~0.3% Tpa, systematic across all stands). Treatment MgmtIDs match BASE exactly.

**Impact:** The RF formula normalizes each scenario to its own t=0, so this divergence does not affect RF values.

### Invariant 2 — Pre-fire (Year 2034), FIC only

**Expected:** BASE matches FIC1-6 at year 2034.

**Actual:** BASE has NO rows at year 2034 in the FIC file. Only FIC1-6 have year 2034 data. FIC1-6 are internally consistent (identical at 2034).

**At Year 2035 (fire year):** BASE and FIC diverge by up to ~22 Tpa due to the re-run. The RF formula handles this by normalizing to each scenario's own t=0.

## RMTF Exception (future)

RMTF (Mowing) uses treatment year 2034, not 2030. Pre-treatment invariant for RMTF should check year 2033. RMTF is NOT in current scope.

## Implementation

- `compute_de_rf()` in `R/functions.R`: fire-specific RF computation
- `compute_weighted_de_rf()`: applies user weights and effect directions
- `check_timing_invariants.R`: reusable invariant check functions
- Future: `compute_te_rf()` for treatment-effect stream (base year 2030)
