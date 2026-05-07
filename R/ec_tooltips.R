# R/ec_tooltips.R
# Tooltip text for ECs exposed in the Weights step.
#
# Keys match the `column` values in ec_labels.R for direct lookup:
#   tooltip <- EC_TOOLTIPS[[ec_labels$column[i]]]
#
# Sources:
#   DBS Guide  Users Guide to the Database Extension of the Forest Vegetation
#              Simulator, v3.0 (Crookston et al., rev. January 2026)
#              https://www.fs.usda.gov/sites/default/files/forest-management/fvs-dbs-user-guide.pdf
#   FFE Guide  The Fire and Fuels Extension to the Forest Vegetation Simulator
#              (Rebain et al., updated)
#              https://www.fs.usda.gov/fmsc/ftp/fvs/docs/gtr/FFEguide.pdf

EC_TOOLTIPS <- list(

  # ── Canopy ──────────────────────────────────────────────────────────────
  "Stratum_1_Crown_Cover" = "Percent canopy cover of the uppermost (overstory) tree stratum, accounting for crown overlap. Source: FVS_StrClass (DBS Guide \u00a72.1.11).",

  "Stratum_2_Crown_Cover" = "Percent canopy cover of the second tree stratum, typically mid-story. Useful for detecting two-storied stand structure. Source: FVS_StrClass.",

  "Stratum_3_Crown_Cover" = "Percent canopy cover of the lowest valid tree stratum. Often near zero in single- or two-storied stands. Source: FVS_StrClass.",

  "Total_Cover"           = "Total tree canopy cover summed across all valid strata. Single-number summary of canopy density. Source: derived from FVS_StrClass.",

  "CCF"                   = "Crown competition factor \u2014 a dimensionless crowding index. ~100 indicates a closed canopy of average-form open-grown trees; lower values indicate more open conditions, higher values indicate crowding. Often used as a canopy-openness proxy. Source: FVS_Summary2 (DBS Guide \u00a72.1.6).",

  # ── Stand metrics (FVS_Summary2 / StdStk) ───────────────────────────────
  "QMD"                   = "Quadratic mean diameter \u2014 DBH of the tree of average basal area. Single-value summary of mean tree size. Source: FVS_Summary2.",

  "BA"                    = "Basal area \u2014 cross-sectional stem area at breast height. Open stands ~30\u201360 sq ft/ac (\u22487\u201314 m\u00b2/ha); dense forest 150\u2013250+ sq ft/ac (\u224834\u201357 m\u00b2/ha). Source: FVS_Summary2.",

  "Tpa"                   = "Trees per acre \u2014 live tree count across all size classes. Open stands typically <200 TPA; closed forest 400\u20131000+. Source: FVS_Summary2 / StdStk.",

  "TopHt"                 = "Top height \u2014 average height (feet) of the largest 40 trees per acre. Used as a productivity / site-quality summary. Source: FVS_Summary2.",

  "SDI"                   = "Stand density index \u2014 relative density measure normalized to a reference 10-inch tree. Species-specific maximums typically 400\u2013800 for Western conifers. Source: FVS_Summary2.",

  "MCuFt"                 = "Merchantable cubic-foot volume per acre \u2014 standing live timber volume above merchantability cutoffs (variant-specific). Source: FVS_Summary2.",

  # ── Understory (live surface vegetation, biomass) ───────────────────────
  "Surface_Shrub"         = "Live shrub biomass loading (tons/acre) in the surface fuel layer. Drives shrub-stratum fire behavior and provides cover for many wildlife species. Source: FVS_Fuels (FFE Guide \u00a72.4.1).",

  "Surface_Herb"          = "Live herbaceous (grass + forb) biomass loading (tons/acre) in the surface fuel layer. Source: FVS_Fuels.",

  # ── Fuels (FVS_Fuels / FVS_Down_Wood_*) ─────────────────────────────────
  "Forest_Down_Dead_Wood" = "Total down dead wood loading (tons/acre) \u2014 combined coarse and fine downed woody material on the forest floor. Sourced from FVS_Down_Wood_Vol or aggregated from FVS_Fuels size classes (DBS Guide \u00a72.3.12\u201313, FFE Guide \u00a72.4.1).",

  "Surface_Litter"        = "Surface litter loading (tons/acre) \u2014 recently fallen needles, leaves, and fine material above the duff layer. Source: FVS_Fuels.",

  "Surface_Duff"          = "Surface duff loading (tons/acre) \u2014 partially to fully decomposed organic layer between litter and mineral soil. Source: FVS_Fuels.",

  "Surface_lt3"           = "Downed wood loading <3\" diameter (tons/acre) \u2014 combined 1-hr (<0.25\"), 10-hr (0.25\u20131\"), and 100-hr (1\u20133\") fuel classes. Drives surface fire ignition and rate of spread. Source: FVS_Fuels.",

  "Surface_ge3"           = "Downed wood loading \u22653\" diameter (tons/acre) \u2014 1000-hr fuels (sound + rotten combined). Coarse woody debris relevant to fire residence time, soil heating, and wildlife habitat (cover, hibernacula). Source: FVS_Fuels.",

  "Surface_Total"         = "Total surface fuel loading (tons/acre) \u2014 sum of litter, duff, and all woody fuel size classes. Source: derived from FVS_Fuels.",

  # ── Wildlife habitat (FVS_SnagSum) ──────────────────────────────────────
  "Hard_snags_total"      = "Hard snags per acre \u2014 relatively intact standing dead trees with bole and major branches in early stages of decay. Used by primary cavity-nesters (e.g., woodpeckers excavating fresh cavities). Source: FVS_SnagSum (DBS Guide \u00a72.3.8, FFE Guide \u00a72.3).",

  "Soft_snags_total"      = "Soft snags per acre \u2014 heavily decayed standing dead trees with loose bark, broken branches, and softened wood. Used by secondary cavity-users and provide invertebrate prey base. Source: FVS_SnagSum.",

  "Hard_soft_snags_total" = "Total standing dead trees per acre \u2014 hard + soft snags combined. Source: FVS_SnagSum.",

  # ── Carbon (FVS_Carbon) ─────────────────────────────────────────────────
  "Total_Stand_Carbon"    = "Total stand carbon (tons C/acre) \u2014 sum across all live, standing-dead, downed-dead, and forest-floor pools. Source: FVS_Carbon (DBS Guide \u00a72.3.10).",

  "Aboveground_Total_Live"= "Aboveground live biomass (tons/acre) \u2014 wood + bark of live trees. Foliage carbon typically reported separately in FVS_Carbon. Source: FVS_Carbon / FVS_FIAVBC_Summary.",

  "Forest_Floor"          = "Forest floor carbon (tons C/acre) \u2014 carbon stored in litter + duff layers. Source: FVS_Carbon.",

  # ── Growth (FVS_Summary2) ───────────────────────────────────────────────
  "Acc"                   = "Accretion \u2014 periodic annual increment of total cubic-foot volume (ft\u00b3/acre/year). Net stand growth before subtracting mortality. Source: FVS_Summary2.",

  "Mort"                  = "Mortality \u2014 annual loss to natural mortality (trees/acre/year). Source: FVS_Summary2."
)

# Helper: safe lookup with fallback for unmapped ECs.
ec_tooltip <- function(column_name) {
  txt <- EC_TOOLTIPS[[column_name]]
  if (is.null(txt)) return("Tooltip not yet defined for this EC.")
  txt
}
