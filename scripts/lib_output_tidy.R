## =================================================================
## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: lib_output_tidy.R
## Created by: Daniel Vilas
##
## Keeps the output folders short. At the end of a run the intermediate
## tables of each block (biomass, fisheries, pbqb-traits, diet) go to
## <block>/intermediate/; what stays on top is the review tables
## (*REVIEW*) and the tables people open or the page reads (KEEP below).
## Each script calls wmed_restore_intermediate() first, so every table is
## where the scripts expect it while they run, also when one script is run
## on its own. validation/ and environment/ are left as they are.
## =================================================================
WMED_TIDY_BLOCKS <- c("biomass", "fisheries", "pbqb-traits", "diet")
WMED_TIDY_KEEP <- list(
  biomass = c("survey_fg_annual_index_regional_combined.csv", "species_density_regional_combined.csv", "species_biomass_by_gsa.csv",
              "biomass_proportion_by_species_fg.csv", "FG_lookup.csv", "FG_Density_by_Stratum.csv",
              "model_area_by_gsa.csv", "model_area_cells.csv", "strata_area_by_area.csv",
              "survey_index_cv_by_fg_year.csv", "fg_species_coverage.csv", "fg_species_biomass_needs_review.csv",
              "survey_outliers_flagged.csv", "survey_outlier_effect_by_fg_year.csv", "survey_area_coverage_by_year.csv",
              "survey_unmatched_for_manual_review.csv", "medias_area_10_200m_fallback.csv",
              "catchability_q_by_species.csv", "catchability_effect_by_fg.csv",
              "manual_density_override_applied.csv", "fg_species_coverage_totals.csv", "multistanza_split_comparison.csv"),
  fisheries = c("F_by_fg.csv", "Catches_Discards_FG_ts.csv", "catch_species_by_fg_year.csv", "Catches_Ecosim.csv", "Fishing_Effort_by_Fleet.csv",
                "catches_by_country_fleet_sector_year.csv", "species_fg_matched.csv", "mollusc_nei_split_by_country_year.csv",
                "ecopath_L_fg_coverage_check.csv", "fg_zero_catch_diagnostic.csv", "ecosim_eu_catch_share.csv",
                "high_F_REVIEW.csv", "assessment_F_threshold_by_fg.csv"),
  `pbqb-traits` = c("ecopath_ready_PB_QB.csv", "multistanza_parameters.csv", "PB_QB.csv", "PB_QB_spp.csv",
                    "traits_ewe.csv", "fg_pb_qb_weighted_with_F.csv", "Ecobase.csv", "References.csv"),
  diet = c("diet_composition_ewe.csv", "diet_provenance_by_predator_fg.csv", "diet_references_by_fg.csv",
           "reference_diet_matrix_fg_long.csv"))

## Move every intermediate table back next to the others (start of a script/run)
wmed_restore_intermediate <- function(out_dir) {
  for (b in WMED_TIDY_BLOCKS) {
    d <- file.path(out_dir, b, "intermediate")
    if (!dir.exists(d)) next
    f <- list.files(d, full.names = TRUE)
    if (length(f)) file.rename(f, file.path(out_dir, b, basename(f)))
  }
  invisible(NULL)
}

## Move the intermediate tables to <block>/intermediate/ (end of a run)
wmed_tidy_intermediate <- function(out_dir) {
  n <- 0L
  for (b in WMED_TIDY_BLOCKS) {
    d <- file.path(out_dir, b)
    if (!dir.exists(d)) next
    f <- list.files(d, "\\.(csv|txt)$", full.names = FALSE)
    mv <- f[!grepl("REVIEW", f) & !f %in% WMED_TIDY_KEEP[[b]]]
    if (!length(mv)) next
    dir.create(file.path(d, "intermediate"), showWarnings = FALSE)
    ok <- file.rename(file.path(d, mv), file.path(d, "intermediate", mv))
    n <- n + sum(ok)
  }
  if (n) message("[Outputs] ", n, " intermediate table(s) moved to <block>/intermediate/ (review tables and the main tables stay on top).")
  invisible(n)
}
