## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: make_input_snapshot.R
## =================================================================
## make_input_snapshot.R
## Created by: Daniel Vilas
##
## Packs the input data the pipeline uses into one zip, to deposit as a
## frozen, citable snapshot (e.g. on Zenodo) for users and reviewers
## without access to the shared pCloud folder:
##   source("make_input_snapshot.R")    # from the scripts/ folder
## Writes out_dir/WMed_EwE_inputs_<date>.zip with:
##   data/...              the files the scripts name (same folder layout)
##   data_catalogue.csv    file, size, date, md5, scripts using it, and
##                         source/citation from data/data_sources.csv
##   README.txt            what it is and how to use it
## Check the licence of each source before publishing (some data, e.g.
## national catch data, may not be redistributable): set
## SNAPSHOT_EXCLUDE <- c("file1.csv", ...) to leave files out.
## To use a snapshot: unzip it and point pcloud_dir to that folder
## (config.local.R or the Folders card of the page).
## =================================================================

.snap_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!file.exists(file.path(.snap_dir, "wmed_paths.R"))) .snap_dir <- getwd()
source(file.path(.snap_dir, "wmed_paths.R"))
source(file.path(.snap_dir, "lib_run_record.R"))
.p <- wmed_find_paths(dirname(.snap_dir))
if (!isTRUE(.p$ok)) stop("Folders not found: ", .p$problem)
cat_dt <- wmed_data_catalogue(.p$git_dir, .p$pcloud_dir)
if (!nrow(cat_dt)) stop("No input files found in ", .p$pcloud_dir)
if (exists("SNAPSHOT_EXCLUDE")) cat_dt <- cat_dt[!cat_dt$file %in% SNAPSHOT_EXCLUDE, ]
stage <- file.path(tempdir(), "wmed_snapshot"); unlink(stage, recursive = TRUE); dir.create(stage)
for (rel in cat_dt$path_in_shared_data) {
  dir.create(file.path(stage, dirname(rel)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(.p$pcloud_dir, rel), file.path(stage, rel), copy.date = TRUE)
}
utils::write.csv(cat_dt, file.path(stage, "data_catalogue.csv"), row.names = FALSE)
writeLines(c("WMed-EwEbuilder input data snapshot",
             paste("Made:", format(Sys.Date())),
             paste("Files:", nrow(cat_dt), "-", round(sum(cat_dt$size_MB), 1), "MB"),
             "Each file's source, citation and licence: data_catalogue.csv (fill data/data_sources.csv for the citations).",
             "Use: unzip, then set pcloud_dir to this folder (config.local.R, or the Folders card of WMed-EwEbuilder.qmd)."),
           file.path(stage, "README.txt"))
zf <- file.path(.p$out_dir, paste0("WMed_EwE_inputs_", format(Sys.Date(), "%Y%m%d"), ".zip"))
old <- setwd(stage)
if (requireNamespace("zip", quietly = TRUE)) zip::zip(zf, list.files(".", recursive = TRUE)) else utils::zip(zf, list.files(".", recursive = TRUE), flags = "-rq")
setwd(old)
message("Snapshot: ", zf, " (", nrow(cat_dt), " files, ", round(file.size(zf) / 1e6, 1), " MB).",
        "\nCheck data_catalogue.csv for licences before uploading it (e.g. zenodo.org -> New upload).")
