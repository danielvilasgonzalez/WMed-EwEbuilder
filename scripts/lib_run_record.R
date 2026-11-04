## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: lib_run_record.R
## =================================================================
## lib_run_record.R
## Created by: Daniel Vilas
##
## Reproducibility helpers, sourced by run_pipeline_demo.R:
##  - wmed_archive_previous_run(): before a run, copies the previous
##    workbook, logs and validation summary to out_dir/previous_runs/<time>/
##    (the last WMED_KEEP_RUNS runs, default 5), so runs can be compared.
##  - wmed_data_catalogue(): every input data file the scripts name, found
##    in the shared data folder, with size, date and the scripts using it;
##    plus source/citation from data/data_sources.csv when that file exists.
##  - wmed_write_run_record(): after a run, out_dir/run_record_<time>.json
##    (settings, steps and minutes, git commit, R and package versions,
##    input files with size and date) and the same key facts on the
##    workbook's info sheet.
##  - wmed_save_package_versions(): writes scripts/package_versions.csv,
##    the package versions of a computer where the pipeline runs well;
##    install_packages.R warns when another computer has older ones.
## No personal information is written: paths are given relative to the
## shared data folder and the output folder, never the user's home.
## =================================================================

wmed_archive_previous_run <- function(out_dir, keep = getOption("wmed.keep_runs", 5)) {
  wb <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
  if (!file.exists(wb)) return(invisible(NULL))
  dest <- file.path(out_dir, "previous_runs", format(file.mtime(wb), "%Y%m%d_%H%M"))
  if (dir.exists(dest)) return(invisible(dest))          # this run's previous state is already saved
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  logs <- list.files(out_dir, "^\\d{8}_0[1-6]_.*_log\\.txt$", full.names = TRUE)
  extra <- file.path(out_dir, c("validation/validation_summary_ALL.csv", "fisheries/F_by_fg.csv",
                                Sys.glob(file.path(out_dir, "run_record_latest.json"))))
  files <- c(wb, logs, extra[file.exists(extra)])
  file.copy(files, dest, overwrite = TRUE, copy.date = TRUE)
  old <- sort(list.dirs(file.path(out_dir, "previous_runs"), recursive = FALSE), decreasing = TRUE)
  if (length(old) > keep) unlink(old[-seq_len(keep)], recursive = TRUE)
  message("[run record] Previous results copied to ", sub(paste0("^", out_dir, "/?"), "out_dir/", dest),
          " (the last ", keep, " runs are kept).")
  invisible(dest)
}

## Data files named in the scripts (string literals ending in a data
## extension), located in the shared data folder.
wmed_data_catalogue <- function(git_dir, pcloud_dir) {
  scripts <- list.files(file.path(git_dir, "scripts"), "\\.R$", full.names = TRUE)
  ext <- "csv|xlsx|xls|zip|shp|gpkg|geojson|nc|tif|tiff|asc|rds|txt|accdb|mdb|json"
  hits <- do.call(rbind, lapply(scripts, function(f) {
    l <- readLines(f, warn = FALSE)
    l <- sub("(^|[^\"'])#.*$", "\\1", l)                      # drop comments
    m <- unlist(regmatches(l, gregexpr(paste0("\"[^\"]+\\.(", ext, ")\""), l, ignore.case = TRUE)))
    if (!length(m)) return(NULL)
    data.frame(name = basename(gsub("\"", "", m)), script = basename(f), stringsAsFactors = FALSE)
  }))
  if (is.null(hits) || !nrow(hits)) return(data.frame())
  data_dir <- file.path(pcloud_dir, "data")
  all_files <- if (dir.exists(data_dir)) list.files(data_dir, recursive = TRUE) else character(0)
  names_used <- unique(hits$name)
  rows <- lapply(names_used, function(n) {
    rel <- all_files[basename(all_files) == n]
    if (!length(rel)) return(NULL)                            # an output name or a file not in the shared data
    p <- file.path(data_dir, rel[1]); fi <- file.info(p)
    data.frame(file = n, path_in_shared_data = file.path("data", rel[1]), size_MB = round(fi$size / 1e6, 2),
               modified = format(fi$mtime, "%Y-%m-%d"), md5 = if (fi$size < 50e6) unname(tools::md5sum(p)) else NA_character_,
               used_by = paste(sort(unique(hits$script[hits$name == n])), collapse = ", "), stringsAsFactors = FALSE)
  })
  cat <- do.call(rbind, rows)
  if (is.null(cat)) return(data.frame())
  src <- file.path(data_dir, "data_sources.csv")             # curated: file, source, citation, years, licence, url
  if (file.exists(src)) {
    s <- tryCatch(utils::read.csv(src, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(s) && "file" %in% names(s)) cat <- merge(cat, s, by = "file", all.x = TRUE)
  }
  cat[order(cat$path_in_shared_data), ]
}

.wmed_git_commit <- function(git_dir) {
  h <- file.path(git_dir, ".git", "HEAD")
  if (!file.exists(h)) return(NA_character_)
  ref <- trimws(readLines(h, n = 1, warn = FALSE))
  if (!startsWith(ref, "ref: ")) return(substr(ref, 1, 12))
  ref <- sub("^ref: ", "", ref)
  f <- file.path(git_dir, ".git", ref)
  sha <- if (file.exists(f)) trimws(readLines(f, n = 1, warn = FALSE)) else {
    pr <- file.path(git_dir, ".git", "packed-refs")
    if (file.exists(pr)) { l <- grep(paste0(" ", ref, "$"), readLines(pr, warn = FALSE), value = TRUE); sub(" .*$", "", l[1]) } else NA_character_
  }
  paste0(substr(sha, 1, 12), " (", sub("^refs/heads/", "", ref), ")")
}

wmed_package_versions <- function(pkgs = NULL) {
  if (is.null(pkgs)) pkgs <- sort(unique(c(loadedNamespaces(),
    c("data.table", "ggplot2", "openxlsx", "sf", "terra", "raster", "rfishbase", "duckdbfs", "worrms", "TropFishR",
      "ncdf4", "pdftools", "httr", "shiny", "rmarkdown", "knitr", "patchwork", "ragg"))))
  pkgs <- pkgs[vapply(pkgs, function(p) nzchar(system.file(package = p)), logical(1))]
  data.frame(package = pkgs, version = vapply(pkgs, function(p) as.character(utils::packageVersion(p)), ""), stringsAsFactors = FALSE)
}

wmed_save_package_versions <- function(git_dir) {
  f <- file.path(git_dir, "scripts", "package_versions.csv")
  v <- wmed_package_versions(); v <- v[!v$package %in% rownames(utils::installed.packages(priority = "base")), ]
  utils::write.csv(v, f, row.names = FALSE)
  message("Package versions of this computer saved to ", f, " (commit it so other computers can compare).")
  invisible(f)
}

wmed_write_run_record <- function(out_dir, git_dir, pcloud_dir, t0, settings = list(), step_minutes = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  cat <- tryCatch(wmed_data_catalogue(git_dir, pcloud_dir), error = function(e) data.frame())
  if (nrow(cat)) utils::write.csv(cat, file.path(out_dir, "data_catalogue.csv"), row.names = FALSE)
  rec <- list(
    run_finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    minutes_total = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    step_minutes = step_minutes,
    settings = settings,
    git_commit = .wmed_git_commit(git_dir),
    R = R.version.string, os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    packages = { v <- wmed_package_versions(); stats::setNames(as.list(v$version), v$package) },
    inputs = if (nrow(cat)) lapply(seq_len(nrow(cat)), function(i) as.list(cat[i, c("path_in_shared_data", "size_MB", "modified", "md5")])) else list())
  js <- if (requireNamespace("jsonlite", quietly = TRUE)) jsonlite::toJSON(rec, auto_unbox = TRUE, pretty = TRUE, null = "null") else
    paste(utils::capture.output(str(rec)), collapse = "\n")
  f <- file.path(out_dir, paste0("run_record_", stamp, ".json"))
  writeLines(js, f); file.copy(f, file.path(out_dir, "run_record_latest.json"), overwrite = TRUE)
  ## the same key facts on the workbook's info sheet
  wb <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
  if (file.exists(wb) && exists("upsert_workbook_sheets") && requireNamespace("openxlsx", quietly = TRUE)) tryCatch({
    info <- if ("info" %in% openxlsx::getSheetNames(wb)) openxlsx::read.xlsx(wb, "info", check.names = FALSE, sep.names = " ") else
      data.frame(Field = character(), Value = character())
    add <- data.frame(Field = c("Git commit of the code", "R version", "Run record", "Input files (data_catalogue.csv)"),
                      Value = c(rec$git_commit, rec$R, basename(f), as.character(nrow(cat))), stringsAsFactors = FALSE)
    info <- rbind(info[!info$Field %in% add$Field, c("Field", "Value")], add)
    upsert_workbook_sheets(list(info = info), wb)
    ## upsert_workbook_sheets() re-adds the sheet at the end: put info first
    ## again, so the workbook opens on it (then FGs, Fleets, ...).
    w <- openxlsx::loadWorkbook(wb); nm <- names(w)
    if (nm[1] != "info" && "info" %in% nm) {
      openxlsx::worksheetOrder(w) <- c(match("info", nm), setdiff(seq_along(nm), match("info", nm)))
      openxlsx::activeSheet(w) <- 1
      openxlsx::saveWorkbook(w, wb, overwrite = TRUE)
    }
  }, error = function(e) message("[run record] info sheet not updated: ", conditionMessage(e)))
  message("[run record] ", basename(f), " written (settings, git commit ", rec$git_commit, ", ", nrow(cat), " input files with size, date and md5).")
  invisible(f)
}
