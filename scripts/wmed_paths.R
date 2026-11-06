## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: wmed_paths.R
## =================================================================
## wmed_paths.R
## Created by: Daniel Vilas
##
## Finds the three folders the pipeline needs, on any laptop that has
## the WMed_EwE repo (GitHub) and the pCloud Drive folder
## "EwE Western Med 2026":
##   git_dir    - the WMed_EwE repo (the folder that holds scripts/);
##                always the repo this file sits in
##   pcloud_dir - the shared data folder "EwE Western Med 2026"
##   out_dir    - where the outputs are written (local to each laptop)
##
## Order for pcloud_dir and out_dir (the first folder that EXISTS on this
## computer wins, so a path written for another laptop is skipped):
##   1. config.local.R  - this computer only (git-ignored; written by
##                        WMed-EwEbuilder.qmd or
##                        run_pipeline_demo.R when you pick a folder)
##   2. config.R        - shared in the repo (Daniel's paths)
##   3. environment variables WMED_PCLOUD_DIR / WMED_OUT_DIR
##   4. standard places: the pCloud Drive mount
##        macOS   ~/pCloud Drive/EwE Western Med 2026
##        Windows P:/EwE Western Med 2026   (pCloud's default drive)
##        Linux   ~/pCloudDrive/EwE Western Med 2026
##      and for out_dir ~/WMed_EwE_output (created if missing)
##
## Local copy of the shared data (since 2026-11-06). The pipeline never
## reads the pCloud folder directly: its data/ folder is copied once to
## a local folder (default ~/WMed_EwE_data/EwE Western Med 2026; set
## local_data_dir in config.local.R or WMED_LOCAL_DATA_DIR), and
## pcloud_dir then points to that copy. Nothing is ever written to the
## shared folder, so a run cannot change or corrupt it, and the shared
## data can later move to another server (only the copy step changes).
## wmed_sync_local_data() copies new or changed files only; run by
## run_pipeline_demo.R when the copy is missing, and by the
## "Update local data" button of the page. WMED_USE_SHARED_DATA=1 reads
## the shared folder directly (not recommended).
##
## Used by run_pipeline_demo.R, 01-05 (when run on their own) and
## WMed-EwEbuilder.qmd.
## =================================================================

.wmed_pcloud_name <- "EwE Western Med 2026"

## This file's own path, taken while it is being source()d
.wmed_self_file <- local({
  self <- NA_character_
  for (i in rev(seq_len(sys.nframe()))) {
    o <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(o) && grepl("wmed_paths\\.R$", o)) { self <- normalizePath(o); break }
  }
  self
})

## Repo root: the folder above scripts/ (this file lives in scripts/)
.wmed_repo_root <- function() {
  if (!is.na(.wmed_self_file)) return(dirname(dirname(.wmed_self_file)))
  for (d in c(getwd(), dirname(getwd()))) if (file.exists(file.path(d, "scripts", "wmed_paths.R"))) return(normalizePath(d))
  NA_character_
}

## Home folder. On some Windows set-ups R's "~" does not expand (the
## 2026-10-27 run wrote its outputs to C:/~/WMed_EwE_output), so on
## Windows fall back to the user's Documents folder (where R puts "~"
## normally), then the user profile.
.wmed_home <- function() {
  h <- gsub("\\\\", "/", path.expand("~"))
  if (.Platform$OS.type == "windows" && (grepl("~", h, fixed = TRUE) || !dir.exists(h))) {
    up <- Sys.getenv("USERPROFILE")
    for (cand in c(file.path(up, "Documents"), up))
      if (nzchar(up) && dir.exists(cand)) return(gsub("\\\\", "/", cand))
  }
  h
}
## Broken HOME on Windows (e.g. HOME set to "~"): repaired for this R
## session and everything it starts (the pipeline's Rscript, git, package
## caches), so no user has to change Windows settings. Windows' own
## default for HOME is the user profile (C:/Users/<name>). The Windows
## setting itself is not changed.
local({
  if (.Platform$OS.type != "windows") return(invisible())
  h <- Sys.getenv("HOME"); up <- Sys.getenv("USERPROFILE")
  if (nzchar(h) && (grepl("~", h, fixed = TRUE) || !dir.exists(h)) && nzchar(up) && dir.exists(up)) {
    Sys.setenv(HOME = gsub("\\\\", "/", up))
    message("[wmed_paths] HOME was '", h, "' (not a folder) - using ", Sys.getenv("HOME"), " for this R session.")
  }
})

## "~/x" -> <home>/x with .wmed_home(); other paths unchanged
.wmed_expand <- function(p) {
  p <- gsub("\\\\", "/", p)
  i <- !is.na(p) & grepl("^~(/|$)", p)
  p[i] <- paste0(.wmed_home(), substring(p[i], 2))
  p
}

## Values of out_dir / pcloud_dir written in one config file
.wmed_read_config <- function(f) {
  e <- new.env()
  ok <- tryCatch({ sys.source(f, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok) return(list(out_dir = NA_character_, pcloud_dir = NA_character_, local_data_dir = NA_character_))
  get1 <- function(n) if (exists(n, envir = e, inherits = FALSE)) as.character(get(n, envir = e))[1] else NA_character_
  list(out_dir = get1("out_dir"), pcloud_dir = get1("pcloud_dir"), local_data_dir = get1("local_data_dir"))
}

## --- Local copy of the shared data ----------------------------------------
.wmed_local_default <- function() file.path(.wmed_home(), "WMed_EwE_data", .wmed_pcloud_name)
## a usable copy: data/ with the FG table in it
wmed_local_data_ok <- function(d) !is.null(d) && !is.na(d) && file.exists(file.path(d, "data", "FG_WMed_2026.csv"))
## Copies <shared_dir>/data to <local_dir>/data: files missing locally,
## of another size, or newer in the shared folder. Read-only on the
## shared side. Local files that are not in the shared folder (caches
## written by the runs) are kept. Returns a one-row summary.
wmed_sync_local_data <- function(shared_dir, local_dir = .wmed_local_default(), subdirs = "data", progress = NULL) {
  if (is.na(shared_dir) || !dir.exists(shared_dir)) stop("Shared data folder not found: ", shared_dir)
  if (normalizePath(shared_dir, mustWork = FALSE) == normalizePath(local_dir, mustWork = FALSE)) stop("The local copy cannot be the shared folder itself.")
  n_new <- 0L; n_upd <- 0L; n_same <- 0L; bytes <- 0
  for (sd in subdirs) {
    src <- file.path(shared_dir, sd)
    if (!dir.exists(src)) next
    f <- list.files(src, recursive = TRUE, all.files = FALSE, no.. = TRUE)
    f <- f[!grepl("(^|/)(\\.DS_Store|~\\$|\\.~lock)", f)]
    si <- file.info(file.path(src, f), extra_cols = FALSE)
    keep <- !is.na(si$isdir) & !si$isdir
    f <- f[keep]; si <- si[keep, , drop = FALSE]
    dst <- file.path(local_dir, sd, f)
    di <- file.info(dst, extra_cols = FALSE)
    is_new <- is.na(di$size)
    is_upd <- !is_new & (di$size != si$size | si$mtime > di$mtime + 1)
    todo <- which(is_new | is_upd)
    for (k in seq_along(todo)) {
      i <- todo[k]
      dir.create(dirname(dst[i]), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(file.path(src, f[i]), dst[i], overwrite = TRUE, copy.date = TRUE)
      if (!isTRUE(ok)) warning("Not copied: ", f[i])
      if (is.function(progress) && (k %% 50 == 0 || k == length(todo))) progress(k, length(todo), f[i])
    }
    n_new <- n_new + sum(is_new); n_upd <- n_upd + sum(is_upd); n_same <- n_same + sum(!is_new & !is_upd)
    bytes <- bytes + sum(si$size[todo], na.rm = TRUE)
  }
  writeLines(c(paste("shared:", shared_dir), paste("synced:", format(Sys.time(), "%Y-%m-%d %H:%M"))),
             file.path(local_dir, "LOCAL_COPY_INFO.txt"))
  data.frame(new = n_new, updated = n_upd, unchanged = n_same, MB_copied = round(bytes / 1e6, 1), local_dir = local_dir)
}

wmed_find_paths <- function(git_dir = NULL) {
  if (is.null(git_dir) || is.na(git_dir)) git_dir <- .wmed_repo_root()
  git_dir <- sub("/+$", "", .wmed_expand(git_dir))
  cfg_files <- c(file.path(git_dir, "config.local.R"), file.path(git_dir, "scripts", "config.local.R"),
                 file.path(git_dir, "config.R"), file.path(git_dir, "scripts", "config.R"))
  cfg_files <- cfg_files[file.exists(cfg_files)]
  cfg <- lapply(cfg_files, .wmed_read_config)
  pick <- function(name, extra) {
    cand <- c(vapply(cfg, function(x) x[[name]], ""), extra)
    src <- c(basename(cfg_files), names(extra))
    keep <- !is.na(cand) & nzchar(cand)
    cand <- .wmed_expand(cand[keep]); src <- src[keep]
    hit <- which(dir.exists(cand))[1]
    if (is.na(hit)) list(path = NA_character_, from = NA_character_) else list(path = sub("/+$", "", cand[hit]), from = src[hit])
  }
  pc <- pick("pcloud_dir", c(env = Sys.getenv("WMED_PCLOUD_DIR"),
                             `pCloud Drive (macOS)` = file.path("~/pCloud Drive", .wmed_pcloud_name),
                             `pCloud Drive (Windows)` = file.path("P:", .wmed_pcloud_name),
                             `pCloud Drive (Linux)` = file.path("~/pCloudDrive", .wmed_pcloud_name),
                             `pCloud Drive (volume)` = file.path("/Volumes/pCloud Drive", .wmed_pcloud_name)))
  ## Local copy: used instead of the shared folder whenever it exists
  shared <- pc
  lc_cand <- c(vapply(cfg, function(x) x[["local_data_dir"]], ""), Sys.getenv("WMED_LOCAL_DATA_DIR"), .wmed_local_default())
  lc_cand <- .wmed_expand(lc_cand[!is.na(lc_cand) & nzchar(lc_cand)])
  local_dir <- sub("/+$", "", lc_cand[1])
  use_shared <- identical(Sys.getenv("WMED_USE_SHARED_DATA"), "1")
  if (!use_shared && wmed_local_data_ok(local_dir)) pc <- list(path = local_dir, from = "local copy of the shared data")
  od <- pick("out_dir", c(env = Sys.getenv("WMED_OUT_DIR")))
  if (is.na(od$path)) {
    od <- list(path = file.path(.wmed_home(), "WMed_EwE_output"), from = "default (~/WMed_EwE_output)")
    dir.create(od$path, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- !is.na(git_dir) && file.exists(file.path(git_dir, "scripts", "01_biomass.R")) && !is.na(pc$path) && dir.exists(od$path)
  list(git_dir = git_dir, pcloud_dir = pc$path, out_dir = od$path,
       pcloud_from = pc$from, out_from = od$from, ok = ok,
       shared_dir = shared$path, local_data_dir = local_dir,
       using_local_copy = identical(pc$path, local_dir), use_shared = use_shared,
       problem = if (ok) "" else paste0(
         if (is.na(git_dir) || !file.exists(file.path(git_dir, "scripts", "01_biomass.R"))) "WMed_EwE repo not found. " else "",
         if (is.na(pc$path)) paste0("pCloud folder '", .wmed_pcloud_name, "' not found (is pCloud Drive running?). ",
                                    "Set it in config.local.R or with the environment variable WMED_PCLOUD_DIR. ") else ""))
}

## Sets out_dir / pcloud_dir / git_dir in the global environment (only
## when all three are found). Returns the wmed_find_paths() result.
wmed_set_paths <- function(git_dir = NULL, quiet = FALSE) {
  p <- wmed_find_paths(git_dir)
  if (isTRUE(p$ok)) {
    assign("out_dir", p$out_dir, envir = .GlobalEnv)
    assign("pcloud_dir", p$pcloud_dir, envir = .GlobalEnv)
    assign("git_dir", p$git_dir, envir = .GlobalEnv)
    if (!quiet) message("[wmed_paths] git_dir = ", p$git_dir, "\n[wmed_paths] pcloud_dir = ", p$pcloud_dir,
                        "  (from ", p$pcloud_from, ")\n[wmed_paths] out_dir = ", p$out_dir, "  (from ", p$out_from, ")")
  } else if (!quiet) message("[wmed_paths] ", p$problem)
  invisible(p)
}

## Saves this computer's folders to config.local.R (git-ignored)
wmed_save_local_config <- function(git_dir, out_dir = NULL, pcloud_dir = NULL, local_data_dir = NULL) {
  f <- file.path(git_dir, "config.local.R")
  old <- if (file.exists(f)) .wmed_read_config(f) else list(out_dir = NA, pcloud_dir = NA, local_data_dir = NA)
  out_dir <- if (is.null(out_dir)) old$out_dir else out_dir
  pcloud_dir <- if (is.null(pcloud_dir)) old$pcloud_dir else pcloud_dir
  local_data_dir <- if (is.null(local_data_dir)) old$local_data_dir else local_data_dir
  if (!is.null(local_data_dir) && !is.na(local_data_dir)) local_data_dir <- gsub("\\\\", "/", local_data_dir)
  ## "/" in the saved paths: a Windows path with "\" inside "..." is not valid R
  if (!is.na(out_dir)) out_dir <- gsub("\\\\", "/", out_dir)
  if (!is.na(pcloud_dir)) pcloud_dir <- gsub("\\\\", "/", pcloud_dir)
  writeLines(c("## config.local.R - folders on THIS computer (not committed; see wmed_paths.R).",
               if (!is.na(out_dir)) sprintf('out_dir    <- "%s"', out_dir),
               if (!is.na(pcloud_dir)) sprintf('pcloud_dir <- "%s"', pcloud_dir),
               if (!is.null(local_data_dir) && !is.na(local_data_dir)) sprintf('local_data_dir <- "%s"', local_data_dir)), f)
  invisible(f)
}

## --- One output folder per set of inputs ------------------------------
## A run writes to out_dir only with the default inputs (GSAs 1-12, whole
## GSA area, Ecopath 1994-1996, Ecosim 1995-2023, FG_WMed_2026.csv). Any
## other area, years or FG table writes to its own folder,
##   out_dir/runs/<area>__EP<years>_TS<years>[__FG-<table>]
## e.g. runs/GSA6-7__EP1994-1996_TS1995-2023, runs/myBox__EP2000-2002_TS2000-2020,
## so runs with different inputs never overwrite each other; the same
## inputs again reuse (and overwrite) their folder. Downloads and caches
## (bathymetry, taxonomy, Copernicus, GSA shapefile) stay in out_dir and
## are shared by every run. Used by run_pipeline_demo.R and the page.
wmed_run_name <- function(area_mode = "westmed", gsas = 1:12, basis = "whole_gsa", area_name = NULL,
                          year_ecopath = 1994:1996, ts_years = 1995:2023, fg_file = NULL,
                          default_fg = "FG_WMed_2026.csv") {
  rng <- function(v) { v <- range(as.integer(v)); if (v[1] == v[2]) v[1] else paste(v, collapse = "-") }
  gsas <- sort(unique(as.integer(gsas)))
  custom <- identical(area_mode, "custom")
  fg <- if (is.null(fg_file) || is.na(fg_file) || !nzchar(fg_file)) default_fg else basename(fg_file)
  is_default <- !custom && identical(gsas, 1:12) && identical(basis, "whole_gsa") &&
    identical(as.integer(range(year_ecopath)), c(1994L, 1996L)) && identical(as.integer(range(ts_years)), c(1995L, 2023L)) &&
    identical(fg, default_fg)
  if (is_default) return("")
  area <- if (custom) gsub("[^A-Za-z0-9_-]", "_", if (is.null(area_name) || !nzchar(area_name)) "custom_region" else area_name) else {
    runs <- split(gsas, cumsum(c(1, diff(gsas) != 1)))
    paste0("GSA", paste(vapply(runs, function(r) if (length(r) > 2) paste0(r[1], "to", r[length(r)]) else paste(r, collapse = "-"), ""),
                        collapse = "-"), if (identical(basis, "medits_10_800")) "_medits10-800" else "")
  }
  years <- paste0("EP", rng(year_ecopath), "_TS", rng(ts_years))
  fgp <- if (!identical(fg, default_fg)) paste0("FG-", gsub("[^A-Za-z0-9_-]", "_", sub("\\.[A-Za-z]+$", "", fg))) else NULL
  paste(c(area, years, fgp), collapse = "__")
}
wmed_run_dir <- function(base_out_dir, run_name) {
  base_out_dir <- sub("/+$", "", base_out_dir)
  if (is.null(run_name) || !nzchar(run_name)) base_out_dir else file.path(base_out_dir, "runs", run_name)
}
