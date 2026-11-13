## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: 05c_habitat_responses.R
## =================================================================
## 05c_habitat_responses.R
## Created by: Daniel Vilas
##
## Environmental response functions of the functional groups (FGs) for
## the Ecospace habitat capacity model, in the form EwE keeps them: one
## function per FG and driver, 1200 values between X min and X max,
## scaled so that the highest value is 1 (relative habitat capacity).
## Two sources, written side by side:
##
##  1. AquaMaps (aquamaps.org; HSPEN table, R package aquamapsdata) -
##     each species' envelope Min / PrefMin / PrefMax / Max is a
##     trapezoid (0 below Min and above Max, 1 between PrefMin and
##     PrefMax, linear in between; Kaschner et al. 2006, Ready et al.
##     2010). FG curve = sum over the FG's species of
##     w_s x trapezoid_s(x), w_s = the species' share of the FG biomass
##     (prop_sp_fg, 01), rescaled to a maximum of 1. One species: the
##     trapezoid itself (EwE function type 9, parameters
##     "Min PrefMin PrefMax Max 0", as EwE's own AquaMaps import).
##     Drivers: depth (m), temperature (AquaMaps 'Layer' b = bottom ->
##     temp_bot, s = surface -> temp_surf; deg C), salinity (sal_surf,
##     PSU), primary production (npp; AquaMaps mg C m-2 d-1 / 1000 =
##     g C m-2 d-1, the unit of 05). Envelopes flagged as not used by
##     AquaMaps (…YN = 0) give no function.
##
##  2. GLOSSA (Mestre-Tomás et al. 2025, github.com/iMARES-group/glossa;
##     Bayesian Additive Regression Trees) - functional responses
##     (partial dependence, posterior mean probability of presence and
##     95% interval) of each FG's presence/absence in the MEDITS hauls
##     (haul_density.rds, 01) against the environment at the haul,
##     year by year. FG curve = the GLOSSA posterior mean, rescaled to a
##     maximum of 1, X range = the range of the driver in the hauls.
##     GLOSSA_MODE:
##       "auto"    (default) "run" when the glossa package is installed,
##                 else "prepare"; FGs that already have results in
##                 GLOSSA_RESULTS_DIR are not fitted again.
##       "prepare" (default) writes the GLOSSA inputs: one occurrence
##                 file per FG (decimalLongitude, decimalLatitude,
##                 timestamp = year, pa; tab-separated) and
##                 fit_layers.zip (one folder per driver, one GeoTIFF per
##                 year: mean of the survey months GLOSSA_MONTHS of the
##                 monthly maps of 05; depth from ECOSPACE_BATHY_FILE).
##                 Load them in the GLOSSA app (glossa::run_glossa()),
##                 tick "Functional responses", export the results (zip)
##                 into GLOSSA_RESULTS_DIR and rerun 05c with "read".
##       "run"     the same inputs, then glossa::glossa_analysis() with
##                 functional responses, here (minutes to hours: one BART
##                 model per FG), then "read".
##       "read"    reads GLOSSA's exported functional responses
##                 (<FG>_func_res_<driver>.tsv: value, mean, q25, q975)
##                 from GLOSSA_RESULTS_DIR (folders or zip files).
##
## OUTPUT (out_dir/environment/ecospace_responses/)
##   ecospace_response_functions.csv   one row per function, as EwE
##       stores it (EcosimShapeMediation): Title (#AM_<driver>_fg_<n> or
##       #GL_<driver>_fg_<n>), FunctionType (9 trapezoid, 0 drawn
##       points), XAxisMin, XAxisMax, FunctionParams, zScale (the 1200
##       values, space-separated, "." decimals), plus Source, FG, driver,
##       units and what it is based on
##   ewe_response_<source>_<driver>.csv  the same functions side by side
##       (one column per FG; rows Title, XAxisMin, XAxisMax, then the
##       1200 values) to paste into EwE's response function editor
##   ecospace_response_points.csv      120 points of every curve (and the
##       GLOSSA 95% interval) for plots and checks
##   ecospace_capacity_drivers.csv     FG x driver: the function to
##       assign in Ecospace > Habitat capacity (AquaMaps and GLOSSA)
##   ecospace_response_<driver>.png    curves of every FG, both sources
##   glossa_inputs/                    occurrences (FG<n>.tsv) and
##       fit_layers.zip for GLOSSA; glossa_fg_presences.csv (hauls,
##       presences per FG, used or not)
##
## References
##   Kaschner K, Watson R, Trites AW, Pauly D (2006) Mar Ecol Prog Ser
##     316: 285-310. Ready J et al. (2010) Ecol Model 221: 467-478.
##   Mestre-Tomás J, Fuster-Alonso A, Bellido JM, Coll M (2025) GLOSSA: a
##     user-friendly R Shiny application for Bayesian machine learning
##     analysis of marine species distribution. arXiv:2505.05862.
##   Christensen V et al. (2014) Representing variable habitat quality in
##     a spatial food web model. Ecosystems 17: 1397-1412 (habitat
##     capacity model, response functions).
## =================================================================

suppressPackageStartupMessages({ library(data.table); library(terra) })
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")

## --- Paths -------------------------------------------------------------
.this_dir_hr <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!exists("out_dir", envir = .GlobalEnv, inherits = FALSE) || !exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path(.this_dir_hr, "wmed_paths.R"))
  wmed_set_paths(git_dir = dirname(.this_dir_hr))
}
if (!exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) git_dir <- dirname(.this_dir_hr)

## --- Settings (set before sourcing to change) --------------------------
.hr_default <- function(name, value) if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) assign(name, value, envir = .GlobalEnv)
.hr_default("RESPONSE_SOURCES", c("aquamaps", "glossa"))                   # one or both
.hr_default("RESPONSE_DRIVERS", c("depth", "temp_bot", "temp_surf", "sal_surf", "npp"))
.hr_default("RESPONSE_N_POINTS", 1200L)                                    # EwE response functions have 1200 values
.hr_default("GLOSSA_MODE", "auto")                                         # "auto", "prepare", "run" or "read"
.hr_default("GLOSSA_FGS", NULL)                                            # NULL = every FG with enough hauls; or FG numbers
.hr_default("GLOSSA_MONTHS", 5:7)                                          # months averaged for the layers (MEDITS: May-July)
.hr_default("GLOSSA_YEARS", NULL)                                          # NULL = every survey year with environmental maps
.hr_default("GLOSSA_MIN_PRESENCES", 30L)                                   # fewer hauls with the FG: no GLOSSA model
.hr_default("GLOSSA_MODEL_ARGS", list())                                   # passed to dbarts::bart (e.g. list(ntree = 200))
.hr_default("GLOSSA_SEED", 1L)
.hr_default("ECOSPACE_BATHY_FILE", file.path(pcloud_dir, "data", "Bathymetry", "EMODnet_WMed_1arcmin.tif"))
.hr_default("ECOSPACE_ENV_DIR", file.path(out_dir, "environment"))
resp_dir <- file.path(ECOSPACE_ENV_DIR, "ecospace_responses")
.hr_default("GLOSSA_RESULTS_DIR", file.path(resp_dir, "glossa_results"))
dir.create(resp_dir, recursive = TRUE, showWarnings = FALSE)

.hr_units <- c(depth = "m", temp_bot = "deg C", temp_surf = "deg C", sal_surf = "PSU", npp = "g C m-2 d-1", phyto_c = "g C m-2")
.hr_msg <- function(...) message("[05c] ", ...)
NPT <- as.integer(RESPONSE_N_POINTS)

## --- FGs and their species (01) --------------------------------------------
.find_out <- function(block, file) {                          # <block>/ or <block>/intermediate/ (lib_output_tidy.R)
  roots <- c(out_dir, if (exists("OUT_BASE_DIR", envir = .GlobalEnv)) OUT_BASE_DIR)
  cand <- c(file.path(roots, block, file), file.path(roots, block, "intermediate", file))
  cand[file.exists(cand)][1]
}
.bp <- .find_out("biomass", "biomass_proportion_by_species_fg.csv")
if (is.na(.bp)) stop("[05c] biomass/biomass_proportion_by_species_fg.csv not found - run 01 (biomass) first.", call. = FALSE)
fg_sp <- fread(.bp)[, .(FG_num = as.integer(FG_num), FG_name, Species = trimws(Species), w = suppressWarnings(as.numeric(prop_sp_fg)))]
fg_sp <- fg_sp[!is.na(FG_num) & !is.na(Species) & nzchar(Species)]
fg_sp[!is.finite(w) | w < 0, w := 0]
fgs <- unique(fg_sp[, .(FG_num, FG_name)])[order(FG_num)]

## --- one function on the EwE grid ----------------------------------------
## x0/y0: the curve; returns the 1200 values between xmin and xmax
## (linear interpolation, ends held), scaled to a maximum of 1
.on_grid <- function(x0, y0, xmin, xmax) {
  o <- order(x0); x0 <- x0[o]; y0 <- y0[o]
  xs <- seq(xmin, xmax, length.out = NPT)
  y <- if (length(unique(x0)) > 1) stats::approx(x0, y0, xout = xs, rule = 2, ties = mean)$y else rep(y0[1], NPT)
  y[!is.finite(y) | y < 0] <- 0
  if (max(y) > 0) y <- y / max(y)
  y
}
.trap <- function(x, mn, pmn, pmx, mx) {
  y <- numeric(length(x))
  y[x >= pmn & x <= pmx] <- 1
  up <- x > mn & x < pmn; if (pmn > mn) y[up] <- (x[up] - mn) / (pmn - mn)
  dn <- x > pmx & x < mx; if (mx > pmx) y[dn] <- (mx - x[dn]) / (mx - pmx)
  y
}
funs <- list()        # one row per function
pts <- list()         # 120 points per function (plots)
.add_fun <- function(source, fg, driver, ftype, xmin, xmax, params, y, basis, lo = NULL, hi = NULL) {
  tag <- if (source == "AquaMaps") "#AM_" else "#GL_"
  title <- paste0(tag, driver, "_fg_", fg$FG_num)
  funs[[length(funs) + 1]] <<- data.table(Title = title, Source = source, FG_num = fg$FG_num, FG_name = fg$FG_name, Driver = driver,
                                          Units = unname(.hr_units[driver]), FunctionType = ftype, XAxisMin = signif(xmin, 6), XAxisMax = signif(xmax, 6),
                                          FunctionParams = params, Basis = basis,
                                          zScale = paste(sprintf("%.7g", y), collapse = " "))
  k <- unique(round(seq(1, NPT, length.out = 120)))
  xs <- seq(xmin, xmax, length.out = NPT)[k]
  pts[[length(pts) + 1]] <<- data.table(Title = title, Source = source, FG_num = fg$FG_num, FG_name = fg$FG_name, Driver = driver,
                                        x = signif(xs, 6), y = round(y[k], 5),
                                        y_q2.5 = if (is.null(lo)) NA_real_ else round(lo[k], 5), y_q97.5 = if (is.null(hi)) NA_real_ else round(hi[k], 5))
}

## =================================================================
## 1. AquaMaps
## =================================================================
if ("aquamaps" %in% RESPONSE_SOURCES) tryCatch({
  if (!exists("resolve_aquamaps_species_ids", mode = "function")) source(file.path(git_dir, "scripts", "lib_aquamaps_depth_extension.R"))
  ## names and envelopes already in the shared cache are enough; the AquaMaps
  ## database (aquamapsdata, ~2 GB once) is only opened for new species,
  ## and when it cannot be, the cached species are used
  ids <- tryCatch(resolve_aquamaps_species_ids(unique(fg_sp$Species)), error = function(e) {
    .hr_msg("AquaMaps database not available (", conditionMessage(e), ") - species in the shared cache only.")
    x <- .am_cache_read("aquamaps_species_ids.csv", c("ScientificName", "SpeciesID", "match_method"))
    if (is.null(x)) stop("no AquaMaps cache either")
    x[ScientificName %in% fg_sp$Species & !is.na(SpeciesID) & nzchar(SpeciesID)]
  })
  ids_long <- ids[!is.na(SpeciesID), .(SpeciesID = strsplit(SpeciesID, "|", fixed = TRUE)[[1]]), by = .(ScientificName, match_method)]
  ## all HSPEN envelopes of these IDs (shared cache, database only for new IDs)
  env_cols <- c("Layer", paste0(rep(c("Depth", "Temp", "Salinity", "PrimProd"), each = 5), c("YN", "Min", "PrefMin", "PrefMax", "Max")))
  hc <- .am_cache_read("aquamaps_envelopes_all.csv", c("SpeciesID", "Layer"))
  if (is.null(hc)) hc <- data.table(SpeciesID = character(0))
  new_ids <- setdiff(unique(ids_long$SpeciesID), hc$SpeciesID)
  if (length(new_ids)) tryCatch({
    .am_need_db()
    hs <- aquamapsdata::am_hspen()
    got <- as.data.table(dplyr::collect(dplyr::filter(hs, SpeciesID %in% new_ids)))
    keep <- intersect(c("SpeciesID", env_cols), names(got))
    got <- got[, ..keep][, SpeciesID := as.character(SpeciesID)]
    got <- rbind(got, data.table(SpeciesID = setdiff(new_ids, got$SpeciesID)), fill = TRUE)
    hc <- unique(rbind(hc, got, fill = TRUE), by = "SpeciesID")
    .am_cache_write(hc, "aquamaps_envelopes_all.csv")
  }, error = function(e) .hr_msg("AquaMaps envelopes of ", length(new_ids), " species not in the cache and the database not available (",
                                 conditionMessage(e), ") - left out."))
  for (cc in setdiff(env_cols, names(hc))) hc[, (cc) := NA]
  env <- merge(ids_long, hc, by = "SpeciesID")
  ## genus entries (several IDs): median envelope per name
  num_cols <- setdiff(env_cols, "Layer")
  env[, (num_cols) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = num_cols]
  env <- env[, c(lapply(.SD, function(v) as.numeric(stats::median(v, na.rm = TRUE))),
                 list(Layer = names(sort(table(tolower(Layer)), decreasing = TRUE))[1])),
             by = .(Species = ScientificName), .SDcols = num_cols]
  for (cc in num_cols) env[!is.finite(get(cc)), (cc) := NA_real_]
  fwrite(env, file.path(resp_dir, "aquamaps_envelopes_by_species.csv"))
  .hr_msg("AquaMaps envelopes for ", nrow(env), " of ", uniqueN(fg_sp$Species), " species.")
  am_map <- list(depth = "Depth", temp = "Temp", sal_surf = "Salinity", npp = "PrimProd")
  for (i in seq_len(nrow(fgs))) {
    fg <- fgs[i]
    s <- merge(fg_sp[FG_num == fg$FG_num], env, by = "Species")
    if (!nrow(s)) next
    for (drv in intersect(RESPONSE_DRIVERS, c("depth", "temp_bot", "temp_surf", "sal_surf", "npp"))) {
      key <- if (drv %in% c("temp_bot", "temp_surf")) "temp" else drv
      p <- am_map[[key]]
      d <- s[, .(Species, w, Layer, YN = get(paste0(p, "YN")), mn = get(paste0(p, "Min")), pmn = get(paste0(p, "PrefMin")),
                 pmx = get(paste0(p, "PrefMax")), mx = get(paste0(p, "Max")))]
      d <- d[is.finite(mn) & is.finite(pmn) & is.finite(pmx) & is.finite(mx) & mx > mn & !(YN %in% 0)]
      if (key == "temp") d <- d[if (drv == "temp_bot") tolower(Layer) == "b" else tolower(Layer) != "b"]
      if (!nrow(d)) next
      if (drv == "npp") d[, c("mn", "pmn", "pmx", "mx") := lapply(.SD, function(v) v / 1000), .SDcols = c("mn", "pmn", "pmx", "mx")]
      if (sum(d$w) <= 0) d[, w := 1]
      d[, w := w / sum(w)]
      xmin <- min(d$mn); xmax <- max(d$mx); xs <- seq(xmin, xmax, length.out = NPT)
      y <- Reduce(`+`, lapply(seq_len(nrow(d)), function(j) d$w[j] * .trap(xs, d$mn[j], d$pmn[j], d$pmx[j], d$mx[j])))
      if (max(y) > 0) y <- y / max(y)
      one <- nrow(d) == 1
      .add_fun("AquaMaps", fg, drv, if (one) 9L else 0L, xmin, xmax,
               if (one) paste(c(signif(c(d$mn, d$pmn, d$pmx, d$mx), 6), 0), collapse = " ") else "-9999", y,
               if (one) paste0("AquaMaps envelope of ", d$Species) else
                 paste0(nrow(d), " species, weighted by their share of the FG biomass (", paste0(head(d[order(-w)]$Species, 3), collapse = ", "),
                        if (nrow(d) > 3) ", ..." else "", ")"))
    }
  }
  .hr_msg("AquaMaps: ", sum(vapply(funs, function(f) f$Source == "AquaMaps", logical(1))), " response functions.")
}, error = function(e) .hr_msg("AquaMaps response functions not made: ", conditionMessage(e)))

## =================================================================
## 2. GLOSSA
## =================================================================
gl_dir <- file.path(resp_dir, "glossa_inputs")
.glossa_prepare <- function() {
  hd_f <- .find_out("biomass", "haul_density.rds")
  if (is.na(hd_f)) stop("biomass/haul_density.rds not found - run 01 (biomass) first.")
  hd <- readRDS(hd_f)
  hauls <- as.data.table(hd$hauls)[is.finite(Lat) & is.finite(Lon)]
  sp <- as.data.table(hd$sp)
  ## environmental layers: mean of GLOSSA_MONTHS per year, every driver
  env_dirs <- unique(c(ECOSPACE_ENV_DIR, if (exists("OUT_BASE_DIR", envir = .GlobalEnv)) file.path(OUT_BASE_DIR, "environment")))
  native <- function(v) { f <- file.path(env_dirs, paste0(v, "_monthly_native.tif")); f[file.exists(f)][1] }
  vars <- setdiff(RESPONSE_DRIVERS, "depth"); vars <- vars[!is.na(vapply(vars, native, ""))]
  if (!length(vars)) stop("no monthly maps of 05 (", paste(setdiff(RESPONSE_DRIVERS, "depth"), collapse = ", "), ") - run 05 (environment) first.")
  rs <- lapply(setNames(vars, vars), function(v) terra::rast(native(v)))
  yrs_env <- Reduce(intersect, lapply(rs, function(r) unique(as.integer(substr(names(r), 1, 4)))))
  yrs <- sort(intersect(unique(hauls$Year), yrs_env))
  if (!is.null(GLOSSA_YEARS)) yrs <- intersect(yrs, GLOSSA_YEARS)
  if (!length(yrs)) stop("no survey year with environmental maps.")
  yrs <- min(yrs):max(yrs)                                    # GLOSSA: one layer per year, consecutive
  yrs <- yrs[yrs %in% yrs_env]
  if (length(yrs) != (max(yrs) - min(yrs) + 1)) { yrs <- yrs[seq_len(which(diff(c(yrs, Inf)) != 1)[1])]
    .hr_msg("GLOSSA needs consecutive years: using ", min(yrs), "-", max(yrs), ".") }
  lay_dir <- file.path(gl_dir, "fit_layers"); unlink(lay_dir, recursive = TRUE); dir.create(lay_dir, recursive = TRUE)
  tmpl <- rs[[1]][[1]]
  for (v in vars) {
    dir.create(file.path(lay_dir, v), showWarnings = FALSE)
    for (k in seq_along(yrs)) {
      nm <- names(rs[[v]]); sel <- which(as.integer(substr(nm, 1, 4)) == yrs[k] & as.integer(substr(nm, 6, 7)) %in% GLOSSA_MONTHS)
      if (!length(sel)) sel <- which(as.integer(substr(nm, 1, 4)) == yrs[k])
      m <- terra::mean(rs[[v]][[sel]], na.rm = TRUE)
      if (!terra::compareGeom(m, tmpl, stopOnError = FALSE)) m <- terra::resample(m, tmpl)
      names(m) <- v
      terra::writeRaster(m, file.path(lay_dir, v, sprintf("%s_%02d.tif", v, k)), overwrite = TRUE)
    }
  }
  if ("depth" %in% RESPONSE_DRIVERS) {
    if (!file.exists(ECOSPACE_BATHY_FILE)) stop("bathymetry not found: ", ECOSPACE_BATHY_FILE)
    b <- terra::rast(ECOSPACE_BATHY_FILE); if (is.na(terra::crs(b)) || !nzchar(terra::crs(b))) terra::crs(b) <- "EPSG:4326"
    b <- terra::ifel(b < 0, -b, NA)                            # depth in m (bathymetry negative below sea level)
    dpt <- terra::resample(b, tmpl, method = "average"); dpt <- terra::mask(dpt, tmpl); names(dpt) <- "depth"
    dir.create(file.path(lay_dir, "depth"), showWarnings = FALSE)
    for (k in seq_along(yrs)) terra::writeRaster(dpt, file.path(lay_dir, "depth", sprintf("depth_%02d.tif", k)), overwrite = TRUE)
    vars <- c("depth", vars)
  }
  zf <- file.path(gl_dir, "fit_layers.zip"); unlink(zf)
  old <- setwd(lay_dir); on.exit(setwd(old), add = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zipr(zf, files = vars) else utils::zip(zf, files = vars, flags = "-r9Xq")
  setwd(old)
  unlink(lay_dir, recursive = TRUE)                          # the zip is what GLOSSA reads
  ## occurrences: every haul of those years; pa = 1 when the FG was caught
  hauls <- hauls[Year %in% yrs]
  pres <- unique(sp[SampleID %in% hauls$SampleID & d > 0, .(SampleID, FG_num)])
  occ_dir <- file.path(gl_dir, "occurrences"); unlink(occ_dir, recursive = TRUE); dir.create(occ_dir, recursive = TRUE)
  summ <- list(); pa_list <- list()
  for (i in seq_len(nrow(fgs))) {
    f <- fgs$FG_num[i]
    x <- hauls[, .(decimalLongitude = Lon, decimalLatitude = Lat, timestamp = Year, pa = as.integer(SampleID %in% pres[FG_num == f, SampleID]))]
    np <- sum(x$pa)
    use <- np >= GLOSSA_MIN_PRESENCES && np < nrow(x)
    summ[[i]] <- data.table(FG_num = f, FG_name = fgs$FG_name[i], hauls = nrow(x), presences = np, used = use,
                            why_not = if (use) "" else if (np == nrow(x)) "present in every haul" else paste0("fewer than ", GLOSSA_MIN_PRESENCES, " hauls with the FG"))
    if (!use) next
    lab <- paste0("FG", f)
    fwrite(x, file.path(occ_dir, paste0(lab, ".tsv")), sep = "\t", dec = ".", quote = FALSE, bom = FALSE)
    pa_list[[lab]] <- as.data.frame(x[, .(decimalLongitude, decimalLatitude, timestamp = timestamp - min(yrs) + 1L, pa, timestamp_original = timestamp)])
  }
  summ <- rbindlist(summ)
  fwrite(summ, file.path(gl_dir, "glossa_fg_presences.csv"))
  writeLines(c("GLOSSA inputs written by 05c_habitat_responses.R",
               paste0("Years: ", min(yrs), "-", max(yrs), " (timestamp = year; layer k = year ", min(yrs), " + k - 1)"),
               paste0("Layers: mean of months ", paste(GLOSSA_MONTHS, collapse = ","), " of the 05 monthly maps; depth from ", basename(ECOSPACE_BATHY_FILE)),
               paste0("Drivers: ", paste(vars, collapse = ", ")),
               "In the GLOSSA app (glossa::run_glossa()): occurrences = occurrences/FG<n>.tsv, fit layers = fit_layers.zip;",
               "tick Functional responses, run, then Exports: functional responses -> save the zip in glossa_results/ and run 05c with GLOSSA_MODE = \"read\"."),
             file.path(gl_dir, "README.txt"))
  .hr_msg("GLOSSA inputs: ", summ[used == TRUE, .N], " FG occurrence files (of ", nrow(summ), " FGs; glossa_fg_presences.csv), ",
          length(yrs), " years x ", length(vars), " drivers in fit_layers.zip -> ", gl_dir)
  list(pa = pa_list, zip = zf, vars = vars)
}

## functional responses -> list(FG label -> list(driver -> data.frame(value, mean, q25, q975)))
.glossa_read <- function(dir) {
  if (!dir.exists(dir)) return(list())
  zips <- list.files(dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  ex <- file.path(tempdir(), "glossa_read"); unlink(ex, recursive = TRUE); dir.create(ex)
  for (z in zips) tryCatch(utils::unzip(z, exdir = file.path(ex, basename(z))), error = function(e) NULL)
  fs <- c(list.files(dir, pattern = "_func_res_.*\\.tsv$", recursive = TRUE, full.names = TRUE),
          list.files(ex, pattern = "_func_res_.*\\.tsv$", recursive = TRUE, full.names = TRUE))
  out <- list()
  for (f in fs) {
    b <- sub("\\.tsv$", "", basename(f)); lab <- sub("_func_res_.*$", "", b); drv <- sub("^.*_func_res_", "", b)
    d <- tryCatch(utils::read.delim(f, dec = "."), error = function(e) NULL)
    if (is.null(d) || !all(c("value", "mean") %in% names(d))) next
    out[[lab]][[drv]] <- d
  }
  out
}

fr <- list()
if ("glossa" %in% RESPONSE_SOURCES) tryCatch({
  mode <- GLOSSA_MODE
  if (identical(mode, "auto")) {
    mode <- if (requireNamespace("glossa", quietly = TRUE)) "run" else "prepare"
    if (mode == "prepare") .hr_msg("GLOSSA: the glossa package is not installed (install.packages(\"glossa\")) - only its inputs are written.")
  }
  if (mode %in% c("prepare", "run")) {
    gi <- .glossa_prepare()
    if (mode == "run") {
      if (!requireNamespace("glossa", quietly = TRUE)) {
        .hr_msg("GLOSSA_MODE = \"run\" needs the glossa package (install.packages(\"glossa\")) - only the inputs were written.")
      } else if (length(gi$pa)) {
        ## one FG at a time, results saved as soon as each is done: a stopped
        ## run goes on from where it was, and one failing FG does not stop the rest
        dir.create(GLOSSA_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
        done <- names(.glossa_read(GLOSSA_RESULTS_DIR))
        todo <- setdiff(names(gi$pa), done)
        if (!is.null(GLOSSA_FGS)) todo <- todo[as.integer(sub("^FG", "", todo)) %in% GLOSSA_FGS]
        .hr_msg("GLOSSA (BART): ", length(todo), " FG(s) to fit, ", length(intersect(done, names(gi$pa))), " already done",
                if (length(todo)) " - several minutes per FG." else ".")
        for (k in seq_along(todo)) {
          lab <- todo[k]; t0 <- Sys.time()
          res <- tryCatch(glossa::glossa_analysis(pa_data = gi$pa[lab], fit_layers = gi$zip, predictor_variables = NULL, scale_layers = TRUE,
                                                  other_analysis = "functional_responses", model_args = GLOSSA_MODEL_ARGS, seed = GLOSSA_SEED),
                          error = function(e) { .hr_msg("GLOSSA ", lab, " failed: ", conditionMessage(e)); NULL })
          rc <- if (!is.null(res)) res$other_results$response_curve[[lab]] else NULL
          for (drv in names(rc))                                           # same files as GLOSSA's export
            utils::write.table(rc[[drv]], file.path(GLOSSA_RESULTS_DIR, paste0(lab, "_func_res_", drv, ".tsv")),
                               sep = "\t", dec = ".", quote = FALSE, row.names = FALSE)
          .hr_msg("GLOSSA ", k, "/", length(todo), " ", lab, " ", fgs[FG_num == as.integer(sub("^FG", "", lab)), FG_name], ": ",
                  if (length(rc)) "done" else "no response curves", " (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
        }
      }
    }
  }
  if (!length(fr)) fr <- .glossa_read(GLOSSA_RESULTS_DIR)
  if (!length(fr)) {
    .hr_msg("GLOSSA: no functional responses yet in ", GLOSSA_RESULTS_DIR,
            " - install the glossa package and run 05c again (or run GLOSSA on glossa_inputs/ and export the functional responses there).")
  } else {
    n0 <- length(funs)
    for (lab in names(fr)) {
      f <- suppressWarnings(as.integer(sub("^FG0*([0-9]+).*$", "\\1", lab)))
      fg <- fgs[FG_num == f]
      if (!nrow(fg)) { .hr_msg("GLOSSA result '", lab, "' does not name an FG of the model (FG<number>) - skipped."); next }
      for (drv in names(fr[[lab]])) {
        d <- as.data.frame(fr[[lab]][[drv]])
        v <- suppressWarnings(as.numeric(d$value)); if (sum(is.finite(v)) < 2) next   # categorical drivers: not a curve
        ok <- is.finite(v) & is.finite(d$mean); v <- v[ok]; d <- d[ok, , drop = FALSE]
        xmin <- min(v); xmax <- max(v)
        mx <- max(d$mean)
        y <- .on_grid(v, d$mean, xmin, xmax)
        lo <- if ("q25" %in% names(d)) .on_grid(v, d$q25, xmin, xmax) * max(d$q25) / mx else NULL
        hi <- if ("q975" %in% names(d)) .on_grid(v, d$q975, xmin, xmax) * max(d$q975) / mx else NULL
        .add_fun("GLOSSA", fg, drv, 0L, xmin, xmax, "-9999", y,
                 paste0("GLOSSA BART functional response (posterior mean probability of presence, maximum ", signif(mx, 3), " -> 1)"), lo, hi)
      }
    }
    .hr_msg("GLOSSA: ", length(funs) - n0, " response functions from ", length(fr), " FGs.")
  }
}, error = function(e) .hr_msg("GLOSSA step failed: ", conditionMessage(e)))

## =================================================================
## 3. Write
## =================================================================
if (!length(funs)) {
  .hr_msg("No response function made.")
} else {
  F <- rbindlist(funs)[order(Driver, FG_num, Source)]
  P <- rbindlist(pts)
  fwrite(F, file.path(resp_dir, "ecospace_response_functions.csv"))
  fwrite(P, file.path(resp_dir, "ecospace_response_points.csv"))
  cap <- dcast(F, FG_num + FG_name + Driver ~ Source, value.var = "Title")
  setnames(cap, intersect(c("AquaMaps", "GLOSSA"), names(cap)), paste0("function_", tolower(intersect(c("AquaMaps", "GLOSSA"), names(cap)))))
  fwrite(cap[order(FG_num, Driver)], file.path(resp_dir, "ecospace_capacity_drivers.csv"))
  ## side by side, per source and driver: rows Title, XAxisMin, XAxisMax, 1200 values
  for (s in unique(F$Source)) for (drv in unique(F[Source == s, Driver])) {
    x <- F[Source == s & Driver == drv]
    m <- matrix(vapply(strsplit(x$zScale, " ", fixed = TRUE), as.numeric, numeric(NPT)), nrow = NPT)
    dec <- if (isTRUE(getOption("wmed.csv_euro", TRUE))) "," else "."
    num <- function(v) sub(".", dec, format(v, digits = 7, trim = TRUE, scientific = FALSE, drop0trailing = TRUE), fixed = TRUE)
    w <- rbind(as.data.table(t(c("Title", x$Title))), as.data.table(t(c("FG", paste0(x$FG_num, " ", x$FG_name)))),
               as.data.table(t(c("XAxisMin", num(x$XAxisMin)))), as.data.table(t(c("XAxisMax", num(x$XAxisMax)))),
               as.data.table(cbind(as.character(seq_len(NPT)), apply(m, 2, function(v) vapply(v, num, "")))), use.names = FALSE)
    fwrite(w, file.path(resp_dir, paste0("ewe_response_", tolower(s), "_", drv, ".csv")), col.names = FALSE)
  }
  ## plots: one page per driver, every FG, both sources
  if (requireNamespace("ggplot2", quietly = TRUE)) for (drv in unique(P$Driver)) tryCatch({
    p <- P[Driver == drv]; p[, lab := paste0(FG_num, " ", FG_name)]
    p[, lab := factor(lab, levels = unique(lab[order(FG_num)]))]
    g <- ggplot2::ggplot(p, ggplot2::aes(x, y, colour = Source, fill = Source)) +
      ggplot2::geom_ribbon(data = p[is.finite(y_q2.5)], ggplot2::aes(ymin = pmin(y_q2.5, 1.5), ymax = pmin(y_q97.5, 1.5)), alpha = 0.2, colour = NA) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::facet_wrap(~ lab, scales = "free_x", ncol = 6) +
      ggplot2::scale_colour_manual(values = c(AquaMaps = "#1f77b4", GLOSSA = "#d62728")) +
      ggplot2::scale_fill_manual(values = c(AquaMaps = "#1f77b4", GLOSSA = "#d62728")) +
      ggplot2::labs(x = paste0(drv, " (", .hr_units[drv], ")"), y = "Relative habitat capacity",
                    title = paste0("Ecospace response functions: ", drv)) +
      ggplot2::theme_bw(base_size = 14) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(size = 9))
    nr <- ceiling(uniqueN(p$lab) / 6)
    ggplot2::ggsave(file.path(resp_dir, paste0("ecospace_response_", drv, ".png")), g, width = 18, height = 2.4 * nr + 1.5, dpi = 150, limitsize = FALSE)
  }, error = function(e) .hr_msg("plot ", drv, ": ", conditionMessage(e)))
  .hr_msg(nrow(F), " response functions (", paste0(F[, .N, by = Source][, paste(Source, N)], collapse = ", "), ") for ",
          uniqueN(F$FG_num), " FGs -> ", resp_dir)
}
