## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: 06_environment.R
## =================================================================
## 06_environment.R
## Created by: Daniel Vilas
##
## Environmental drivers from Copernicus Marine, for Ecosim (time series),
## Ecospace (monthly maps) and the Ecopath phytoplankton inputs (B, P/B).
## Runs on its own (source("06_environment.R") from the scripts/ folder);
## it does not need 01-05. Outputs in out_dir/environment/.
##
## WHAT IT MAKES
##  Monthly, for the run's years (TS_YEARS, plus the Ecopath years):
##   temp_surf  sea surface temperature, deg C   MedPHY reanalysis, thetao, first level (~1 m)
##   sal_surf   sea surface salinity             MedPHY reanalysis, so, first level
##   temp_bot   sea bottom temperature, deg C    MedPHY reanalysis, bottomT
##   npp        net primary production, g C m-2 d-1, integrated over the euphotic zone
##   phyto_c    phytoplankton carbon, g C m-2, integrated over the euphotic zone
##  Euphotic zone (ENV_INTEGRATION = "euphotic", default): per cell and month,
##  the depth above which ENV_EU_FRACTION (99%) of the 0-ENV_INTEGRATION_DEPTH m
##  NPP happens, from MedBFM's own NPP profile (MedBFM publishes no euphotic
##  depth). NPP = that 99%; phytoplankton carbon is summed down to the same
##  depth, so non-producing biomass below it (sinking cells) is left out.
##  ENV_INTEGRATION = "fixed" integrates both over 0-ENV_INTEGRATION_DEPTH m.
##  The PISCES gap fill keeps 0-ENV_INTEGRATION_DEPTH m: only its
##  year-to-year ratio is used, the level comes from MedBFM (see below).
##  For each: one GeoTIFF per variable on the native 1/24 deg grid (one
##  layer per month; 06b_ecospace_drivers.R turns them into monthly ASCII
##  maps on any Ecospace base map), and the monthly and annual means over the model
##  area (area-weighted) with the value relative to 1995 (Ecosim forcing).
##  Ecopath: phytoplankton B (1995) and P/B (NPP 1994-1995 / B 1995) per
##  group (Large / Small phytoplankton).
##
## SOURCES (Copernicus Marine Service; free account needed)
##  - Physics: MEDSEA_MULTIYEAR_PHY_006_004 (Med reanalysis, 1/24 deg,
##    from 1987), datasets cmems_mod_med_phy-temp_my_4.2km_P1M-m (thetao,
##    bottomT) and cmems_mod_med_phy-sal_my_4.2km_P1M-m (so); the most
##    recent months from the "myint" (interim) datasets.
##  - Biogeochemistry: MEDSEA_MULTIYEAR_BGC_006_008 (MedBFM, 1/24 deg,
##    from 1999), cmems_mod_med_bgc-bio_my_4.2km_P1M-m (nppv, mg C m-3 d-1)
##    and cmems_mod_med_bgc-plankton_my_4.2km_P1M-m (phyc, mmol C m-3).
##  - 1994-1998 (before MedBFM starts): GLOBAL_MULTIYEAR_BGC_001_029
##    (PISCES global hindcast, 1/4 deg, from 1993),
##    cmems_mod_glo_bgc_my_0.25deg_P1M-m (nppv, phyc), SCALED TO MedBFM:
##      X_gap(y, m, cell) = MedBFM_clim(m, cell) x PISCES(y, m, cell) / PISCES_clim(m, cell)
##    where m = calendar month, the climatologies are the means over
##    ENV_OVERLAP_YEARS (1999-2010) and PISCES is resampled to the MedBFM
##    grid. So the level and seasonal cycle are MedBFM's; only the
##    year-to-year change comes from PISCES. The ratio is kept within
##    [1/ENV_GAP_RATIO_MAX, ENV_GAP_RATIO_MAX] (4) so a near-zero PISCES
##    climatology cannot blow up a cell. These months are marked
##    source = "PISCES scaled to MedBFM" in the tables.
##  - Large / Small split: satellite phytoplankton size classes (ocean
##    colour, chlorophyll of micro-, nano- and picophytoplankton; Med
##    product first, then the global one), mean of ENV_PFT_YEARS
##    (1998-2000, the first full years; the record starts Sept 1997):
##      share_large = MICRO / (MICRO + NANO + PICO), share_small = 1 - share_large
##    ASSUMPTION (flagged): the chlorophyll share is used as the carbon
##    share, and both groups get the same P/B (no size-resolved NPP in
##    these products).
##
## ECOPATH VALUES
##   B (t WW km-2)  = phyto_c(ENV_ECOPATH_B_YEAR) x PHYTO_C_TO_WW x share
##   P/B (yr-1)     = NPP_annual(ENV_ECOPATH_PB_YEARS) / phyto_c(ENV_ECOPATH_B_YEAR)
##   NPP_annual (g C m-2 yr-1) = sum over months of npp (g C m-2 d-1) x days in the month
##   phyto_c (g C m-2) = annual mean of the monthly values
##   all area-weighted (cos latitude) means over the model area
##   (ENV_DOMAIN: "area" = the run's model area, i.e. the GSAs of
##   MODEL_GSAS or the custom region (CUSTOM_BBOX / CUSTOM_SHAPEFILE_PATH);
##   "ecospace" = the cells of ECOSPACE_DEPTH_FILE with depth > 0).
##   Years: TS_YEARS (maps and series), YEAR_ECOPATH (B = its middle year,
##   P/B = NPP of its years up to that one), as set on the page.
##   PHYTO_C_TO_WW = 9 (1 g C = 9 g wet weight, Pauly & Christensen 1995).
##
## CHECKS before trusting the numbers (all written to the log):
##   units of every variable as the NetCDF files declare them, the depth
##   levels used in the integration, and the gap-fill ratios
##   (environment/diagnostics/).
##
## Downloads are cached per variable and year (environment/cache/), so a
## rerun only fetches new years. The 3-D biogeochemistry (0-200 m) is the
## big part: about 0.2-0.5 GB per variable and year, deleted after the
## year is integrated.
## =================================================================

suppressPackageStartupMessages({
  library(data.table); library(terra); library(ncdf4); library(sf)
})
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")   # no .aux.xml next to every map

## --- Paths -------------------------------------------------------------
.this_dir_env <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!exists("out_dir", envir = .GlobalEnv, inherits = FALSE) || !exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path(.this_dir_env, "wmed_paths.R"))
  wmed_set_paths(git_dir = dirname(.this_dir_env))
}
if (!exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) git_dir <- dirname(.this_dir_env)
source(file.path(git_dir, "scripts", "lib_cmems_phytoplankton_biomass.R"))   # copernicusmarine CLI + login helpers

## --- Settings (set any of these before sourcing to change them) --------
.env_default <- function(name, value) if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) assign(name, value, envir = .GlobalEnv)
## Years and area follow the run's own settings (the page's Years and
## Model area cards): TS_YEARS for the maps and series, YEAR_ECOPATH for
## the Ecopath values, MODEL_GSAS or the custom region for the area.
.env_default("MODEL_GSAS", 1:12)
.env_default("ENV_YEARS", if (exists("TS_YEARS", envir = .GlobalEnv)) TS_YEARS else 1995:as.integer(format(Sys.Date(), "%Y")))
.env_default("ENV_ECOPATH_B_YEAR", if (exists("YEAR_ECOPATH", envir = .GlobalEnv)) as.integer(round(stats::median(YEAR_ECOPATH))) else 1995L)
## P/B: NPP of the Ecopath years up to the B year (1994:1996 -> 1994-1995)
.env_default("ENV_ECOPATH_PB_YEARS", if (exists("YEAR_ECOPATH", envir = .GlobalEnv)) YEAR_ECOPATH[YEAR_ECOPATH <= ENV_ECOPATH_B_YEAR] else 1994:1995)
.env_default("ENV_BASE_YEAR", ENV_ECOPATH_B_YEAR)                     # Ecosim forcing = value / mean of this year
.env_default("ENV_OVERLAP_YEARS", 1999:2010)                          # MedBFM vs PISCES, for the gap fill
.env_default("ENV_GAP_RATIO_MAX", 4)
.env_default("ENV_PFT_YEARS", 1998:2000)
.env_default("ENV_INTEGRATION_DEPTH", 200)                            # m, deepest level read for NPP and phytoplankton carbon
.env_default("ENV_INTEGRATION", "euphotic")                           # "euphotic" (depth with 99% of the column NPP) or "fixed" (0-ENV_INTEGRATION_DEPTH m)
.env_default("ENV_EU_FRACTION", 0.99)                                 # share of the column NPP above the euphotic depth
.int_lab <- if (identical(ENV_INTEGRATION, "euphotic")) paste0("over the euphotic zone (", round(100 * ENV_EU_FRACTION), "% of the 0-", ENV_INTEGRATION_DEPTH, " m NPP)") else paste0("0-", ENV_INTEGRATION_DEPTH, " m")
.env_default("ENV_DOMAIN", "area")                                    # "area" = the run's model area (GSAs or custom region); "ecospace" = cells of ECOSPACE_DEPTH_FILE
.env_default("PHYTO_C_TO_WW", 9)
.env_default("ECOSPACE_DEPTH_FILE", file.path(pcloud_dir, "data", "Bathymetry", "depth_wmed.asc"))
.env_default("ENV_BBOX", c(lon_min = -6, lon_max = 16.5, lat_min = 34.2, lat_max = 45))
.env_default("ENV_KEEP_NETCDF", FALSE)                                # TRUE keeps the raw downloads
.env_default("ENV_TIMEOUT_SEC", 3600)

env_dir   <- file.path(out_dir, "environment")
## Downloads are for the whole Western Med box and cached once per laptop:
## a custom region (out_dir/<AREA_NAME>) uses the same cache as the GSA run.
.is_custom_env <- exists("AREA_MODE", envir = .GlobalEnv) && identical(AREA_MODE, "custom")
.base_out_env <- if (exists("OUT_BASE_DIR", envir = .GlobalEnv)) OUT_BASE_DIR else
  if (.is_custom_env && exists("AREA_NAME", envir = .GlobalEnv) && basename(sub("/+$", "", out_dir)) == AREA_NAME)
  dirname(sub("/+$", "", out_dir)) else out_dir   # OUT_BASE_DIR: the main output folder, shared by every run folder
cache_dir <- file.path(.base_out_env, "environment", "cache")
nc_dir    <- file.path(env_dir, "netcdf")
asc_dir   <- file.path(env_dir, "ecospace_asc")
diag_dir  <- file.path(env_dir, "diagnostics")
for (d in c(env_dir, cache_dir, nc_dir, asc_dir, diag_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

.env_log <- file.path(out_dir, paste0(format(Sys.Date(), "%Y%m%d"), "_06_environment_log.txt"))
.env_msg <- function(...) {
  txt <- paste0(...)
  message(txt)
  cat(txt, "\n", sep = "", file = .env_log, append = TRUE)
}
cat("Run started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "", file = .env_log)
.t0_env <- Sys.time()
.env_tick <- function(what) .env_msg(sprintf("[time] %s: %.1f min since the start of 06", what,
                                             as.numeric(difftime(Sys.time(), .t0_env, units = "mins"))))
.env_msg("[06_environment] out_dir/environment = ", env_dir, " | downloads cached in ", cache_dir)

## --- Datasets (my = reanalysis, myint = interim for the latest months) --
ENV_DATASETS <- list(
  temp_surf = list(ids = c("cmems_mod_med_phy-temp_my_4.2km_P1M-m", "cmems_mod_med_phy-temp_myint_4.2km_P1M-m"),
                   var = "thetao", depth = c(0, 1.5), integrate = FALSE),
  sal_surf  = list(ids = c("cmems_mod_med_phy-sal_my_4.2km_P1M-m", "cmems_mod_med_phy-sal_myint_4.2km_P1M-m"),
                   var = "so", depth = c(0, 1.5), integrate = FALSE),
  temp_bot  = list(ids = c("cmems_mod_med_phy-temp_my_4.2km_P1M-m", "cmems_mod_med_phy-temp_myint_4.2km_P1M-m"),
                   var = "bottomT", depth = NULL, integrate = FALSE),
  npp       = list(ids = c("cmems_mod_med_bgc-bio_my_4.2km_P1M-m", "cmems_mod_med_bgc-bio_myint_4.2km_P1M-m"),
                   var = "nppv", depth = c(0, ENV_INTEGRATION_DEPTH), integrate = TRUE),
  phyto_c   = list(ids = c("cmems_mod_med_bgc-plankton_my_4.2km_P1M-m", "cmems_mod_med_bgc-plankton_myint_4.2km_P1M-m"),
                   var = "phyc", depth = c(0, ENV_INTEGRATION_DEPTH), integrate = TRUE))
ENV_GLOBAL_BGC <- "cmems_mod_glo_bgc_my_0.25deg_P1M-m"   # PISCES, nppv + phyc, from 1993
ENV_PFT_DATASETS <- c("cmems_obs-oc_med_bgc-plankton_my_l4-multi-1km_P1M",   # Med ocean colour, monthly
                      "cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M")   # global ocean colour, monthly
ENV_UNITS_OUT <- c(temp_surf = "deg C", sal_surf = "PSU", temp_bot = "deg C", npp = "g C m-2 d-1", phyto_c = "g C m-2")

## =================================================================
## Helpers
## =================================================================
## One copernicusmarine subset call; returns the NetCDF path or NULL when
## the dataset has nothing for these dates (e.g. MedBFM before 1999).
.env_subset <- function(dataset_id, variables, t0, t1, depth = NULL, bbox = ENV_BBOX, tag = "x") {
  f <- file.path(nc_dir, paste0(tag, "_", gsub("[^A-Za-z0-9]", "", dataset_id), "_", substr(t0, 1, 4), ".nc"))
  if (file.exists(f)) return(f)
  args <- c("subset", "--dataset-id", dataset_id, unlist(lapply(variables, function(v) c("--variable", v))),
            "--start-datetime", t0, "--end-datetime", t1,
            "--minimum-longitude", bbox[["lon_min"]], "--maximum-longitude", bbox[["lon_max"]],
            "--minimum-latitude", bbox[["lat_min"]], "--maximum-latitude", bbox[["lat_max"]],
            if (!is.null(depth)) c("--minimum-depth", depth[1], "--maximum-depth", depth[2]),
            "--coordinates-selection-method", "inside",
            "--output-directory", shQuote(dirname(f)), "--output-filename", shQuote(basename(f)))   # quoted: paths can contain spaces
  out <- suppressWarnings(system2("copernicusmarine", args, stdout = TRUE, stderr = TRUE, timeout = ENV_TIMEOUT_SEC))
  if (!file.exists(f) && any(grepl("Invalid username or password", out, ignore.case = TRUE))) {
    ensure_copernicusmarine_credentials(force_prompt = TRUE)
    out <- suppressWarnings(system2("copernicusmarine", args, stdout = TRUE, stderr = TRUE, timeout = ENV_TIMEOUT_SEC))
  }
  ## A command-line error (not a missing year) is the same for every request: stop at once
  if (!file.exists(f) && any(grepl("^Usage: copernicusmarine|Got unexpected extra arguments|No such option", out)))
    stop("[06_environment] copernicusmarine rejected the command: ", paste(utils::tail(out[nzchar(trimws(out))], 2), collapse = " | "))
  if (!file.exists(f) && any(grepl("requires a Copernicus Marine username and password|Copernicus Marine username:|Invalid username or password", out)))
    stop("[06_environment] Copernicus Marine refused the login (missing or wrong). Enter it again on the page (06 environment ->",
         " 'enter or change the login'), or in the R console:\n",
         "  source(\"", file.path(git_dir, "scripts", "lib_cmems_phytoplankton_biomass.R"), "\"); wmed_copernicus_login()")
  if (!file.exists(f)) {
    .env_msg("  [download] nothing from ", dataset_id, " ", paste(variables, collapse = "+"), " ", t0, " to ", t1,
             " - last CLI lines: ", paste(utils::tail(out[nzchar(trimws(out))], 3), collapse = " | "))
    return(NULL)
  }
  f
}

## Time axis of a NetCDF file as Dates (CF "units since origin")
.env_nc_dates <- function(nc) {
  tn <- grep("^time", names(nc$dim), value = TRUE)[1]
  u <- nc$dim[[tn]]$units; v <- nc$dim[[tn]]$vals
  step <- tolower(sub(" since.*$", "", u)); origin <- as.POSIXct(sub("^.* since ", "", u), tz = "UTC")
  mult <- switch(step, seconds = 1, minutes = 60, hours = 3600, days = 86400,
                 stop("Unknown time units in NetCDF: ", u))
  as.Date(origin + v * mult)
}

## Unit conversion to the output units, from the units the file declares
.env_unit_factor <- function(name, units) {
  u <- tolower(gsub("\\s+", " ", units))
  switch(name,
    temp_surf = , temp_bot = if (grepl("kelvin|^k$", u)) NA_real_ else 1,   # NA = subtract 273.15 (handled below)
    sal_surf = 1,
    npp = if (grepl("^mg", u)) 1e-3 else if (grepl("^mmol", u)) 12.011e-3 else if (grepl("^mol", u)) 12.011 else
      stop("Unrecognised units for npp: '", units, "'"),                     # -> g C m-3 d-1 (per day assumed; checked below)
    phyto_c = if (grepl("^mmol", u)) 12.011e-3 else if (grepl("^mol", u)) 12.011 else if (grepl("^mg", u)) 1e-3 else
      stop("Unrecognised units for phyto_c: '", units, "'"))
}

## Layer thickness (m) of each depth level for an integration from 0 to zmax
.env_thickness <- function(z, zmax) {
  edges <- c(0, (head(z, -1) + tail(z, -1)) / 2, max(z) + (max(z) - if (length(z) > 1) z[length(z) - 1] else 0) / 2)
  edges <- pmin(edges, zmax)
  pmax(diff(edges), 0)
}

## lon x lat matrix -> SpatRaster (CMEMS latitudes go south -> north)
.env_mat2rast <- function(m, lon, lat) {
  rx <- abs(diff(lon[1:2])); ry <- abs(diff(lat[1:2]))
  if (lat[1] < lat[length(lat)]) m <- m[, ncol(m):1, drop = FALSE]
  r <- terra::rast(t(m), extent = terra::ext(min(lon) - rx / 2, max(lon) + rx / 2, min(lat) - ry / 2, max(lat) + ry / 2),
                   crs = "EPSG:4326")
  r
}

## Read one NetCDF into a monthly SpatRaster (layers named YYYY-MM):
## surface variables take the first depth level, integrated ones are
## summed over depth x layer thickness (cells where every level is NA stay
## NA). Read one month at a time to keep memory low (a year of 0-200 m
## MedBFM is ~0.5 GB in memory otherwise).
.env_read_monthly <- function(f, name, var, integrate, eu = FALSE, zeu_file = NULL) {
  nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  if (!var %in% names(nc$var)) stop("'", var, "' not in ", basename(f), " (has: ", paste(names(nc$var), collapse = ", "), ")")
  units <- ncdf4::ncatt_get(nc, var, "units")$value
  vd <- nc$var[[var]]$dim
  dims <- vapply(vd, function(d) d$name, "")
  dimvals <- lapply(vd, function(d) d$vals); names(dimvals) <- dims
  i_lon <- grep("^lon", dims); i_lat <- grep("^lat", dims); i_dep <- grep("depth|^lev", dims); i_tim <- grep("^time", dims)
  dates <- .env_nc_dates(nc)
  fac <- .env_unit_factor(name, units)
  per_second <- grepl("s-1|/s\\b|s\\^-1", units)
  z <- if (length(i_dep)) dimvals[[i_dep]] else 0
  th <- .env_thickness(z, ENV_INTEGRATION_DEPTH)
  edges_lo <- c(0, head(cumsum(th), -1)); edges_hi <- cumsum(th)
  ## euphotic depth: computed from the NPP profile (name == "npp") and kept
  ## per month in zeu_file; phytoplankton carbon (name == "phyto_c") reads it
  zeu_store <- list()
  zeu_in <- if (eu && name == "phyto_c" && !is.null(zeu_file) && file.exists(zeu_file)) readRDS(zeu_file) else NULL
  if (eu && name == "phyto_c" && is.null(zeu_in))
    .env_msg("  [euphotic] no euphotic depth for ", basename(f), " - phytoplankton carbon integrated 0-", ENV_INTEGRATION_DEPTH, " m.")
  zeu_vals <- list()
  layers <- vector("list", length(dates))
  for (k in seq_along(dates)) {
    st <- rep(1, length(dims)); ct <- rep(-1, length(dims)); st[i_tim] <- k; ct[i_tim] <- 1
    a <- ncdf4::ncvar_get(nc, var, start = st, count = ct, collapse_degen = FALSE)
    keep <- c(i_lon, i_lat, i_dep)
    a <- aperm(a, c(keep, setdiff(seq_along(dims), keep)))
    dim(a) <- c(length(dimvals[[i_lon]]), length(dimvals[[i_lat]]), if (length(i_dep)) length(z) else 1)
    if (integrate) {
      ok <- apply(is.finite(a), c(1, 2), any)
      a[!is.finite(a)] <- 0
      contrib <- sweep(a, 3, th, `*`)
      m <- apply(contrib, c(1, 2), sum); m[!ok] <- NA
      mkey <- format(dates[k], "%Y-%m")
      if (eu && name == "npp" && length(z) > 1) {
        ## depth where the cumulative NPP reaches ENV_EU_FRACTION of the column
        nx <- dim(contrib)[1]; ny <- dim(contrib)[2]; nz <- dim(contrib)[3]
        cm <- matrix(contrib, nx * ny, nz)
        cs <- t(apply(cm, 1, cumsum)); tot <- cs[, nz]; target <- ENV_EU_FRACTION * tot
        kk <- max.col(cs >= target, ties.method = "first")
        before <- ifelse(kk > 1, cs[cbind(seq_len(nrow(cs)), pmax(kk - 1, 1))], 0)
        inlayer <- cm[cbind(seq_len(nrow(cm)), kk)]
        frac <- ifelse(inlayer > 0, (target - before) / inlayer, 1)
        zeu <- edges_lo[kk] + pmin(pmax(frac, 0), 1) * th[kk]
        zeu[!(tot > 0) | !as.vector(ok)] <- NA
        zeu_store[[mkey]] <- list(zeu = matrix(zeu, nx, ny), lon = dimvals[[i_lon]], lat = dimvals[[i_lat]])
        zeu_vals[[mkey]] <- zeu
        m <- m * ENV_EU_FRACTION                                         # NPP above the euphotic depth
      }
      if (eu && name == "phyto_c" && !is.null(zeu_in[[mkey]]) && length(z) > 1 &&
          isTRUE(all.equal(zeu_in[[mkey]]$lon, dimvals[[i_lon]])) && isTRUE(all.equal(zeu_in[[mkey]]$lat, dimvals[[i_lat]]))) {
        zv <- as.vector(zeu_in[[mkey]]$zeu)
        zv[!is.finite(zv)] <- ENV_INTEGRATION_DEPTH
        ## thickness of each level that lies above the euphotic depth, per cell
        w <- pmax(outer(zv, edges_hi, pmin) - matrix(edges_lo, length(zv), length(edges_lo), byrow = TRUE), 0)
        am <- matrix(a, length(zv), length(z))
        m <- matrix(rowSums(am * w), dim(a)[1], dim(a)[2]); m[!ok] <- NA
        zeu_vals[[mkey]] <- zv[as.vector(ok)]   # water cells only (land would show as 200 m in the diagnostics)
      }
    } else {
      m <- a[, , 1]
    }
    m <- if (is.na(fac)) m - 273.15 else m * fac
    if (per_second) m <- m * 86400
    layers[[k]] <- .env_mat2rast(m, dimvals[[i_lon]], dimvals[[i_lat]])
  }
  r <- terra::rast(layers)
  names(r) <- format(dates, "%Y-%m")
  if (length(zeu_store) && !is.null(zeu_file)) saveRDS(c(if (file.exists(zeu_file)) readRDS(zeu_file) else list(), zeu_store), zeu_file)
  zq <- if (length(zeu_vals)) stats::quantile(unlist(zeu_vals), c(0.1, 0.5, 0.9), na.rm = TRUE) else rep(NA_real_, 3)
  if (length(zeu_vals) && name == "npp")
    .env_msg("  [euphotic] ", basename(f), ": euphotic depth (", round(100 * ENV_EU_FRACTION), "% of NPP) median ", round(zq[[2]]),
             " m (10-90%: ", round(zq[[1]]), "-", round(zq[[3]]), " m).")
  attr(r, "diag") <- data.table(variable = name, file = basename(f), units = units, per_second_converted = per_second,
                                n_levels = length(z), top_m = min(z), bottom_m = max(z),
                                thickness_sum_m = if (integrate) sum(th) else NA_real_,
                                integration = if (integrate) (if (length(zeu_vals)) "euphotic" else paste0("0-", ENV_INTEGRATION_DEPTH, " m")) else "surface/bottom",
                                zeu_p10_m = zq[[1]], zeu_median_m = zq[[2]], zeu_p90_m = zq[[3]])
  r
}

## Cached monthly SpatRaster of one variable for one year from one product
.env_year <- function(name, year, ids, var, depth, integrate, tag = name) {
  eu <- integrate && identical(ENV_INTEGRATION, "euphotic") && !grepl("_pisces$", tag)
  if (eu) tag <- paste0(tag, "_eu", round(100 * ENV_EU_FRACTION))       # own cache, so a 0-200 m cache is not reused
  zeu_file <- if (eu) file.path(cache_dir, sprintf("zeu_eu%d_%d.rds", round(100 * ENV_EU_FRACTION), year)) else NULL
  cf <- file.path(cache_dir, sprintf("%s_%d.tif", tag, year))
  if (file.exists(cf) && !(eu && name == "npp" && !file.exists(zeu_file))) return(terra::rast(cf))
  got <- list(); months_have <- character(0)
  ## no request for years before a product starts; the interim ("myint")
  ## datasets only hold the last few years, so they are only asked then
  first <- if (any(grepl("med_bgc", ids))) 1999 else if (any(grepl("glo_bgc", ids))) 1993 else 1987
  if (year < first) return(NULL)
  if (year < as.integer(format(Sys.Date(), "%Y")) - 5) ids <- ids[!grepl("myint", ids)]
  for (id in ids) {
    f <- .env_subset(id, var, sprintf("%d-01-01", year), sprintf("%d-12-31", year), depth = depth, tag = tag)
    if (is.null(f)) next
    r <- tryCatch(.env_read_monthly(f, name, var, integrate, eu = eu, zeu_file = zeu_file), error = function(e) {
      .env_msg("  [read] ", basename(f), ": ", conditionMessage(e)); NULL })
    if (!ENV_KEEP_NETCDF) unlink(f)
    if (is.null(r)) next
    .df <- file.path(diag_dir, "netcdf_units_and_levels.csv")
    .same <- file.exists(.df) && identical(names(fread(.df, nrows = 0)), names(attr(r, "diag")))
    fwrite(attr(r, "diag"), .df, append = .same)
    new <- setdiff(names(r), months_have)
    if (length(new)) { got[[length(got) + 1]] <- r[[new]]; months_have <- c(months_have, new) }
    if (length(months_have) == 12) break
  }
  if (!length(got)) return(NULL)
  r <- if (length(got) == 1) got[[1]] else do.call(c, unname(lapply(got, function(x) terra::resample(x, got[[1]]))))
  r <- r[[order(names(r))]]
  terra::writeRaster(r, cf, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  terra::rast(cf)
}

## =================================================================
## Model area mask and area weights
## =================================================================
ecospace_grid <- if (file.exists(ECOSPACE_DEPTH_FILE)) {
  g <- terra::rast(ECOSPACE_DEPTH_FILE); terra::crs(g) <- "EPSG:4326"; g
} else { .env_msg("[06_environment] Ecospace base map not found (", ECOSPACE_DEPTH_FILE, ") - no ASCII maps."); NULL }

## The run's model area: the custom region (CUSTOM_BBOX or the uploaded
## CUSTOM_SHAPEFILE_PATH) or the GSAs in MODEL_GSAS.
.gsa_poly <- tryCatch({
  if (.is_custom_env) {
    if (exists("CUSTOM_AREA_TYPE", envir = .GlobalEnv) && identical(CUSTOM_AREA_TYPE, "shapefile")) {
      g <- sf::st_read(CUSTOM_SHAPEFILE_PATH, quiet = TRUE)
      if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
      g <- sf::st_transform(g, 4326)
    } else {
      b <- CUSTOM_BBOX
      g <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = unname(b["xmin"]), ymin = unname(b["ymin"]),
                                                               xmax = unname(b["xmax"]), ymax = unname(b["ymax"])), crs = 4326)))
    }
    terra::vect(sf::st_union(g))
  } else {
    g <- sf::st_read(file.path(git_dir, "scripts", "gsa_boundaries_westmed.geojson"), quiet = TRUE)
    terra::vect(g[g$gsa_num %in% MODEL_GSAS, ])
  }
}, error = function(e) { .env_msg("[06_environment] model area could not be read: ", conditionMessage(e)); NULL })
.area_label <- if (ENV_DOMAIN == "ecospace") "the Ecospace map" else if (.is_custom_env)
  paste0("the custom region '", if (exists("AREA_NAME", envir = .GlobalEnv)) AREA_NAME else "custom", "'") else
  paste0("GSAs ", paste(MODEL_GSAS, collapse = ", "))
.env_msg("[06_environment] Area: ", .area_label, " | years ", min(ENV_YEARS), "-", max(ENV_YEARS),
         " | Ecopath B ", ENV_ECOPATH_B_YEAR, ", P/B NPP ", paste(range(ENV_ECOPATH_PB_YEARS), collapse = "-"))

## Area-weighted (cos latitude) statistics of every layer over the model
## area: mean, and the 10th and 90th percentiles across the grid cells
## (the spatial spread inside the area, shown as the band in the plot).
.env_wquant <- function(x, w, p) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[which(cw >= q)[1]], numeric(1))
}
.env_mask_memo <- new.env()
.env_domain_stats <- function(r) {
  key <- paste(c(as.vector(terra::ext(r)), terra::res(r)), collapse = "_")
  ww <- .env_mask_memo[[key]]
  if (is.null(ww)) {
  w <- terra::init(r[[1]], "y"); w <- cos(w * pi / 180)
  m <- if (ENV_DOMAIN == "ecospace" && !is.null(ecospace_grid)) {
    terra::resample(ecospace_grid > 0, r[[1]], method = "near")
  } else if (!is.null(.gsa_poly)) {
    terra::rasterize(.gsa_poly, r[[1]], field = 1, touches = FALSE)
  } else stop("No model-area mask: neither the GSA polygons nor ECOSPACE_DEPTH_FILE could be read.")
  w <- terra::mask(w, m, maskvalues = c(NA, 0))
  ww <- terra::values(w, mat = FALSE); assign(key, ww, envir = .env_mask_memo)
  }
  rbindlist(lapply(seq_len(terra::nlyr(r)), function(k) {
    x <- terra::values(r[[k]], mat = FALSE)
    ok <- is.finite(x) & is.finite(ww)
    if (!any(ok)) return(data.table(value = NA_real_, p10 = NA_real_, p90 = NA_real_, n_cells = 0L))
    q <- .env_wquant(x[ok], ww[ok], c(0.1, 0.9))
    data.table(value = sum(x[ok] * ww[ok]) / sum(ww[ok]), p10 = q[1], p90 = q[2], n_cells = sum(ok))
  }))
}
.env_domain_mean <- function(r) .env_domain_stats(r)$value

## ASCII maps on the Ecospace base map are made by 06b_ecospace_drivers.R
## (any base map, coastal gaps filled, index file). ENV_WRITE_ASCII = TRUE
## also writes the old unfilled copies here, in environment/ecospace_asc/.
.env_default("ENV_WRITE_ASCII", FALSE)
.env_write_ascii <- function(r, name) {
  if (is.null(ecospace_grid) || !isTRUE(ENV_WRITE_ASCII)) return(invisible())
  d <- file.path(asc_dir, name); dir.create(d, showWarnings = FALSE)
  a <- terra::resample(r, ecospace_grid, method = "average")
  a <- terra::mask(a, ecospace_grid > 0, maskvalues = c(NA, 0))
  for (k in seq_len(terra::nlyr(a)))
    terra::writeRaster(a[[k]], file.path(d, paste0(name, "_", names(r)[k], ".asc")), filetype = "AAIGrid",
                       NAflag = -9999, overwrite = TRUE)
}

## =================================================================
## STEP 1 - monthly maps per variable (MedPHY / MedBFM)
## =================================================================
if (!ensure_copernicusmarine_cli(auto_install = FALSE))
  stop("[06_environment] The copernicusmarine tool is not installed or not found. Install it (pip install copernicusmarine)",
       " and run 'copernicusmarine login' once, then rerun.")
## A login is needed before anything is downloaded: either saved with
## wmed_copernicus_login() (lib_cmems_phytoplankton_biomass.R) or by
## 'copernicusmarine login' (credentials file in ~/.copernicusmarine).
.cm_creds <- ensure_copernicusmarine_credentials()
.cm_file <- unique(c(path.expand("~/.copernicusmarine/.copernicusmarine-credentials"),
                     if (nzchar(Sys.getenv("USERPROFILE"))) file.path(Sys.getenv("USERPROFILE"), ".copernicusmarine", ".copernicusmarine-credentials")))
if (is.null(.cm_creds) && !any(file.exists(.cm_file)))
  stop("[06_environment] No Copernicus Marine login on this laptop. Enter it on the page (06 environment ->",
       " 'enter or change the login'), or once in the R console:\n",
       "  source(\"", file.path(git_dir, "scripts", "lib_cmems_phytoplankton_biomass.R"), "\"); wmed_copernicus_login()\n",
       "then run 06 again (free account: https://data.marine.copernicus.eu/register).")

env_maps <- list()
for (nm in names(ENV_DATASETS)) {
  ds <- ENV_DATASETS[[nm]]
  .env_msg("[06_environment] ", nm, " (", ds$var, ") ...")
  yrs <- lapply(ENV_YEARS, function(y) .env_year(nm, y, ds$ids, ds$var, ds$depth, ds$integrate))
  yrs <- Filter(Negate(is.null), yrs)
  if (!length(yrs)) { .env_msg("[06_environment] ", nm, ": no data downloaded - skipped."); next }
  env_maps[[nm]] <- do.call(c, yrs)
  .env_msg("[06_environment] ", nm, ": ", terra::nlyr(env_maps[[nm]]), " months, ", names(env_maps[[nm]])[1], " to ",
           names(env_maps[[nm]])[terra::nlyr(env_maps[[nm]])], ".")
  .env_tick(nm)
}

## =================================================================
## STEP 2 - 1994-1998 for npp and phyto_c: PISCES scaled to MedBFM
## =================================================================
.env_gap_fill <- function(nm, var) {
  med <- env_maps[[nm]]
  if (is.null(med)) return(NULL)
  first_year <- as.integer(substr(names(med)[1], 1, 4))
  gap_years <- intersect(c(ENV_ECOPATH_PB_YEARS, ENV_YEARS), seq(min(c(ENV_ECOPATH_PB_YEARS, ENV_YEARS)), first_year - 1))
  if (!length(gap_years)) return(NULL)
  ov <- intersect(ENV_OVERLAP_YEARS, as.integer(substr(names(med), 1, 4)))
  if (length(ov) < 3) { .env_msg("[gap fill] ", nm, ": fewer than 3 overlap years - not filled."); return(NULL) }
  .env_msg("[gap fill] ", nm, ": ", min(gap_years), "-", max(gap_years), " from PISCES (", ENV_GLOBAL_BGC,
           ") scaled to MedBFM over ", min(ov), "-", max(ov), " ...")
  glo <- lapply(sort(unique(c(gap_years, ov))), function(y)
    .env_year(nm, y, ENV_GLOBAL_BGC, var, c(0, ENV_INTEGRATION_DEPTH), TRUE, tag = paste0(nm, "_pisces")))
  glo <- do.call(c, Filter(Negate(is.null), glo))
  if (is.null(glo)) { .env_msg("[gap fill] ", nm, ": PISCES download failed - not filled."); return(NULL) }
  glo <- terra::resample(glo, med[[1]], method = "bilinear")
  mo <- function(x) as.integer(substr(names(x), 6, 7)); yr <- function(x) as.integer(substr(names(x), 1, 4))
  out <- list(); ratios <- data.table()
  for (m in 1:12) {
    med_clim <- terra::app(med[[which(mo(med) == m & yr(med) %in% ov)]], mean, na.rm = TRUE)
    glo_clim <- terra::app(glo[[which(mo(glo) == m & yr(glo) %in% ov)]], mean, na.rm = TRUE)
    for (y in gap_years) {
      k <- which(mo(glo) == m & yr(glo) == y); if (!length(k)) next
      ratio <- glo[[k]] / glo_clim
      ratio <- terra::clamp(ratio, 1 / ENV_GAP_RATIO_MAX, ENV_GAP_RATIO_MAX)
      x <- med_clim * ratio; names(x) <- sprintf("%d-%02d", y, m)
      out[[names(x)]] <- x
      ratios <- rbind(ratios, data.table(variable = nm, month = names(x),
                                         domain_ratio_pisces_to_clim = .env_domain_mean(ratio)))
    }
  }
  fwrite(ratios, file.path(diag_dir, paste0(nm, "_gapfill_ratios.csv")))
  r <- do.call(c, unname(out)); r[[order(names(r))]]
}
env_gap <- list()
for (nm in c("npp", "phyto_c")) {
  g <- .env_gap_fill(nm, ENV_DATASETS[[nm]]$var)
  if (!is.null(g)) env_gap[[nm]] <- g
}
.env_tick("gap fill")

## =================================================================
## STEP 3 - write maps and time series
## =================================================================
ts_rows <- list(); ann_rows <- list()
for (nm in names(env_maps)) {
  r <- env_maps[[nm]]; src <- rep(if (nm %in% c("npp", "phyto_c")) "MedBFM reanalysis" else "MedPHY reanalysis", terra::nlyr(r))
  if (!is.null(env_gap[[nm]])) {
    g <- env_gap[[nm]]
    r <- c(g, terra::resample(r, g[[1]])); src <- c(rep("PISCES scaled to MedBFM", terra::nlyr(g)), src)
    o <- order(names(r)); r <- r[[o]]; src <- src[o]
  }
  keep <- as.integer(substr(names(r), 1, 4)) >= min(c(ENV_YEARS, ENV_ECOPATH_PB_YEARS))
  r <- r[[which(keep)]]; src <- src[keep]
  terra::writeRaster(r, file.path(env_dir, paste0(nm, "_monthly_native.tif")), overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  .env_write_ascii(r[[which(as.integer(substr(names(r), 1, 4)) %in% ENV_YEARS)]], nm)
  keep_y <- as.integer(substr(names(r), 1, 4)) %in% c(ENV_YEARS, ENV_ECOPATH_PB_YEARS, ENV_ECOPATH_B_YEAR)
  r <- r[[which(keep_y)]]; src <- src[keep_y]
  ts_rows[[nm]] <- cbind(data.table(variable = nm, unit = ENV_UNITS_OUT[[nm]], year = as.integer(substr(names(r), 1, 4)),
                                    month = as.integer(substr(names(r), 6, 7))), .env_domain_stats(r), data.table(source = src))
  ## annual map (NPP: sum of the months x days in month; others: mean), then its statistics
  yrs <- as.integer(substr(names(r), 1, 4)); mo <- as.integer(substr(names(r), 6, 7))
  dim_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  ann_rows[[nm]] <- rbindlist(lapply(sort(unique(yrs)), function(y) {
    k <- which(yrs == y)
    amap <- if (nm == "npp") sum(r[[k]] * dim_days[mo[k]]) * 12 / length(k) else terra::app(r[[k]], mean)
    cbind(data.table(variable = nm, year = y, n_months = length(k), source = paste(unique(src[k]), collapse = " + ")),
          .env_domain_stats(amap))
  }))
  .env_tick(paste("maps and series", nm))
}
env_ts <- rbindlist(ts_rows)
if (nrow(env_ts)) {
  env_ts[, base := mean(value[year == ENV_BASE_YEAR], na.rm = TRUE), by = variable]
  env_ts[, relative_to_base := value / base][, base := NULL]
  fwrite(env_ts, file.path(env_dir, "environment_timeseries_monthly.csv"))
  env_annual <- rbindlist(ann_rows)
  env_annual[variable == "npp", unit := "g C m-2 yr-1"]
  env_annual[variable != "npp", unit := ENV_UNITS_OUT[variable]]
  env_annual[, relative_to_base := value / value[year == ENV_BASE_YEAR][1], by = variable]
  fwrite(env_annual, file.path(env_dir, "environment_timeseries_annual.csv"))
  ## Ecosim forcing functions: one file per variable, value relative to the
  ## base year (Ecopath year) = 1, monthly (time step 1 = January of the
  ## first year) and annual, ready to paste into EwE's forcing function
  ## table (Ecosim > Input > Time series / Forcing functions).
  for (nm in unique(env_ts$variable)) {
    m <- env_ts[variable == nm & year %in% sort(unique(c(ENV_BASE_YEAR, ENV_YEARS)))][order(year, month)]
    m[, time_step := (year - min(year)) * 12 + month]
    fwrite(m[, .(time_step, year, month, forcing = round(relative_to_base, 4), value, source)],
           file.path(env_dir, paste0("ecosim_forcing_", nm, "_monthly.csv")))
    a <- env_annual[variable == nm & year %in% sort(unique(c(ENV_BASE_YEAR, ENV_YEARS)))][order(year)]
    fwrite(a[, .(year, forcing = round(relative_to_base, 4), value, n_months, source)],
           file.path(env_dir, paste0("ecosim_forcing_", nm, "_annual.csv")))
  }
  .env_msg("[06_environment] Ecosim forcing functions (relative to ", ENV_BASE_YEAR, "): ecosim_forcing_<variable>_monthly.csv / _annual.csv")
  .env_msg("[06_environment] Time series: environment_timeseries_monthly.csv and _annual.csv (value = area-weighted mean,",
           " p10/p90 = 10th/90th percentile across the grid cells of the area; relative_to_base = value / ",
           ENV_BASE_YEAR, " mean, the Ecosim forcing shape).")
}

## Plot: one panel per variable over the run's years and area. Grey band
## and line = monthly 10th-90th percentile across the grid cells and the
## monthly mean; red band and line = the same for the annual maps. The
## band is the spatial spread inside the area, not a model error. Years
## filled from PISCES (scaled to MedBFM) are shaded. Shown on the page.
if (nrow(env_ts)) tryCatch({
  suppressPackageStartupMessages(library(ggplot2))
  plot_dir_env <- file.path(out_dir, "plots", "environment"); dir.create(plot_dir_env, recursive = TRUE, showWarnings = FALSE)
  lab <- c(temp_surf = "Surface temperature (deg C)", sal_surf = "Surface salinity", temp_bot = "Bottom temperature (deg C)",
           npp = paste0("NPP ", .int_lab, " (monthly g C m-2 d-1; annual g C m-2 yr-1 / 365)"), phyto_c = paste0("Phytoplankton C ", .int_lab, " (g C m-2)"))
  yr_keep <- sort(unique(c(ENV_YEARS, ENV_ECOPATH_PB_YEARS)))
  d <- copy(env_ts)[year %in% yr_keep][, date := as.Date(sprintf("%d-%02d-15", year, month))][, panel := factor(lab[variable], levels = lab)]
  a <- copy(env_annual)[year %in% yr_keep][, date := as.Date(sprintf("%d-07-01", year))][, panel := factor(lab[variable], levels = lab)]
  a[variable == "npp", `:=`(value = value / 365, p10 = p10 / 365, p90 = p90 / 365)]   # same axis as the monthly daily rate
  gap <- d[grepl("PISCES", source), .(xmin = min(date) - 15, xmax = max(date) + 15), by = panel]
  p <- ggplot(d, aes(date, value)) +
    geom_rect(data = gap, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "grey92") +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = "grey70", alpha = 0.35) +
    geom_line(colour = "grey45", linewidth = 0.3) +
    geom_ribbon(data = a, aes(ymin = p10, ymax = p90), fill = "firebrick", alpha = 0.18) +
    geom_line(data = a, colour = "firebrick", linewidth = 0.8) + geom_point(data = a, colour = "firebrick", size = 1) +
    geom_vline(xintercept = as.Date(sprintf("%d-07-01", ENV_ECOPATH_B_YEAR)), linetype = "dashed", colour = "steelblue") +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    labs(title = paste0("Environment of ", .area_label, ", ", min(yr_keep), "-", max(yr_keep), " (Copernicus Marine)"),
         subtitle = paste0("Grey: monthly mean and 10th-90th percentile across grid cells; red: annual mean and its 10th-90th percentile.\n",
                           "Band = spatial spread inside the area. Shaded years: PISCES scaled to MedBFM. Dashed: Ecopath year ",
                           ENV_ECOPATH_B_YEAR, "."),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) + theme(strip.text = element_text(hjust = 0, face = "bold"))
  ggplot2::ggsave(file.path(plot_dir_env, "environment_timeseries.png"), p, width = 10, height = 12, dpi = 200, bg = "white")
  .env_msg("[06_environment] Plot: plots/environment/environment_timeseries.png")
}, error = function(e) .env_msg("[06_environment] plot skipped - ", conditionMessage(e)))

## =================================================================
## STEP 4 - Large / Small share from satellite size classes
## =================================================================
pft_share <- NULL
.pft_file <- file.path(diag_dir, "phytoplankton_size_classes_chl.csv")
if (file.exists(.pft_file)) {   # downloaded on an earlier run: reuse when it covers the same years and has values
  pft_share <- fread(.pft_file)
  if (!setequal(pft_share$year, ENV_PFT_YEARS[ENV_PFT_YEARS >= 1997]) ||
      !all(is.finite(as.matrix(pft_share[, .(MICRO, NANO, PICO)])))) pft_share <- NULL
}
for (id in if (is.null(pft_share)) ENV_PFT_DATASETS else character(0)) {
  rows <- list()
  for (y in ENV_PFT_YEARS[ENV_PFT_YEARS >= 1997]) {
    f <- .env_subset(id, c("MICRO", "NANO", "PICO"), sprintf("%d-01-01", y), sprintf("%d-12-31", y), tag = "pft")
    if (is.null(f)) next
    rr <- tryCatch(local({
      nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
      lon <- nc$dim[[grep("^lon", names(nc$dim))]]$vals; lat <- nc$dim[[grep("^lat", names(nc$dim))]]$vals
      vapply(c("MICRO", "NANO", "PICO"), function(v) {
        a <- ncdf4::ncvar_get(nc, v, collapse_degen = FALSE); dm <- dim(a)
        if (length(dm) == 4) a <- a[, , 1, , drop = TRUE]              # a depth level of 1: drop it
        dm <- dim(a); if (length(dm) == 2) { a <- array(a, c(dm, 1)); dm <- dim(a) }
        vals <- vapply(seq_len(dm[3]), function(k) {
          mm <- a[, , k]
          x <- tryCatch(.env_domain_mean(.env_mat2rast(mm, lon, lat)), error = function(e) NA_real_)
          if (!is.finite(x) && any(is.finite(mm))) x <- mean(mm, na.rm = TRUE)   # model-area mask found no cells: mean of the box
          x }, numeric(1))
        if (!any(is.finite(vals))) .env_msg("  [PFT] ", basename(f), " ", v, ": no values (dims ", paste(dim(a), collapse = "x"),
                                             ", finite cells ", sum(is.finite(a)), ")")
        mean(vals, na.rm = TRUE)
      }, numeric(1))
    }), error = function(e) { .env_msg("  [PFT] ", basename(f), ": ", conditionMessage(e)); NULL })
    if (!ENV_KEEP_NETCDF) unlink(f)
    if (!is.null(rr) && all(is.finite(rr))) rows[[as.character(y)]] <- data.table(dataset = id, year = y, MICRO = rr[["MICRO"]], NANO = rr[["NANO"]], PICO = rr[["PICO"]])
  }
  if (length(rows)) { pft_share <- rbindlist(rows); break }
}
if (!is.null(pft_share)) {
  share_large <- sum(pft_share$MICRO) / sum(pft_share$MICRO + pft_share$NANO + pft_share$PICO)
  fwrite(pft_share, .pft_file)
  .env_msg("[06_environment] Size classes (", pft_share$dataset[1], ", ", min(pft_share$year), "-", max(pft_share$year),
           "): micro ", round(100 * share_large), " % of chlorophyll -> Large; nano + pico ", round(100 * (1 - share_large)), " % -> Small.")
} else {
  share_large <- 0.55
  .env_msg("[06_environment] No satellite size classes could be downloaded - FALLBACK Large/Small 0.55/0.45 (Delgado et al. 1992).")
}

## =================================================================
## STEP 5 - Ecopath phytoplankton B and P/B
## =================================================================
tryCatch(if (nrow(env_ts) && all(c("npp", "phyto_c") %in% env_ts$variable)) {
  b_c  <- env_ts[variable == "phyto_c" & year == ENV_ECOPATH_B_YEAR, mean(value)]
  b_src <- env_ts[variable == "phyto_c" & year == ENV_ECOPATH_B_YEAR, paste(unique(source), collapse = " + ")]
  npp_y <- env_annual[variable == "npp" & year %in% ENV_ECOPATH_PB_YEARS & n_months == 12]
  npp_c <- mean(npp_y$value)
  if (!is.finite(b_c) || !nrow(npp_y) || !is.finite(npp_c))
    stop("[06_environment] no phytoplankton carbon for ", ENV_ECOPATH_B_YEAR, " or no full year of NPP in ",
         paste(range(ENV_ECOPATH_PB_YEARS), collapse = "-"), " (before 1999 these come from the PISCES gap fill,",
         " which needs at least 3 overlap years; see the [gap fill] lines above) - no Ecopath phytoplankton values.")
  pb <- npp_c / b_c
  b_ww <- b_c * PHYTO_C_TO_WW
  shares <- c(LargePhytoplankton = share_large, SmallPhytoplankton = 1 - share_large)
  ecopath_phyto <- data.table(
    TargetGroup = names(shares), share = unname(shares),
    B_gC_m2 = b_c * shares, Biomass_t_km2 = b_ww * shares, PB = pb,
    NPP_gC_m2_yr = npp_c * shares,
    B_year = ENV_ECOPATH_B_YEAR, PB_npp_years = paste(range(npp_y$year), collapse = "-"),
    Source_citation = paste0("Copernicus Marine: phytoplankton carbon (phyc) and NPP (nppv) integrated ", .int_lab,
                             ", area-weighted over ", .area_label, "; ", b_src, " for ", ENV_ECOPATH_B_YEAR,
                             " (MedBFM MEDSEA_MULTIYEAR_BGC_006_008 level and seasonality, year-to-year change from PISCES",
                             " GLOBAL_MULTIYEAR_BGC_001_029 before 1999); C:WW 1:", PHYTO_C_TO_WW,
                             " (Pauly & Christensen 1995); Large/Small = satellite micro vs nano+pico chlorophyll share",
                             if (!is.null(pft_share)) paste0(" (", pft_share$dataset[1], ", ", min(pft_share$year), "-", max(pft_share$year), ")") else " (fallback 0.55/0.45, Delgado et al. 1992)",
                             ", same P/B for both (FLAGGED)."))
  fwrite(ecopath_phyto, file.path(env_dir, "ecopath_phytoplankton_B_PB.csv"))
  .env_msg("[06_environment] Ecopath phytoplankton: B ", signif(b_ww, 3), " t WW/km2 (", signif(b_c, 3), " g C/m2, ",
           ENV_ECOPATH_B_YEAR, "), NPP ", signif(npp_c, 3), " g C/m2/yr (", paste(range(npp_y$year), collapse = "-"),
           "), P/B ", signif(pb, 3), " /yr; Large ", signif(b_ww * share_large, 3), ", Small ", signif(b_ww * (1 - share_large), 3),
           " t/km2 -> ecopath_phytoplankton_B_PB.csv")
} else {
  .env_msg("[06_environment] NPP or phytoplankton carbon missing - no Ecopath phytoplankton values.")
}, error = function(e) .env_msg(conditionMessage(e)))
.env_tick("done")
