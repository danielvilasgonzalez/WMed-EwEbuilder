## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: 06b_ecospace_drivers.R
## =================================================================
## 06b_ecospace_drivers.R
## Created by: Daniel Vilas
##
## Ecospace environmental drivers: monthly ASCII maps on YOUR Ecospace
## base map. Give a depth ASCII grid (ESRI .asc, any resolution, any
## extent inside the Western Med box; any projection when a .prj sits
## next to it, else lon/lat WGS84) and every monthly map of 06 is
## regridded onto it, cell for cell, ready to load in Ecospace
## (Spatial-temporal data framework, or Map > Import).
##
## INPUT - the base map, either
##   ECOSPACE_CELL_SIZE    a cell size in degrees (e.g. 1/12 = 5 arc-min):
##                         06b MAKES the base map from the EMODnet
##                         bathymetry (ECOSPACE_BATHY_FILE, 1 arc-min):
##                           water fraction f = share of the bathymetry
##                           cells below sea level in the map cell;
##                           depth = mean depth of those cells when
##                           f >= ECOSPACE_MIN_WATER_FRACTION (0.5), else 0 (land)
##                         over ECOSPACE_EXTENT (xmin, xmax, ymin, ymax;
##                         default the bounding box of the model GSAs),
##                         cells outside the model GSAs = land
##                         (ECOSPACE_CLIP_MODEL_AREA). It is written as
##                         depth_used.asc: load THAT file as the Ecospace
##                         depth map, so the drivers match it cell for cell.
##   or (ECOSPACE_CELL_SIZE = NA, default)
##   ECOSPACE_DEPTH_FILE   your depth .asc (water depth in m; land = 0 or NODATA;
##                         negative depths (bathymetry) are read as -depth)
##   <env>/<var>_monthly_native.tif   written by 06_environment.R (one layer
##                         per month, 1/24 deg). 06b does not download
##                         anything, so it reruns in minutes for a new grid.
##
## METHOD, per variable and month
##   1. Regrid native -> base map: area mean of the native cells inside a
##      base-map cell when the base map is coarser ("average"), bilinear
##      when it is finer (terra::project; also reprojects).
##   2. Water cells of the base map left empty by the native land mask
##      (coast, narrow bays) are filled from their neighbours: mean of the
##      valid cells in a 3 x 3 window, repeated up to ECOSPACE_FILL_PASSES
##      times; any cell still empty gets the month's mean over the map
##      (counted in the index). Land stays NODATA.
##   3. Bottom temperature is only meaningful where depth > 0 (every water
##      cell); surface variables, NPP and phytoplankton carbon likewise.
##   4. ECOSPACE_DRIVER_SCALING: "absolute" (default; units of 06: deg C,
##      PSU, g C m-2 d-1, g C m-2) or "relative" (value / the map mean of
##      the base year, ENV_BASE_YEAR = 1, as the Ecosim forcing).
##   5. ECOSPACE_DRIVER_STEP: "monthly" (default) or "annual" (mean of the
##      months; NPP mean daily rate).
##
## OUTPUT (out_dir/environment/ecospace_drivers/<base map name>/)
##   <var>/<var>_<YYYY>_<MM>.asc   same ncols, nrows, corner and cell size as
##                                  the depth file, NODATA -9999
##   ecospace_driver_index.csv      variable, unit, time_step (1 = first
##                                  month), year, month, file, water cells,
##                                  cells filled from neighbours / map mean,
##                                  map mean, min, max
##   ecospace_driver_mean_maps.png  mean map of each variable (check)
##   depth_used.asc                 the base map (depth > 0 = water; land 0) - the
##                                  Ecospace depth map to use with these drivers
## =================================================================

suppressPackageStartupMessages({ library(data.table); library(terra) })
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE")

## --- Paths -------------------------------------------------------------
.this_dir_esp <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!exists("out_dir", envir = .GlobalEnv, inherits = FALSE) || !exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE)) {
  source(file.path(.this_dir_esp, "wmed_paths.R"))
  wmed_set_paths(git_dir = dirname(.this_dir_esp))
}

## --- Settings (set before sourcing to change) --------------------------
.esp_default <- function(name, value) if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) assign(name, value, envir = .GlobalEnv)
.esp_default("ECOSPACE_DEPTH_FILE", file.path(pcloud_dir, "data", "Bathymetry", "depth_wmed.asc"))
.esp_default("ECOSPACE_CELL_SIZE", NA_real_)                               # degrees; NA = the grid of ECOSPACE_DEPTH_FILE
.esp_default("ECOSPACE_EXTENT", NULL)                                      # c(xmin, xmax, ymin, ymax); NULL = model GSAs' box
.esp_default("ECOSPACE_BATHY_FILE", file.path(pcloud_dir, "data", "Bathymetry", "EMODnet_WMed_1arcmin.tif"))
.esp_default("ECOSPACE_MIN_WATER_FRACTION", 0.5)
.esp_default("ECOSPACE_CLIP_MODEL_AREA", TRUE)
.esp_default("ECOSPACE_DRIVER_VARS", c("temp_surf", "temp_bot", "sal_surf", "npp", "phyto_c"))
.esp_default("ECOSPACE_DRIVER_YEARS", if (exists("ENV_YEARS", envir = .GlobalEnv)) ENV_YEARS else
  if (exists("TS_YEARS", envir = .GlobalEnv)) TS_YEARS else NULL)          # NULL = every month 06 made
.esp_default("ECOSPACE_DRIVER_STEP", "monthly")                            # "monthly" or "annual"
.esp_default("ECOSPACE_DRIVER_SCALING", "absolute")                        # "absolute" or "relative"
.esp_default("ECOSPACE_FILL_PASSES", 10)                                   # 3 x 3 neighbour passes for coastal gaps
.esp_default("ECOSPACE_DECIMALS", 4)
.esp_default("ENV_BASE_YEAR", if (exists("YEAR_ECOPATH", envir = .GlobalEnv)) as.integer(round(stats::median(YEAR_ECOPATH))) else 1995L)
.esp_default("ECOSPACE_ENV_DIR", file.path(out_dir, "environment"))       # where 06 wrote <var>_monthly_native.tif

.esp_units <- c(temp_surf = "deg C", sal_surf = "PSU", temp_bot = "deg C", npp = "g C m-2 d-1", phyto_c = "g C m-2")
.esp_msg <- function(...) message(...)

## --- Native maps from 06 -------------------------------------------------
## The run folder first, then the main output folder (06 may have run there).
.env_dirs <- unique(c(ECOSPACE_ENV_DIR, if (exists("OUT_BASE_DIR", envir = .GlobalEnv)) file.path(OUT_BASE_DIR, "environment")))
.native <- function(v) { f <- file.path(.env_dirs, paste0(v, "_monthly_native.tif")); f[file.exists(f)][1] }
vars <- ECOSPACE_DRIVER_VARS[!is.na(vapply(ECOSPACE_DRIVER_VARS, .native, ""))]
if (!length(vars)) stop("[06b] No monthly maps from 06_environment.R in ", paste(.env_dirs, collapse = " or "),
                        " - run 06 (environment) first.", call. = FALSE)
if (length(setdiff(ECOSPACE_DRIVER_VARS, vars)))
  .esp_msg("[06b] Not made by 06 yet (skipped): ", paste(setdiff(ECOSPACE_DRIVER_VARS, vars), collapse = ", "))

## --- Base map --------------------------------------------------------------
.make_grid <- is.finite(suppressWarnings(as.numeric(ECOSPACE_CELL_SIZE))) && as.numeric(ECOSPACE_CELL_SIZE) > 0
if (.make_grid) {
  cs <- as.numeric(ECOSPACE_CELL_SIZE)
  if (!file.exists(ECOSPACE_BATHY_FILE)) stop("[06b] Bathymetry not found: ", ECOSPACE_BATHY_FILE, call. = FALSE)
  bathy <- terra::rast(ECOSPACE_BATHY_FILE); if (is.na(terra::crs(bathy)) || !nzchar(terra::crs(bathy))) terra::crs(bathy) <- "EPSG:4326"
  ## model area: the run's GSAs (or the custom region's box)
  .poly <- tryCatch({
    if (exists("AREA_MODE", envir = .GlobalEnv) && identical(AREA_MODE, "custom") && exists("CUSTOM_BBOX", envir = .GlobalEnv)) {
      b <- CUSTOM_BBOX; terra::as.polygons(terra::ext(b[["xmin"]], b[["xmax"]], b[["ymin"]], b[["ymax"]]), crs = "EPSG:4326")
    } else {
      g <- terra::vect(file.path(if (exists("git_dir", envir = .GlobalEnv)) git_dir else dirname(.this_dir_esp), "scripts", "gsa_boundaries_westmed.geojson"))
      gs <- if (exists("MODEL_GSAS", envir = .GlobalEnv)) MODEL_GSAS else 1:12
      g[g$gsa_num %in% gs, ]
    }
  }, error = function(e) NULL)
  ex <- if (!is.null(ECOSPACE_EXTENT)) terra::ext(ECOSPACE_EXTENT) else if (!is.null(.poly)) terra::ext(.poly) else terra::ext(bathy)
  ## snap the extent to whole cells
  ex <- terra::ext(floor(ex$xmin / cs) * cs, ceiling(ex$xmax / cs) * cs, floor(ex$ymin / cs) * cs, ceiling(ex$ymax / cs) * cs)
  tmpl <- terra::rast(ex, resolution = cs, crs = "EPSG:4326")
  sea <- bathy < 0
  dpos <- terra::ifel(sea, -bathy, NA)
  m <- if (cs >= mean(terra::res(bathy))) "average" else "bilinear"
  frac <- terra::project(sea * 1, tmpl, method = m)
  dmean <- terra::project(dpos, tmpl, method = m)
  depth <- terra::ifel(!is.na(frac) & frac >= ECOSPACE_MIN_WATER_FRACTION & !is.na(dmean), dmean, 0)
  if (isTRUE(ECOSPACE_CLIP_MODEL_AREA) && !is.null(.poly)) {
    inside <- terra::rasterize(.poly, tmpl, field = 1, touches = FALSE)
    depth <- terra::ifel(is.na(inside), 0, depth)
  }
  names(depth) <- "depth"
  .esp_msg(sprintf("[06b] Base map made at %.4g deg (%.3g arc-min) from %s: water = >= %d%% of the cell below sea level%s.",
                   cs, cs * 60, basename(ECOSPACE_BATHY_FILE), round(100 * ECOSPACE_MIN_WATER_FRACTION),
                   if (isTRUE(ECOSPACE_CLIP_MODEL_AREA) && !is.null(.poly)) ", outside the model area = land" else ""))
} else {
if (!file.exists(ECOSPACE_DEPTH_FILE)) stop("[06b] Ecospace depth file not found: ", ECOSPACE_DEPTH_FILE, call. = FALSE)
depth <- terra::rast(ECOSPACE_DEPTH_FILE)
.prj <- sub("\\.asc$", ".prj", ECOSPACE_DEPTH_FILE, ignore.case = TRUE)
if (!file.exists(.prj) || is.na(terra::crs(depth)) || !nzchar(terra::crs(depth))) terra::crs(depth) <- "EPSG:4326"
}
.dv <- terra::values(depth, mat = FALSE)
if (sum(.dv < 0, na.rm = TRUE) > sum(.dv > 0, na.rm = TRUE)) {        # bathymetry (negative down) -> depth
  depth <- -depth; .esp_msg("[06b] Depth file has negative depths (bathymetry): read as depth = -value.")
}
water <- depth > 0
n_water <- as.integer(terra::global(water, "sum", na.rm = TRUE)[1, 1])
map_name <- if (.make_grid) paste0("grid_", gsub("\\.", "p", format(signif(as.numeric(ECOSPACE_CELL_SIZE) * 60, 4))), "arcmin") else
  gsub("[^A-Za-z0-9_-]", "_", sub("\\.asc$", "", basename(ECOSPACE_DEPTH_FILE), ignore.case = TRUE))
esp_dir <- file.path(ECOSPACE_ENV_DIR, "ecospace_drivers", map_name)
dir.create(esp_dir, recursive = TRUE, showWarnings = FALSE)
.esp_msg(sprintf("[06b] Base map %s: %d x %d cells of %.4g x %.4g (%s), %d water cells -> %s",
                 if (.make_grid) map_name else basename(ECOSPACE_DEPTH_FILE), terra::ncol(depth), terra::nrow(depth), terra::res(depth)[1], terra::res(depth)[2],
                 if (terra::is.lonlat(depth)) "degrees" else "map units", n_water, esp_dir))
terra::writeRaster(terra::ifel(is.na(depth), 0, depth), file.path(esp_dir, "depth_used.asc"),
                   filetype = "AAIGrid", NAflag = -9999, overwrite = TRUE, gdal = "DECIMAL_PRECISION=1")
unlink(file.path(esp_dir, "depth_used.prj"))

## --- Helpers ---------------------------------------------------------------
## coarser base map -> area mean; finer -> bilinear (compared in metres)
.cell_km <- function(r) {
  if (terra::is.lonlat(r)) { e <- terra::ext(r); mean(terra::res(r)) * 111.32 * cos(mean(c(e$ymin, e$ymax)) * pi / 180) }
  else mean(terra::res(r)) / 1000
}
.fill_coast <- function(a, passes) {
  n_fill <- 0L
  for (p in seq_len(passes)) {
    gap <- is.na(a) & water
    if (!as.logical(terra::global(gap, "max", na.rm = TRUE)[1, 1])) break
    f <- terra::focal(a, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
    n_fill <- n_fill + as.integer(terra::global(gap & !is.na(f), "sum", na.rm = TRUE)[1, 1])
    a <- terra::cover(a, terra::mask(f, water, maskvalues = c(NA, 0)))
  }
  gap <- is.na(a) & water
  n_mean <- as.integer(terra::global(gap, "sum", na.rm = TRUE)[1, 1])
  if (n_mean > 0) {
    mu <- terra::global(terra::mask(a, water, maskvalues = c(NA, 0)), "mean", na.rm = TRUE)[1, 1]
    a <- terra::ifel(gap, mu, a)
  }
  list(r = terra::mask(a, water, maskvalues = c(NA, 0)), n_fill = n_fill, n_mean = n_mean)
}
.write_asc <- function(a, f) {
  terra::writeRaster(a, f, filetype = "AAIGrid", NAflag = -9999, overwrite = TRUE,
                     gdal = paste0("DECIMAL_PRECISION=", ECOSPACE_DECIMALS))
  unlink(sub("\\.asc$", ".prj", f))      # EwE reads the .asc header only
}

## --- Regrid, fill and write ----------------------------------------------
index <- list(); mean_maps <- list()
for (v in vars) {
  r <- terra::rast(.native(v))
  yrs <- as.integer(substr(names(r), 1, 4)); mos <- as.integer(substr(names(r), 6, 7))
  keep <- if (is.null(ECOSPACE_DRIVER_YEARS)) rep(TRUE, length(yrs)) else yrs %in% ECOSPACE_DRIVER_YEARS
  base_keep <- yrs == ENV_BASE_YEAR
  if (!any(keep)) { .esp_msg("[06b] ", v, ": no months in the selected years - skipped."); next }
  method <- if (.cell_km(depth) >= .cell_km(r)) "average" else "bilinear"
  .esp_msg("[06b] ", v, ": ", sum(keep), " month(s), native ", signif(.cell_km(r), 3), " km -> base map ",
           signif(.cell_km(depth), 3), " km (", method, ")")
  ## relative scaling: the map mean of the base year's months
  rel_den <- NA_real_
  if (identical(ECOSPACE_DRIVER_SCALING, "relative")) {
    if (!any(base_keep)) stop("[06b] ECOSPACE_DRIVER_SCALING = 'relative' needs the base year ", ENV_BASE_YEAR, " in the 06 maps.", call. = FALSE)
    b <- terra::project(r[[which(base_keep)]], depth, method = method)
    rel_den <- mean(terra::global(terra::mask(b, water, maskvalues = c(NA, 0)), "mean", na.rm = TRUE)[, 1], na.rm = TRUE)
  }
  steps <- if (identical(ECOSPACE_DRIVER_STEP, "annual")) {
    lapply(sort(unique(yrs[keep])), function(y) list(y = y, m = NA_integer_, k = which(keep & yrs == y)))
  } else lapply(which(keep), function(k) list(y = yrs[k], m = mos[k], k = k))
  vdir <- file.path(esp_dir, v); dir.create(vdir, showWarnings = FALSE)
  acc <- NULL
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    src <- if (length(s$k) > 1) terra::app(r[[s$k]], mean, na.rm = TRUE) else r[[s$k]]
    a <- terra::project(src, depth, method = method)
    fl <- .fill_coast(a, ECOSPACE_FILL_PASSES)
    a <- fl$r
    if (is.finite(rel_den) && rel_den != 0) a <- a / rel_den
    fn <- if (is.na(s$m)) sprintf("%s_%d.asc", v, s$y) else sprintf("%s_%d_%02d.asc", v, s$y, s$m)
    .write_asc(a, file.path(vdir, fn))
    st <- terra::global(a, c("mean", "min", "max"), na.rm = TRUE)
    index[[length(index) + 1]] <- data.table(variable = v, unit = if (is.finite(rel_den)) paste0("relative to ", ENV_BASE_YEAR, " (map mean = 1)") else .esp_units[[v]],
                                             time_step = i, year = s$y, month = s$m, file = file.path(v, fn), water_cells = n_water,
                                             filled_from_neighbours = fl$n_fill, filled_with_map_mean = fl$n_mean,
                                             mean = st$mean, min = st$min, max = st$max)
    acc <- if (is.null(acc)) a else acc + a
  }
  mean_maps[[v]] <- acc / length(steps)
}
index <- rbindlist(index)
fwrite(index, file.path(esp_dir, "ecospace_driver_index.csv"))

## --- Check figure: mean map per variable -----------------------------------
tryCatch({
  png(file.path(esp_dir, "ecospace_driver_mean_maps.png"), width = 1800, height = 300 + 450 * ceiling(length(mean_maps) / 2), res = 150)
  op <- par(mfrow = c(ceiling(length(mean_maps) / 2), 2), mar = c(2, 2, 2.5, 4))
  for (v in names(mean_maps)) terra::plot(mean_maps[[v]], main = paste0(v, " (", unique(index[variable == v, unit]), "), mean of the time steps"))
  par(op); dev.off()
}, error = function(e) { try(dev.off(), silent = TRUE); .esp_msg("[06b] mean-map figure not drawn: ", conditionMessage(e)) })

.mx <- index[, .(steps = .N, max_filled = max(filled_from_neighbours + filled_with_map_mean), any_map_mean = sum(filled_with_map_mean)), by = variable]
.esp_msg("[06b] Ecospace drivers written: ", paste(sprintf("%s %d %s step(s) (up to %d of %d water cells filled from neighbours%s)",
         .mx$variable, .mx$steps, ECOSPACE_DRIVER_STEP, .mx$max_filled, n_water,
         ifelse(.mx$any_map_mean > 0, ", some with the map mean", "")), collapse = "; "),
         ". Index: ", file.path(esp_dir, "ecospace_driver_index.csv"))
