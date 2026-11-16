## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: 05b_ecospace_drivers.R
## =================================================================
## 05b_ecospace_drivers.R
## Created by: Daniel Vilas
##
## Ecospace environmental drivers: monthly ASCII maps on YOUR Ecospace
## base map. Give a depth ASCII grid (ESRI .asc, any resolution, any
## extent inside the Western Med box; any projection when a .prj sits
## next to it, else lon/lat WGS84) and every monthly map of 05 is
## regridded onto it, cell for cell, ready to load in Ecospace
## (Spatial-temporal data framework, or Map > Import).
##
## INPUT - the base map, either
##   ECOSPACE_CELL_SIZE    a cell size in degrees (e.g. 1/12 = 5 arc-min):
##                         05b MAKES the base map from the EMODnet
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
##   <env>/<var>_monthly_native.tif   written by 05_environment.R (one layer
##                         per month, 1/24 deg). 05b does not download
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
##   historical/                    the Copernicus hindcast of 05, and one
##   ssp126/, ssp370/, ...          folder per climate scenario of 05s, each with
##     sst/ sbt/ sss/ pp/ phyc/     one folder per driver (surface and bottom
##                                  temperature, surface salinity, NPP,
##                                  phytoplankton carbon):
##     <drv>/<drv>_<YYYY>_<MM>.asc  same ncols, nrows, corner and cell size as
##                                  the depth file, NODATA -9999
##   ecospace_driver_index.csv      variable, unit, time_step (1 = first
##                                  month), year, month, file, water cells,
##                                  cells filled from neighbours / map mean,
##                                  map mean, min, max
##   ecospace_driver_mean_maps.png  mean map of each variable (check)
##   depth_used.asc                 the base map (depth > 0 = water; land 0) - the
##                                  Ecospace depth map to use with these drivers
##   (each folder also has its ecospace_driver_index.csv, check figure and a
##    copy of depth_used.asc)
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
  if (exists("TS_YEARS", envir = .GlobalEnv)) TS_YEARS else NULL)          # NULL = every month 05 made
.esp_default("ECOSPACE_DRIVER_STEP", "monthly")                            # "monthly" or "annual"
.esp_default("ECOSPACE_DRIVER_SCALING", "absolute")                        # "absolute" or "relative"
.esp_default("ECOSPACE_FILL_PASSES", 10)                                   # 3 x 3 neighbour passes for coastal gaps
.esp_default("ECOSPACE_DECIMALS", 4)
.esp_default("ENV_BASE_YEAR", if (exists("YEAR_ECOPATH", envir = .GlobalEnv)) as.integer(round(stats::median(YEAR_ECOPATH))) else 1995L)
.esp_default("ECOSPACE_ENV_DIR", file.path(out_dir, "environment"))       # where 05 wrote <var>_monthly_native.tif

.esp_units <- c(temp_surf = "deg C", sal_surf = "PSU", temp_bot = "deg C", npp = "g C m-2 d-1", phyto_c = "g C m-2")
## folder and file names of the drivers in the Ecospace maps
.esp_folder <- c(temp_surf = "sst", temp_bot = "sbt", sal_surf = "sss", npp = "pp", phyto_c = "phyc")
.esp_folder_of <- function(v) if (v %in% names(.esp_folder)) .esp_folder[[v]] else v
.esp_msg <- function(...) message(...)

## --- Native maps from 05 -------------------------------------------------
## The run folder first, then the main output folder (05 may have run there).
.env_dirs <- unique(c(ECOSPACE_ENV_DIR, if (exists("OUT_BASE_DIR", envir = .GlobalEnv)) file.path(OUT_BASE_DIR, "environment")))
.native <- function(v) { f <- file.path(.env_dirs, paste0(v, "_monthly_native.tif")); f[file.exists(f)][1] }
vars <- ECOSPACE_DRIVER_VARS[!is.na(vapply(ECOSPACE_DRIVER_VARS, .native, ""))]
if (!length(vars)) stop("[05b] No monthly maps from 05_environment.R in ", paste(.env_dirs, collapse = " or "),
                        " - run 05 (environment) first.", call. = FALSE)
if (length(setdiff(ECOSPACE_DRIVER_VARS, vars)))
  .esp_msg("[05b] Not made by 05 yet (skipped): ", paste(setdiff(ECOSPACE_DRIVER_VARS, vars), collapse = ", "))

## --- Base map --------------------------------------------------------------
.make_grid <- is.finite(suppressWarnings(as.numeric(ECOSPACE_CELL_SIZE))) && as.numeric(ECOSPACE_CELL_SIZE) > 0
if (.make_grid) {
  cs <- as.numeric(ECOSPACE_CELL_SIZE)
  if (!file.exists(ECOSPACE_BATHY_FILE)) stop("[05b] Bathymetry not found: ", ECOSPACE_BATHY_FILE, call. = FALSE)
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
  .esp_msg(sprintf("[05b] Base map made at %.4g deg (%.3g arc-min) from %s: water = >= %d%% of the cell below sea level%s.",
                   cs, cs * 60, basename(ECOSPACE_BATHY_FILE), round(100 * ECOSPACE_MIN_WATER_FRACTION),
                   if (isTRUE(ECOSPACE_CLIP_MODEL_AREA) && !is.null(.poly)) ", outside the model area = land" else ""))
} else {
if (!file.exists(ECOSPACE_DEPTH_FILE)) stop("[05b] Ecospace depth file not found: ", ECOSPACE_DEPTH_FILE, call. = FALSE)
depth <- terra::rast(ECOSPACE_DEPTH_FILE)
.prj <- sub("\\.asc$", ".prj", ECOSPACE_DEPTH_FILE, ignore.case = TRUE)
if (!file.exists(.prj) || is.na(terra::crs(depth)) || !nzchar(terra::crs(depth))) terra::crs(depth) <- "EPSG:4326"
}
.dv <- terra::values(depth, mat = FALSE)
if (sum(.dv < 0, na.rm = TRUE) > sum(.dv > 0, na.rm = TRUE)) {        # bathymetry (negative down) -> depth
  depth <- -depth; .esp_msg("[05b] Depth file has negative depths (bathymetry): read as depth = -value.")
}
water <- depth > 0
n_water <- as.integer(terra::global(water, "sum", na.rm = TRUE)[1, 1])
map_name <- if (.make_grid) paste0("grid_", gsub("\\.", "p", format(signif(as.numeric(ECOSPACE_CELL_SIZE) * 60, 4))), "arcmin") else
  gsub("[^A-Za-z0-9_-]", "_", sub("\\.asc$", "", basename(ECOSPACE_DEPTH_FILE), ignore.case = TRUE))
esp_dir <- file.path(ECOSPACE_ENV_DIR, "ecospace_drivers", map_name)
dir.create(esp_dir, recursive = TRUE, showWarnings = FALSE)
.esp_msg(sprintf("[05b] Base map %s: %d x %d cells of %.4g x %.4g (%s), %d water cells -> %s",
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
## One set of maps: native maps of 05 (native_of(v)) -> out_d. Relative
## scaling always uses the hindcast's base year (so scenarios are on the
## same scale); years = the years kept (NULL = all).
.esp_make <- function(native_of, out_d, years_keep, label = "") {
dir.create(out_d, recursive = TRUE, showWarnings = FALSE)
index <- list(); mean_maps <- list()
for (v in vars) {
  if (is.na(native_of(v))) next
  r <- terra::rast(native_of(v))
  yrs <- as.integer(substr(names(r), 1, 4)); mos <- as.integer(substr(names(r), 6, 7))
  keep <- if (is.null(years_keep)) rep(TRUE, length(yrs)) else yrs %in% years_keep
  if (!any(keep)) { .esp_msg("[05b] ", label, v, ": no months in the selected years - skipped."); next }
  method <- if (.cell_km(depth) >= .cell_km(r)) "average" else "bilinear"
  .esp_msg("[05b] ", label, v, ": ", sum(keep), " month(s), native ", signif(.cell_km(r), 3), " km -> base map ",
           signif(.cell_km(depth), 3), " km (", method, ")")
  ## relative scaling: the map mean of the hindcast base year's months
  rel_den <- NA_real_
  if (identical(ECOSPACE_DRIVER_SCALING, "relative")) {
    rb <- terra::rast(.native(v)); base_keep <- as.integer(substr(names(rb), 1, 4)) == ENV_BASE_YEAR
    if (!any(base_keep)) stop("[05b] ECOSPACE_DRIVER_SCALING = 'relative' needs the base year ", ENV_BASE_YEAR, " in the 05 maps.", call. = FALSE)
    b <- terra::project(rb[[which(base_keep)]], depth, method = method)
    rel_den <- mean(terra::global(terra::mask(b, water, maskvalues = c(NA, 0)), "mean", na.rm = TRUE)[, 1], na.rm = TRUE)
  }
  steps <- if (identical(ECOSPACE_DRIVER_STEP, "annual")) {
    lapply(sort(unique(yrs[keep])), function(y) list(y = y, m = NA_integer_, k = which(keep & yrs == y)))
  } else lapply(which(keep), function(k) list(y = yrs[k], m = mos[k], k = k))
  vf <- .esp_folder_of(v)
  vdir <- file.path(out_d, vf); dir.create(vdir, showWarnings = FALSE)
  acc <- NULL
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    src <- if (length(s$k) > 1) terra::app(r[[s$k]], mean, na.rm = TRUE) else r[[s$k]]
    a <- terra::project(src, depth, method = method)
    fl <- .fill_coast(a, ECOSPACE_FILL_PASSES)
    a <- fl$r
    if (is.finite(rel_den) && rel_den != 0) a <- a / rel_den
    fn <- if (is.na(s$m)) sprintf("%s_%d.asc", vf, s$y) else sprintf("%s_%d_%02d.asc", vf, s$y, s$m)
    .write_asc(a, file.path(vdir, fn))
    st <- terra::global(a, c("mean", "min", "max"), na.rm = TRUE)
    index[[length(index) + 1]] <- data.table(variable = v, unit = if (is.finite(rel_den)) paste0("relative to ", ENV_BASE_YEAR, " (map mean = 1)") else .esp_units[[v]],
                                             time_step = i, year = s$y, month = s$m, file = file.path(vf, fn), water_cells = n_water,
                                             filled_from_neighbours = fl$n_fill, filled_with_map_mean = fl$n_mean,
                                             mean = st$mean, min = st$min, max = st$max)
    acc <- if (is.null(acc)) a else acc + a
  }
  mean_maps[[v]] <- acc / length(steps)
}
index <- rbindlist(index)
if (!nrow(index)) return(invisible(NULL))
fwrite(index, file.path(out_d, "ecospace_driver_index.csv"))

## --- Check figure: mean map per variable -----------------------------------
tryCatch({
  png(file.path(out_d, "ecospace_driver_mean_maps.png"), width = 1800, height = 300 + 450 * ceiling(length(mean_maps) / 2), res = 150)
  op <- par(mfrow = c(ceiling(length(mean_maps) / 2), 2), mar = c(2, 2, 2.5, 4))
  for (v in names(mean_maps)) terra::plot(mean_maps[[v]], main = paste0(label, v, " (", unique(index[variable == v, unit]), "), mean of the time steps"))
  par(op); dev.off()
}, error = function(e) { try(dev.off(), silent = TRUE); .esp_msg("[05b] mean-map figure not drawn: ", conditionMessage(e)) })

.mx <- index[, .(steps = .N, max_filled = max(filled_from_neighbours + filled_with_map_mean), any_map_mean = sum(filled_with_map_mean)), by = variable]
.esp_msg("[05b] ", label, "Ecospace drivers written: ", paste(sprintf("%s %d %s step(s) (up to %d of %d water cells filled from neighbours%s)",
         .mx$variable, .mx$steps, ECOSPACE_DRIVER_STEP, .mx$max_filled, n_water,
         ifelse(.mx$any_map_mean > 0, ", some with the map mean", "")), collapse = "; "),
         ". Index: ", file.path(out_d, "ecospace_driver_index.csv"))
invisible(index)
}

## Layout: <base map>/historical/ (Copernicus hindcast of 05) and one
## folder per climate scenario of 05s (<base map>/ssp126/, ...), each with
## a folder per driver (sst, sbt, sss, pp, phyc), its own index and a copy
## of depth_used.asc, so each loads on its own in Ecospace.
unlink(file.path(esp_dir, c(vars, "scenarios")), recursive = TRUE)                  # older layouts
unlink(file.path(esp_dir, c("ecospace_driver_index.csv", "ecospace_driver_mean_maps.png")))
index <- .esp_make(.native, file.path(esp_dir, "historical"), ECOSPACE_DRIVER_YEARS, label = "historical ")
file.copy(file.path(esp_dir, "depth_used.asc"), file.path(esp_dir, "historical", "depth_used.asc"), overwrite = TRUE)

## climate scenarios (05s): <env>/scenarios/<scenario>/<var>_monthly_native.tif
## -> <base map>/<scenario>/ (same layout and index, every year it covers)
## Only the scenarios asked for in this run (ENV_SCENARIOS; none = historical only)
.scn_want <- tolower(gsub("[^a-z0-9]", "", tolower(get0("ENV_SCENARIOS", envir = .GlobalEnv, ifnotfound = character(0)))))
.scn_dirs <- list.dirs(file.path(.env_dirs[1], "scenarios"), recursive = FALSE)
.scn_dirs <- .scn_dirs[basename(.scn_dirs) %in% .scn_want]
for (sd in .scn_dirs) {
  sc <- basename(sd)
  nat <- function(v) { f <- file.path(sd, paste0(v, "_monthly_native.tif")); if (file.exists(f)) f else NA_character_ }
  if (all(is.na(vapply(vars, nat, "")))) next
  od <- file.path(esp_dir, sc)
  tryCatch({
    .esp_make(nat, od, NULL, label = paste0(sc, " "))
    file.copy(file.path(esp_dir, "depth_used.asc"), file.path(od, "depth_used.asc"), overwrite = TRUE)
  }, error = function(e) .esp_msg("[05b] scenario ", sc, " failed: ", conditionMessage(e)))
}

## =================================================================
## Habitat drivers (static): <base map>/habitat/
## =================================================================
## Depth (the base map) and the seabed habitats of EMODnet Seabed Habitats
## EUSeaMap 2025 (broad-scale habitat map, EUNIS 2019 classification;
## Vasquez et al. 2021, EMODnet Seabed Habitats), as the proportion of each
## water cell covered by the habitat (0-1; terra::rasterize(cover = TRUE)),
## the form of an Ecospace habitat layer:
##   seagrass       Posidonia oceanica MB252 / MB2522, Cymodocea nodosa
##                  MB5521 / MB5534 (the codes used in 01b)
##   coralligenous  MC151, MC251 (01b)
##   and by the substrate digit of the EUNIS 2019 code (third character:
##   M + zone letter + substrate; EUNIS marine habitat classification 2019):
##   rock (1), biogenic (2, other than seagrass), gravel (3, coarse
##   sediment), mixed (4, mixed sediment), sand (5), mud (6).
## Source file: the first of ECOSPACE_HABITAT_GDB found (EUSeaMap 2025 as in
## 01b, else EUSeaMap 2023; downloaded once from
## emodnet.ec.europa.eu/en/seabed-habitats, ~1 GB). Only the base map's box
## is read. The layers are written once per base map and reused while the
## depth file and the geodatabase do not change. Without the geodatabase
## only depth is written (the log says where to put it).
if (!exists("ECOSPACE_HABITAT")) ECOSPACE_HABITAT <- TRUE
if (!exists("ECOSPACE_HABITAT_GDB")) ECOSPACE_HABITAT_GDB <- unique(c(   # first one found is used
  if (exists("EUSEAMAP_2025_GDB")) EUSEAMAP_2025_GDB,
  file.path(pcloud_dir, "data", "Complementary data", "EUSeaMap_2025", "EUSeaMap_2025.gdb"),
  path.expand("~/Downloads/EUSeaMap_2025/EUSeaMap_2025.gdb"),
  file.path(pcloud_dir, "data", "Complementary data", "EUSeaMap_2023", "EUSeaMap_2023.gdb"),   # older release, used when 2025 is not there
  path.expand("~/Downloads/EUSeaMap_2023/EUSeaMap_2023.gdb")))
.hab_classes <- c("seagrass", "coralligenous", "rock", "biogenic", "gravel", "mixed", "sand", "mud")
.hab_class_of <- function(code) {
  cc <- toupper(sub("[^A-Za-z0-9].*$", "", trimws(as.character(code))))
  out <- rep(NA_character_, length(cc))
  out[grepl("^(MB252|MB5521|MB5534)", cc)] <- "seagrass"
  out[is.na(out) & grepl("^(MC151|MC251)", cc)] <- "coralligenous"
  dg <- substr(cc, 3, 3)
  lk <- c(`1` = "rock", `2` = "biogenic", `3` = "gravel", `4` = "mixed", `5` = "sand", `6` = "mud")
  i <- is.na(out) & grepl("^M[A-H]", cc) & dg %in% names(lk)
  out[i] <- lk[dg[i]]
  out
}
if (isTRUE(ECOSPACE_HABITAT)) tryCatch({
  hd <- file.path(esp_dir, "habitat"); dir.create(hd, showWarnings = FALSE)
  file.copy(file.path(esp_dir, "depth_used.asc"), file.path(hd, "depth.asc"), overwrite = TRUE)
  gdb <- ECOSPACE_HABITAT_GDB[file.exists(ECOSPACE_HABITAT_GDB)][1]
  hix <- file.path(hd, "habitat_index.csv")
  fresh <- !is.na(gdb) && file.exists(hix) && file.mtime(hix) > max(file.mtime(c(ECOSPACE_DEPTH_FILE, gdb))) &&
    isTRUE(any(fread(hix)$layer != "depth")) &&                       # a depth-only index (no geodatabase last time) is rebuilt
    all(file.exists(file.path(hd, paste0(.hab_classes, ".asc"))))
  if (is.na(gdb)) {
    .esp_msg("[05b] Habitat drivers: depth only - EUSeaMap not found (", paste(ECOSPACE_HABITAT_GDB, collapse = " or "),
             "; EMODnet Seabed Habitats, emodnet.ec.europa.eu/en/seabed-habitats).")
    fwrite(data.table(layer = "depth", file = "depth.asc", unit = "m", source = "Ecospace base map", water_cells = n_water,
                      cells_with_habitat = n_water, area_km2 = NA_real_), hix)
  } else if (fresh) {
    .esp_msg("[05b] Habitat drivers: kept from the previous run (", hd, ").")
  } else {
    old_s2 <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    e <- terra::ext(depth)
    bb <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4]), crs = sf::st_crs(4326))))
    li <- sf::st_layers(gdb); k <- grep("eusea", li$name, ignore.case = TRUE)
    lay <- if (length(k)) li$name[k][which.max(li$features[k])] else li$name[which.max(li$features)]
    flds <- names(sf::st_read(gdb, query = sprintf("SELECT * FROM \"%s\" WHERE FID = 1", lay), quiet = TRUE))
    fld <- c(grep("^EUNIS2019C$", flds, value = TRUE, ignore.case = TRUE), grep("EUNIS2019", flds, value = TRUE, ignore.case = TRUE))[1]
    if (is.na(fld)) stop("no EUNIS 2019 code field in ", lay, " (fields: ", paste(flds, collapse = ", "), ")")
    .esp_msg("[05b] Habitat drivers: reading ", basename(gdb), " (layer ", lay, ", field ", fld, ") in the base map's box...")
    hab <- sf::st_read(gdb, query = sprintf("SELECT %s FROM \"%s\"", fld, lay), wkt_filter = bb, quiet = TRUE)
    hab <- sf::st_transform(hab[!sf::st_is_empty(hab), ], terra::crs(depth))
    hab$cls <- .hab_class_of(hab[[fld]])
    cell_km2 <- terra::cellSize(depth, unit = "km")
    rows <- list(data.table(layer = "depth", file = "depth.asc", unit = "m", source = "Ecospace base map", water_cells = n_water,
                            cells_with_habitat = n_water, area_km2 = as.numeric(terra::global(terra::mask(cell_km2, water, maskvalues = c(NA, 0)), "sum", na.rm = TRUE)[1, 1])))
    for (h in .hab_classes) {
      sub <- hab[!is.na(hab$cls) & hab$cls == h, ]
      r <- if (nrow(sub)) terra::rasterize(terra::vect(sf::st_make_valid(sub)), depth, cover = TRUE) else depth * 0
      r <- terra::ifel(water, terra::ifel(is.na(r), 0, terra::clamp(r, 0, 1)), NA)
      .write_asc(r, file.path(hd, paste0(h, ".asc")))
      rows[[length(rows) + 1]] <- data.table(layer = h, file = paste0(h, ".asc"), unit = "proportion of the cell (0-1)",
                                             source = paste0(sub("\\.gdb$", "", basename(gdb)), ", ", fld, " ", switch(h,
                                               seagrass = "MB252/MB2522/MB5521/MB5534", coralligenous = "MC151/MC251",
                                               rock = "M?1", biogenic = "M?2 (not seagrass)", gravel = "M?3 (coarse sediment)",
                                               mixed = "M?4", sand = "M?5", mud = "M?6")),
                                             water_cells = n_water, cells_with_habitat = as.integer(terra::global(r > 0, "sum", na.rm = TRUE)[1, 1]),
                                             area_km2 = as.numeric(terra::global(r * cell_km2, "sum", na.rm = TRUE)[1, 1]))
    }
    hi <- rbindlist(rows); fwrite(hi, hix)
    .esp_msg("[05b] Habitat drivers (static) in ", hd, ": ", paste(sprintf("%s %.0f km2", hi$layer[-1], hi$area_km2[-1]), collapse = ", "), ".")
  }
}, error = function(e) .esp_msg("[05b] Habitat drivers not made: ", conditionMessage(e)))
