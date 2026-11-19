## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: 05s_environment_scenarios.R
## =================================================================
## 05s_environment_scenarios.R
## Created by: Daniel Vilas
##
## Future scenarios of the environmental drivers: the monthly maps of 05
## (Copernicus hindcast) continued to ENV_SCEN_END under each climate
## scenario in ENV_SCENARIOS, for Ecosim (forcing series) and, through
## 05b, for Ecospace (driver maps on the base map). Sourced at the end of
## 05_environment.R when ENV_SCENARIOS is not empty (it uses 05's maps,
## model-area mask and helpers).
##
## SOURCE
##   ISIMIP3b ocean climate input data (Frieler et al. 2024, Geosci Model
##   Dev 17: 1-51; files.isimip.org, "uncorrected", monthly, 1 deg):
##   CMIP6 Earth system models ENV_SCEN_MODELS (GFDL-ESM4, IPSL-CM6A-LR),
##   experiment "historical" (1850-2014) and each SSP (2015-2100).
##   Variables: tos (temp_surf), tob (temp_bot), so-surf / sos
##   (sal_surf), intpp (npp). Only the model-area box is taken: the
##   ISIMIP files service cuts it out (select_bbox); if that fails the
##   whole global file is downloaded once and cropped. Cut-outs are kept
##   in ISIMIP3B_CACHE_DIR, so later runs download nothing.
##   ISIMIP3b has SSP1-2.6, SSP3-7.0 and SSP5-8.5; a scenario with no
##   files (e.g. SSP2-4.5 for most models) is reported and skipped.
##
## METHOD (delta / change-factor method, per model, variable and month)
##   Reference period R = ENV_SCEN_REF_YEARS (default 2000-2014): the
##   overlap of the reference of each variable (05: satellite L4 SST,
##   MedPHY salinity and bottom T, MedBFM NPP) with the ISIMIP3b
##   historical runs, which end in 2014 - so the monthly statistics are
##   the same for every SSP. The hindcast warmed more than the models
##   after 2014; the continuity step (3c) makes each scenario start from
##   the last hindcast year, so that gap does not show as a jump.
##   NULL = the last ENV_SCEN_REF_N = 20 years of the hindcast (then the
##   models' years after 2014 come from each SSP).
##   Reference of each variable, and the label of its scenario:
##   environment_scenarios_reference.csv (bottom T: no observation-based
##   reference - delta on the reanalysis, a stated assumption).
##   1. Model anomaly of each future month (year y, month m):
##        temperature, salinity: A(y,m) = G(y,m) - mean_R G(., m)
##        NPP:                   A(y,m) = G(y,m) / mean_R G(., m),
##                               held to [1/ENV_SCEN_RATIO_MAX, ENV_SCEN_RATIO_MAX]
##      with G the model field (historical + SSP joined at 2014/2015).
##      Model cells next to the coast without values are filled from
##      their neighbours (3 x 3 mean, up to 5 passes).
##   2. Ensemble: mean of the models' anomalies (NPP: mean ratio), then
##      smoothed along the years, each calendar month and cell apart, with
##      a centred running mean of ENV_SCEN_SMOOTH_YEARS (11) years: two
##      model runs carry their own year-to-year noise (one run's warm year
##      can put SSP5-8.5 below SSP1-2.6 in a given year), so the scenario
##      keeps the models' trend and seasonal change, not their noise.
##      ENV_SCEN_SMOOTH_YEARS = 0 keeps every model year.
##   3. Bias adjustment, per map cell and calendar month (spatial and
##      seasonal), with the Copernicus hindcast H as the reference:
##      a. Mean: regrid the anomaly to the 05 map grid (bilinear) and add
##         it to the hindcast climatology of that month over R (NPP:
##         multiply): X(y,m) = mean_R H(., m) + A(y,m). The model's own
##         bias cancels (its change is taken on its own climatology), so
##         X keeps the hindcast's level, spatial pattern and seasonal cycle.
##      b. Variability (ENV_SCEN_VAR_SCALE, only when not smoothed): the
##         detrended year-to-year spread of A over R is scaled to that of
##         H (factor held to [1/ENV_SCEN_VAR_MAX, ENV_SCEN_VAR_MAX]); the
##         per-cell linear trend of A is kept (trend-preserving scaling).
##      c. Continuity (ENV_SCEN_JOIN): D(m) = H - X in the last hindcast
##         year (ENV_SCEN_JOIN_YEARS = 1; more years = their mean; NPP:
##         ratio), per cell and month, split into a systematic part Ds =
##         hindcast trend line - model trend line over R, at that year
##         (the hindcast warming faster or slower than the models; kept
##         for the whole scenario) and the rest D - Ds (that year's
##         anomaly, e.g. a marine heatwave), whose weight falls linearly
##         from 1 in the last hindcast year to 0 ENV_SCEN_BLEND_YEARS (10)
##         later (0 = kept throughout). The scenario continues from the
##         last hindcast year, keeps the observed level of the trend and
##         takes the models' change from there.
##      The step left at the join (last hindcast year(s) vs as many first
##      scenario years, model-area mean) is written to
##      environment_scenarios_continuity.csv and to the log.
##   Years: from the year after the hindcast to ENV_SCEN_END.
##   ASSUMPTION (flagged): the hindcast pattern is kept; interannual
##   variability comes from the models only.
##
## OUTPUT (out_dir/environment/scenarios/<scenario>/)
##   environment_scenarios_continuity.csv  step at the hindcast -> scenario join
##   <var>_monthly_native.tif     monthly maps (same grid as 05), read by
##                                05b for the Ecospace maps of the scenario
##   ecosim_forcing_<var>_monthly.csv / _annual.csv
##                                hindcast + scenario, relative to
##                                ENV_BASE_YEAR (= 1), as 05's forcing files
## and in out_dir/environment/scenarios/:
##   environment_scenarios_monthly.csv / _annual.csv  area mean and
##                                10-90 % range per scenario, variable, year
##   environment_scenarios_models.csv  which model files each scenario used
##   plots/environment/environment_scenarios.png  hindcast and scenarios
##
## References
##   Frieler K et al. (2024) Scenario setup and forcing data for impact
##     model evaluation and impact attribution within the third round of
##     the Inter-Sectoral Model Intercomparison Project (ISIMIP3a). Geosci
##     Model Dev 17: 1-51; ISIMIP3b protocol, isimip.org.
##   Lange S (2019) Trend-preserving bias adjustment and statistical
##     downscaling with ISIMIP3BASD. Geosci Model Dev 12: 3055-3070
##     (delta and change-factor methods).
## =================================================================

.scn_default <- function(name, value) if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) assign(name, value, envir = .GlobalEnv)
.scn_default("ENV_SCENARIOS", character(0))                       # e.g. c("ssp126", "ssp245", "ssp370", "ssp585")
.scn_default("ENV_SCEN_END", 2050L)                               # last year of the scenarios
.scn_default("ENV_SCEN_MODELS", c("GFDL-ESM4", "IPSL-CM6A-LR"))   # ISIMIP3b climate models
.scn_default("ENV_SCEN_REF_YEARS", 2000:2014)                     # overlap period of the bias adjustment: hindcast reference x ISIMIP3b historical (ends 2014); NULL = the last ENV_SCEN_REF_N years of the hindcast
.scn_default("ENV_SCEN_REF_N", 20L)
.scn_default("ENV_SCEN_SMOOTH_YEARS", 11L)                        # running mean of the model change (years); 0 = none
.scn_default("ENV_SCEN_VARS", c("temp_surf", "temp_bot", "sal_surf", "npp"))
.scn_default("ENV_SCEN_RATIO_MAX", 4)                            # NPP change factor held to [1/4, 4]
.scn_default("ENV_SCEN_VAR_SCALE", TRUE)                         # scale the model's inter-annual spread to the hindcast (only when ENV_SCEN_SMOOTH_YEARS <= 1)
.scn_default("ENV_SCEN_VAR_MAX", 2)                               # variance factor held to [1/2, 2]
.scn_default("ENV_SCEN_JOIN", TRUE)                               # continuity adjustment at the hindcast -> scenario join
.scn_default("ENV_SCEN_JOIN_YEARS", 1L)                           # last hindcast years the join is matched on (1 = the scenario continues from the last hindcast year)
.scn_default("ENV_SCEN_BLEND_YEARS", 10L)                         # years over which the join offset fades out; 0 = kept throughout
.scn_default("ENV_SCEN_VARIABILITY", "observed")                  # with the smoothed change: "observed" replays the hindcast's detrended year-to-year anomalies (delta change); "none" = forced change only
.scn_default("ISIMIP3B_CACHE_DIR", file.path(out_dir, "isimip3b_cache"))
.scn_default("ISIMIP_FILES_API", "https://files.isimip.org/api/v2")
.scn_default("ISIMIP_FILES_URL", "https://files.isimip.org")

.scn_msg <- function(...) message("[05s] ", ...)
.scn_isimip_var <- list(temp_surf = c("tos"), temp_bot = c("tob"), sal_surf = c("so-surf", "sos"), npp = c("intpp"))
.scn_type <- c(temp_surf = "add", temp_bot = "add", sal_surf = "add", npp = "ratio")
.scn_label <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")

ENV_SCENARIOS <- unique(tolower(gsub("[^a-z0-9]", "", tolower(ENV_SCENARIOS))))
scen_dir <- file.path(env_dir, "scenarios")
dir.create(scen_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ISIMIP3B_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

## --- ISIMIP3b files ---------------------------------------------------------
.scn_path <- function(model, exp, var) {
  yrs <- if (exp == "historical") "1850_2014" else "2015_2100"
  sprintf("ISIMIP3b/InputData/climate/ocean/uncorrected/global/monthly/%s/%s/%s_r1i1p1f1_%s_%s_60arcmin_global_monthly_%s.nc",
          exp, model, tolower(model), exp, var, yrs)
}
.scn_box <- function() { b <- ENV_BBOX; c(west = b[["lon_min"]] - 2, east = b[["lon_max"]] + 2, south = b[["lat_min"]] - 2, north = b[["lat_max"]] + 2) }

## monthly layers named "YYYY-MM" (by position: the files are complete monthly series)
.scn_name_layers <- function(r, exp) {
  y0 <- if (exp == "historical") 1850L else 2015L
  n <- terra::nlyr(r); n_full <- if (exp == "historical") 165L * 12L else 86L * 12L
  tt <- tryCatch(terra::time(r), error = function(e) NULL)
  if (n != n_full && !is.null(tt) && !anyNA(tt)) {          # not the full series: use the file's own dates
    names(r) <- format(as.Date(tt), "%Y-%m"); return(r)
  }
  names(r) <- sprintf("%d-%02d", y0 + (seq_len(n) - 1L) %/% 12L, (seq_len(n) - 1L) %% 12L + 1L)
  r
}
.scn_read_nc <- function(nc, var, exp) {
  r <- tryCatch(terra::rast(nc, subds = sub("-.*$", "", var)), error = function(e) terra::rast(nc))
  if (terra::nlyr(r) %% 12L != 0L) stop("unexpected number of months (", terra::nlyr(r), ") in ", basename(nc))
  if (is.na(terra::crs(r)) || !nzchar(terra::crs(r))) terra::crs(r) <- "EPSG:4326"
  b <- .scn_box()
  r <- terra::crop(r, terra::ext(b[["west"]], b[["east"]], b[["south"]], b[["north"]]))
  .scn_name_layers(r, exp)
}

## 1. cut-out by the ISIMIP files service (only the box is downloaded)
.scn_cutout <- function(path, var, exp, dest) tryCatch({
  b <- .scn_box()
  body <- list(paths = list(path), operations = list(list(operation = "select_bbox", bbox = unname(c(b[["west"]], b[["east"]], b[["south"]], b[["north"]])))))
  t0 <- Sys.time()
  repeat {
    res <- httr::POST(ISIMIP_FILES_API, body = body, encode = "json", httr::timeout(120))
    j <- httr::content(res, as = "parsed", type = "application/json")
    if (httr::status_code(res) >= 400 || identical(j$status, "failed"))
      stop("service answered ", httr::status_code(res), ": ", paste(unlist(if (!is.null(j$errors)) j$errors else j), collapse = " "))
    if (identical(j$status, "finished") && !is.null(j$file_url)) break
    if (as.numeric(difftime(Sys.time(), t0, units = "mins")) > 30) stop("cut-out not ready after 30 min")
    Sys.sleep(10)
  }
  zf <- tempfile(fileext = ".zip"); on.exit(unlink(zf), add = TRUE)
  utils::download.file(j$file_url, zf, mode = "wb", quiet = TRUE)
  ex <- tempfile(); dir.create(ex); on.exit(unlink(ex, recursive = TRUE), add = TRUE)
  utils::unzip(zf, exdir = ex)
  nc <- list.files(ex, "\\.nc$", full.names = TRUE, recursive = TRUE)[1]
  if (is.na(nc)) stop("no netCDF in the cut-out")
  terra::writeRaster(.scn_read_nc(nc, var, exp), dest, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  dest
}, error = function(e) { .scn_msg("  cut-out of ", basename(path), " not available (", conditionMessage(e), ") - trying the whole file."); NULL })

## 2. the whole global file, cropped and deleted
.scn_full <- function(path, var, exp, dest) tryCatch({
  tmp <- file.path(ISIMIP3B_CACHE_DIR, basename(path)); on.exit(unlink(tmp), add = TRUE)
  .scn_msg("  downloading ", basename(path), " (whole global file, one-off; it is cropped and deleted)...")
  op <- options(timeout = max(3600, getOption("timeout"))); on.exit(options(op), add = TRUE)
  utils::download.file(paste(ISIMIP_FILES_URL, path, sep = "/"), tmp, mode = "wb", quiet = TRUE)
  terra::writeRaster(.scn_read_nc(tmp, var, exp), dest, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  dest
}, error = function(e) { .scn_msg("  ", basename(path), " not available (", conditionMessage(e), ")."); NULL })

## model field of a variable for one experiment: the cached cut-out, else download
.scn_get <- function(model, exp, v) {
  for (iv in .scn_isimip_var[[v]]) {
    dest <- file.path(ISIMIP3B_CACHE_DIR, sprintf("%s_%s_%s_wmed.tif", tolower(model), exp, iv))
    if (file.exists(dest)) {
      r <- terra::rast(dest)
      if (anyNA(suppressWarnings(as.integer(substr(names(r), 1, 4))))) r <- .scn_name_layers(r, exp)
      return(r)
    }
    path <- .scn_path(model, exp, iv)
    f <- .scn_cutout(path, iv, exp, dest)
    if (is.null(f)) f <- .scn_full(path, iv, exp, dest)
    if (!is.null(f)) return(terra::rast(f))
  }
  NULL
}

## fill model cells without values next to the coast (3 x 3 mean, 5 passes)
.scn_fill <- function(r, passes = 5) {
  for (p in seq_len(passes)) {
    if (!any(is.na(terra::values(r[[1]], mat = FALSE)))) break
    f <- terra::focal(r, w = 3, fun = "mean", na.rm = TRUE, na.policy = "only")
    r <- terra::cover(r, f)
  }
  r
}
.scn_ym <- function(r) list(y = as.integer(substr(names(r), 1, 4)), m = as.integer(substr(names(r), 6, 7)))

## =================================================================
## Scenario maps
## =================================================================
.scn_native <- function(v) { f <- file.path(env_dir, paste0(v, "_monthly_native.tif")); if (file.exists(f)) f else NA_character_ }
scn_vars <- ENV_SCEN_VARS[!is.na(vapply(ENV_SCEN_VARS, .scn_native, ""))]
scn_used <- list(); scn_ts <- list(); scn_join <- list(); scn_chk <- list()
if (!length(scn_vars)) {
  .scn_msg("No monthly maps of 05 to continue - scenarios skipped.")
} else for (v in scn_vars) {
  H <- terra::rast(.scn_native(v)); hym <- .scn_ym(H)
  full_y <- as.integer(names(which(table(hym$y) == 12)))           # complete hindcast years
  ref <- if (is.null(ENV_SCEN_REF_YEARS)) utils::tail(sort(full_y), ENV_SCEN_REF_N) else intersect(ENV_SCEN_REF_YEARS, full_y)
  tw <- utils::tail(sort(full_y), ENV_SCEN_REF_N)                    # recent window: join trends and the observed year-to-year anomalies
  if (length(ref) < 5) { .scn_msg(v, ": the hindcast covers only ", length(ref), " year(s) of the reference period - skipped."); next }
  yrs_f <- (max(hym$y) + 1L):as.integer(ENV_SCEN_END)
  if (max(hym$y) >= ENV_SCEN_END) { .scn_msg(v, ": the hindcast already reaches ", ENV_SCEN_END, " - nothing to add."); next }
  Hclim <- terra::rast(lapply(1:12, function(m) terra::app(H[[which(hym$y %in% ref & hym$m == m)]], mean, na.rm = TRUE)))
  hist_fields <- setNames(lapply(ENV_SCEN_MODELS, function(mod) .scn_get(mod, "historical", v)), ENV_SCEN_MODELS)
  for (sc in ENV_SCENARIOS) {
    anoms <- list()
    nsm <- max(0L, as.integer(ENV_SCEN_SMOOTH_YEARS)); half <- nsm %/% 2L
    yrs_out <- sort(unique(c(ref, tw, yrs_f)))                         # reference years + join window + scenario years
    yrs_calc <- if (nsm > 1) (min(yrs_out) - half):(max(yrs_out) + half) else yrs_out   # margin so the running mean is centred
    for (mod in ENV_SCEN_MODELS) {
      Gh <- hist_fields[[mod]]; Gs <- .scn_get(mod, sc, v)
      if (is.null(Gh) || is.null(Gs)) next
      G <- c(Gh, Gs); g <- .scn_ym(G)
      Gclim <- lapply(1:12, function(m) terra::app(G[[which(g$y %in% ref & g$m == m)]], mean, na.rm = TRUE))
      k <- which(g$y %in% yrs_calc)
      A <- terra::rast(lapply(k, function(i) {
        cl <- Gclim[[g$m[i]]]
        if (.scn_type[[v]] == "ratio") terra::clamp(G[[i]] / cl, 1 / ENV_SCEN_RATIO_MAX, ENV_SCEN_RATIO_MAX, values = TRUE) else G[[i]] - cl
      }))
      names(A) <- names(G)[k]
      anoms[[mod]] <- .scn_fill(A)
      scn_used[[length(scn_used) + 1]] <- data.table(scenario = sc, variable = v, model = mod, isimip_files = paste(names(Gh)[1], "...", names(Gs)[terra::nlyr(Gs)]))
    }
    if (!length(anoms)) {
      .scn_msg(v, " ", sc, ": no ISIMIP3b files for any model (", paste(ENV_SCEN_MODELS, collapse = ", "), ") - skipped.")
      next
    }
    A <- if (length(anoms) == 1) anoms[[1]] else Reduce(`+`, anoms) / length(anoms)
    if (nsm > 1) {                                                     # running mean per calendar month (model noise out, trend kept)
      am <- as.integer(substr(names(A), 6, 7)); nm <- names(A)
      for (m in 1:12) { kk <- which(am == m); if (length(kk) > 2) A[[kk]] <- terra::roll(A[[kk]], n = min(nsm, 2L * ((length(kk) - 1L) %/% 2L) + 1L), fun = "mean", type = "around", na.rm = TRUE) }
      names(A) <- nm
    }
    A <- A[[which(as.integer(substr(names(A), 1, 4)) %in% yrs_out)]]
    A <- terra::resample(A, H[[1]], method = "bilinear")              # model change on the hindcast grid
    ay <- as.integer(substr(names(A), 1, 4)); mo <- as.integer(substr(names(A), 6, 7)); nm <- names(A)
    ratio <- .scn_type[[v]] == "ratio"
    ## Bias adjustment, per cell and calendar month (cells x years matrices, one month at a time):
    ##  1 mean        - the change is taken on the model's own reference climatology and put on the
    ##                  Copernicus one (delta / mean scaling), so the model's bias cancels;
    ##  2 variability - (only without smoothing) the detrended year-to-year spread of the model over
    ##                  the reference years is scaled to the hindcast's; the per-cell trend is kept;
    ##  3 continuity  - the gap between the last hindcast year (ENV_SCEN_JOIN_YEARS) and the adjusted
    ##                  model in that year is split into a systematic part (hindcast trend line minus
    ##                  model trend line over the reference years, at that year: kept) and the rest
    ##                  (that year's weather: fades to 0 over ENV_SCEN_BLEND_YEARS; 0 = kept throughout).
    .trend <- function(Y, x) {                                         # per-row linear fit on x: list(fit, residual sd)
      xc <- x - mean(x); mu <- rowMeans(Y); b <- as.vector((Y - mu) %*% xc) / sum(xc^2)
      fit <- mu + outer(b, xc)
      list(fit = fit, sd = sqrt(rowSums((Y - fit)^2) / max(1, length(x) - 2)), at = function(x0) mu + b * (mean(x0) - mean(x)))
    }
    jy <- utils::tail(sort(intersect(full_y, ay)), ENV_SCEN_JOIN_YEARS)   # the last hindcast year(s): ay holds the join window tw
    do_obs <- identical(ENV_SCEN_VARIABILITY, "observed") && nsm > 1
    do_var <- isTRUE(ENV_SCEN_VAR_SCALE) && nsm <= 1 && !ratio
    var_note <- if (do_var) ", variance scaled" else if (do_obs) paste0(", observed year-to-year variability (", min(tw), "-", max(tw), ", detrended) replayed") else ""
    kf <- which(ay %in% yrs_f); out <- vector("list", 12)
    for (m in 1:12) {
      ka <- which(mo == m); xa <- ay[ka]
      Am <- terra::values(A[[ka]], mat = TRUE)
      if (do_var) {
        kh <- which(hym$m == m & hym$y %in% ref); kr <- which(xa %in% ref)
        if (length(kh) >= 5 && length(kr) >= 5) {
          sH <- .trend(terra::values(H[[kh]], mat = TRUE), hym$y[kh])$sd
          sA <- .trend(Am[, kr, drop = FALSE], xa[kr])$sd
          sf <- pmin(pmax(sH / sA, 1 / ENV_SCEN_VAR_MAX), ENV_SCEN_VAR_MAX); sf[!is.finite(sf)] <- 1
          tr <- .trend(Am, xa)$fit
          Am <- tr + sf * (Am - tr)
        }
      }
      hc <- terra::values(Hclim[[m]], mat = FALSE)
      Xm <- if (ratio) Am * hc else Am + hc
      kfm <- which(xa %in% yrs_f)
      w <- if (isTRUE(ENV_SCEN_JOIN) && length(jy) && ENV_SCEN_BLEND_YEARS > 0) pmax(0, 1 - (xa[kfm] - max(jy)) / ENV_SCEN_BLEND_YEARS) else
        rep(if (isTRUE(ENV_SCEN_JOIN) && length(jy)) 1 else 0, length(kfm))   # 1 at the last hindcast year
      if (isTRUE(ENV_SCEN_JOIN) && length(jy)) {
        hm <- rowMeans(terra::values(H[[which(hym$m == m & hym$y %in% jy)]], mat = TRUE), na.rm = TRUE)
        xm <- rowMeans(Xm[, xa %in% jy, drop = FALSE], na.rm = TRUE)
        ## systematic part: hindcast trend line vs adjusted-model trend line over the reference
        ## years, both at the join (kept); the rest is the last years' weather (fades out)
        ## trend lines over the recent window tw (it ends at the join), not over the bias reference
        kh <- which(hym$m == m & hym$y %in% tw); kr <- which(xa %in% tw)
        th <- .trend(terra::values(H[[kh]], mat = TRUE), hym$y[kh])$at(jy)
        tx <- .trend(Xm[, kr, drop = FALSE], xa[kr])$at(jy)
        if (ratio) {
          .cl <- function(z, d) { z <- pmin(pmax(z, 1 / ENV_SCEN_RATIO_MAX), ENV_SCEN_RATIO_MAX); z[!is.finite(z)] <- d; z }
          D <- .cl(hm / xm, 1); Ds <- .cl(th / tx, 1)
          Xm[, kfm] <- Xm[, kfm, drop = FALSE] * Ds * outer(D / Ds, w, `^`)
        } else {
          D <- hm - xm; D[!is.finite(D)] <- 0
          Ds <- th - tx; Ds[!is.finite(Ds)] <- 0
          Xm[, kfm] <- Xm[, kfm, drop = FALSE] + Ds + outer(D - Ds, w)
        }
      }
      ## Observed year-to-year variability (delta change): the smoothed change is the forced
      ## change only, so the detrended anomalies of the hindcast's recent window tw are replayed
      ## in order, year after year; they come in as the last year's anomaly fades (weight 1 - w).
      if (do_obs && length(kfm)) {
        kw <- which(hym$m == m & hym$y %in% tw); kw <- kw[order(hym$y[kw])]
        if (length(kw) >= 5) {
          Hw <- terra::values(H[[kw]], mat = TRUE); fw <- .trend(Hw, hym$y[kw])$fit
          E <- if (ratio) pmin(pmax(Hw / fw, 1 / ENV_SCEN_RATIO_MAX), ENV_SCEN_RATIO_MAX) else Hw - fw
          E[!is.finite(E)] <- if (ratio) 1 else 0
          Er <- E[, ((seq_along(kfm) - 1L) %% ncol(E)) + 1L, drop = FALSE]
          wr <- matrix(1 - w, nrow(Er), ncol(Er), byrow = TRUE)
          Xm[, kfm] <- if (ratio) Xm[, kfm, drop = FALSE] * Er^wr else Xm[, kfm, drop = FALSE] + Er * wr
        }
      }
      r <- terra::rast(H[[1]], nlyrs = length(kfm)); terra::values(r) <- Xm[, kfm, drop = FALSE]
      names(r) <- nm[ka][kfm]; out[[m]] <- r
    }
    X <- terra::rast(out); X <- X[[match(nm[kf], names(X))]]
    mo <- mo[kf]
    X <- terra::mask(X, Hclim[[mo]])
    names(X) <- nm[kf]
    ## Continuity check over the model area
    hl <- terra::global(H[[which(hym$y %in% jy)]], "mean", na.rm = TRUE)$mean
    fy <- utils::head(yrs_f, length(jy))
    sl <- terra::global(X[[which(as.integer(substr(names(X), 1, 4)) %in% fy)]], "mean", na.rm = TRUE)$mean
    ## Check of the correction: negatives (a positive variable must stay > 0)
    ## and the change of the last 20 scenario years against the reference,
    ## model's own change (mean of its monthly change) vs the corrected
    ## annual change; positive variables (NPP) are corrected by a monthly
    ## ratio, so a -20 % month stays -20 %, the annual % can differ a little.
    .fy <- utils::tail(yrs_f, 20)
    .kx <- which(as.integer(substr(names(X), 1, 4)) %in% .fy); .ka <- which(ay %in% .fy)
    if (length(.kx) && length(.ka)) {
      .hc <- terra::app(Hclim, mean, na.rm = TRUE); .xf <- terra::app(X[[.kx]], mean, na.rm = TRUE); .af <- terra::app(A[[.ka]], mean, na.rm = TRUE)
      scn_chk[[length(scn_chk) + 1]] <- data.table(scenario = sc, variable = v,
        method = if (ratio) "monthly ratio (multiplicative)" else "monthly additive (linear scaling)",
        years = paste(range(.fy), collapse = "-"), reference = paste(range(ref), collapse = "-"),
        unit = if (ratio) "%" else ENV_UNITS_OUT[[v]],
        model_change = if (ratio) 100 * (.env_domain_mean(.af) - 1) else .env_domain_mean(.af),
        corrected_change = if (ratio) 100 * (.env_domain_mean(.xf / .hc) - 1) else .env_domain_mean(.xf - .hc),
        negative_cell_months_pct = 100 * mean(terra::global(X < 0, "mean", na.rm = TRUE)$mean, na.rm = TRUE))
    }
    scn_join[[length(scn_join) + 1]] <- data.table(scenario = sc, variable = v, unit = ENV_UNITS_OUT[[v]],
      hindcast_years = paste(range(jy), collapse = "-"), hindcast_mean = mean(hl), scenario_years = paste(range(fy), collapse = "-"),
      scenario_mean = mean(sl), step = mean(sl) - mean(hl))
    d <- file.path(scen_dir, sc); dir.create(d, showWarnings = FALSE)
    terra::writeRaster(X, file.path(d, paste0(v, "_monthly_native.tif")), overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
    st <- .env_domain_stats(X)
    scn_ts[[length(scn_ts) + 1]] <- cbind(data.table(scenario = sc, variable = v, unit = ENV_UNITS_OUT[[v]],
                                                     year = as.integer(substr(names(X), 1, 4)), month = mo,
                                                     source = paste0("ISIMIP3b ", paste(names(anoms), collapse = " + "), " change, bias-adjusted to the ",
                                                                     min(ref), "-", max(ref), " Copernicus hindcast per cell and month")), st)
    .scn_msg(v, " ", sc, ": ", terra::nlyr(X), " months (", min(yrs_f), "-", ENV_SCEN_END, "), ", length(anoms), " model(s)", var_note,
             "; step ", paste(range(jy), collapse = "-"), " -> ", paste(range(fy), collapse = "-"), ": ", signif(mean(sl) - mean(hl), 2), " ", ENV_UNITS_OUT[[v]], ".")
  }
}

## =================================================================
## Series, Ecosim forcing and plot
## =================================================================
if (length(scn_ts)) {
  S <- rbindlist(scn_ts, fill = TRUE)
  fwrite(rbindlist(scn_used), file.path(scen_dir, "environment_scenarios_models.csv"))
  .ref_tab <- data.table(
    variable  = c("temp_surf", "sal_surf", "temp_bot", "npp"),
    reference = c(if (!exists("ENV_SST_SOURCE") || identical(ENV_SST_SOURCE, "reanalysis")) "MedPHY reanalysis (model, assimilates satellite SST)" else
                    if (exists("ENV_SST_SOURCE") && identical(ENV_SST_SOURCE, "satellite")) "Copernicus Med L4 SST analysis (satellite, observation-based, gap-filled)" else
                    "MedPHY reanalysis corrected to the Copernicus Med L4 satellite SST (per cell and month, 2000-2014)",
                  "MedPHY reanalysis (model, assimilates profiles)", "MedPHY reanalysis bottomT (model; no observation-based reference)",
                  "MedBFM regional biogeochemical reanalysis (model)"),
    overlap_years = paste(range(if (is.null(ENV_SCEN_REF_YEARS)) NA else ENV_SCEN_REF_YEARS), collapse = "-"),
    label = c(if (!exists("ENV_SST_SOURCE") || identical(ENV_SST_SOURCE, "reanalysis")) "bias-adjusted toward a reanalysis (assimilates satellite SST)" else "bias-adjusted toward satellite observations",
              "bias-adjusted toward a reanalysis",
              "not bias-corrected toward observations: model change added to the reanalysis (stated assumption)",
              "bias-adjusted toward a regional model's view"))
  fwrite(.ref_tab[variable %in% scn_vars], file.path(scen_dir, "environment_scenarios_reference.csv"), sep = ";", dec = ",")
  if (length(scn_join)) fwrite(rbindlist(scn_join), file.path(scen_dir, "environment_scenarios_continuity.csv"), sep = ";", dec = ",")
  if (length(scn_chk)) {
    .ck <- rbindlist(scn_chk)
    fwrite(.ck, file.path(scen_dir, "environment_scenarios_change_check.csv"), sep = ";", dec = ",")
    .scn_msg("Change check (last 20 years vs reference; model vs corrected): ",
             paste(sprintf("%s %s %+.2f vs %+.2f %s (negatives %.2f%%)", .ck$variable, .ck$scenario, .ck$model_change, .ck$corrected_change,
                           .ck$unit, .ck$negative_cell_months_pct), collapse = "; "), " - environment_scenarios_change_check.csv.")
  }
  hind <- if (exists("env_ts") && nrow(env_ts)) env_ts else fread(file.path(env_dir, "environment_timeseries_monthly.csv"))
  base <- hind[year == ENV_BASE_YEAR, .(base = mean(value, na.rm = TRUE)), by = variable]
  S <- merge(S, base, by = "variable", all.x = TRUE)
  S[, relative_to_base := value / base][, base := NULL]
  fwrite(S[order(scenario, variable, year, month)], file.path(scen_dir, "environment_scenarios_monthly.csv"))
  SA <- S[, .(value = mean(value), p10 = mean(p10), p90 = mean(p90), relative_to_base = mean(relative_to_base), n_months = .N),
          by = .(scenario, variable, unit, year)]
  fwrite(SA[order(scenario, variable, year)], file.path(scen_dir, "environment_scenarios_annual.csv"))
  ## Ecosim forcing: the hindcast months and then the scenario's, relative to the base year
  for (sc in unique(S$scenario)) for (v in unique(S[scenario == sc, variable])) {
    h <- hind[variable == v & year %in% sort(unique(c(ENV_BASE_YEAR, ENV_YEARS))), .(year, month, value, relative_to_base, source)]
    m <- rbind(h, S[scenario == sc & variable == v, .(year, month, value, relative_to_base, source)])[order(year, month)]
    m[, time_step := (year - min(year)) * 12 + month]
    fwrite(m[, .(time_step, year, month, forcing = round(relative_to_base, 4), value, source)],
           file.path(scen_dir, sc, paste0("ecosim_forcing_", v, "_monthly.csv")))
    a <- m[, .(forcing = round(mean(relative_to_base), 4), value = mean(value), n_months = .N, source = source[.N]), by = year]
    fwrite(a, file.path(scen_dir, sc, paste0("ecosim_forcing_", v, "_annual.csv")))
  }
  .scn_msg("Scenarios ", paste(.scn_label[unique(S$scenario)], collapse = ", "), " to ", ENV_SCEN_END, ": maps, Ecosim forcing (relative to ",
           ENV_BASE_YEAR, ") and series in ", scen_dir, ". Run 05b for the Ecospace maps of each scenario.")
  miss <- setdiff(ENV_SCENARIOS, unique(S$scenario))
  if (length(miss)) .scn_msg("Not made (no ISIMIP3b files): ", paste(.scn_label[miss], collapse = ", "), ".")
  tryCatch(if (requireNamespace("ggplot2", quietly = TRUE)) {
    ha <- hind[variable %in% unique(S$variable), .(value = mean(value)), by = .(variable, year)][, scenario := "hindcast"]
    pd <- rbind(ha, SA[, .(variable, year, value, scenario)])
    pd[, scenario := factor(scenario, levels = c("hindcast", names(.scn_label)), labels = c("Hindcast (Copernicus)", .scn_label))]
    pd[, variable := paste0(variable, " (", ENV_UNITS_OUT[variable], ")")]
    g <- ggplot2::ggplot(pd, ggplot2::aes(year, value, colour = scenario)) + ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
      ggplot2::scale_colour_manual(values = c("black", "#1a9850", "#fdae61", "#d73027", "#7b3294"), drop = TRUE) +
      ggplot2::labs(x = NULL, y = "Annual mean over the model area", colour = NULL,
                    title = "Environmental drivers: hindcast and scenarios",
                    caption = "ISIMIP3b change, bias-adjusted to the Copernicus hindcast per cell and calendar month (mean, variability, continuity at the join).") +
      ggplot2::theme_bw(base_size = 14) + ggplot2::theme(legend.position = "top")
    pdir <- file.path(out_dir, "plots", "environment"); dir.create(pdir, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(file.path(pdir, "environment_scenarios.png"), g, width = 13, height = 3.2 * ceiling(uniqueN(pd$variable) / 2) + 1.2, dpi = 150)
  }, error = function(e) .scn_msg("plot skipped - ", conditionMessage(e)))
} else if (length(ENV_SCENARIOS)) {
  .scn_msg("No scenario made (see the lines above).")
}
