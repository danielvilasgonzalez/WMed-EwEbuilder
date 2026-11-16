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
## Source:
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
##  The values each driver takes in the Ecospace driver maps (05b: every
##  water cell and month of the base map; depth from depth_used.asc) are
##  written as histograms and drawn behind the curves, so each function
##  can be read against the conditions of the model area.
##
## OUTPUT (out_dir/environment/ecospace_responses/)
##   ecospace_response_functions.csv   one row per function, as EwE
##       stores it (EcosimShapeMediation): Title (#AM_<driver>_fg_<n>),
##       FunctionType (9 trapezoid, 0 drawn points), XAxisMin, XAxisMax, FunctionParams, zScale (the 1200
##       values, space-separated, "." decimals), plus Source, FG, driver,
##       units and what it is based on
##   ewe_response_<source>_<driver>.csv  the same functions side by side
##       (one column per FG; rows Title, XAxisMin, XAxisMax, then the
##       1200 values) to paste into EwE's response function editor
##   ecospace_response_points.csv      120 points of every curve, for
##       plots and checks
##   ecospace_capacity_drivers.csv     FG x driver: the function to
##       assign in Ecospace > Habitat capacity
##   ecospace_driver_distribution.csv  histogram of each driver in the
##       Ecospace driver maps (bins, cells x months, share)
##   ecospace_response_<driver>.png    curves of every FG over that histogram
##
## References
##   Kaschner K, Watson R, Trites AW, Pauly D (2006) Mar Ecol Prog Ser
##     316: 285-310. Ready J et al. (2010) Ecol Model 221: 467-478.
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
.hr_default("RESPONSE_SOURCES", "aquamaps")
.hr_default("RESPONSE_DRIVERS", c("depth", "temp_bot", "temp_surf", "sal_surf", "npp"))
.hr_default("RESPONSE_N_POINTS", 1200L)                                    # EwE response functions have 1200 values
.hr_default("ECOSPACE_ENV_DIR", file.path(out_dir, "environment"))
resp_dir <- file.path(ECOSPACE_ENV_DIR, "ecospace_responses")
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
.add_fun <- function(source, fg, driver, ftype, xmin, xmax, params, y, basis) {
  title <- paste0("#AM_", driver, "_fg_", fg$FG_num)
  funs[[length(funs) + 1]] <<- data.table(Title = title, Source = source, FG_num = fg$FG_num, FG_name = fg$FG_name, Driver = driver,
                                          Units = unname(.hr_units[driver]), FunctionType = ftype, XAxisMin = signif(xmin, 6), XAxisMax = signif(xmax, 6),
                                          FunctionParams = params, Basis = basis,
                                          zScale = paste(sprintf("%.7g", y), collapse = " "))
  k <- unique(round(seq(1, NPT, length.out = 120)))
  xs <- seq(xmin, xmax, length.out = NPT)[k]
  pts[[length(pts) + 1]] <<- data.table(Title = title, Source = source, FG_num = fg$FG_num, FG_name = fg$FG_name, Driver = driver,
                                        x = signif(xs, 6), y = round(y[k], 5))
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
## 2. Values of the drivers in the Ecospace maps (05b)
## =================================================================
## Base map: the one 05b made in this run (esp_dir), else the most recent
## folder of environment/ecospace_drivers. One value per time step: the
## mean of that monthly map over its water cells (05b index); depth: every
## water cell of the base map. Absolute values only (relative maps are not
## comparable).
.drv_dist <- tryCatch({
  .hist_of <- function(d) if (file.exists(file.path(d, "historical", "ecospace_driver_index.csv"))) file.path(d, "historical") else d
  ed <- get0("esp_dir", envir = .GlobalEnv, ifnotfound = NA_character_)
  if (is.na(ed) || !dir.exists(ed)) {
    cand <- list.dirs(file.path(ECOSPACE_ENV_DIR, "ecospace_drivers"), recursive = FALSE)
    cand <- cand[file.exists(file.path(vapply(cand, .hist_of, ""), "ecospace_driver_index.csv"))]
    ed <- if (length(cand)) cand[which.max(file.mtime(cand))] else NA_character_
  }
  map_lab <- if (is.na(ed)) NA_character_ else basename(ed)
  if (!is.na(ed)) ed <- .hist_of(ed)
  if (is.na(ed)) { .hr_msg("No Ecospace driver maps (05b) - curves drawn without the map values."); NULL } else {
    ix <- fread(file.path(ed, "ecospace_driver_index.csv"))
    vals <- list()
    for (v in intersect(RESPONSE_DRIVERS, unique(ix$variable))) {
      if (any(grepl("^relative", ix[variable == v, unit]))) next
      vals[[v]] <- as.numeric(ix[variable == v & is.finite(mean), mean])        # mean of each monthly map (water cells)
    }
    if ("depth" %in% RESPONSE_DRIVERS && file.exists(file.path(ed, "depth_used.asc"))) {
      x <- terra::values(terra::rast(file.path(ed, "depth_used.asc")), mat = FALSE); vals$depth <- abs(x[is.finite(x) & x != 0])
    }
    out <- rbindlist(lapply(names(vals), function(v) {
      x <- vals[[v]]; if (length(x) < 2) return(NULL)
      br <- pretty(range(x), 40); h <- graphics::hist(x, breaks = br, plot = FALSE)
      data.table(Driver = v, bin_lo = head(br, -1), bin_hi = br[-1], x_mid = h$mids, n = h$counts, share = h$counts / sum(h$counts),
                 base_map = map_lab)
    }))
    if (nrow(out)) {
      fwrite(out, file.path(resp_dir, "ecospace_driver_distribution.csv"))
      .hr_msg("Driver values of the Ecospace maps (", map_lab, ", historical): ", paste(unique(out$Driver), collapse = ", "), ".")
    }
    out
  }
}, error = function(e) { .hr_msg("driver values of the Ecospace maps skipped - ", conditionMessage(e)); NULL })

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
  setnames(cap, intersect("AquaMaps", names(cap)), "function_aquamaps")
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
    hd <- if (!is.null(.drv_dist)) .drv_dist[Driver == drv] else NULL
    g <- ggplot2::ggplot(p, ggplot2::aes(x, y)) +
      (if (!is.null(hd) && nrow(hd)) ggplot2::geom_rect(data = hd[, .(xmin = bin_lo, xmax = bin_hi, ymax = share / max(share))],
                                                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax), inherit.aes = FALSE,
                                                       fill = "grey75", colour = NA)) +
      ggplot2::geom_line(linewidth = 0.8, colour = "#1f77b4") +
      ggplot2::facet_wrap(~ lab, scales = "free_x", ncol = 6) +
      ggplot2::labs(x = paste0(drv, " (", .hr_units[drv], ")"), y = "Relative habitat capacity",
                    title = paste0("Ecospace response functions: ", drv),
                    subtitle = if (!is.null(hd) && nrow(hd)) paste0("Grey: ", if (drv == "depth") "depth of the water cells of the Ecospace base map (" else "mean of each monthly Ecospace driver map (",
                                                                  hd$base_map[1], "; tallest bar = 1)") else NULL) +
      ggplot2::theme_bw(base_size = 14) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(size = 9))
    nr <- ceiling(uniqueN(p$lab) / 6)
    ggplot2::ggsave(file.path(resp_dir, paste0("ecospace_response_", drv, ".png")), g, width = 18, height = 2.4 * nr + 1.5, dpi = 150, limitsize = FALSE)
  }, error = function(e) .hr_msg("plot ", drv, ": ", conditionMessage(e)))
  .hr_msg(nrow(F), " response functions (", paste0(F[, .N, by = Source][, paste(Source, N)], collapse = ", "), ") for ",
          uniqueN(F$FG_num), " FGs -> ", resp_dir)
}
