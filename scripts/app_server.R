## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: app_server.R
## =================================================================
## app_server.R
## Created by: Daniel Vilas
##
## The page's server code: runs for every visitor (input, output and session
## come from the page). Runs the pipeline, shows progress, results and checks.
## Kept in its own R file (not inside WMed-EwEbuilder.qmd) so it can be
## edited and tested like any script, and RStudio's Visual editor cannot
## rewrap or overwrite it. Sourced by the page with local = TRUE.
## =================================================================

## Paths: found by wmed_paths.R, the same way run_pipeline_demo.R finds them.
.git_dir <- normalizePath("..")   # this .qmd sits in <git_dir>/scripts
source(file.path(.git_dir, "scripts", "wmed_paths.R"), local = TRUE)
.paths <- wmed_find_paths(.git_dir)  # config.local.R > config.R > standard pCloud Drive place
## Paths shown on the page: the real full path on this computer (e.g.
## /Users/<name>/WMed_EwE_output, C:/Users/<name>/Documents/...), so each
## user sees exactly where their folders are. Set
## options(wmed.short_paths = TRUE) to show the home folder as "~" instead
## (e.g. for a projector or a screenshot).
.show_path <- function(p) {
  if (is.null(p) || !length(p) || is.na(p)) return(p)
  p2 <- gsub("\\\\", "/", normalizePath(path.expand(p), winslash = "/", mustWork = FALSE))
  if (!isTRUE(getOption("wmed.short_paths", FALSE))) return(p2)
  h <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  if (startsWith(p2, h)) paste0("~", substring(p2, nchar(h) + 1)) else p2
}
.base_out <- unname(.paths$out_dir)   # main output folder: default-input results, caches, the page's run files
## Results shown on the page: the folder of one run (wmed_paths.R:
## wmed_run_name). Default inputs -> .base_out; any other area, years or FG
## table -> .base_out/runs/<name>. Picked in the Results card, and set to
## the run's folder when a run from this page finishes. .out_dir, .plot_dir,
## .wb_path and .diet_path follow it (active bindings).
.results_dir <- reactiveVal(.base_out)
makeActiveBinding(".out_dir", function() shiny::isolate(.results_dir()), environment())
## Run files live in out_dir/.qmd_run (not in R's temporary folder), so a page
## reopened later still knows the last run started from here, and a run
## that is still going.
.run_dir <- if (!is.na(.base_out) && dir.exists(.base_out)) file.path(.base_out, ".qmd_run") else file.path(tempdir(), "wmed_qmd_run")
dir.create(.run_dir, showWarnings = FALSE, recursive = TRUE)
.status_file <- file.path(.run_dir, "status.txt")       # "done" or "error: ..." when the run ends; its time = end
.console <- file.path(.run_dir, "console.txt")          # everything the run prints
.progress_file <- file.path(.run_dir, "progress.txt")   # "--- Step i/7 ..." lines
.pid_file <- file.path(.run_dir, "pid.txt")             # process id of the run
.settings_file <- file.path(.run_dir, "settings.R")     # settings block of the run; its time = start

## Whole-GSA area of each GSA from the map polygons (km2). An approximate
## preview only: 01 computes the real A_model from the GSA shapefile and
## the bathymetry (795,431 km2 for GSAs 1-11 in the 2026-10-15 run, before GSA 12 was added; the
## simplified polygons give about 1 % more).
.gsa_km2 <- tryCatch({
  g <- sf::st_read(file.path(.git_dir, "scripts", "gsa_boundaries_westmed.geojson"), quiet = TRUE)
  a <- tapply(as.numeric(sf::st_area(g)) / 1e6, g$gsa_num, sum)
  setNames(as.numeric(a), names(a))
}, error = function(e) NULL)
## Depth cells of the model area written by 01 (out_dir/biomass/
## model_area_cells.csv: GSA x depth band, km2, from the NOAA bathymetry).
## With whole_gsa it holds every depth band, so both bases can be summed
## from it: 10-800 m = cells with Depth_min_m >= 10 and Depth_max_m <= 800.
model_cells <- reactive({
  last_finished()
  f <- if (!is.na(.out_dir)) file.path(.out_dir, "biomass", "model_area_cells.csv") else ""
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(utils::read.csv(f), error = function(e) NULL)
  if (is.null(x) || !all(c("AreaID", "Depth_min_m", "Depth_max_m", "area_km2") %in% names(x))) NULL else x
})
## --- Functional groups: the species -> FG csv the run uses -------------------
.pcloud_dir <- .paths$pcloud_dir
.fg_default <- if (!is.na(.pcloud_dir)) file.path(.pcloud_dir, "data", "FG_WMed_2026.csv") else NA_character_
.fg_choices <- if (!is.na(.pcloud_dir) && dir.exists(file.path(.pcloud_dir, "data")))
  list.files(file.path(.pcloud_dir, "data"), "^FG.*\\.csv$", full.names = TRUE, ignore.case = TRUE) else character(0)
updateSelectInput(session, "fg_file", choices = setNames(.fg_choices, basename(.fg_choices)),
                  selected = if (.fg_default %in% .fg_choices) .fg_default else .fg_choices[1])
fg_path <- reactiveVal(.fg_default)
run_target <- reactiveVal(NULL)   # output folder of the run started from this page
## Output folder of a run with the inputs set on the page now (same rule as run_pipeline_demo.R)
.inputs_run_dir <- function() {
  custom <- identical(input$area_mode, "custom")
  yr <- function(v) if (length(v) == 2) seq(v[1], v[2]) else v
  wmed_run_dir(.base_out, wmed_run_name(
    area_mode = if (custom) "custom" else "westmed", gsas = if (custom) integer(0) else as.integer(input$gsas),
    basis = if (is.null(input$basis)) "whole_gsa" else input$basis, area_name = input$area_name,
    year_ecopath = yr(input$ecopath_years), ts_years = yr(input$ts_years),
    fg_file = { f <- fg_path(); if (is.null(f) || is.na(f)) NULL else f },
    own_survey = length(survey_files()) > 0 || !is.na(other_survey())))
}
.run_label <- function(d) {
  if (is.null(d) || is.na(d)) return("-")
  ## the full path on this computer, so the user can open the folder
  if (identical(normalizePath(d, mustWork = FALSE), normalizePath(.base_out, mustWork = FALSE)))
    paste0(.show_path(d), " (main output folder, default inputs)") else .show_path(d)
}
observeEvent(input$fg_file, if (nzchar(input$fg_file)) fg_path(input$fg_file))
catch_path <- reactiveVal(NA_character_)
observeEvent(input$catch_upload, {   # an uploaded catchability table is kept next to the run files
  dest <- file.path(.run_dir, paste0("catchability_", gsub("[^A-Za-z0-9_.-]", "_", input$catch_upload$name)))
  file.copy(input$catch_upload$datapath, dest, overwrite = TRUE)
  x <- tryCatch(data.table::fread(file = dest), error = function(e) NULL)
  if (is.null(x) || !all(c("ScientificName", "q") %in% names(x))) {
    showNotification("The catchability table needs columns ScientificName and q.", type = "error"); return() }
  catch_path(normalizePath(dest))
  showNotification(sprintf("Catchability table loaded: %d species.", nrow(x)), duration = 5)
})
## --- Survey biomass upload (card 4a) -------------------------------------
## Biomass from any survey: species, year, biomass_t_km2 (OTHER_SURVEY_FILE).
## Kept in <run files>/survey_upload/. Own MEDITS/MEDIAS files can still be
## set in a settings file (MEDITS_TA_FILE, MEDITS_TB_FILE, MEDITS_TC_FILE,
## MEDIAS_DIR), not on the page.
survey_files <- reactiveVal(list())
.survey_dir <- file.path(.run_dir, "survey_upload")
## --- Ecospace base map (depth .asc) for 05b ------------------------------
ecospace_depth <- reactiveVal(NA_character_)
observeEvent(input$ecospace_depth_upload, {
  up <- input$ecospace_depth_upload
  d <- file.path(.run_dir, "ecospace_basemap"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  asc <- up[grepl("\\.asc$", up$name, ignore.case = TRUE), ]
  if (!nrow(asc)) { showNotification("Upload the depth map as an ESRI ASCII grid (.asc), optionally with its .prj.", type = "error"); return() }
  for (i in seq_len(nrow(up))) file.copy(up$datapath[i], file.path(d, up$name[i]), overwrite = TRUE)
  f <- normalizePath(file.path(d, asc$name[1]))
  g <- tryCatch(terra::rast(f), error = function(e) NULL)
  if (is.null(g)) { showNotification("Could not read the .asc file.", type = "error"); return() }
  ecospace_depth(f)
  showNotification(sprintf("Ecospace base map: %d x %d cells of %.4g, %d water cells (depth > 0).", terra::ncol(g), terra::nrow(g),
                           terra::res(g)[1], sum(abs(terra::values(g, mat = FALSE)) > 0, na.rm = TRUE)), duration = 8)
})
output$esp_cell_km <- renderText({
  a <- input$esp_cell_arcmin; if (is.null(a) || !is.finite(a)) return("")
  sprintf("= %.4g degrees, about %.1f km E-W x %.1f km N-S at 40\u00b0N", a / 60, a / 60 * 111.32 * cos(40 * pi / 180), a / 60 * 111.32)
})
.esp_map_name <- function() {
  if (identical(input$esp_grid_mode, "file")) { f <- ecospace_depth(); if (is.na(f)) f <- "depth_wmed.asc"
    gsub("[^A-Za-z0-9_-]", "_", sub("\\.asc$", "", basename(f), ignore.case = TRUE)) }
  else paste0("grid_", gsub("\\.", "p", format(signif(as.numeric(input$esp_cell_arcmin), 4))), "arcmin")
}
output$esp_out_where <- renderUI({
  d <- file.path(.inputs_run_dir(), "environment", "ecospace_drivers", .esp_map_name())
  tags$div(class = "wm-help mt-2", "Written to ", tags$code(.show_path(d)), ": one folder per variable (", tags$code("<var>/<var>_YYYY_MM.asc"),
           "), ", tags$code("depth_used.asc"), " (the Ecospace depth map), ", tags$code("ecospace_driver_index.csv"), " and a check figure.",
           if (dir.exists(d)) tags$span(" Last made ", format(file.mtime(d), "%d %b %Y %H:%M"), ".") else " Not made yet.")
})
## --- Ecospace map viewer: the .asc files of 05b, one month at a time ------
## colour-bar ticks: the bar's own min and max plus round values inside
.esp_bar_ticks <- function(rng, n = 4) {
  p <- pretty(rng, n); p <- p[p > rng[1] + 0.06 * diff(rng) & p < rng[2] - 0.06 * diff(rng)]
  sort(c(rng[1], p, rng[2]))
}
.esp_bar_axis <- function(rng, cex = 0.85) {
  at <- .esp_bar_ticks(rng)
  axis(1, at = at, labels = formatC(signif(at, 3), format = "fg", digits = 3), cex.axis = cex, mgp = c(3, 0.4, 0), tcl = -0.25)
  box(col = "black", lwd = 0.8)   # black frame around the colour bar
}
.esp_root <- function() file.path(.inputs_run_dir(), "environment", "ecospace_drivers")
.esp_maps <- reactive({
  run_state(); input$esp_grid_mode
  d <- .esp_root(); if (!dir.exists(d)) return(character(0))
  m <- list.dirs(d, full.names = FALSE, recursive = FALSE)
  m[file.exists(file.path(d, m, "ecospace_driver_index.csv"))]
})
observe({
  m <- .esp_maps(); cur <- isolate(input$esp_view_map)
  updateSelectInput(session, "esp_view_map", choices = if (length(m)) m else c("(no maps yet - run 05b)" = ""),
                    selected = if (!is.null(cur) && cur %in% m) cur else if (length(m)) m[1] else "")
})
.esp_index <- reactive({
  m <- input$esp_view_map; req(m, nzchar(m))
  f <- file.path(.esp_root(), m, "ecospace_driver_index.csv"); req(file.exists(f))
  data.table::fread(f)
})
observe({
  ix <- .esp_index(); v <- c("all", unique(ix$variable), "depth"); cur <- isolate(input$esp_view_var)
  updateSelectInput(session, "esp_view_var", choices = setNames(v, ifelse(v == "depth", "depth (base map)", ifelse(v == "all", "All drivers", v))),
                    selected = if (!is.null(cur) && cur %in% v) cur else v[1])
})
output$esp_view_slider <- renderUI({
  ix <- .esp_index(); v <- input$esp_view_var; req(v)
  if (v == "depth") return(NULL)
  iv <- if (v == "all") ix[variable == ix$variable[1]] else ix[variable == v]; req(nrow(iv) > 0)
  if (identical(input$esp_view_mode, "year")) {
    yrs <- sort(unique(iv$year))
    sliderInput("esp_view_year", "Year", min = min(yrs), max = max(yrs), value = min(yrs), step = 1, sep = "", width = "100%",
                animate = animationOptions(interval = 1500, loop = TRUE), ticks = FALSE)
  } else sliderInput("esp_view_step", NULL, min = 1, max = nrow(iv), value = 1, step = 1, width = "100%",
                     animate = animationOptions(interval = 700, loop = TRUE), ticks = FALSE)
})
output$esp_view_plot_ui <- renderUI({
  v <- input$esp_view_var; yr <- identical(input$esp_view_mode, "year")
  h <- if (identical(v, "all")) (if (yr) 12 * 115 + 110 else 620) else if (yr && !identical(v, "depth")) 760 else 480
  plotOutput("esp_view_plot", height = paste0(h, "px"))
})
.esp_raster <- function(f) { r <- terra::rast(f); terra::NAflag(r) <- -9999; r }
## Climatology of a variable and month (mean over all years of that month),
## cached; used by the "difference from the average year" view.
.esp_clim_memo <- new.env()
.esp_clim <- function(base, ix, vv, mo) {
  key <- paste(base, vv, mo, sep = "|")
  if (!is.null(.esp_clim_memo[[key]])) return(.esp_clim_memo[[key]])
  f <- ix[variable == vv & (if (is.na(mo)) is.na(month) else month == mo), file]
  cl <- terra::app(terra::rast(lapply(file.path(base, f), .esp_raster)), mean, na.rm = TRUE)
  assign(key, cl, envir = .esp_clim_memo); cl
}
## one map: the value, or its difference from the average year
.esp_get <- function(base, ix, row, anom) {
  r <- .esp_raster(file.path(base, row$file))
  if (isTRUE(anom)) r - .esp_clim(base, ix, row$variable, row$month) else r
}
.esp_pal <- function(v, anom = FALSE) {
  if (isTRUE(anom)) return(grDevices::hcl.colors(101, "Blue-Red 3"))
  if (grepl("^temp", v)) grDevices::hcl.colors(100, "RdYlBu", rev = TRUE) else if (v == "sal_surf") grDevices::hcl.colors(100, "Viridis") else
    grDevices::hcl.colors(100, "YlGn", rev = TRUE)
}
.esp_sym <- function(rs) { m <- max(abs(unlist(lapply(rs, function(r) terra::global(r, "range", na.rm = TRUE)))), na.rm = TRUE); if (!is.finite(m) || m == 0) m <- 1; c(-m, m) }
.esp_ulab <- function(u, anom) if (isTRUE(anom)) paste0("difference from the mean year, ", u) else u

## Draws the selected view (also used for the PNG download)
.esp_draw <- function() {
  ix <- .esp_index(); v <- input$esp_view_var; m <- input$esp_view_map; req(v, m)
  anom <- identical(input$esp_view_values, "anom") && v != "depth"
  base <- file.path(.esp_root(), m)
  dep <- .esp_raster(file.path(base, "depth_used.asc"))
  land <- terra::ifel(is.na(dep) | dep <= 0, 1, NA)
  if (v == "depth") {
    r <- terra::ifel(dep > 0, dep, NA)
    terra::plot(r, col = rev(grDevices::hcl.colors(100, "Blues 3")), colNA = NA, main = paste0("Depth (m) - ", m), mar = c(3, 3, 3, 5), plg = list(box = TRUE))
    terra::plot(land, col = "grey80", legend = FALSE, add = TRUE)
    return(invisible())
  }
  fixed <- identical(input$esp_view_scale, "fixed") && !anom
  if (v == "all") {   # every driver: a year = months as rows x drivers as columns; a month = one map per driver
    vars <- unique(ix$variable); i1 <- ix[variable == vars[1]][order(time_step)]
    if (identical(input$esp_view_mode, "year")) {
      y <- input$esp_view_year; if (is.null(y) || !y %in% i1$year) y <- min(i1$year)
      nm <- max(ix[year == y, .N, by = variable]$N); nv <- length(vars)
      mat <- rbind(c(0, seq_len(nv)), cbind(nv + seq_len(nm), matrix(nv + nm + seq_len(nm * nv), nm, nv)), c(0, nv + nm + nm * nv + seq_len(nv)))
      layout(mat, widths = c(0.25, rep(1, nv)), heights = c(0.25, rep(1, nm), 0.42))
      par(mar = c(0, 0, 0, 0))
      for (vv in vars) { plot.new(); text(0.5, 0.4, paste0(vv, " (", .esp_ulab(ix[variable == vv, unit][1], anom), ")"), font = 2, cex = if (anom) 0.95 else 1.15) }
      mlab <- ix[variable == vars[1] & year == y][order(time_step)]
      for (i in seq_len(nm)) { plot.new(); text(0.6, 0.5, if (i <= nrow(mlab)) (if (is.na(mlab$month[i])) y else paste(month.abb[mlab$month[i]], y)) else "", srt = 90, font = 2, cex = 1.05) }
      rngs <- list()
      for (vv in vars) {
        iy <- ix[variable == vv & year == y][order(time_step)]; iall <- ix[variable == vv]
        rs <- lapply(seq_len(nrow(iy)), function(i) .esp_get(base, ix, iy[i], anom))
        rngs[[vv]] <- if (anom) .esp_sym(rs) else if (fixed) range(c(iall$min, iall$max), na.rm = TRUE) else range(c(iy$min, iy$max), na.rm = TRUE)
        for (i in seq_len(nm)) {
          if (i > length(rs)) { plot.new(); next }
          terra::plot(rs[[i]], col = .esp_pal(vv, anom), range = rngs[[vv]], colNA = NA, legend = FALSE, axes = FALSE, mar = c(0.15, 0.15, 0.15, 0.15))
          terra::plot(land, col = "grey80", legend = FALSE, add = TRUE, axes = FALSE)
        }
      }
      for (vv in vars) {
        par(mar = c(2.4, 1.2, 0.8, 1.2)); z <- seq(rngs[[vv]][1], rngs[[vv]][2], length.out = 101)
        image(x = z, y = 0:1, z = matrix(z[-1], ncol = 1), col = .esp_pal(vv, anom), axes = FALSE, xlab = "", ylab = "")
        .esp_bar_axis(rngs[[vv]])
      }
      return(invisible())
    }
    k <- input$esp_view_step; if (is.null(k) || k > nrow(i1)) k <- 1
    y <- i1$year[k]; mo <- i1$month[k]
    op <- par(mfrow = c(ceiling(length(vars) / 3), min(3, length(vars))))
    for (vv in vars) {
      iv2 <- ix[variable == vv]; row <- iv2[year == y & (is.na(month) | month == mo)][1]
      r <- .esp_get(base, ix, row, anom)
      rng <- if (anom) .esp_sym(list(r)) else if (fixed) range(c(iv2$min, iv2$max), na.rm = TRUE) else c(row$min, row$max)
      terra::plot(r, col = .esp_pal(vv, anom), range = rng, colNA = NA, mar = c(1.5, 1.5, 2.5, 4.5), plg = list(box = TRUE),
                  main = sprintf("%s (%s) %s", vv, .esp_ulab(row$unit, anom), if (is.na(mo)) y else sprintf("%d-%02d", y, mo)), cex.main = 0.95)
      terra::plot(land, col = "grey80", legend = FALSE, add = TRUE)
    }
    par(op); return(invisible())
  }
  iv <- ix[variable == v][order(time_step)]
  pal <- .esp_pal(v, anom)
  if (identical(input$esp_view_mode, "year")) {   # the 12 months of one year, one colour scale
    y <- input$esp_view_year; if (is.null(y) || !y %in% iv$year) y <- min(iv$year)
    iy <- iv[year == y]
    rs <- lapply(seq_len(nrow(iy)), function(i) .esp_get(base, ix, iy[i], anom))
    labs <- if (all(is.na(iy$month))) as.character(y) else month.abb[iy$month]
    rng <- if (anom) .esp_sym(rs) else if (fixed) range(c(iv$min, iv$max), na.rm = TRUE) else range(c(iy$min, iy$max), na.rm = TRUE)
    nc <- if (length(rs) > 6) 4 else min(3, length(rs)); nr <- ceiling(length(rs) / nc)
    layout(rbind(matrix(seq_len(nr * nc), nr, nc, byrow = TRUE), nr * nc + 1), heights = c(rep(1, nr), 0.3))
    for (i in seq_len(nr * nc)) {
      if (i > length(rs)) { plot.new(); next }
      mu <- terra::global(rs[[i]], "mean", na.rm = TRUE)[1, 1]
      terra::plot(rs[[i]], col = pal, range = rng, colNA = NA, legend = FALSE, axes = FALSE, mar = c(0.3, 0.3, 1.6, 0.3),
                  main = sprintf("%s %d  (mean %s%.3g)", labs[i], y, if (anom && mu > 0) "+" else "", mu), cex.main = 1)
      terra::plot(land, col = "grey80", legend = FALSE, add = TRUE, axes = FALSE)
    }
    par(mar = c(2.2, 6, 0.6, 6))
    z <- seq(rng[1], rng[2], length.out = 101)
    image(x = z, y = 0:1, z = matrix(z[-1], ncol = 1), col = pal, axes = FALSE, xlab = "", ylab = "")
    .esp_bar_axis(rng, 0.9); mtext(sprintf("%s (%s) - %d", v, .esp_ulab(iv$unit[1], anom), y), side = 3, line = 0.2, cex = 0.85, font = 2)
    return(invisible())
  }
  k <- input$esp_view_step; if (is.null(k) || k > nrow(iv)) k <- 1
  r <- .esp_get(base, ix, iv[k], anom)
  rng <- if (anom) .esp_sym(list(r)) else if (fixed) range(c(iv$min, iv$max), na.rm = TRUE) else range(terra::values(r), na.rm = TRUE)
  when <- if (is.na(iv$month[k])) iv$year[k] else sprintf("%d-%02d", iv$year[k], iv$month[k])
  layout(matrix(1:2, ncol = 1), heights = c(4, 1.3))
  terra::plot(r, col = pal, range = rng, colNA = NA, mar = c(2, 3, 3, 5), plg = list(box = TRUE),
              main = sprintf("%s (%s) - %s   mean %.3g, range %.3g to %.3g", v, .esp_ulab(iv$unit[k], anom), when,
                             terra::global(r, "mean", na.rm = TRUE)[1, 1], rng[1], rng[2]))
  terra::plot(land, col = "grey80", legend = FALSE, add = TRUE)
  tt <- if (all(is.na(iv$month))) iv$year else iv$year + (iv$month - 0.5) / 12
  par(mar = c(3, 4, 0.5, 5.5))
  plot(tt, iv$mean, type = "l", col = "grey30", xlab = "", ylab = "map mean", las = 1, cex.axis = 0.8)
  abline(v = tt[k], col = "firebrick", lwd = 2)
}
output$esp_view_plot <- renderPlot(.esp_draw())
.esp_fig_size <- function() {
  v <- input$esp_view_var; yr <- identical(input$esp_view_mode, "year")
  if (identical(v, "all")) (if (yr) c(1300, 12 * 125 + 120) else c(1300, 700)) else if (yr && !identical(v, "depth")) c(1300, 860) else c(1100, 640)
}
output$dl_esp_fig <- downloadHandler(
  filename = function() paste0("ecospace_", input$esp_view_var, "_", if (identical(input$esp_view_mode, "year")) input$esp_view_year else "month",
                               if (identical(input$esp_view_values, "anom")) "_anomaly" else "", ".png"),
  content = function(file) {
    sz <- .esp_fig_size()
    grDevices::png(file, width = sz[1], height = sz[2], res = 110)
    on.exit(grDevices::dev.off())
    .esp_draw()
  })
## all the .asc files of the base map shown (+ depth_used.asc and the index)
output$dl_esp_zip <- downloadHandler(
  filename = function() paste0("ecospace_drivers_", input$esp_view_map, ".zip"),
  content = function(file) {
    d <- file.path(.esp_root(), input$esp_view_map)
    if (!dir.exists(d)) stop("No maps for this base map - run 05b first.")
    old <- setwd(d); on.exit(setwd(old))
    f <- list.files(".", recursive = TRUE)
    if (requireNamespace("zip", quietly = TRUE)) zip::zip(file, f) else utils::zip(file, f, flags = "-q")
  })
output$ecospace_depth_info <- renderUI({
  f <- ecospace_depth()
  tags$div(class = "wm-help", if (is.na(f)) "Default base map: data/Bathymetry/depth_wmed.asc (1/6 deg)." else paste("Base map:", basename(f)))
})
other_survey <- reactiveVal(NA_character_)
observeEvent(input$other_survey_upload, {
  dest <- file.path(.survey_dir, paste0("other_survey_", gsub("[^A-Za-z0-9_.-]", "_", input$other_survey_upload$name)))
  dir.create(.survey_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(input$other_survey_upload$datapath, dest, overwrite = TRUE)
  x <- tryCatch(data.table::fread(dest), error = function(e) NULL)
  if (!is.null(x)) data.table::setnames(x, tolower(trimws(names(x))))
  if (is.null(x) || !all(c("species", "year") %in% names(x)) || !any(c("biomass_t_km2", "biomass", "density_t_km2", "density") %in% names(x))) {
    showNotification("The survey csv needs columns species (scientific name), year and biomass_t_km2.", type = "error"); return() }
  other_survey(normalizePath(dest))
  showNotification(sprintf("Survey biomass: %d rows, %d species, %s.", nrow(x), data.table::uniqueN(x$species),
                           paste(range(x$year, na.rm = TRUE), collapse = "-")), duration = 6)
})
output$other_survey_info <- renderUI({
  f <- other_survey()
  if (is.na(f)) return(NULL)
  tags$div(class = "wm-help", "Survey biomass: ", tags$strong(sub("^other_survey_", "", basename(f))), " ",
           actionLink("other_survey_reset", "remove"))
})
observeEvent(input$other_survey_reset, other_survey(NA_character_))
output$dl_other_survey_template <- downloadHandler("survey_biomass_template.csv", function(file)
  writeLines(c("species,year,biomass_t_km2,area_km2,reference"), file))
observeEvent(input$fg_upload, {   # an uploaded csv is kept next to the run files so the run can read it
  dest <- file.path(.run_dir, paste0("uploaded_", gsub("[^A-Za-z0-9_.-]", "_", input$fg_upload$name)))
  file.copy(input$fg_upload$datapath, dest, overwrite = TRUE)
  fg_path(dest)
})
fg_table <- reactive({
  f <- fg_path()
  if (is.na(f) || !file.exists(f)) return(list(error = paste("File not found:", f)))
  x <- tryCatch(data.table::fread(f, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x)) return(list(error = paste("Could not read", basename(f))))
  need <- c("FG_number", "FG_name", "species")
  if (!all(need %in% names(x))) return(list(error = paste0(basename(f), " is missing column(s): ", paste(setdiff(need, names(x)), collapse = ", "),
                                                         ". Columns found: ", paste(names(x), collapse = ", "))))
  list(data = x, path = f)
})
output$fg_summary <- renderUI({
  ft <- fg_table()
  if (!is.null(ft$error)) return(tags$div(class = "alert alert-danger py-2 small mb-0", ft$error))
  x <- data.table::copy(ft$data)
  has_group <- "FG_group" %in% names(x)
  ## q: the FG file's own q column; where empty, the q the last run used
  ## (biomass/catchability_q_by_species.csv)
  .qf <- file.path(.inputs_run_dir(), "biomass", "catchability_q_by_species.csv")
  if (file.exists(.qf)) {
    .qs <- tryCatch(unique(data.table::fread(.qf)[, .(species = ScientificName, q_run = q_resolved)], by = "species"), error = function(e) NULL)
    if (!is.null(.qs)) {
      if (!"q" %in% names(x)) x[, q := NA_real_]
      x[, q := suppressWarnings(as.numeric(q))]
      x[, q_run := .qs$q_run[match(trimws(species), .qs$species)]]
      x[is.na(q), q := q_run][, q_run := NULL]
    }
  }
  has_q <- "q" %in% names(x)
  x[, FG_number := suppressWarnings(as.numeric(FG_number))]
  fg <- x[!is.na(FG_number), .(FG_name = paste(unique(FG_name), collapse = " / "),
                               FG_group = if (has_group) paste(unique(stats::na.omit(FG_group)), collapse = " / ") else NA_character_,
                               n_species = data.table::uniqueN(species),
                               q = if (has_q) { v <- suppressWarnings(as.numeric(q)); v <- v[is.finite(v)]
                                                if (!length(v)) NA_character_ else if (length(unique(v)) == 1) format(v[1], digits = 3)
                                                else paste0(format(min(v), digits = 2), "-", format(max(v), digits = 2)) } else NA_character_),
          by = FG_number][order(FG_number)]
  ## Checks: one name per FG number, each species in one FG only, no gaps in the numbers
  multi_name <- fg[grepl(" / ", FG_name), FG_number]
  ## Multistanza: two or more FGs made of the same single species (e.g.
  ## hake juv./adult). Their biomass, Z and the split between stanzas should
  ## come from a stock assessment (GFCM SAF), not from the survey alone.
  sp_fg <- unique(x[!is.na(FG_number) & !is.na(species) & nzchar(species), .(FG_number, species = trimws(species))])
  one_sp <- sp_fg[, .(n = .N, species = species[1]), by = FG_number][n == 1]
  ms <- one_sp[, .(stanzas = list(sort(FG_number)), n_fg = .N), by = species][n_fg > 1]
  fg[, stanza := NA_character_]
  for (k in seq_len(nrow(ms))) {
    st <- ms$stanzas[[k]]
    fg[FG_number %in% st, stanza := paste0("stanza ", match(FG_number, st), "/", length(st))]
  }
  saf_sp <- tryCatch({
    f <- file.path(.pcloud_dir, "data", "fisheries", "GFCM_SAF", "gfcm_saf_stock_assessments_ALL.csv")
    if (file.exists(f)) unique(data.table::fread(f, select = "species")$species) else character(0)
  }, error = function(e) character(0))
  ms[, has_saf := species %in% saf_sp]
  sp2 <- x[!is.na(species) & nzchar(species), .(n = data.table::uniqueN(FG_number)), by = species][n > 1, species]
  sp2 <- setdiff(sp2, ms$species)                                  # shared species that are not a multistanza group
  gaps <- setdiff(seq(min(fg$FG_number), max(fg$FG_number)), fg$FG_number)
  warn <- c(if (length(multi_name)) paste("FG number(s) with more than one FG_name:", paste(multi_name, collapse = ", ")),
            if (length(sp2)) paste0(length(sp2), " species in more than one FG (", paste(head(sp2, 3), collapse = ", "),
                                    if (length(sp2) > 3) ", ...", ") outside a multistanza group - each species should be in one FG only"),
            if (length(gaps)) paste("Missing FG number(s):", paste(gaps, collapse = ", ")))
  stat <- function(label, value, note = NULL)
    column(3, tags$div(class = "wm-stat", tags$div(class = "l", label), tags$div(class = "v", value), if (!is.null(note)) tags$div(class = "n", note)))
  ## Species with biomass in the last run / species in the FG file (01 writes
  ## fg_species_coverage.csv); refreshed when a run finishes.
  run_state()
  .cov_f <- file.path(.inputs_run_dir(), "biomass", "fg_species_coverage.csv")
  cov <- if (file.exists(.cov_f)) tryCatch(data.table::fread(.cov_f), error = function(e) NULL) else NULL
  has_cov <- !is.null(cov) && all(c("FG_num", "coverage", "coverage_survey") %in% names(cov))
  if (has_cov) fg <- merge(fg, cov[, .(FG_number = as.numeric(FG_num), coverage, coverage_survey, species_without_biomass)], by = "FG_number", all.x = TRUE)
  ## Ecopath B of each FG in the last run (Ecopath_B sheet) and where it
  ## comes from, read from its Reference column
  .wbf <- file.path(.inputs_run_dir(), "ecopath_ecosim_inputs.xlsx")
  eb <- if (file.exists(.wbf)) tryCatch(data.table::as.data.table(openxlsx::read.xlsx(.wbf, "Ecopath_B", check.names = FALSE, sep.names = " ")), error = function(e) NULL) else NULL
  has_b <- !is.null(eb) && all(c("FG_num", "Biomass") %in% names(eb))
  if (has_b) {
    .src_class <- function(ref, b, ee) {
      r <- tolower(sub(" \\|.*$", "", ifelse(is.na(ref), "", ref)))
      out <- data.table::fcase(grepl("^manual density", r), "Manual density",
                   grepl("^medias", r), "MEDIAS (acoustic survey)",
                   grepl("^medits", r), "MEDITS (trawl survey)",
                   grepl("stock assessment|iccat|gfcm saf|ram legacy|star", r), "Stock assessment",
                   grepl("cobalt|isimip|copernicus|seapodym|maredat|plankton|phyto|detritus", r), "Plankton model / data",
                   grepl("habitat|euseamap|emodnet|mediseh|meadow|seagrass|coralligenous|gorgonian", r), "Habitat map x density",
                   grepl("abundance|sighting|accobams|forcada|megafauna|whale|dolphin|turtle|seabird|seal", r), "Megafauna survey (literature)",
                   grepl("manual", r), "Manual density",
                   nzchar(r), "Literature / other",
                   default = NA_character_)
      out[!is.finite(b) & is.finite(ee)] <- "Estimated by Ecopath (EE)"
      out[!is.finite(b) & !is.finite(ee)] <- "No biomass"
      out
    }
    .b <- suppressWarnings(as.numeric(eb$Biomass)); .ee <- if ("EE_input" %in% names(eb)) suppressWarnings(as.numeric(eb$EE_input)) else rep(NA_real_, nrow(eb))
    ebx <- data.table::data.table(FG_number = as.numeric(eb$FG_num), B = .b,
                      B_source = .src_class(if ("Reference" %in% names(eb)) as.character(eb$Reference) else NA_character_, .b, .ee),
                      B_ref = if ("Reference" %in% names(eb)) as.character(eb$Reference) else NA_character_)
    fg <- merge(fg, ebx, by = "FG_number", all.x = TRUE)
    fg[is.na(B_source), B_source := "No biomass"]
  }
  .fmt_b2 <- function(v) ifelse(!is.finite(v), "-", ifelse(v >= 0.01, formatC(v, format = "fg", digits = 3), formatC(v, format = "e", digits = 2)))
  rows <- lapply(seq_len(nrow(fg)), function(i)
    tags$tr(class = if (!is.na(fg$stanza[i])) "wm-stanza" else NULL,
            title = if (!is.na(fg$stanza[i])) "Multistanza group: biomass, Z and the stanza split from the stock assessment" else NULL,
            tags$td(fg$FG_number[i]),
            tags$td(fg$FG_name[i], if (!is.na(fg$stanza[i])) tags$span(class = "badge wm-stanza-badge ms-1", fg$stanza[i])), if (has_group) tags$td(fg$FG_group[i]), tags$td(fg$n_species[i]),
            if (has_cov) tags$td(title = if (!is.na(fg$species_without_biomass[i]) && nzchar(fg$species_without_biomass[i])) paste("No biomass:", fg$species_without_biomass[i]) else NULL,
                                 fg$coverage[i]),
            if (has_cov) tags$td(fg$coverage_survey[i]),
            if (has_b) tags$td(.fmt_b2(fg$B[i])),
            if (has_b) tags$td(title = if (!is.na(fg$B_ref[i])) substr(fg$B_ref[i], 1, 400) else NULL,
                               class = if (fg$B_source[i] %in% c("No biomass")) "text-danger" else NULL, fg$B_source[i]),
            if (has_q) tags$td(if (is.na(fg$q[i])) "-" else fg$q[i])))
  tagList(
    fluidRow(
      stat("File", basename(ft$path), format(file.mtime(ft$path), "edited %d %b %Y, %H:%M")),
      stat("Functional groups", nrow(fg), paste0("FG ", min(fg$FG_number), "-", max(fg$FG_number))),
      stat("Species", data.table::uniqueN(x$species),
           if (sum(fg$n_species) != data.table::uniqueN(x$species)) "unique species; a multistanza species is in two FGs but counted once" else NULL),
      if (has_b) stat("FGs with Ecopath B (last run)", paste0(fg[is.finite(B), .N], "/", nrow(fg)),
                      paste0(fg[B_source == "Estimated by Ecopath (EE)", .N], " estimated by Ecopath (EE), ",
                             fg[B_source == "No biomass", .N], " without biomass")),
      if (has_cov) {
        ## unique species (01 writes the totals); older runs: species in the
        ## FG file minus those without biomass in every FG they are in
        .tf <- file.path(dirname(.cov_f), "fg_species_coverage_totals.csv")
        .tot <- if (file.exists(.tf)) tryCatch(data.table::fread(.tf), error = function(e) NULL) else NULL
        if (is.null(.tot) || !nrow(.tot)) {
          .nm <- table(x$species)
          .wo <- table(unlist(strsplit(cov$species_without_biomass[!is.na(cov$species_without_biomass)], "; ", fixed = TRUE)))
          .never <- sum(names(.wo) %in% names(.nm) & as.integer(.wo[names(.wo)]) >= as.integer(.nm[names(.wo)]), na.rm = TRUE)
          .tot <- data.table::data.table(n_species = data.table::uniqueN(x$species), n_with_biomass = data.table::uniqueN(x$species) - .never,
                                         n_observed_survey = NA_integer_)
        }
        stat("With biomass (last run)", paste0(.tot$n_with_biomass, "/", .tot$n_species),
             paste0(if (is.finite(.tot$n_observed_survey)) paste0(.tot$n_observed_survey, " observed in the surveys; ") else "",
                    "unique species; ", format(file.mtime(.cov_f), "%d %b %H:%M")))
      }),
    if (length(warn)) tags$div(class = "alert alert-warning py-2 small mt-2 mb-2", lapply(warn, function(w) tags$div(w))),
    if (nrow(ms)) tags$div(class = "alert alert-info py-2 small mt-2 mb-2",
      tags$strong(sprintf("Multistanza group(s) (%d): ", nrow(ms))),
      "FGs made of the same single species (highlighted below). Their biomass, Z (P/B) and the split between stanzas should come from the stock assessment of that species, with the survey giving the trend.",
      tags$ul(class = "mb-0 mt-1", lapply(seq_len(nrow(ms)), function(k) {
        st <- ms$stanzas[[k]]
        tags$li(tags$em(ms$species[k]), ": ", paste(paste(st, fg$FG_name[match(st, fg$FG_number)]), collapse = " + "), " - ",
                if (ms$has_saf[k]) tags$span(class = "text-success", "GFCM stock assessment available")
                else tags$span(class = "text-danger", "no GFCM stock assessment found (gfcm_saf_stock_assessments_ALL.csv) - the stanza split rests on the survey only"))
      }))),
    tags$style(HTML(".wm-stanza td { background: #fff4e0 !important; } .wm-stanza-badge { background: #e8a33d; color: #fff; font-weight: 500; }")),
    tags$div(class = "wm-fg-table mt-2",
      tags$table(class = "table table-sm table-striped",
        tags$thead(tags$tr(tags$th("FG_number"), tags$th("FG_name"), if (has_group) tags$th("FG_group"), tags$th("N species"),
                           if (has_cov) tags$th(title = "Species with biomass in the last run / species in the FG file (hover a value for the species without biomass)", "With biomass"),
                           if (has_cov) tags$th(title = "Species observed by MEDITS/MEDIAS in the Ecopath years / species in the FG file", "Observed in survey"),
                           if (has_b) tags$th(title = "Ecopath B of the FG in the last run (Ecopath_B sheet, mean of the Ecopath years)", "B (t/km\u00b2)"),
                           if (has_b) tags$th(title = "Where the FG's Ecopath B comes from (hover a value for the full reference)", "B source"),
                           if (has_q) tags$th(title = "Catchability q of the FG's species (column q of the FG file; range when they differ)", "q"))),
        tags$tbody(rows))))
})

## Coastal state of each GSA - the same table as GSA_COASTAL_STATE in 02_fisheries.R
.gsa_state <- c("1" = "Spain", "2" = "Spain", "3" = "Morocco", "4" = "Algeria", "5" = "Spain", "6" = "Spain",
                "7" = "France", "8" = "France", "9" = "Italy", "10" = "Italy", "11" = "Italy", "12" = "Tunisia")
.unsurveyed <- c(3, 4, 12)
## Expected minutes per script (01..05): the median of the last 5 runs
## started from this page on this laptop (out_dir/.qmd_run/step_times.csv,
## filled from the "--- Step i/7 done in x min" lines at the end of every
## run). Until a script has run here, the times of the Windows run of
## 2026-10-29 are used. 01 is slower the first time after a change of GSAs
## (bathymetry download).
.step_min_default <- c(6.8, 11.7, 4.9, 5.1, 15, 2, 1.6)   # 05: a rerun with the years cached; the first run takes hours; 05b: minutes
.step_names <- c("01 biomass", "02 fisheries", "03 P/B, Q/B", "04 diets", "05 environment", "05b Ecospace maps", "06 diagnostics")

## --- Custom region from an uploaded shapefile / polygon file ---------------
custom_shape <- reactiveVal(NULL)   # list(path, sf) of the uploaded region
## Built-in example region: the Mediterranean cetacean migration corridor
## (Real Decreto 699/2018; scripts/example_regions/README.md)
observeEvent(input$custom_example, {
  f <- file.path(.git_dir, "scripts", "example_regions", "cetacean_migration_corridor_RD699_2018.shp")
  if (!file.exists(f)) { showNotification(paste("Example file not found:", f), type = "error"); return() }
  g <- tryCatch(sf::st_transform(sf::st_read(f, quiet = TRUE), 4326), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  if (is.null(g)) return()
  updateTextInput(session, "area_name", value = "cetacean_corridor")
  custom_shape(list(path = normalizePath(f), sf = g))
  tmp <- tempfile(fileext = ".geojson"); sf::st_write(sf::st_geometry(g), tmp, quiet = TRUE)
  session$sendCustomMessage("wm-custom-shape", list(geojson = paste(readLines(tmp, warn = FALSE), collapse = "")))
  showNotification("Example region loaded: Mediterranean cetacean migration corridor (area name 'cetacean_corridor').", type = "message")
})
observeEvent(input$custom_shp, {
  up <- input$custom_shp
  name <- gsub("[^A-Za-z0-9_-]", "_", if (nzchar(input$area_name)) input$area_name else "custom_region")
  dest <- file.path(.base_out, "shapefiles", "custom", name)   # kept in the main output folder, one folder per region
  ## Files from earlier uploads for this region are kept, so the parts of a
  ## shapefile can also be uploaded one after the other.
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(up))) {
    if (grepl("\\.zip$", up$name[i], ignore.case = TRUE)) utils::unzip(up$datapath[i], exdir = dest, junkpaths = TRUE)
    else file.copy(up$datapath[i], file.path(dest, up$name[i]), overwrite = TRUE)
  }
  ## macOS zips carry "._name" / __MACOSX copies that are not real files
  junk <- list.files(dest, "^\\._", full.names = TRUE, all.files = TRUE)
  if (length(junk)) unlink(junk)
  all_f <- list.files(dest, full.names = TRUE)
  stem <- tolower(tools::file_path_sans_ext(basename(all_f))); ext <- tolower(tools::file_ext(all_f))
  shp <- all_f[ext == "shp"]
  if (length(shp)) {
    ## the shapefile whose .shx and .dbf are also there (same name, any case)
    ok <- vapply(shp, function(x) all(c("shx", "dbf") %in% ext[stem == tolower(tools::file_path_sans_ext(basename(x)))]), logical(1))
    if (!any(ok)) {
      have <- sort(unique(ext[stem == tolower(tools::file_path_sans_ext(basename(shp[1])))]))
      showNotification(paste0("Shapefile '", basename(shp[1]), "': found ", paste0(".", have, collapse = ", "),
                              "; still missing ", paste0(".", setdiff(c("shx", "dbf"), have), collapse = " and "),
                              " (with the same name). Upload the missing part(s), or all parts together, or a .zip."),
                       type = "error", duration = 12)
      return()
    }
    f <- shp[ok][1]
  } else {
    f <- all_f[ext %in% c("geojson", "json", "gpkg", "kml")][1]
    if (is.na(f)) { showNotification("No .shp, .geojson, .gpkg or .kml found in the upload.", type = "error"); return() }
  }
  if (grepl("\\.shp$", f, ignore.case = TRUE) && !"prj" %in% ext[stem == tolower(tools::file_path_sans_ext(basename(f)))])
    showNotification("No .prj file: coordinates are assumed to be lon/lat (WGS84).", type = "warning", duration = 8)
  g <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  if (is.null(g)) return()
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326 else g <- sf::st_transform(g, 4326)
  g <- g[!sf::st_is_empty(g), ]
  custom_shape(list(path = normalizePath(f), sf = g))
  tmp <- tempfile(fileext = ".geojson"); sf::st_write(sf::st_geometry(g), tmp, quiet = TRUE)
  session$sendCustomMessage("wm-custom-shape", list(geojson = paste(readLines(tmp, warn = FALSE), collapse = "")))
})

## --- Area summary cards ---------------------------------------------------
output$area_summary <- renderUI({
  stat <- function(label, value, note = NULL, warn = FALSE)
    column(3, tags$div(class = paste("wm-stat", if (warn) "warn"),
                       tags$div(class = "l", label), tags$div(class = "v", value),
                       if (!is.null(note)) tags$div(class = "n", note)))
  if (identical(input$area_mode, "custom") && identical(input$custom_type, "shapefile")) {
    cs <- custom_shape()
    if (is.null(cs)) return(tags$div(class = "alert alert-secondary mb-0", "Upload the region file to see it on the map."))
    a <- suppressWarnings(as.numeric(sum(sf::st_area(cs$sf)))) / 1e6
    bb <- sf::st_bbox(cs$sf)
    return(fluidRow(
      stat("Region", input$area_name, basename(cs$path)),
      stat("Polygons", nrow(cs$sf), "one model area each"),
      stat("Polygon area", paste(format(round(a, -2), big.mark = ","), "km\u00b2"), "as drawn, land included"),
      stat("Extent", sprintf("%.1f-%.1f\u00b0E", bb["xmin"], bb["xmax"]), sprintf("%.1f-%.1f\u00b0N", bb["ymin"], bb["ymax"]))))
  }
  if (identical(input$area_mode, "custom")) {
    b <- c(input$bbox_xmin, input$bbox_xmax, input$bbox_ymin, input$bbox_ymax)
    ok <- all(is.finite(b)) && b[2] > b[1] && b[4] > b[3]
    km2 <- if (ok) (b[2] - b[1]) * 111.32 * cos(mean(b[3:4]) * pi / 180) * (b[4] - b[3]) * 110.57 else NA
    return(fluidRow(
      stat("Region", input$area_name),
      stat("Box", if (ok) sprintf("%.1f-%.1f\u00b0E, %.1f-%.1f\u00b0N", b[1], b[2], b[3], b[4]) else "invalid", warn = !ok),
      stat("Box area", if (ok) paste(format(round(km2, -2), big.mark = ","), "km\u00b2") else "-", "rectangle, land included"),
      stat("Scripts", "01-04, 06", "catch from the overlapping GSAs")))
  }
  g <- as.numeric(input$gsas)
  if (!length(g)) return(tags$div(class = "alert alert-danger mb-0", "No GSA selected - click the map or a quick-select button."))
  ## Area: from the depth cells 01 wrote (exact, from the bathymetry) when
  ## the last run covered these GSAs; otherwise the map polygons (whole GSA)
  cells <- model_cells()
  basis <- input$basis
  per_gsa <- if (!is.null(cells)) {
    keep <- if (identical(basis, "medits_10_800")) cells$Depth_min_m >= 10 & cells$Depth_max_m <= 800 else rep(TRUE, nrow(cells))
    tapply(cells$area_km2[keep], cells$AreaID[keep], sum)
  } else NULL
  have <- !is.null(per_gsa) && all(as.character(g) %in% names(per_gsa))
  fmt <- function(x) paste(format(round(x, -2), big.mark = ","), "km\u00b2")
  area_box <- if (have) {
    stat("Model area", fmt(sum(per_gsa[as.character(g)])),
         if (identical(basis, "medits_10_800")) "MEDITS strata 10-800 m, from the bathymetry (01)" else "whole GSA, coast to bottom (01)")
  } else if (identical(basis, "whole_gsa") && !is.null(.gsa_km2)) {
    stat("Model area", paste0("\u2248 ", fmt(sum(.gsa_km2[as.character(g)], na.rm = TRUE))), "from the map polygons; 01 computes the exact value")
  } else stat("Model area", "computed by 01", "these GSAs are not in the last run's model_area_cells.csv", warn = TRUE)
  uns <- intersect(g, .unsurveyed)
  fluidRow(
    stat("GSAs", length(g), paste(sort(g), collapse = ", ")),
    area_box,
    stat("Survey coverage", paste0(length(setdiff(g, .unsurveyed)), " of ", length(g)),
         if (length(uns)) paste0("GSA ", paste(uns, collapse = ", "), ": survey density assumed") else "all GSAs sampled by MEDITS",
         warn = length(uns) > 0),
    stat("Catch countries", length(unique(.gsa_state[as.character(g)])), paste(unique(.gsa_state[as.character(g)]), collapse = ", ")))
})

## --- Settings block: the exact code the Run button executes ---------------
## Paths written into R code must use "/" - on Windows a path such as
## C:\Users\... inside "..." is read as escape codes (\U) and the run fails.
.fwd <- function(p) gsub("\\\\", "/", p)
## --- Which scripts to run ---------------------------------------------------
## Tick boxes -> RUN_STEPS (1-4 = 01-04, 5 = 05 environment, 6 = 05b Ecospace maps,
## 7 = 06 diagnostics, always last). A later script reads what the earlier ones
## wrote to the output folder, so a script can run alone only if those files
## are already there (from an earlier run). What each one reads:
##   02: ecopath_ecosim_inputs.xlsx (Ecopath_B) and biomass_proportion_by_species_fg.csv, from 01
##   03: biomass_proportion_by_species_fg.csv from 01; F_by_fg.csv from 02
##   04: ecopath_ecosim_inputs.xlsx and biomass_proportion_by_species_fg.csv, from 01
##        (03's P/B, Q/B only feed the references sheet - used if present)
##   06 (step 7): ecopath_ecosim_inputs.xlsx from 01; F_by_fg.csv from 02
steps_sel <- reactive(sort(as.integer(input$steps_chk)))
steps_txt <- reactive({
  s <- steps_sel()
  if (!length(s)) "integer(0)"
  else if (length(s) == 1) as.character(s)
  else if (all(diff(s) == 1)) paste0(min(s), ":", max(s))
  else paste0("c(", paste(s, collapse = ", "), ")")
})
observeEvent(input$steps_all,    updateCheckboxGroupInput(session, "steps_chk", selected = 1:7))
observeEvent(input$steps_main,   updateCheckboxGroupInput(session, "steps_chk", selected = c(1:4, 7)))
observeEvent(input$steps_only_env, updateCheckboxGroupInput(session, "steps_chk", selected = 5:6))
observeEvent(input$steps_only_esp, updateCheckboxGroupInput(session, "steps_chk", selected = 6))
observeEvent(input$steps_none,   updateCheckboxGroupInput(session, "steps_chk", selected = character(0)))
observeEvent(input$steps_after1, updateCheckboxGroupInput(session, "steps_chk", selected = c(2:4, 7)))
observeEvent(input$steps_only_diag, updateCheckboxGroupInput(session, "steps_chk", selected = 7))
.step_needs <- list(
  `2` = list(list(1, "ecopath_ecosim_inputs.xlsx"), list(1, "biomass/biomass_proportion_by_species_fg.csv")),
  `3` = list(list(1, "biomass/biomass_proportion_by_species_fg.csv"), list(2, "fisheries/F_by_fg.csv")),
  `4` = list(list(1, "ecopath_ecosim_inputs.xlsx"), list(1, "biomass/biomass_proportion_by_species_fg.csv")),
  `7` = list(list(1, "ecopath_ecosim_inputs.xlsx"), list(2, "fisheries/F_by_fg.csv")))
.step_out <- c(`1` = "biomass", `2` = "fisheries", `3` = "pbqb-traits", `4` = "diet", `5` = "environment", `7` = "validation")
## settings lines that change results (no comments, RUN_STEPS, CLEAN_START or source())
.result_settings <- function(x) {
  x <- trimws(sub("#.*$", "", x[!grepl("^\\s*##", x)]))
  x[nzchar(x) & !grepl("^(RUN_STEPS|CLEAN_START)\\b|^source\\(", x)]
}
steps_problems <- reactive({
  s <- steps_sel()
  if (is.na(.base_out)) return(list(missing = character(0), stale = character(0), changed = character(0)))
  od <- .inputs_run_dir()   # this run's folder: the outputs of earlier runs with the same inputs
  missing <- character(0)
  for (k in s) for (nd in .step_needs[[as.character(k)]]) {
    p <- nd[[1]]; f <- nd[[2]]
    if (!(p %in% s) && !file.exists(file.path(od, f)))
      missing <- c(missing, sprintf("%s needs %s from %s, which is not in the output folder yet - tick %s too.",
                                    .step_names[k], basename(f), .step_names[p], .step_names[p]))
  }
  ## later scripts left unticked whose outputs on disk came from the old run of an earlier ticked one
  stale <- character(0)
  if (length(s)) {
    later <- setdiff(c(1:4, 7), s)   # the main scripts (not 05/05b)
    later <- later[later > min(s)]
    later <- later[dir.exists(file.path(od, .step_out[as.character(later)]))]
    if (length(later)) stale <- sprintf("The outputs of %s on disk will still come from the previous run, so they won't match the new %s.",
                                        paste(.step_names[later], collapse = ", "), .step_names[min(s)])
  }
  ## skipped earlier scripts: were their outputs made with the same settings?
  changed <- character(0)
  if (length(s) && min(s) > 1 && file.exists(.settings_file)) {
    old <- .result_settings(.settings_lines()); new <- .result_settings(config_code())
    diff_new <- setdiff(new, old)
    if (length(diff_new)) changed <- diff_new
  }
  list(missing = unique(missing), stale = stale, changed = changed)
})
output$steps_check <- renderUI({
  pr <- steps_problems()
  out <- list()
  if (length(pr$missing))
    out <- c(out, list(tags$div(class = "alert alert-danger py-1 px-2 mb-1 small", lapply(pr$missing, tags$div))))
  if (length(pr$stale))
    out <- c(out, list(tags$div(class = "alert alert-warning py-1 px-2 mb-1 small", pr$stale)))
  if (length(pr$changed))
    out <- c(out, list(tags$div(class = "alert alert-warning py-1 px-2 mb-1 small",
      tags$div(sprintf("Settings differ from the last run (%s), whose outputs the unticked earlier scripts would reuse. Changed or new:",
                       format(file.mtime(.settings_file), "%d %b %H:%M"))),
      tags$code(style = "white-space: pre-wrap; font-size: .8rem;", paste(pr$changed, collapse = "\n")),
      tags$div("Tick the earlier scripts too if those settings should apply to them."))))
  tagList(out)
})

config_code <- reactive({
  yr <- function(v) paste0(v[1], ":", v[2])
  tf <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"
  common <- c(
    sprintf("YEAR_ECOPATH     <- %s   # Ecopath baseline", yr(input$ecopath_years)),
    sprintf("TS_YEARS         <- %s   # Ecosim biomass series", yr(input$ts_years)),
    sprintf("STRATA           <- %s", tf(input$strata)),
    sprintf("DROP_OUTLIERS    <- %s", tf(input$drop_outliers)),
    sprintf("NORMALIZE_TS     <- %s", tf(input$normalize_ts)),
    sprintf("ECOPATH_B_FILL_MODE  <- \"%s\"   # FGs with no Ecopath-years biomass", if (isTRUE(input$fill_b)) "window_mean" else "nearest"),
    if (isTRUE(input$fill_b) && length(input$fill_years) == 2) sprintf("ECOPATH_B_FILL_YEARS <- %s", yr(input$fill_years)),
    if (!is.na(fg_path())) sprintf("FG_SPECIES_FILE  <- \"%s\"   # species -> FG table", .fwd(fg_path())),
    sprintf("APPLY_CATCHABILITY <- %s   # MEDITS catchability correction", tf(input$catch_q)),
    if (isTRUE(input$catch_q) && !is.na(catch_path())) sprintf("CATCHABILITY_FILE <- \"%s\"   # uploaded catchability table", .fwd(catch_path())),
    if (!is.na(other_survey())) sprintf("OTHER_SURVEY_FILE <- \"%s\"   # biomass from any survey: species, year, biomass_t_km2", .fwd(other_survey())),
    if (identical(input$esp_grid_mode, "file") && !is.na(ecospace_depth())) sprintf("ECOSPACE_DEPTH_FILE <- \"%s\"   # Ecospace base map (05b)", .fwd(ecospace_depth())),
    if (!identical(input$esp_grid_mode, "file") && is.finite(as.numeric(input$esp_cell_arcmin)))
      sprintf("ECOSPACE_CELL_SIZE <- %s/60   # Ecospace base map at %s arc-min, depth from EMODnet (05b)", input$esp_cell_arcmin, input$esp_cell_arcmin),
    if (6 %in% steps_sel() && !is.null(input$esp_step)) sprintf("ECOSPACE_DRIVER_STEP <- \"%s\"", input$esp_step),
    if (6 %in% steps_sel() && !is.null(input$esp_scaling)) sprintf("ECOSPACE_DRIVER_SCALING <- \"%s\"", input$esp_scaling),
    if (!is.null(survey_files()$TA)) sprintf("MEDITS_TA_FILE   <- \"%s\"   # own MEDITS hauls", .fwd(survey_files()$TA$path)),
    if (!is.null(survey_files()$TB)) sprintf("MEDITS_TB_FILE   <- \"%s\"   # own MEDITS catch", .fwd(survey_files()$TB$path)),
    if (!is.null(survey_files()$TC)) sprintf("MEDITS_TC_FILE   <- \"%s\"   # own MEDITS length/maturity", .fwd(survey_files()$TC$path)),
    if (!is.null(survey_files()$MEDIAS_abundance) && !is.null(survey_files()$MEDIAS_biomass))
      sprintf("MEDIAS_DIR       <- \"%s\"   # own MEDIAS abundance.csv + biomass.csv", .fwd(dirname(survey_files()$MEDIAS_biomass$path))),
    sprintf("PHYTO_SOURCE     <- \"%s\"   # phytoplankton B and P/B: isimip / copernicus (05)", if (is.null(input$phyto_src)) "isimip" else input$phyto_src))
  head <- c(sprintf("## WMed-EwEbuilder settings - from WMed-EwEbuilder.qmd, %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
            "## Run from the scripts/ folder of the repo (folders found by wmed_paths.R).",
            "CLEAN_START      <- FALSE   # keep the settings below")
  if (identical(input$area_mode, "custom")) {
    c(head,
      sprintf("RUN_STEPS        <- %s", steps_txt()),
      "AREA_MODE        <- \"custom\"",
      sprintf("AREA_NAME        <- \"%s\"", gsub("[^A-Za-z0-9_-]", "_", input$area_name)),
      if (identical(input$custom_type, "shapefile")) c(
        "CUSTOM_AREA_TYPE <- \"shapefile\"",
        sprintf("CUSTOM_SHAPEFILE_PATH <- \"%s\"", if (is.null(custom_shape())) "<upload the region file>" else .fwd(custom_shape()$path)))
      else c(
        "CUSTOM_AREA_TYPE <- \"bbox\"",
        sprintf("CUSTOM_BBOX      <- c(xmin = %s, xmax = %s, ymin = %s, ymax = %s)",
                input$bbox_xmin, input$bbox_xmax, input$bbox_ymin, input$bbox_ymax)),
      common,
      "## the ticked steps run for the region; catch (02) from the GSA(s) it overlaps, weighted by overlap",
      "source(\"run_pipeline_demo.R\")")
  } else {
    c(head,
      sprintf("RUN_STEPS        <- %s", steps_txt()),
      sprintf("MODEL_GSAS       <- c(%s)", paste(sort(as.numeric(input$gsas)), collapse = ", ")),
      sprintf("MODEL_AREA_BASIS <- \"%s\"", input$basis),
      common,
      "## catch / effort years (02) follow the Ecopath and Ecosim years",
      "source(\"run_pipeline_demo.R\")")
  }
})
output$config_text <- renderText(paste(config_code(), collapse = "\n"))
output$dl_config <- downloadHandler(
  filename = function() if (identical(input$area_mode, "custom")) "wmed_custom_settings.R" else "wmed_pipeline_settings.R",
  content = function(file) writeLines(config_code(), file))

## --- Run -------------------------------------------------------------------
run_state <- reactiveVal("idle")          # idle | running | done | error
run_started <- reactiveVal(NULL)
run_finished <- reactiveVal(NULL)
run_steps <- reactiveVal(c(1:4, 7))
step_seen <- reactiveVal(list())          # step number -> time its start line was first seen
last_finished <- reactiveVal(Sys.time())  # changes after every run -> images reload
run_note <- reactiveVal("")

.pid_alive <- function() {
  pid <- if (file.exists(.pid_file)) suppressWarnings(as.integer(readLines(.pid_file, warn = FALSE)[1])) else NA
  if (is.na(pid)) return(FALSE)
  if (.Platform$OS.type == "windows") {   # pskill(pid, 0) does not test a process on Windows
    out <- tryCatch(suppressWarnings(system2("tasklist", c("/FI", shQuote(paste("PID eq", pid)), "/NH", "/FO", "CSV"),
                                             stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    return(any(grepl(sprintf('"%d"', pid), out, fixed = TRUE)))
  }
  isTRUE(tryCatch(tools::pskill(pid, 0L), error = function(e) FALSE))
}
## Text read from the run's files is made valid UTF-8 before it is shown:
## a Windows message in the run's console (e.g. the Microsoft Store
## "Python not found" text, in the system code page) is not UTF-8, and
## one such line closed the page's connection ("Could not decode a text
## frame as UTF-8") - the page went grey (2026-10-27).
.utf8 <- function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], "CP1252", "UTF-8", sub = "?")
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "?")
  x
}
.settings_lines <- function() if (file.exists(.settings_file)) readLines(.settings_file, warn = FALSE) else character(0)

## Minutes each script took, per run (run = start time of the run)
.times_file <- file.path(.run_dir, "step_times_v2.csv")   # v2: steps renumbered 2026-11 (5 = environment, 7 = diagnostics)
.step_done_min <- function() {   # minutes of the scripts done so far in this run, named by step
  l <- if (file.exists(.progress_file)) readLines(.progress_file, warn = FALSE) else character(0)
  l <- grep("^--- Step \\d/\\d done in [0-9.]+ min", l, value = TRUE)
  setNames(as.numeric(sub("^--- Step \\d/\\d done in ([0-9.]+) min.*", "\\1", l)), sub("^--- Step (\\d)/\\d.*", "\\1", l))
}
.record_step_times <- function() {
  m <- .step_done_min(); if (!length(m) || !file.exists(.settings_file)) return(invisible())
  run <- format(file.mtime(.settings_file), "%Y-%m-%d %H:%M:%S")
  old <- if (!file.exists(.times_file)) NULL else tryCatch(utils::read.csv(.times_file, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(old) && run %in% old$run) return(invisible())
  new <- data.frame(run = run, step = as.integer(names(m)), minutes = unname(m))
  tryCatch(utils::write.csv(rbind(old, new), .times_file, row.names = FALSE), error = function(e) NULL)
}
.step_min_now <- function() {
  d <- .step_min_default
  h <- if (!file.exists(.times_file)) NULL else tryCatch(utils::read.csv(.times_file, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(h) && nrow(h)) for (k in seq_along(d)) {
    x <- utils::tail(h$minutes[h$step == k & is.finite(h$minutes)], 5)
    if (length(x)) d[k] <- stats::median(x)
  }
  d[!is.finite(d)] <- 2   # a script with no timing yet
  d
}

## Reopened page: pick up the last run started from here (finished, failed,
## or still going)
if (file.exists(.settings_file)) {
  run_started(file.mtime(.settings_file))
  .st <- grep("^RUN_STEPS", .settings_lines(), value = TRUE)
  run_steps(tryCatch(eval(parse(text = sub("^[^<]*<-", "", .st[1]))), error = function(e) c(1:4, 7)))
  if (file.exists(.status_file)) {
    run_state(if (identical(readLines(.status_file, warn = FALSE)[1], "done")) "done" else "error")
    run_finished(file.mtime(.status_file))
    if (identical(isolate(run_state()), "done")) .record_step_times()   # a run that ended while the page was closed (isolate: no reactive context here)
  } else if (.pid_alive()) {
    run_state("running")
  } else {
    run_state("error"); run_finished(file.mtime(.console))
    run_note("The last run from this page stopped without finishing (R or RStudio was closed?).")
  }
}

observe({   # Run button label; disabled while running or with no shapefile (disabled= needs shiny >= 1.11)
  custom <- identical(input$area_mode, "custom")
  no_shape <- custom && identical(input$custom_type, "shapefile") && is.null(custom_shape())
  lab <- if (identical(run_state(), "running")) "Running..." else if (custom) "Run pipeline for this region" else "Run pipeline"
  if (utils::packageVersion("shiny") >= "1.11.0")
    updateActionButton(session, "run", label = lab, disabled = no_shape || identical(run_state(), "running")) else
    updateActionButton(session, "run", label = lab)
})

## Stop: ends the run's R process and everything it started (e.g. a
## copernicusmarine download), after a confirmation. Outputs of scripts
## that already finished stay; the script that was running is incomplete.
observe({
  if (utils::packageVersion("shiny") >= "1.11.0")
    updateActionButton(session, "stop", disabled = !identical(run_state(), "running"))
})
observeEvent(input$stop, {
  if (!identical(run_state(), "running")) { showNotification("No run is going.", duration = 4); return() }
  showModal(modalDialog(title = "Stop the run?",
    "The script that is running now stops halfway; scripts that already finished keep their outputs.",
    footer = tagList(modalButton("Cancel"), actionButton("stop_yes", "Stop the run", class = "btn-danger"))))
})
.stopped_by_user <- function() file.exists(.status_file) && isTRUE(grepl("stopped by the user", readLines(.status_file, warn = FALSE)[1]))
observeEvent(input$stop_yes, {
  removeModal()
  pid <- if (file.exists(.pid_file)) suppressWarnings(as.integer(readLines(.pid_file, warn = FALSE)[1])) else NA
  if (!is.na(pid)) {
    if (.Platform$OS.type == "windows") {
      ## /T also ends the processes the run started (Rscript children, copernicusmarine)
      suppressWarnings(system2("taskkill", c("/PID", pid, "/T", "/F"), stdout = FALSE, stderr = FALSE))
    } else {
      suppressWarnings(system2("pkill", c("-TERM", "-P", pid), stdout = FALSE, stderr = FALSE))
      tools::pskill(pid)
    }
  }
  writeLines("error: stopped by the user", .status_file)
  run_state("error"); run_finished(Sys.time()); last_finished(Sys.time())
  run_note("Stopped by the user - outputs of the scripts that had finished are kept.")
  showNotification("Run stopped.", type = "warning", duration = 6)
})

observeEvent(input$run, {
  custom <- identical(input$area_mode, "custom")
  if (identical(run_state(), "running")) return()
  if (custom && identical(input$custom_type, "shapefile") && is.null(custom_shape())) {
    showNotification("Upload the region file first.", type = "error"); return() }
  if (!custom && !length(input$gsas)) { showNotification("Select at least one GSA.", type = "error"); return() }
  if (!length(steps_sel())) { showNotification("Tick at least one script to run.", type = "error"); return() }
  .chk <- steps_problems()
  if (length(.chk$missing)) {
    showNotification(paste(c("Can't start:", .chk$missing), collapse = " "), type = "error", duration = 15); return() }
  ## 05 needs a Copernicus Marine login: ask for it first if this laptop has none
  if (5 %in% steps_sel() && !.cm_has_login()) { .cm_login_modal(then_run = TRUE); return() }
  .start_run()
})

## Starts the run (Run button, or after the Copernicus login window)
.start_run <- function() {
  custom <- identical(isolate(input$area_mode), "custom")
  unlink(c(.status_file, .progress_file, .console, .pid_file))
  writeLines(isolate(config_code()), .settings_file)
  Sys.setenv(WMED_PROGRESS_FILE = .progress_file)   # inherited by the Rscript process
  ## The settings block, with the final source() wrapped so the result is reported back here
  body <- isolate(config_code())
  body <- body[!grepl("^##", body)]
  body <- sub("^source\\(\"01_biomass.R\"\\).*$", paste0(
    "res <- tryCatch({ cat('--- Step 1/5: 01_biomass.R\\n', file = Sys.getenv('WMED_PROGRESS_FILE'), append = TRUE); ",
    "source('01_biomass.R'); cat('--- Step 1/5 done\\n', file = Sys.getenv('WMED_PROGRESS_FILE'), append = TRUE); 'done' }, ",
    "error = function(e) paste('error:', conditionMessage(e)))"), body)
  ## Windows: the run writes its own console.txt through an R connection.
  ## With system2(stdout = file) Windows keeps that file locked while the
  ## run goes, so this page could not read it ("cannot open the
  ## connection") and the box stayed empty. Every message is flushed at
  ## once so the box is live; system2's own output (R start-up errors
  ## only) goes to console_raw.txt.
  win_head <- if (.Platform$OS.type == "windows") c(
    sprintf(".wm_con <- file('%s', open = 'wt')", .fwd(.console)),
    "sink(.wm_con); sink(.wm_con, type = 'message')",
    "globalCallingHandlers(message = function(m) { cat(conditionMessage(m), file = .wm_con); flush(.wm_con); invokeRestart('muffleMessage') })")
  win_tail <- if (.Platform$OS.type == "windows") "flush(.wm_con); sink(type = 'message'); sink(); close(.wm_con)"
  code <- paste(c(sprintf("writeLines(as.character(Sys.getpid()), '%s')", .fwd(.pid_file)), win_head, sub("^source\\(\"run_pipeline_demo.R\"\\)$",
                      "res <- tryCatch({ source('run_pipeline_demo.R'); 'done' }, error = function(e) paste('error:', conditionMessage(e)))", body),
                  win_tail, sprintf("writeLines(res, '%s')", .fwd(.status_file))), collapse = "\n")
  old <- setwd(file.path(.git_dir, "scripts")); on.exit(setwd(old))
  ## The run's code goes in a file: on Windows a multi-line "Rscript -e" does
  ## not survive the command line (cmd.exe), so the run never started there.
  .run_script <- file.path(.run_dir, "run.R")
  writeLines(code, .run_script)
  .out_file <- if (.Platform$OS.type == "windows") file.path(.run_dir, "console_raw.txt") else .console
  system2(file.path(R.home("bin"), "Rscript"), shQuote(.fwd(.run_script)), wait = FALSE, stdout = .out_file, stderr = .out_file)
  run_state("running"); run_started(Sys.time()); run_finished(NULL); run_note(""); step_seen(list())
  run_target(isolate(.inputs_run_dir()))
  run_steps(isolate(steps_sel()))
  showNotification(paste0("Pipeline started - outputs in ", .run_label(isolate(run_target())), ". Progress is shown below the button."), duration = 8)
}

## --- Copernicus Marine login (needed by 05 environment) -------------------
## Each user types their own account once in a window on the page. It is
## saved in that user's own .Renviron (as COPERNICUSMARINE_SERVICE_USERNAME
## / _PASSWORD, read by lib_cmems_phytoplankton_biomass.R) and set for this
## R session, so the run started from here inherits it. It never goes into
## the settings block, the run's script or the logs.
.renviron_candidates <- function() {
  up <- Sys.getenv("USERPROFILE")
  unique(c(path.expand("~/.Renviron"), if (nzchar(up)) file.path(up, c(".Renviron", "Documents/.Renviron"))))
}
.cm_has_login <- function() {
  if (!nzchar(Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME")))
    for (f in .renviron_candidates()) if (file.exists(f)) try(readRenviron(f), silent = TRUE)
  if (nzchar(Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME")) && nzchar(Sys.getenv("COPERNICUSMARINE_SERVICE_PASSWORD"))) return(TRUE)
  up <- Sys.getenv("USERPROFILE")
  any(file.exists(unique(c(path.expand("~/.copernicusmarine/.copernicusmarine-credentials"),
                           if (nzchar(up)) file.path(up, ".copernicusmarine", ".copernicusmarine-credentials")))))
}
cm_then_run <- reactiveVal(FALSE)
.cm_login_modal <- function(then_run = FALSE) {
  cm_then_run(then_run)
  showModal(modalDialog(title = "Copernicus Marine login",
    tags$p("05 environment downloads data from the Copernicus Marine Service, which needs your own free account (",
           tags$a("register here", href = "https://data.marine.copernicus.eu/register", target = "_blank"), ")."),
    textInput("cm_user", "Username", value = Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME")),
    passwordInput("cm_pass", "Password"),
    tags$small(class = "text-muted", "Saved only on this computer, in your own .Renviron file; never in the code, the settings or the logs."),
    footer = tagList(modalButton("Cancel"), actionButton("cm_save", if (then_run) "Save and run" else "Save", class = "btn-primary"))))
}
observeEvent(input$cm_login_link, .cm_login_modal(then_run = FALSE))
observeEvent(input$cm_save, {
  u <- trimws(input$cm_user); p <- input$cm_pass
  if (!nzchar(u) || !nzchar(p)) { showNotification("Type the username and the password.", type = "error"); return() }
  Sys.setenv(COPERNICUSMARINE_SERVICE_USERNAME = u, COPERNICUSMARINE_SERVICE_PASSWORD = p)
  f <- path.expand("~/.Renviron")
  ok <- tryCatch({
    old <- if (file.exists(f)) readLines(f, warn = FALSE) else character(0)
    old <- old[!grepl("^COPERNICUSMARINE_SERVICE_(USERNAME|PASSWORD)=", old)]
    writeLines(c(old, paste0("COPERNICUSMARINE_SERVICE_USERNAME=", u), paste0("COPERNICUSMARINE_SERVICE_PASSWORD=", p)), f)
    Sys.chmod(f, mode = "0600"); TRUE
  }, error = function(e) FALSE)
  removeModal()
  showNotification(if (ok) "Copernicus login saved." else "Login set for this session only (could not write .Renviron).",
                   type = if (ok) "message" else "warning", duration = 6)
  if (isTRUE(cm_then_run())) { cm_then_run(FALSE); .start_run() }
})

## Poll the status file while a run is going
observe({
  if (!identical(run_state(), "running")) return()
  invalidateLater(5000)
  if (file.exists(.status_file)) {
    res <- readLines(.status_file, warn = FALSE)[1]
    run_state(if (identical(res, "done")) "done" else "error")
    run_finished(file.mtime(.status_file))
    if (!is.null(run_target()) && dir.exists(run_target())) .results_dir(run_target())   # show this run's results
    last_finished(Sys.time())
    if (identical(res, "done")) .record_step_times()
    showNotification(if (identical(res, "done")) "Pipeline finished - plots updated." else res,
                     type = if (identical(res, "done")) "message" else "error", duration = NULL)
  } else if (file.exists(.pid_file) && !.pid_alive()) {
    run_state("error"); run_finished(Sys.time()); last_finished(Sys.time())
    run_note("The run stopped without finishing - see the last lines below.")
  }
})

## Progress: read the "--- Step i/7: ..." / "--- Step i/7 done in x min ---"
## lines that run_pipeline_demo.R writes, and estimate the share done from
## the expected minutes per script.
output$run_progress <- renderUI({
  invalidateLater(3000)
  st <- run_state()
  if (identical(st, "idle")) {
    steps <- steps_sel()
    if (!length(steps)) return(tags$small(class = "text-danger", "Tick at least one script."))
    sm <- .step_min_now()
    return(tags$small(class = "text-muted", sprintf("Expected run time about %.0f min (%s; from the last runs on this laptop).",
                                                   sum(sm[steps]), paste(sprintf("%s %.0f", .step_names[steps], sm[steps]), collapse = ", "))))
  }
  lines <- if (file.exists(.progress_file)) readLines(.progress_file, warn = FALSE) else character(0)
  started <- as.integer(sub("^--- Step (\\d)/\\d:.*", "\\1", grep("^--- Step \\d/\\d: ", lines, value = TRUE)))
  finished <- as.integer(sub("^--- Step (\\d)/\\d done.*", "\\1", grep("^--- Step \\d/\\d done", lines, value = TRUE)))
  seen <- isolate(step_seen())
  for (k in setdiff(started, as.integer(names(seen)))) seen[[as.character(k)]] <- Sys.time()
  isolate(step_seen(seen))
  ## time left = expected minutes of the scripts not started yet + what is
  ## left of the expected time of the running one (at least 5 % of it)
  sm <- .step_min_now(); took <- .step_done_min()
  steps <- run_steps()
  current <- setdiff(started, finished); current <- if (length(current)) max(current) else NA
  done_min <- sum(took[as.character(intersect(finished, steps))], na.rm = TRUE)
  cur_min <- if (!is.na(current)) as.numeric(difftime(Sys.time(), seen[[as.character(current)]], units = "mins")) else 0
  left_min <- sum(sm[setdiff(steps, c(finished, current))]) +
              (if (!is.na(current)) max(sm[current] - cur_min, 0.05 * sm[current]) else 0)
  pct <- if (st == "done") 100 else if (st == "error") round(100 * done_min / max(done_min + left_min, 1e-9)) else round(100 * (done_min + cur_min) / (done_min + cur_min + left_min))
  elapsed <- as.numeric(difftime(if (is.null(run_finished())) Sys.time() else run_finished(), run_started(), units = "mins"))
  label <- switch(st,
    running = paste0(if (!is.na(current)) paste0("Running ", .step_names[current]) else "Starting",
                     " - ", length(intersect(finished, steps)), " of ", length(steps), " script(s) done - ",
                     sprintf("%.1f", elapsed), " min elapsed, about ", sprintf("%.0f", left_min), " min left"),
    done = sprintf("Finished %s, in %.1f min", format(run_finished(), "%d %b %H:%M"), elapsed),
    error = if (.stopped_by_user()) "Stopped with the Stop button" else "Stopped with an error - see below")
  colour <- switch(st, running = "bg-info", done = "bg-success", error = "bg-danger")
  ## one chip per script: done / running / waiting
  chips <- lapply(steps, function(k) {
    cls <- if (k %in% finished || st == "done") "bg-success"
           else if (identical(k, current)) (if (st == "error") "bg-danger" else "bg-info")
           else "bg-light text-dark"
    tags$span(class = paste("badge rounded-pill me-1 mb-1", cls), style = "font-size: .95rem; padding: .45em .8em;", .step_names[k])
  })
  tagList(
    tags$div(style = "font-size: 1.15rem; font-weight: 600; margin-bottom: .35rem;",
             switch(st, running = if (!is.na(current)) paste0("Step ", current, "/7: ", .step_names[current]) else "Starting...",
                    done = "Finished", error = if (.stopped_by_user()) "Stopped" else "Stopped with an error")),
    tags$div(class = "progress mb-2", style = "height: 34px; border-radius: 17px; font-size: 1.05rem;",
             tags$div(class = paste("progress-bar progress-bar-striped", colour, if (st == "running") "progress-bar-animated"),
                      role = "progressbar", style = paste0("width: ", pct, "%; font-weight: 600;"), paste0(pct, "%"))),
    tags$div(class = "mb-2", chips),
    tags$div(style = "font-size: 1rem;", label))
})

output$run_status <- renderText({
  invalidateLater(5000)
  st <- run_state()
  .tail30 <- function(f) if (file.exists(f)) tryCatch(paste(.utf8(tail(readLines(f, warn = FALSE), 30)), collapse = "\n"),
                                                    error = function(e) "") else ""
  tail_txt <- .tail30(.console)
  tail_txt <- gsub(normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE), "~", gsub("\\\\", "/", tail_txt), fixed = TRUE)   # no user name on screen
  ## Windows: R start-up errors land in console_raw.txt (see the Run button)
  if (!nzchar(tail_txt) && st == "error") tail_txt <- .tail30(file.path(.run_dir, "console_raw.txt"))
  paste0("Status: ", st, "\nOutput folder: ", .show_path(.out_dir),
         if (st == "error" && file.exists(.status_file)) paste0("\n", .utf8(readLines(.status_file, warn = FALSE)[1])) else "",
         if (nzchar(run_note())) paste0("\n", run_note()) else "",
         if (nzchar(tail_txt)) paste0("\n--- last lines ---\n", tail_txt) else "")
})

## --- Files under out_dir for the figures in the text ------------------------
.ctype <- c(png = "image/png", csv = "text/csv", pdf = "application/pdf", txt = "text/plain",
            xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
.file_url <- session$registerDataObj("wmfile", .base_out, function(data, req) {
  q <- shiny::parseQueryString(req$QUERY_STRING)
  data <- .out_dir                                  # the results folder shown now
  root <- normalizePath(data, mustWork = FALSE)
  f <- normalizePath(file.path(data, if (is.null(q$f)) "" else q$f), mustWork = FALSE)
  ext <- tolower(tools::file_ext(f))
  if (is.null(q$f) || !startsWith(f, root) || !file.exists(f) || dir.exists(f) || !ext %in% names(.ctype))
    return(list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "not found"))
  list(status = 200L, headers = list("Content-Type" = .ctype[[ext]]), body = readBin(f, "raw", file.info(f)$size))
})
## Images kept in the repo (top folder and docs/): the workflow figure
.repo_url <- session$registerDataObj("wmrepo", .git_dir, function(data, req) {
  q <- shiny::parseQueryString(req$QUERY_STRING)
  root <- normalizePath(data, mustWork = FALSE)
  f <- normalizePath(file.path(data, if (is.null(q$f)) "" else q$f), mustWork = FALSE)
  ext <- tolower(tools::file_ext(f))
  types <- c(jpg = "image/jpeg", jpeg = "image/jpeg", png = "image/png", svg = "image/svg+xml")
  if (is.null(q$f) || !startsWith(f, root) || !file.exists(f) || !ext %in% names(types))
    return(list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "not found"))
  list(status = 200L, headers = list("Content-Type" = types[[ext]]), body = readBin(f, "raw", file.info(f)$size))
})
observe({   # tell the page where to load the figures from; again after every run
  last_finished()
  session$sendCustomMessage("wmfile-base", list(base = .file_url, repo = .repo_url, v = as.integer(Sys.time())))
})

## --- Folders on this computer -------------------------------------------------
output$folders <- renderUI({
  row <- function(label, path, from, ok)
    tags$tr(tags$td(if (ok) "\u2705" else "\u274C"), tags$td(tags$strong(label)),
            tags$td(tags$code(if (is.na(path)) "not found" else .show_path(path))), tags$td(class = "text-muted", if (!is.na(from)) from))
  tagList(
    tags$table(class = "table table-sm mb-2", style = "font-size:.85rem;",
      row("Repo (git_dir)", .paths$git_dir, "this page's folder", dir.exists(file.path(.paths$git_dir, "scripts"))),
      row("Shared data (pCloud, read-only)", .paths$shared_dir, "copied from, never read by the run", !is.na(.paths$shared_dir) || isTRUE(.paths$using_local_copy)),
      row("Data used by the run (pcloud_dir)", .paths$pcloud_dir, .paths$pcloud_from, isTRUE(.paths$using_local_copy) || isTRUE(.paths$use_shared)),
      row("Outputs (out_dir)", .paths$out_dir, .paths$out_from, dir.exists(.paths$out_dir))),
    tags$div(class = "d-flex align-items-center gap-2 mb-2",
      actionButton("sync_local", if (isTRUE(.paths$using_local_copy)) "Update local data" else "Make local copy of the data",
                   class = "btn-sm btn-outline-primary", icon = icon("download")),
      tags$small(class = "text-muted", if (isTRUE(.paths$using_local_copy)) {
        inf <- file.path(.paths$local_data_dir, "LOCAL_COPY_INFO.txt")
        paste0("The run reads the local copy ", if (file.exists(inf)) paste0("(", sub("^synced: ", "last ", grep("^synced", readLines(inf), value = TRUE)[1]), ")") else "",
               ". Update it after the shared data change; only new or changed files are copied.")
      } else "No local copy yet: the first run makes it (~2.5 GB), or click here. The pCloud folder is then never read or written by a run.")),
    if (is.na(.paths$pcloud_dir)) tags$div(class = "alert alert-danger py-2 small",
      "The pCloud folder '", .wmed_pcloud_name, "' was not found on this computer. Is pCloud Drive running? ",
      "Otherwise paste its path below and save."),
    tags$details(open = if (is.na(.paths$pcloud_dir)) NA else NULL,
      tags$summary(class = "small text-muted", "Change the folders for this computer"),
      fluidRow(
        column(5, textInput("set_pcloud", "pCloud folder", value = if (is.na(.paths$pcloud_dir)) "" else .show_path(.paths$pcloud_dir), width = "100%")),
        column(5, textInput("set_out", "Output folder", value = .show_path(.paths$out_dir), width = "100%")),
        column(2, tags$label(HTML("&nbsp;")), actionButton("save_folders", "Save", class = "btn-outline-primary w-100"))),
      tags$div(class = "wm-help", "Saved in config.local.R in the repo, for this computer only (not committed). Reopen the page after saving.")))
})
observeEvent(input$sync_local, {
  if (is.na(.paths$shared_dir) || !dir.exists(.paths$shared_dir)) {
    showNotification("Shared (pCloud) folder not found - start pCloud Drive, then try again.", type = "error"); return() }
  id <- showNotification("Copying new or changed files from the shared folder ...", duration = NULL, type = "message")
  res <- tryCatch(withProgress(message = "Local data copy", value = 0,
                    wmed_sync_local_data(.paths$shared_dir, .paths$local_data_dir,
                                         progress = function(k, n, f) setProgress(k / n, detail = sprintf("%d / %d files", k, n)))),
                  error = function(e) e)
  removeNotification(id)
  if (inherits(res, "error")) { showNotification(paste("Copy failed:", conditionMessage(res)), type = "error", duration = NULL); return() }
  showNotification(sprintf("Local copy up to date: %d new, %d updated, %d unchanged files (%s MB copied).%s",
                           res$new, res$updated, res$unchanged, res$MB_copied,
                           if (!isTRUE(.paths$using_local_copy)) " Refresh the page (reload in the browser) so it uses the copy." else ""), duration = NULL)
})
observeEvent(input$save_folders, {
  pc <- path.expand(trimws(input$set_pcloud)); od <- path.expand(trimws(input$set_out))
  if (!dir.exists(pc)) { showNotification(paste("Folder not found:", pc), type = "error"); return() }
  if (!dir.exists(od) && !dir.create(od, recursive = TRUE, showWarnings = FALSE)) {
    showNotification(paste("Cannot create:", od), type = "error"); return() }
  f <- wmed_save_local_config(.git_dir, out_dir = od, pcloud_dir = pc)
  showNotification(paste("Saved to", f, "- stop the page and open it again (Run Document)."), duration = NULL)
})

## --- Downloads: the output files of the last run ----------------------------
makeActiveBinding(".wb_path", function() if (!is.na(.out_dir)) file.path(.out_dir, "ecopath_ecosim_inputs.xlsx") else NA_character_, environment())
makeActiveBinding(".diet_path", function() if (!is.na(.out_dir)) file.path(.out_dir, "diet", "diet_matrix_main_vs_reference.xlsx") else NA_character_, environment())
.out_tables <- function(pattern) {   # csv/xlsx under out_dir (not the plots or the page's run files)
  if (is.na(.out_dir)) return(character(0))
  f <- list.files(.out_dir, "\\.(csv|xlsx)$", recursive = TRUE)
  f <- f[!grepl("^(plots|plots_old|\\.qmd_run|isimip3a_cache|shapefiles|GFCM_GSA_shp|runs|previous_runs)", f)]
  f[grepl(pattern, f)]
}
.send_file <- function(path) function(file) {
  if (is.function(path)) path <- path()   # the file of the results folder shown when the button is clicked
  if (!file.exists(path)) stop("Not found: ", path, " - run the pipeline first.")
  file.copy(path, file)
}
.send_zip <- function(pattern) function(file) {
  f <- .out_tables(pattern)
  if (!length(f)) stop("No tables found in ", .out_dir, " - run the pipeline first.")
  old <- setwd(.out_dir); on.exit(setwd(old))
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(file, f) else utils::zip(file, f, flags = "-q")
}
.stamp <- function() format(Sys.time(), "%Y%m%d")
output$dl_workbook <- downloadHandler(function() paste0("ecopath_ecosim_inputs_", .stamp(), ".xlsx"), .send_file(function() .wb_path))
output$dl_diet     <- downloadHandler(function() paste0("diet_matrix_main_vs_reference_", .stamp(), ".xlsx"), .send_file(function() .diet_path))
output$dl_review   <- downloadHandler(function() paste0("WMed_EwE_REVIEW_tables_", .stamp(), ".zip"), .send_zip("REVIEW"))
output$dl_all      <- downloadHandler(function() paste0("WMed_EwE_all_tables_", .stamp(), ".zip"), .send_zip("."))
output$dl_info <- renderUI({
  last_finished()
  if (is.na(.wb_path) || !file.exists(.wb_path)) return(tags$small(class = "text-muted", " No workbook yet - run the pipeline."))
  tags$small(class = "text-muted", sprintf(" Workbook written %s; %d tables in the output folder.",
                                           format(file.mtime(.wb_path), "%d %b %Y, %H:%M"), length(.out_tables("."))))
})

## --- Setup check (card 0): what a new computer needs ------------------------
## One line per item: ok / problem and what to do. Nothing is installed or
## changed here; it only looks.
.setup_items <- function() {
  row <- function(item, ok, detail, fix = "") data.frame(item = item, ok = ok, detail = detail, fix = fix, stringsAsFactors = FALSE)
  out <- list()
  out[[1]] <- row("Repository", !is.na(.git_dir) && file.exists(file.path(.git_dir, "scripts", "01_biomass.R")), .show_path(.git_dir))
  pc <- .paths$pcloud_dir
  out[[2]] <- row("Data folder (local copy)", isTRUE(.paths$using_local_copy) || (!is.na(pc) && dir.exists(pc)),
                  if (is.na(pc)) "not found" else paste0(.show_path(pc), if (isTRUE(.paths$using_local_copy)) " (local copy)" else " (shared folder - a local copy is made on the first run)"),
                  "Start pCloud Drive, or set the folder above (card 0).")
  out[[3]] <- row("FG table", !is.na(pc) && file.exists(file.path(pc, "data", "FG_WMed_2026.csv")),
                  if (is.na(pc)) "-" else .show_path(file.path(pc, "data", "FG_WMed_2026.csv")), "Check that pCloud has finished syncing the data folder.")
  wr <- !is.na(.base_out) && dir.exists(.base_out) && {
    tf <- file.path(.base_out, ".write_test"); ok <- tryCatch(file.create(tf), error = function(e) FALSE); unlink(tf); isTRUE(ok) }
  out[[4]] <- row("Output folder writable", wr, if (is.na(.base_out)) "not set" else .show_path(.base_out), "Choose another output folder above.")
  need <- c("data.table", "ggplot2", "openxlsx", "readxl", "sf", "terra", "raster", "rfishbase", "duckdbfs", "worrms",
            "httr", "jsonlite", "ncdf4", "pdftools", "TropFishR", "ragg", "patchwork", "shiny", "rmarkdown")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  out[[5]] <- row("R packages", !length(miss), if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else
                    paste0("all ", length(need), " core packages found (R ", getRversion(), ")"),
                  "They are installed on the first run (install_packages.R), or run source('install_packages.R') in the scripts folder.")
  old_to <- options(timeout = 8); on.exit(options(old_to), add = TRUE)
  net <- tryCatch({ con <- url("https://cloud.r-project.org", open = "rb"); close(con); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
  out[[6]] <- row("Internet", net, if (net) "reachable" else "no connection", "Needed for the first run (downloads) and for 05.")
  cli <- tryCatch({
    source(file.path(.git_dir, "scripts", "lib_cmems_phytoplankton_biomass.R"), local = TRUE)
    suppressMessages(ensure_copernicusmarine_cli(auto_install = FALSE))
  }, error = function(e) FALSE)
  out[[7]] <- row("Copernicus tool (05 only)", isTRUE(cli), if (isTRUE(cli)) Sys.which("copernicusmarine") else "copernicusmarine not found",
                  "Only for 05 environment: install Python, then 'pip install copernicusmarine'.")
  out[[8]] <- row("Copernicus login (05 only)", .cm_has_login(), if (.cm_has_login()) "saved on this computer" else "not saved",
                  "Use 'enter or change the login' in card 4c (environment and Ecospace).")
  do.call(rbind, out)
}
observeEvent(input$setup_check, {
  output$setup_report <- renderUI({
    x <- .setup_items()
    tags$table(class = "table table-sm small mt-2 mb-0",
      tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(
        tags$td(if (x$ok[i]) "\u2705" else "\u274C"), tags$td(tags$strong(x$item[i])),
        tags$td(tags$code(x$detail[i]), if (!x$ok[i] && nzchar(x$fix[i])) tags$div(class = "text-danger", x$fix[i]))))))
  })
})

## --- Run summary (card 6): what to look at after a run ----------------------
## Per script: the log of the last run, its errors and warnings; then the
## FGs with F > 1 and the FGs with no Ecopath B (from the REVIEW tables).
.last_logs <- function() {
  if (is.na(.out_dir)) return(character(0))
  f <- list.files(.out_dir, "^\\d{8}_0[1-6]_.*_log\\.txt$", full.names = TRUE)
  if (!length(f)) return(character(0))
  step <- sub("^\\d{8}_(0[1-6])_.*$", "\\1", basename(f))
  f <- f[order(file.mtime(f), decreasing = TRUE)]; step <- sub("^\\d{8}_(0[1-6])_.*$", "\\1", basename(f))
  f <- f[!duplicated(step)]
  f[order(basename(f))]
}
output$dl_logs <- downloadHandler(function() paste0("WMed_EwE_logs_", .stamp(), ".zip"), function(file) {
  f <- .last_logs(); if (!length(f)) stop("No logs yet - run the pipeline first.")
  old <- setwd(.out_dir); on.exit(setwd(old))
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(file, basename(f)) else utils::zip(file, basename(f), flags = "-q")
})
output$run_summary <- renderUI({
  last_finished()
  logs <- .last_logs()
  if (!length(logs)) return(NULL)
  rows <- lapply(logs, function(f) {
    l <- .utf8(readLines(f, warn = FALSE))
    err <- grep("^ERROR|^Error|ERROR:|\\bfailed\\b|FAILED|Execution halted|no data downloaded|rejected the|refused the login|No Copernicus Marine login", l, value = TRUE)
    err <- err[!grepl("error = function|0 error", err)]
    wrn <- grep("^Warning|^Aviso|Warning message|skipped -|could not|\\[download\\] nothing|FALLBACK", l, value = TRUE)
    wrn <- setdiff(wrn, err)
    ## Click a row's message to see every error and warning of that script
    ## (home folder shown as ~, so no user name on screen)
    .hm <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
    all_msg <- gsub(.hm, "~", c(err, wrn), fixed = TRUE)
    first <- if (length(err)) tags$code(substr(all_msg[1], 1, 160)) else if (length(wrn)) tags$small(substr(all_msg[1], 1, 160)) else "ok"
    tags$tr(tags$td(sub("_log\\.txt$", "", sub("^\\d{8}_", "", basename(f)))),
            tags$td(format(file.mtime(f), "%d %b %H:%M")),
            tags$td(class = if (length(err)) "text-danger fw-bold" else "", length(err)),
            tags$td(length(wrn)),
            tags$td(if (length(all_msg) > 1 || (length(all_msg) == 1 && nchar(all_msg[1]) > 160))
              tags$details(tags$summary(style = "cursor: pointer;", first, tags$span(class = "text-primary ms-1", sprintf("(show all %d)", length(all_msg)))),
                tags$ol(class = "small mb-1 mt-1 ps-3", style = "max-height: 260px; overflow-y: auto;",
                  lapply(seq_along(all_msg), function(i) tags$li(class = if (i <= length(err)) "text-danger" else NULL, substr(all_msg[i], 1, 400)))))
              else first))
  })
  rd <- function(p) { f <- file.path(.out_dir, p); if (file.exists(f)) tryCatch(data.table::fread(f), error = function(e) NULL) else NULL }
  fhi <- rd("validation/F_above_1_REVIEW.csv"); nob <- rd("biomass/fg_missing_ecopath_B_REVIEW.csv")
  tags$div(class = "card mb-3", tags$div(class = "card-body py-2",
    tags$strong("Summary of the last run"),
    tags$table(class = "table table-sm small mb-2",
      tags$thead(tags$tr(tags$th("Script"), tags$th("Log written"), tags$th("Errors"), tags$th("Warnings / notes"), tags$th("First one"))),
      tags$tbody(rows)),
    if (!is.null(fhi) && nrow(fhi) && all(c("FG_name", "F") %in% names(fhi)))
      tags$div(class = "small", tags$strong(sprintf("F > 1 (%d FG): ", nrow(fhi))),
               paste(sprintf("%s %.2g", fhi$FG_name, fhi$F), collapse = ", "),
               " - see the 'F by country' figure for where it comes from."),
    if (!is.null(nob) && nrow(nob)) {
      nm <- if ("FG_name" %in% names(nob)) nob$FG_name else as.character(nob[[1]])
      det <- grepl("detritus|discard", nm, ignore.case = TRUE)     # detritus groups: no P/B or EE, B is not estimated
      tagList(
        if (any(!det)) tags$div(class = "small", tags$strong(sprintf("No Ecopath B (%d FG): ", sum(!det))),
                                paste(nm[!det], collapse = ", "), " - Ecopath estimates them from EE."),
        if (any(det)) tags$div(class = "small", tags$strong("Detritus group(s) without B: "), paste(nm[det], collapse = ", "),
                               " - Ecopath does not estimate a detritus biomass from EE; enter one in EwE if you have it (it only sets the detritus turnover)."))
    },
    tags$div(class = "small text-muted", "Full logs: 'Logs of the last run (.zip)' above, or the *_log.txt files in ", tags$code(.show_path(.out_dir)), ".")))
})

## --- Results: key plots and every plot in out_dir/plots ----------------------
## The PNGs are served straight from out_dir/plots (no copy); "?v=<time>"
## makes the browser fetch a plot again after a run rewrites it.
makeActiveBinding(".plot_dir", function() if (!is.na(.out_dir)) file.path(.out_dir, "plots") else NA_character_, environment())
## Served through the session (registerDataObj), which works inside Quarto;
## only files under out_dir/plots can be read this way.
.plot_url <- session$registerDataObj("wmplot", .base_out, function(data, req) {
  q <- shiny::parseQueryString(req$QUERY_STRING)
  data <- .plot_dir                                 # plots of the results folder shown now
  root <- normalizePath(data, mustWork = FALSE)
  f <- normalizePath(file.path(data, if (is.null(q$f)) "" else q$f), mustWork = FALSE)
  if (is.null(q$f) || !startsWith(f, root) || !file.exists(f) || dir.exists(f))
    return(list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "not found"))
  list(status = 200L, headers = list("Content-Type" = "image/png", "Cache-Control" = "max-age=3600"),
       body = readBin(f, "raw", file.info(f)$size))
})
.key_plots <- c("Biomass by source"   = "biomass/biomass_by_fg_source.png",
                "Biomass series"      = "biomass/survey_fg_density_timeseries_regional.png",
                "Catch by FG"         = "fisheries/01_catches_per_FG.png",
                "Catch by FG x fleet" = "fisheries/07_catch_by_fg_and_fleet.png",
                "Discards by FG x fleet" = "fisheries/08_discards_by_fg_and_fleet.png",
                "Effort by fleet"     = "fisheries/05_effort_by_country_fleet.png",
                "P/B vs Q/B"          = "validation/fg_pb_qb_scatter.png",
                "F by FG"             = "validation/F_by_fg.png",
                "F by country"        = "validation/12b_F_by_country.png",
                "P/Q by FG"           = "validation/PQ_ratio_by_fg.png",
                "Diet matrix"         = "diets/diet_matrix_heatmap.png",
                "PREBAL"              = "validation/15_prebal.png",
                "B vs other models"   = "validation/15d_all_models_B.png",
                "P/B vs other models" = "validation/15d_all_models_PB.png",
                "Q/B vs other models" = "validation/15d_all_models_QB.png",
                "Phytoplankton sources" = "validation/11b_phytoplankton_sources.png",
                "Environment"         = "environment/environment_timeseries.png")

output$results <- renderUI({
  last_finished(); st <- run_state()
  files <- if (!is.na(.plot_dir)) list.files(.plot_dir, "\\.png$", recursive = TRUE) else character(0)
  if (!length(files)) return(tags$div(class = "alert alert-secondary py-2 small mb-0", "No plots yet - click Run pipeline."))
  mt <- file.mtime(file.path(.plot_dir, files)); names(mt) <- files
  src <- function(f) paste0(.plot_url, "&f=", utils::URLencode(f, reserved = TRUE), "&v=", as.integer(mt[[f]]))
  fmt <- function(t) format(t, "%d %b %Y, %H:%M")

  ## Where these plots come from
  newest <- max(mt)
  set <- .settings_lines()
  set <- gsub("\\s+", " ", trimws(sub("#.*", "", set[grepl("^(MODEL_GSAS|MODEL_AREA_BASIS|YEAR_ECOPATH|TS_YEARS|START_YEAR|END_YEAR|DROP_OUTLIERS)", set)])))
  from_here <- !is.null(run_finished()) && !is.null(run_started()) &&
               newest >= run_started() && newest <= run_finished() + 120
  banner <- if (identical(st, "running")) {
    tags$div(class = "alert alert-warning py-2 small", "A run is going. These are the previous plots; they are replaced when it finishes.")
  } else if (from_here && identical(st, "done")) {
    tags$div(class = "alert alert-success py-2 small",
             tags$strong("From the run started on this page "), sprintf("on %s, finished %s. ", fmt(run_started()), fmt(run_finished())),
             tags$br(), tags$code(paste(set, collapse = "; ")))
  } else {
    tags$div(class = "alert alert-info py-2 small",
             sprintf("Plots last written %s by a run outside this page (from the R console or another session).", fmt(newest)),
             if (!is.null(run_started())) sprintf(" The last run from this page was on %s.", fmt(run_started())))
  }

  ## Key plots: one tab each
  ids <- paste0("wm-key-", seq_along(.key_plots))
  key_nav <- tags$ul(class = "nav nav-tabs", role = "tablist",
    lapply(seq_along(.key_plots), function(i)
      tags$li(class = "nav-item", role = "presentation",
              tags$button(class = paste("nav-link", if (i == 1) "active"), type = "button", role = "tab",
                          `data-bs-toggle` = "tab", `data-bs-target` = paste0("#", ids[i]), names(.key_plots)[i]))))
  key_body <- tags$div(class = "tab-content",
    lapply(seq_along(.key_plots), function(i) {
      f <- .key_plots[[i]]
      tags$div(class = paste("tab-pane fade", if (i == 1) "show active"), id = ids[i], role = "tabpanel",
               if (f %in% files) tags$img(class = "wm-key-img", src = src(f), alt = basename(f))
               else tags$div(class = "alert alert-secondary small mt-2", "Not available yet: plots/", f))
    }))

  ## All plots: thumbnails, filter by folder, search by name
  folder <- dirname(files)
  ord <- order(folder, basename(files)); files <- files[ord]; folder <- folder[ord]
  counts <- table(folder)
  filt <- tags$div(class = "wm-filter",
    tags$button(type = "button", class = "btn btn-sm btn-outline-primary active", `data-folder` = "", sprintf("All (%d)", length(files))),
    lapply(names(counts), function(d)
      tags$button(type = "button", class = "btn btn-sm btn-outline-primary", `data-folder` = d, sprintf("%s (%d)", d, counts[[d]]))),
    tags$input(id = "wm-search", type = "search", class = "form-control form-control-sm", placeholder = "Search plot name"))
  grid <- tags$div(class = "wm-grid",
    lapply(seq_along(files), function(i)
      tags$div(class = "wm-thumb", `data-folder` = folder[i], `data-name` = tolower(basename(files[i])),
               tags$img(src = src(files[i]), loading = "lazy", alt = basename(files[i]), title = "Click to zoom"),
               tags$div(class = "wm-thumb-folder", folder[i]),
               tags$div(class = "wm-thumb-name", sub("\\.png$", "", basename(files[i]))))))

  tagList(
    banner,
    tags$ul(class = "nav nav-pills wm-view", role = "tablist",
      tags$li(class = "nav-item", tags$button(class = "nav-link active", type = "button", `data-bs-toggle` = "pill",
                                              `data-bs-target` = "#wm-view-key", "Key plots")),
      tags$li(class = "nav-item", tags$button(class = "nav-link", type = "button", `data-bs-toggle` = "pill",
                                              `data-bs-target` = "#wm-view-all", sprintf("All plots (%d)", length(files))))),
    tags$div(class = "tab-content",
      tags$div(class = "tab-pane fade show active", id = "wm-view-key", key_nav, key_body),
      tags$div(class = "tab-pane fade", id = "wm-view-all", filt, grid)),
    tags$small(class = "text-muted", "Click any plot to zoom. Plots are read from ", tags$code(.show_path(.plot_dir)), "."))
})

## --- FG explorer: one FG's Ecosim series from the workbook ----------------
## Reads the Ecosim_ts sheet (rows: Name, Type, ..., Target "N: FG", then one
## row per year) and plots the relative biomass, with the survey sampling
## band (index x (1 +/- 1.96 CV), from biomass/survey_index_cv_by_fg_year.csv)
## when the FG has one, and the catches and discards.
.ecosim_ts <- reactive({
  last_finished()
  if (is.na(.wb_path) || !file.exists(.wb_path)) return(NULL)
  x <- tryCatch(openxlsx::read.xlsx(.wb_path, "Ecosim_ts", colNames = FALSE), error = function(e) NULL)
  if (is.null(x) || nrow(x) < 11) return(NULL)
  lab <- trimws(as.character(x[[1]]))
  r_type <- which(lab == "Type")[1]; r_tgt <- which(lab == "Target")[1]; r_int <- which(lab == "Interval")[1]
  if (anyNA(c(r_type, r_tgt, r_int))) return(NULL)
  yrs <- suppressWarnings(as.integer(lab[(r_int + 1):nrow(x)]))
  out <- lapply(2:ncol(x), function(j) {
    tgt <- as.character(x[r_tgt, j]); if (is.na(tgt) || !grepl("^\\d+:", tgt)) return(NULL)
    data.frame(FG_num = as.integer(sub(":.*", "", tgt)), FG_name = trimws(sub("^\\d+:", "", tgt)),
               type = as.character(x[r_type, j]), Year = yrs,
               value = suppressWarnings(as.numeric(x[(r_int + 1):nrow(x), j])), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) NULL else out[!is.na(out$Year) & !is.na(out$value), ]
})
## Ecopath baseline of each FG (Ecopath_B sheet): B in t/km2, the years it
## averages (the info sheet / column name), EE when B is left to Ecopath,
## and its source (Reference column).
.ecopath_b <- reactive({
  last_finished()
  if (is.na(.wb_path) || !file.exists(.wb_path)) return(NULL)
  x <- tryCatch(openxlsx::read.xlsx(.wb_path, "Ecopath_B", check.names = FALSE, sep.names = " "), error = function(e) NULL)
  if (is.null(x) || !all(c("FG_num", "Biomass") %in% names(x))) return(NULL)
  yc <- grep("^Biomass_\\d{4}(_\\d{4})?$", names(x), value = TRUE)
  yrs <- if (length(yc)) { v <- as.integer(unlist(regmatches(yc[1], gregexpr("\\d{4}", yc[1])))); seq(min(v), max(v)) } else integer(0)
  data.frame(FG_num = as.integer(x$FG_num), B = suppressWarnings(as.numeric(x$Biomass)),
             EE = if ("EE_input" %in% names(x)) suppressWarnings(as.numeric(x$EE_input)) else NA_real_,
             Ref = if ("Reference" %in% names(x)) as.character(x$Reference) else NA_character_,
             years = if (length(yrs)) paste(range(yrs), collapse = "-") else NA_character_,
             y0 = if (length(yrs)) min(yrs) else NA_integer_, y1 = if (length(yrs)) max(yrs) else NA_integer_, stringsAsFactors = FALSE)
})
.fmt_b <- function(v) if (!is.finite(v)) "-" else if (v == 0) "0" else if (v >= 0.01) formatC(v, format = "fg", digits = 3) else formatC(v, format = "e", digits = 2)
output$fg_explore_info <- renderUI({
  e <- .ecopath_b(); req(input$fg_explore)
  if (is.null(e)) return(NULL)
  r <- e[e$FG_num == as.integer(input$fg_explore), ]
  if (!nrow(r)) return(NULL)
  r <- r[1, ]
  ref <- if (is.na(r$Ref) || !nzchar(r$Ref)) "no source recorded" else r$Ref
  tags$div(class = "small mt-1",
    tags$strong("Ecopath B", if (!is.na(r$years)) paste0(" (", r$years, ")"), ": "),
    if (is.finite(r$B)) paste(.fmt_b(r$B), "t/km\u00b2") else
      if (is.finite(r$EE)) paste0("left to Ecopath (EE = ", r$EE, ")") else "none",
    tags$details(class = "d-inline ms-2", tags$summary(class = "d-inline text-muted", "source"),
                 tags$div(class = "text-muted", style = "white-space: normal;", ref)))
})
## Every FG of the workbook (FGs sheet, else Ecopath_B), also those without
## an Ecosim series; those are marked "(no series)" in the list.
.fg_list <- reactive({
  last_finished()
  if (is.na(.wb_path) || !file.exists(.wb_path)) return(NULL)
  sh <- tryCatch(openxlsx::getSheetNames(.wb_path), error = function(e) character(0))
  x <- NULL
  if ("FGs" %in% sh) x <- tryCatch(openxlsx::read.xlsx(.wb_path, "FGs", check.names = FALSE), error = function(e) NULL)
  if (is.null(x) || !all(c("FG_num", "FG_name") %in% names(x)))
    x <- tryCatch(openxlsx::read.xlsx(.wb_path, "Ecopath_B", check.names = FALSE, sep.names = " "), error = function(e) NULL)
  if (is.null(x) || !all(c("FG_num", "FG_name") %in% names(x))) return(NULL)
  unique(data.frame(FG_num = as.integer(x$FG_num), FG_name = as.character(x$FG_name), stringsAsFactors = FALSE))
})
observe({
  d <- .ecosim_ts(); fl <- .fg_list()
  fg <- rbind(if (!is.null(fl)) fl, if (!is.null(d) && nrow(d)) unique(d[, c("FG_num", "FG_name")]))
  if (is.null(fg) || !nrow(fg)) return()
  fg <- fg[!duplicated(fg$FG_num), ]; fg <- fg[order(fg$FG_num), ]
  has_b <- if (!is.null(d)) fg$FG_num %in% d$FG_num[d$type == "Biomass (relative)"] else rep(FALSE, nrow(fg))
  ch <- stats::setNames(fg$FG_num, paste0(fg$FG_num, " - ", fg$FG_name, ifelse(has_b, "", "  (no biomass series)")))
  updateSelectInput(session, "fg_explore", choices = ch, selected = { s <- isolate(input$fg_explore); if (is.null(s) || !s %in% ch) ch[1] else s })
})
## The two panels are built here and drawn as two plots (biomass, catches),
## so the biomass plot can be hovered: Shiny maps the mouse to data
## coordinates only for a single ggplot.
.fg_panels <- reactive({
  d <- .ecosim_ts()
  validate(need(!is.null(d) && nrow(d), "No Ecosim_ts sheet yet - run the pipeline (01 and 02)."))
  req(input$fg_explore)
  .fg_now <- as.integer(input$fg_explore)
  d <- d[d$FG_num == .fg_now, ]
  cvf <- file.path(.out_dir, "biomass", "survey_index_cv_by_fg_year.csv")
  b <- d[d$type == "Biomass (relative)", ]
  if (nrow(b) && file.exists(cvf)) {
    cv <- tryCatch(data.table::fread(cvf), error = function(e) NULL)
    if (!is.null(cv) && all(c("FG_num", "Year", "cv_index") %in% names(cv))) {
      cv <- as.data.frame(cv)[cv$FG_num == .fg_now, c("Year", "cv_index")]
      b <- merge(b, cv, by = "Year", all.x = TRUE)
      b$lo <- pmax(0, b$value * (1 - 1.96 * b$cv_index)); b$hi <- b$value * (1 + 1.96 * b$cv_index)
    }
  }
  ## One colour per series, one legend for both panels (patchwork collects it)
  ## show.legend = TRUE on every line/point layer and the same fixed scale in
  ## both panels make the two legends identical, so patchwork shows one
  cols <- c("Biomass" = "#0072B2", "Catches" = "#D55E00", "Discards" = "#009E73")
  sc <- function(keys) ggplot2::scale_colour_manual(values = cols[keys], limits = keys, breaks = keys, drop = FALSE, name = NULL,
                                                    guide = ggplot2::guide_legend(override.aes = list(linewidth = 1.1, size = 2)))
  th <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 11),
                   plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9, colour = "grey30"),
                   axis.title.y = ggplot2::element_text(size = 10), plot.margin = ggplot2::margin(4, 10, 4, 4))
  .ln <- function() list(ggplot2::geom_line(show.legend = TRUE, na.rm = TRUE), ggplot2::geom_point(size = 1.3, show.legend = TRUE, na.rm = TRUE))
  note_panel <- function(title, txt) ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = txt, size = 3.4, colour = "grey30", lineheight = 1.1) +
    ggplot2::labs(title = title) + ggplot2::theme_void(base_size = 11) + ggplot2::xlim(-1, 1) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"))
  empty <- function(title) ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = paste("No", tolower(title))) +
    ggplot2::labs(title = title) + ggplot2::theme_void(base_size = 11)
  ## Both panels relative to the Ecopath years (mean over them = 1, grey
  ## band), so biomass and catch trends read on the same scale; the
  ## absolute Ecopath values are in the subtitles. Without values in the
  ## Ecopath years, the first year with a value is the reference.
  e_all <- .ecopath_b(); r0 <- if (!is.null(e_all)) e_all[e_all$FG_num == .fg_now, ][1, ] else NULL
  base_yrs <- if (!is.null(r0) && nrow(r0) && is.finite(r0$y0)) seq(r0$y0, r0$y1) else integer(0)
  absolute <- identical(input$fg_explore_scale, "abs")
  ## Relative series start at 1 in the first Ecosim year, and that first
  ## year holds the Ecopath baseline (mean over the Ecopath years) - as in
  ## Ecosim_ts, where 01 sets the first year of each biomass series to the
  ## Ecopath B before scaling it. ref_first = TRUE (biomass): the series is
  ## already scaled that way, so its first value is the reference. Catches
  ## and discards (t/km2 in Ecosim_ts): the first year is replaced by their
  ## Ecopath-years mean, which becomes 1.
  .rel <- function(v, yr, ref_first = FALSE) {
    y1 <- min(yr)
    ref <- if (ref_first) v[yr == y1][1] else mean(v[yr %in% base_yrs & is.finite(v)])
    lab <- "Ecopath years"
    if (!is.finite(ref) || ref <= 0) { k <- which(is.finite(v) & v > 0)[1]; ref <- if (is.na(k)) NA_real_ else v[k]; lab <- if (is.na(k)) "" else paste("first year with a value,", yr[k]) }
    else if (!ref_first) v[yr == y1] <- ref
    list(x = v / ref, v = v, ref = ref, lab = lab)
  }
  p1 <- if (nrow(b)) {
    b$series <- "Biomass"
    rb <- .rel(b$value, b$Year, ref_first = TRUE); b$value <- rb$x
    if ("lo" %in% names(b)) { b$lo <- b$lo / rb$ref * 1; b$hi <- b$hi / rb$ref * 1 }
    ## absolute: the relative series x the Ecopath B (the Ecosim index is
    ## relative; its level is the Ecopath baseline). Not possible when B is
    ## left to Ecopath (EE) or the reference is not the Ecopath years.
    b_abs <- absolute && !is.null(r0) && nrow(r0) && is.finite(r0$B) && r0$B > 0 && rb$lab == "Ecopath years"
    if (b_abs) { b$value <- b$value * r0$B; if ("lo" %in% names(b)) { b$lo <- b$lo * r0$B; b$hi <- b$hi * r0$B } }
    p <- ggplot2::ggplot(b, ggplot2::aes(Year, value, colour = series))
    if ("lo" %in% names(b) && any(!is.na(b$lo)))
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = cols[["Biomass"]], colour = NA, alpha = 0.2, na.rm = TRUE)
    e <- .ecopath_b(); r <- if (!is.null(e)) e[e$FG_num == .fg_now, ][1, ] else NULL
    sub <- if (!is.null(r) && nrow(r) && !is.na(r$FG_num))
      paste0("Ecopath B", if (!is.na(r$years)) paste0(" ", r$years), ": ",
             if (is.finite(r$B)) paste(.fmt_b(r$B), "t/km\u00b2") else if (is.finite(r$EE)) paste0("left to Ecopath (EE ", r$EE, ")") else "none") else NULL
    if (!is.null(r) && nrow(r) && is.finite(r$y0))   # the Ecopath baseline years
      p <- p + ggplot2::annotate("rect", xmin = r$y0 - 0.5, xmax = r$y1 + 0.5, ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.15)
    if (absolute && !b_abs) sub <- paste0(sub, " - shown relative (no Ecopath B to scale to)")
    p + ggplot2::geom_hline(yintercept = if (b_abs) r0$B else 1, colour = "grey70", linetype = "dashed") + .ln() + sc("Biomass") + th +
      ggplot2::labs(title = "Biomass", subtitle = if (is.null(sub)) "Point the mouse at a year: the species shares of the FG biomass" else paste0(sub, "\nPoint the mouse at a year: the species shares of the FG biomass"), x = NULL,
                    y = if (b_abs) "t/km\u00b2" else if (rb$lab == "Ecopath years") "Relative to Ecopath years (= 1)" else paste0("Relative to ", sub("^first year with a value, ", "", rb$lab), " (= 1)"))
  } else {
    .nm <- if (!is.null(.fg_list())) .fg_list()$FG_name[match(.fg_now, .fg_list()$FG_num)] else ""
    note_panel("Biomass", if (grepl("detritus|discard", .nm, ignore.case = TRUE))
      "Detritus group: no biomass series.\nEcosim does not fit detritus\nto a time series."
    else "No biomass series for this group.\nNo survey index, literature or\nstock-assessment trend was found,\nso Ecosim has no biomass series\nto fit for it. Its Ecopath B is below.")
  }
  ct <- d[d$type %in% c("Catches", "Discards"), ]
  p2 <- if (nrow(ct)) {
    ct$series <- ct$type
    refs <- character(0)
    for (tp in unique(ct$type)) {
      k <- ct$type == tp; rc <- .rel(ct$value[k], ct$Year[k]); ct$value[k] <- if (absolute) rc$v else rc$x
      refs <- c(refs, paste0(if (tp == "Catches") "C " else "Di ", .fmt_b(rc$ref), if (rc$lab != "Ecopath years" && nzchar(rc$lab)) paste0(" (", sub("^first year with a value, ", "", rc$lab), ")")))
    }
    pc <- ggplot2::ggplot(ct, ggplot2::aes(Year, value, colour = series))
    if (length(base_yrs)) pc <- pc + ggplot2::annotate("rect", xmin = min(base_yrs) - 0.5, xmax = max(base_yrs) + 0.5, ymin = -Inf, ymax = Inf, fill = "grey60", alpha = 0.15)
    (if (absolute) pc else pc + ggplot2::geom_hline(yintercept = 1, colour = "grey70", linetype = "dashed")) + .ln() +
      sc(intersect(c("Catches", "Discards"), unique(ct$type))) + th + ggplot2::labs(title = "Catches and discards",
                              subtitle = paste0("Ecopath years: ", paste(refs, collapse = ", "), " t/km\u00b2"), x = NULL,
                              y = if (absolute) "t/km\u00b2" else "Relative to Ecopath years (= 1)")
  } else note_panel("Catches and discards", "No catch or discards recorded\nfor this group in the data sources.")
  list(p1 = p1, p2 = p2, b = if (nrow(b)) b[, c("Year", "value")] else NULL, fg = .fg_now)
})
output$fg_explore_plot <- renderPlot(.fg_panels()$p1, res = 96)
output$fg_explore_plot_catch <- renderPlot(.fg_panels()$p2, res = 96)

## Species shares of an FG's biomass, for the hover box of the FG explorer.
## Per year: the survey species densities (species_density_regional_combined.csv,
## MEDITS + MEDIAS). Ecopath years, and years without a survey split: the
## Ecopath baseline composition (biomass_proportion_by_species_fg.csv, every
## source: surveys, literature, assessments).
.sp_year <- reactive({
  .ecosim_ts()
  f <- file.path(.out_dir, "biomass", "species_density_regional_combined.csv")
  if (is.na(.out_dir) || !file.exists(f)) return(NULL)
  x <- tryCatch(as.data.frame(data.table::fread(f)), error = function(e) NULL)
  if (is.null(x) || !all(c("Year", "FG_num", "ScientificName", "mean_density") %in% names(x))) NULL else x
})
.sp_ecopath <- reactive({
  .ecosim_ts()
  f <- file.path(.out_dir, "biomass", "biomass_proportion_by_species_fg.csv")
  if (is.na(.out_dir) || !file.exists(f)) return(NULL)
  x <- tryCatch(as.data.frame(data.table::fread(f)), error = function(e) NULL)
  if (is.null(x) || !all(c("FG_num", "Species") %in% names(x))) return(NULL)
  x$w <- if ("Biomass_t_km2" %in% names(x)) suppressWarnings(as.numeric(x$Biomass_t_km2)) else NA_real_
  if ("prop_sp_fg" %in% names(x)) x$w[!is.finite(x$w)] <- suppressWarnings(as.numeric(x$prop_sp_fg[!is.finite(x$w)]))
  x
})
.fg_species_shares <- function(fg, yr) {
  e <- .ecopath_b(); r <- if (!is.null(e)) e[e$FG_num == fg, ][1, ] else NULL
  in_base <- !is.null(r) && nrow(r) && is.finite(r$y0) && yr >= r$y0 && yr <= r$y1
  sy <- .sp_year()
  if (!in_base && !is.null(sy)) {
    x <- sy[sy$FG_num == fg & sy$Year == yr & is.finite(sy$mean_density) & sy$mean_density > 0, ]
    if (nrow(x)) {
      x <- stats::aggregate(mean_density ~ ScientificName, x, sum)
      return(list(sp = x$ScientificName, w = x$mean_density, src = paste0("surveys ", yr, " (MEDITS + MEDIAS species only)")))
    }
  }
  se <- .sp_ecopath()
  if (!is.null(se)) {
    x <- se[se$FG_num == fg & is.finite(se$w) & se$w > 0, ]
    if (nrow(x)) {
      x <- stats::aggregate(w ~ Species, x, sum)
      return(list(sp = x$Species, w = x$w, src = paste0("Ecopath baseline", if (!is.null(r) && !is.na(r$years)) paste0(" ", r$years),
                                                          if (!in_base) paste0(" (no survey split for ", yr, ")") else "", ", all sources")))
    }
  }
  NULL
}
output$fg_explore_tip <- renderUI({
  h <- input$fg_explore_hover
  if (is.null(h) || is.null(h$x)) return(NULL)
  pn <- .fg_panels(); if (is.null(pn$b) || !nrow(pn$b)) return(NULL)
  pt <- shiny::nearPoints(pn$b, h, xvar = "Year", yvar = "value", threshold = 12, maxpoints = 1)
  if (!nrow(pt)) {   # anywhere above a year: that year's point
    yr <- round(h$x); pt <- pn$b[pn$b$Year == yr, , drop = FALSE]
    if (!nrow(pt)) return(NULL)
  }
  yr <- pt$Year[1]
  sh <- .fg_species_shares(pn$fg, yr)
  rows <- if (is.null(sh)) list(tags$div(class = "text-muted", "No species split recorded for this FG.")) else {
    o <- order(-sh$w); p <- sh$w[o] / sum(sh$w); sp <- sh$sp[o]
    k <- min(length(p), 10)
    c(lapply(seq_len(k), function(i) tags$div(tags$span(style = "display:inline-block; min-width:3.6em; text-align:right; margin-right:.5em; font-variant-numeric: tabular-nums;",
                                                            if (p[i] >= 0.001) sprintf("%.1f%%", 100 * p[i]) else "<0.1%"), tags$em(sp[i]))),
      if (length(p) > k) list(tags$div(class = "text-muted", sprintf("+ %d more species: %.1f%%", length(p) - k, 100 * sum(p[-seq_len(k)])))),
      list(tags$div(class = "text-muted mt-1", style = "font-size:.75rem;", "Source: ", sh$src)))
  }
  left <- h$coords_css$x; top <- h$coords_css$y
  flip <- !is.null(h$range) && !is.null(h$range$right) && h$coords_img$x > (h$range$left + h$range$right) / 2
  tags$div(style = paste0("position:absolute; z-index:20; pointer-events:none; top:", top + 12, "px; ",
                          if (flip) paste0("right:calc(100% - ", left - 12, "px); ") else paste0("left:", left + 12, "px; "),
                          "background:rgba(255,255,255,.97); border:1px solid #9fb3c8; border-radius:6px; padding:.4rem .6rem; ",
                          "box-shadow:0 2px 8px rgba(16,42,67,.18); font-size:.8rem; max-width:340px; line-height:1.3;"),
    tags$strong(paste0(yr, ": species shares of the FG biomass")), rows)
})

## --- Load a settings file (presets) -------------------------------------
## A settings .R from 'Download .R' (or written by hand) sets the inputs
## again. The file is NOT run: only its simple assignments are read
## (numbers, text, TRUE/FALSE, c(), a:b), so a file from someone else
## cannot execute code on this computer.
.read_settings <- function(path) {
  ex <- tryCatch(parse(path, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  safe <- function(e) {
    if (is.atomic(e)) return(TRUE)
    if (is.call(e)) return(as.character(e[[1]])[1] %in% c("c", ":", "-", "(") && all(vapply(as.list(e)[-1], safe, logical(1))))
    FALSE
  }
  out <- list()
  for (e in ex) {
    if (!is.call(e) || !as.character(e[[1]]) %in% c("<-", "=") || !is.name(e[[2]]) || !safe(e[[3]])) next
    out[[as.character(e[[2]])]] <- eval(e[[3]], baseenv())
  }
  out
}
observeEvent(input$settings_upload, {
  s <- .read_settings(input$settings_upload$datapath)
  if (is.null(s) || !length(s)) {
    showNotification("No settings found in that file (it must be an .R settings file, e.g. from 'Download .R').", type = "error"); return()
  }
  rng <- function(v) c(min(v), max(v))
  used <- character(0); u <- function(k) used <<- c(used, k)
  if (!is.null(s$YEAR_ECOPATH)) { updateSliderInput(session, "ecopath_years", value = rng(s$YEAR_ECOPATH)); u("YEAR_ECOPATH") }
  if (!is.null(s$TS_YEARS))     { updateSliderInput(session, "ts_years", value = rng(s$TS_YEARS)); u("TS_YEARS") }
  sw <- c(STRATA = "strata", DROP_OUTLIERS = "drop_outliers", NORMALIZE_TS = "normalize_ts", APPLY_CATCHABILITY = "catch_q")
  for (k in names(sw)) if (is.logical(s[[k]])) { updateCheckboxInput(session, sw[[k]], value = isTRUE(s[[k]])); u(k) }
  if (!is.null(s$ECOPATH_B_FILL_MODE)) { updateCheckboxInput(session, "fill_b", value = identical(s$ECOPATH_B_FILL_MODE, "window_mean")); u("ECOPATH_B_FILL_MODE") }
  if (!is.null(s$ECOPATH_B_FILL_YEARS)) { updateSliderInput(session, "fill_years", value = rng(s$ECOPATH_B_FILL_YEARS)); u("ECOPATH_B_FILL_YEARS") }
  ## HIGH_F_B_FIX: no longer a choice (high F is reported only)
  if (!is.null(s$PHYTO_SOURCE)) { updateSelectInput(session, "phyto_src", selected = s$PHYTO_SOURCE); u("PHYTO_SOURCE") }
  if (!is.null(s$RUN_STEPS))    { updateCheckboxGroupInput(session, "steps_chk", selected = as.character(s$RUN_STEPS)); u("RUN_STEPS") }
  if (identical(s$AREA_MODE, "custom")) {
    updateRadioButtons(session, "area_mode", selected = "custom"); u("AREA_MODE")
    if (!is.null(s$AREA_NAME)) { updateTextInput(session, "area_name", value = s$AREA_NAME); u("AREA_NAME") }
    if (!is.null(s$CUSTOM_AREA_TYPE)) { updateRadioButtons(session, "custom_type", selected = s$CUSTOM_AREA_TYPE); u("CUSTOM_AREA_TYPE") }
    b <- s$CUSTOM_BBOX
    if (length(b) == 4) {
      if (is.null(names(b))) names(b) <- c("xmin", "xmax", "ymin", "ymax")
      for (k in c("xmin", "xmax", "ymin", "ymax")) updateNumericInput(session, paste0("bbox_", k), value = unname(b[[k]]))
      u("CUSTOM_BBOX")
    }
  } else {
    if (!is.null(s$MODEL_GSAS) || !is.null(s$AREA_MODE)) updateRadioButtons(session, "area_mode", selected = "westmed")
    if (!is.null(s$MODEL_GSAS)) { updateCheckboxGroupInput(session, "gsas", selected = as.character(s$MODEL_GSAS)); u("MODEL_GSAS") }
    if (!is.null(s$MODEL_AREA_BASIS)) { updateRadioButtons(session, "basis", selected = s$MODEL_AREA_BASIS); u("MODEL_AREA_BASIS") }
  }
  skipped <- setdiff(names(s), c(used, "CLEAN_START"))
  showNotification(HTML(paste0("Settings loaded: ", paste(used, collapse = ", "),
                               if (length(skipped)) paste0("<br>Not set on the page (set them in the R console): ", paste(skipped, collapse = ", ")),
                               if (!is.null(s$CUSTOM_SHAPEFILE_PATH)) "<br>Upload the region file again (a shapefile path cannot be loaded).")),
                   type = "message", duration = 12)
})

## --- Which run's results to show -------------------------------------------
## Every output folder with a workbook: the main one (default inputs) and
## each out_dir/runs/<name>. Newest first.
.run_folders <- function() {
  if (is.na(.base_out)) return(character(0))
  d <- unname(c(.base_out, list.dirs(file.path(.base_out, "runs"), recursive = FALSE)))
  d <- d[file.exists(file.path(d, "ecopath_ecosim_inputs.xlsx")) | dir.exists(file.path(d, "plots"))]
  if (!length(d)) return(character(0))
  mt <- vapply(d, function(x) { f <- file.path(x, "ecopath_ecosim_inputs.xlsx"); as.numeric(if (file.exists(f)) file.mtime(f) else file.mtime(x)) }, 0, USE.NAMES = FALSE)
  o <- order(-mt); d <- d[o]; mt <- mt[o]
  stats::setNames(d, paste0(vapply(d, .run_label, ""), " - ", format(as.POSIXct(mt, origin = "1970-01-01"), "%d %b %H:%M")))
}
observe({
  last_finished()
  ch <- .run_folders()
  sel <- isolate(.results_dir())
  if (length(ch) && !sel %in% ch) sel <- ch[[1]]
  updateSelectInput(session, "results_run", choices = ch, selected = sel)
  if (length(ch) && !identical(sel, isolate(.results_dir()))) .results_dir(sel)
})
observeEvent(input$results_run, {
  if (!nzchar(input$results_run) || identical(input$results_run, .results_dir())) return()
  .results_dir(input$results_run); last_finished(Sys.time())
}, ignoreInit = TRUE)
output$results_run_note <- renderUI({
  last_finished(); want <- .inputs_run_dir(); shown <- .results_dir()
  if (identical(normalizePath(want, mustWork = FALSE), normalizePath(shown, mustWork = FALSE))) return(NULL)
  tags$div(class = "wm-help", "The inputs set above would write to ", tags$code(.run_label(want)),
           if (!want %in% .run_folders()) " (no results there yet)" else "", "; shown here: ", tags$code(.run_label(shown)), ".")
})


## --- Templates: the FG table and the catchability table ---------------------
## The FG table shown in card 2 (the selected pCloud file or the uploaded
## one) and the default catchability table, to edit and upload back.
.catch_default <- if (!is.na(.pcloud_dir)) file.path(.pcloud_dir, "data", "Complementary data",
                                                     "catchability_factors_ecotrans_medits_2021_spp.csv") else NA_character_
output$dl_fg_template <- downloadHandler(
  filename = function() { f <- fg_path(); if (is.null(f) || is.na(f)) "FG_table_template.csv" else basename(f) },
  content = function(file) {
    f <- fg_path()
    if (is.null(f) || is.na(f) || !file.exists(f)) stop("FG table not found - is pCloud Drive running?")
    ## with a q column: the file's own q where filled in, else the q the
    ## last run used for that species (catchability_q_by_species.csv)
    x <- tryCatch(data.table::fread(f), error = function(e) NULL)
    qf <- file.path(.inputs_run_dir(), "biomass", "catchability_q_by_species.csv")
    if (!is.null(x) && file.exists(qf) && "species" %in% names(x)) {
      qs <- unique(data.table::fread(qf)[, .(species = ScientificName, q_run = q_resolved)], by = "species")
      if (!"q" %in% names(x)) x[, q := NA_real_]
      x[, q := suppressWarnings(as.numeric(q))]
      x[, q_run := qs$q_run[match(trimws(species), qs$species)]]
      x[is.na(q), q := q_run][, q_run := NULL]
      data.table::fwrite(x, file, sep = if (grepl(";", readLines(f, n = 1))) ";" else ",")
    } else file.copy(f, file)
  })
output$dl_catch_template <- downloadHandler(
  filename = function() basename(.catch_default),
  content = function(file) {
    if (is.na(.catch_default) || !file.exists(.catch_default)) stop("Default catchability table not found in the pCloud data folder.")
    file.copy(.catch_default, file)
  })
