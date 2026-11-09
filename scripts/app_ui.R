## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: app_ui.R
## =================================================================
## app_ui.R
## Created by: Daniel Vilas
##
## The page's controls (cards 0-6): built once when the page renders; the last
## expression is the UI that the page shows.
## Kept in its own R file (not inside WMed-EwEbuilder.qmd) so it can be
## edited and tested like any script, and RStudio's Visual editor cannot
## rewrap or overwrite it. Sourced by the page with local = TRUE.
## =================================================================

library(shiny)
## GFCM GSA polygons (official GFCM_GSA.zip, simplified to 5 % of the
## vertices for display; GSA 11 has two polygons, W and E Sardinia).
.gsa_geojson <- if (file.exists("gsa_boundaries_westmed.geojson"))
  paste(readLines("gsa_boundaries_westmed.geojson", warn = FALSE), collapse = "") else "null"
.gsa_names <- c("Northern Alboran", "Alboran Island", "Southern Alboran", "Algeria", "Balearic Islands",
                "Northern Spain", "Gulf of Lion", "Corsica", "Ligurian & N Tyrrhenian", "S & C Tyrrhenian", "Sardinia", "Northern Tunisia")
.card <- function(step, title, icon, ..., class = "") {
  tags$div(class = paste("card wm-card mb-3", class), id = paste0("wm-card-", step), `data-wmtitle` = title,
           tags$div(class = "card-header wm-card-header",
                    tags$span(class = "wm-step", step), tags$span(class = "wm-icon", icon), title),
           tags$div(class = "card-body", ...))
}
.switch <- function(id, label, help, value = TRUE) {
  tags$div(class = "form-check form-switch wm-switch",
           tags$input(class = "form-check-input", type = "checkbox", role = "switch", id = id,
                      checked = if (value) NA else NULL),
           tags$label(class = "form-check-label", `for` = id, label),
           tags$div(class = "wm-help", help))
}

tagList(
  ## Leaflet 1.9.4 (the GSA map) ships with the repo in scripts/leaflet/, so
  ## the map does not depend on reaching a CDN; the CDN copy is only a
  ## fallback if the local file is missing.
  ## (as an htmlDependency, so Shiny serves the folder itself: a plain
  ## src = "leaflet/leaflet.js" gave 404 under Run Document)
  if (file.exists("leaflet/leaflet.js"))
    htmltools::htmlDependency("leaflet-wm", "1.9.4", src = c(file = normalizePath("leaflet")),
                              script = "leaflet.js", stylesheet = "leaflet.css"),
  tags$script(HTML("if (typeof L === 'undefined') {
    document.write('<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css\">');
    document.write('<script src=\"https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js\"><\\/script>');
  }")),
  tags$style(HTML("
    .wm-card { border: 0; border-radius: 14px; box-shadow: 0 2px 12px rgba(16, 42, 67, .10); overflow: hidden; }
    .wm-card-header { background: linear-gradient(90deg, #0b4f6c 0%, #1b7f9e 100%); color: #fff; font-weight: 600;
                      font-size: 1.05rem; display: flex; align-items: center; gap: .55rem; border: 0; }
    .wm-step { background: rgba(255,255,255,.22); border-radius: 50%; width: 1.7rem; height: 1.7rem;
               display: inline-flex; align-items: center; justify-content: center; font-size: .9rem; }
    .wm-icon { font-size: 1.1rem; }
    .wm-help { font-size: .82rem; color: #6c757d; margin: .1rem 0 .6rem 0; }
    #wm-map { height: 430px; border-radius: 10px; border: 1px solid #dfe6ec; }
    #wm-map .leaflet-tooltip { font-weight: 600; }
    .wm-legend { background: rgba(255,255,255,.92); padding: .4rem .6rem; border-radius: 8px; font-size: .75rem;
                 line-height: 1.5; box-shadow: 0 1px 4px rgba(0,0,0,.2); }
    .wm-legend i { display: inline-block; width: 12px; height: 12px; border-radius: 3px; margin-right: 6px;
                   vertical-align: -1px; opacity: .8; }
    .wm-quick .btn { margin: 0 .3rem .35rem 0; border-radius: 20px; font-size: .8rem; padding: .2rem .7rem; }
    .wm-gsa-grid .shiny-options-group { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: .1rem .8rem; }
    .wm-gsa-grid .checkbox, .wm-gsa-grid .form-check { margin: 0; font-size: .88rem; }
    .wm-stat { background: #f3f8fb; border-radius: 12px; padding: .7rem .9rem; height: 100%; border-left: 4px solid #1b7f9e; }
    .wm-stat .v { font-size: 1.35rem; font-weight: 700; color: #0b4f6c; line-height: 1.2; }
    .wm-stat .l { font-size: .75rem; text-transform: uppercase; letter-spacing: .04em; color: #6c757d; }
    .wm-stat .n { font-size: .78rem; color: #6c757d; }
    .wm-stat.warn { border-left-color: #e0a100; }
    .wm-switch { margin-bottom: .2rem; }
    .wm-switch .form-check-input { width: 2.4em; height: 1.25em; cursor: pointer; }
    .wm-switch .form-check-label { font-weight: 600; margin-left: .35rem; cursor: pointer; }
    #run { font-size: 1.1rem; padding: .65rem 1.6rem; border-radius: 30px; box-shadow: 0 3px 10px rgba(13,110,253,.3); }
    .wm-fg-table { max-height: 340px; overflow-y: auto; border: 1px solid #e3e9ee; border-radius: 8px; }
    .wm-fg-table table { font-size: .8rem; margin: 0; }
    .wm-fg-table thead th { position: sticky; top: 0; background: #f3f8fb; }
    .wm-config { background: #1e1e2e; color: #cdd6f4; padding: .9rem 1.1rem; border-radius: 10px;
                 font-size: .8rem; white-space: pre; overflow-x: auto; margin: 0; border: 0; }
    .wm-results .nav-link { font-weight: 600; }
    .wm-downloads .btn { margin: 0 .3rem .3rem 0; border-radius: 20px; }
    .wm-results img { border-radius: 8px; cursor: zoom-in; }
    .wm-view { margin-bottom: .8rem; gap: .4rem; }
    .wm-view .nav-link { border-radius: 20px; padding: .35rem 1rem; }
    .wm-key-img { width: 100%; margin-top: .6rem; }
    .wm-filter { display: flex; flex-wrap: wrap; gap: .35rem; align-items: center; margin-bottom: .8rem; }
    .wm-filter .btn { border-radius: 20px; font-size: .8rem; padding: .2rem .75rem; }
    .wm-filter input { max-width: 240px; margin-left: auto; border-radius: 20px; font-size: .85rem; }
    .wm-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: .8rem; }
    .wm-thumb { background: #fff; border: 1px solid #e3e9ee; border-radius: 10px; padding: .45rem; transition: box-shadow .15s; }
    .wm-thumb:hover { box-shadow: 0 4px 14px rgba(16, 42, 67, .15); }
    .wm-thumb img { width: 100%; height: 170px; object-fit: contain; background: #fff; }
    .wm-thumb-name { font-size: .76rem; color: #34495e; margin-top: .3rem; word-break: break-word; line-height: 1.25; }
    .wm-thumb-folder { font-size: .68rem; color: #1b7f9e; text-transform: uppercase; letter-spacing: .03em; }
    #wm-lightbox { position: fixed; inset: 0; background: rgba(10, 20, 30, .92); z-index: 2000; display: none; overflow: hidden; }
    #wm-lightbox img { position: absolute; top: 0; left: 0; transform-origin: 0 0; cursor: grab; max-width: none; user-select: none; }
    #wm-lightbox .wm-lb-bar { position: absolute; top: 12px; right: 16px; z-index: 2001; display: flex; gap: .4rem; }
    #wm-lightbox .wm-lb-bar button { border: 0; border-radius: 20px; padding: .35rem .85rem; font-weight: 600; background: #fff; color: #0b4f6c; }
    #wm-lightbox .wm-lb-name { position: absolute; top: 16px; left: 18px; color: #eceff1; font-size: .9rem; z-index: 2001; max-width: 55%; }
    #wm-lightbox .wm-lb-hint { position: absolute; bottom: 12px; left: 50%; transform: translateX(-50%); color: #cfd8dc; font-size: .8rem; z-index: 2001; }
  ")),

  ## Page navigation: jump to Run / Results / Methods, and hide the methods
  ## text while working with the run controls (the choice is remembered in
  ## this browser).
  tags$style(HTML("
    #wm-nav { position: fixed; top: 10px; right: 14px; z-index: 1500; display: flex; gap: .3rem; background: rgba(255,255,255,.95);
              border-radius: 22px; padding: .25rem .35rem; box-shadow: 0 2px 10px rgba(16,42,67,.18); }
    #wm-nav a, #wm-nav button { border: 0; border-radius: 18px; padding: .2rem .7rem; font-size: .85rem; font-weight: 600;
              color: #0b4f6c; background: transparent; text-decoration: none; }
    #wm-nav a:hover, #wm-nav button:hover { background: #e6f2f6; }
    body.wm-hide-methods .wm-methods-section { display: none !important; }
    #TOC ul.wm-toc-app, #TOC ul.wm-toc-app ul { display: block !important; padding-left: .8rem; }
    #TOC ul.wm-toc-app a.nav-link { font-size: .85em; padding-top: .1rem; padding-bottom: .1rem; }
    @media (max-width: 600px) { #wm-nav { top: auto; bottom: 10px; right: 10px; } }
  ")),
  tags$div(id = "wm-nav",
    tags$a(href = "#sec-configure", "Settings"),
    tags$a(href = "#wm-results-card", "Results"),
    tags$a(href = "#overview", id = "wm-nav-methods", "Methods"),
    tags$button(type = "button", id = "wm-toggle-methods", title = "Show or hide the methods text", "Hide methods")),
  ## Table of contents (right-hand band): the cards of section 1 (and the
  ## parts of Results) as sub-entries of "Configure and run the pipeline",
  ## so plots, results and the FG explorer can be reached in one click.
  tags$script(HTML("
    $(function () {
      var top = $('#TOC a[href=\"#sec-configure\"]').first();
      if (!top.length || top.siblings('ul.wm-toc-app').length) return;
      var ul = $('<ul class=\"wm-toc-app\"></ul>');
      function add(list, href, txt, tab) {
        var a = $('<a class=\"nav-link\"></a>').attr('href', href).text(txt);
        if (tab) a.on('click', function () { var b = $('[data-bs-target=\"' + tab + '\"]'); if (b.length) b.trigger('click'); });
        var li = $('<li></li>').append(a); list.append(li); return li;
      }
      $('.wm-card[id^=\"wm-card-\"]').each(function () {
        var li = add(ul, '#' + this.id, $(this).data('wmtitle'));
        if (this.id === 'wm-card-6') {
          var sub = $('<ul></ul>');
          add(sub, '#run_summary', 'Run summary');
          add(sub, '#wm-fg-explorer', 'FG explorer');
          add(sub, '#results', 'Key plots', '#wm-view-key');
          add(sub, '#results', 'All plots', '#wm-view-all');
          li.append(sub);
        }
      });
      top.after(ul);
    });
  ")),
  tags$script(HTML("
    $(function () {
      var conf = document.getElementById('sec-configure');
      var sec = conf ? conf.closest('section') : null;
      if (sec) $(sec).nextAll('section').addClass('wm-methods-section');
      var key = 'wm-hide-methods';
      function apply(h) { $('body').toggleClass('wm-hide-methods', h); $('#wm-toggle-methods').text(h ? 'Show methods' : 'Hide methods'); }
      var h = false; try { h = localStorage.getItem(key) === '1'; } catch (e) {}
      apply(h);
      $('#wm-toggle-methods').on('click', function () { h = !h; apply(h); try { localStorage.setItem(key, h ? '1' : '0'); } catch (e) {} });
      $('#wm-nav-methods').on('click', function () { if (h) { h = false; apply(h); try { localStorage.setItem(key, '0'); } catch (e) {} } });
    });
  ")),

  ## 0. Folders on this computer -------------------------------------------
  .card("0", "Folders on this computer", "\U0001F4C1", uiOutput("folders"),
    tags$div(class = "mt-2",
      actionButton("setup_check", "Check setup", icon = icon("stethoscope"), class = "btn-sm btn-outline-secondary"),
      tags$small(class = "text-muted ms-2", "folders, pCloud, R packages, internet and the Copernicus login - run it on a new computer first"),
      uiOutput("setup_report"))),

  ## 1. Area -------------------------------------------------------------
  .card("1", "Model area", "\U0001F5FA",
    fluidRow(
      column(7, tags$div(id = "wm-map"),
             tags$div(class = "wm-help", "Click a GSA to add or remove it. GSAs 3, 4 and 12 (orange) have no MEDITS survey and take the survey density per km\u00b2 of the others.")),
      column(5,
        radioButtons("area_mode", NULL, inline = TRUE,
                     choices = c("Western Med GSAs" = "westmed", "Custom region" = "custom"), selected = "westmed"),
        conditionalPanel("input.area_mode == 'westmed'",
          tags$div(class = "wm-quick",
            tags$button(type = "button", class = "btn btn-sm btn-outline-primary", `data-gsas` = "1,2,3,4,5,6,7,8,9,10,11,12", "All 1-12"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-primary", `data-gsas` = "1,2,5,6,7,8,9,10,11", "MEDITS GSAs"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", `data-gsas` = "1,2,5,6", "Spain"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", `data-gsas` = "7,8", "France"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", `data-gsas` = "9,10,11", "Italy"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", `data-gsas` = "1,2,3,4,5,6,7,8,9,10,11", "Without Tunisia"),
            tags$button(type = "button", class = "btn btn-sm btn-outline-danger", `data-gsas` = "", "Clear")),
          tags$div(class = "wm-gsa-grid",
            checkboxGroupInput("gsas", NULL, choices = setNames(1:12, paste0(1:12, " - ", .gsa_names)), selected = 1:12)),
          radioButtons("basis", "Area basis (A_model)",
                       choices = c("Whole GSA, coast to bottom" = "whole_gsa", "MEDITS strata 10-800 m only" = "medits_10_800"),
                       selected = "whole_gsa"),
          tags$div(class = "wm-help", "Every t/km\u00b2 is per km\u00b2 of this area. Whole GSA: depths outside 10-800 m filled with AquaMaps."),
          tags$div(class = "wm-help", tags$strong("Note: "), "groups the surveys cannot measure (plankton, seagrass, megafauna...) were estimated for the whole model area (GSAs 1-12). With a filtered area their biomass can change and may not represent the smaller area well.")),
        conditionalPanel("input.area_mode == 'custom'",
          textInput("area_name", "Area name (AREA_NAME)", "custom_region"),
          tags$div(class = "wm-help", "Used for the output subfolder, so runs for different regions do not overwrite each other."),
          radioButtons("custom_type", "Region defined by", inline = TRUE,
                       choices = c("Shapefile / polygon file" = "shapefile", "Lon/lat box" = "bbox"), selected = "shapefile"),
          conditionalPanel("input.custom_type == 'shapefile'",
            fileInput("custom_shp", "Upload the region (CUSTOM_SHAPEFILE_PATH)", multiple = TRUE, width = "100%",
                      accept = c(".zip", ".shp", ".shx", ".dbf", ".prj", ".cpg", ".geojson", ".json", ".gpkg", ".kml"),
                      placeholder = "a .zip of the shapefile, or .shp+.shx+.dbf+.prj, or .geojson/.gpkg"),
            tags$div(class = "wm-help", "Any CRS (reprojected to WGS84). Each polygon becomes one area of the model."),
            actionButton("custom_example", "Use the example: Mediterranean cetacean migration corridor", icon = icon("map"), class = "btn-outline-primary btn-sm mt-1"),
            tags$div(class = "wm-help", "Spanish Marine Protected Area between the Catalan-Valencian coast and the Balearic Islands (46,386 km\u00b2; boundary from Real Decreto 699/2018). Files in scripts/example_regions/.")),
          conditionalPanel("input.custom_type == 'bbox'",
            fluidRow(column(6, numericInput("bbox_xmin", "Lon min", 2, step = 0.1)), column(6, numericInput("bbox_xmax", "Lon max", 8, step = 0.1))),
            fluidRow(column(6, numericInput("bbox_ymin", "Lat min", 38, step = 0.1)), column(6, numericInput("bbox_ymax", "Lat max", 42, step = 0.1)))),
          tags$div(class = "alert alert-info py-2 small",
                   "A custom region runs the ticked scripts (normally 01-04 and 06) for that region; outputs go to their own subfolder, out_dir/<area name>. Biomass comes from the hauls inside the region (FGs the survey cannot measure take the Western Med density per km\u00b2, so run the Western Med first); catch comes from the GSA(s) the region overlaps, per km\u00b2; P/B, Q/B, F and diets then follow the region's own biomass and species shares.")))),
    tags$hr(),
    uiOutput("area_summary")),

  ## Functional groups ----------------------------------------------------
  .card("2", "Food web structure: species and FG table", "\U0001F41F",
    fluidRow(
      column(6, selectInput("fg_file", "FG file in the pCloud data folder (FG_SPECIES_FILE)", choices = NULL, width = "100%")),
      column(6, fileInput("fg_upload", "or upload a csv", accept = ".csv", width = "100%",
                          placeholder = "columns FG_number, FG_name, species"))),
    tags$div(class = "mb-2",
      downloadButton("dl_fg_template", "Download the FG table shown (template)", class = "btn-sm btn-outline-secondary"),
      tags$span(class = "wm-help ms-2", "Edit it (add species, merge or split FGs) and upload it back above.")),
    uiOutput("fg_summary")),

  ## 3. Years and 4. scripts to run ------------------------------------------
  fluidRow(
    column(6, .card("3", "Years", "\U0001F4C5",
      sliderInput("ecopath_years", "Ecopath baseline (YEAR_ECOPATH)", min = 1990, max = 2023, value = c(1994, 1996), step = 1, sep = "", width = "100%"),
      sliderInput("ts_years", "Ecosim time series (TS_YEARS)", min = 1990, max = 2023, value = c(1995, 2023), step = 1, sep = "", width = "100%"),
      tags$div(class = "wm-help", "Used by every script: biomass (01), catch, landings, discards and effort (02, from the first Ecopath or Ecosim year to the last Ecosim year), P/B and Q/B (03), diets (04) and the environmental drivers (05, 05b)."))),
    column(6, .card("4", "Scripts to run", "\u2699\ufe0f",
      checkboxGroupInput("steps_chk", "Scripts to run (RUN_STEPS)", inline = TRUE,
                         choices = c("01 biomass" = 1, "02 fisheries" = 2, "03 P/B, Q/B" = 3, "04 diets" = 4, "05 environment" = 5,
                                     "05b Ecospace maps" = 6, "06 diagnostics" = 7),
                         selected = c(1:4, 7)),
      tags$div(class = "mb-2",
        actionLink("steps_all", "all 01-06"), " \u00b7 ", actionLink("steps_main", "01-04 + 06"), " \u00b7 ", actionLink("steps_none", "none"), " \u00b7 ",
        actionLink("steps_after1", "02-04 + 06 (keep biomass)"), " \u00b7 ", actionLink("steps_only_diag", "only 06"), " \u00b7 ", actionLink("steps_only_env", "only 05 + 05b"), " \u00b7 ", actionLink("steps_only_esp", "only 05b (new base map)")),
      tags$small(class = "text-muted d-block mb-1",
        "The scripts run in this order; 06 diagnostics runs last, so it checks the outputs of all the others. ",
        "The settings of each script are in the cards below (4a biomass, 4b fisheries, 4c environment and Ecospace); 03 and 04 have none of their own."),
      uiOutput("steps_check")))),

  ## 4a. Biomass settings (01, 01b) ---------------------------------------------
  .card("4a", "Biomass settings (01 biomass, 01b unsurveyed groups)", "\U0001F4E5",
    tags$div(class = "wm-help mb-2", "Biomass comes from the MEDITS trawl and MEDIAS acoustic surveys (2024 MEDBS data) and, for groups they cannot measure, from literature and models (01b)."),
    fluidRow(
      column(6,
        tags$h6("Survey density (MEDITS, MEDIAS)"),
        .switch("strata", "Strata-weighted density (STRATA)", "Off = simple mean density instead of the depth-stratum (area-weighted) estimator.", TRUE),
        .switch("drop_outliers", "Remove flagged outlier hauls (DROP_OUTLIERS)", "Off = outliers are only reported in survey_outliers_flagged.csv.", TRUE),
        .switch("catch_q", "Catchability correction (APPLY_CATCHABILITY)",
                "Divides MEDITS density by the species catchability q. Default table: catchability_factors_ecotrans_medits_2021_spp.csv (pCloud). Upload your own below (columns ScientificName, q).", TRUE),
        conditionalPanel("input.catch_q",
          fileInput("catch_upload", "Own catchability table (CATCHABILITY_FILE, optional)", accept = ".csv", width = "100%",
                    placeholder = "csv with ScientificName and q"),
          tags$div(class = "mb-2", downloadButton("dl_catch_template", "Download the default catchability table (template)",
                                                  class = "btn-sm btn-outline-secondary")))),
      column(6,
        tags$h6("Ecopath baseline and Ecosim series"),
        .switch("fill_b", "Fill missing Ecopath_B with a later-years mean (ECOPATH_B_FILL_MODE)",
                "FGs with no biomass in the Ecopath years get their mean over the years below (else the nearest year with data). Expanding FGs always get 0.00001 t/km\u00b2. Off = nearest single year only.", TRUE),
        conditionalPanel("input.fill_b",
          sliderInput("fill_years", "Years for the mean (ECOPATH_B_FILL_YEARS)", min = 1990, max = 2023, value = c(1995, 2000),
                      step = 1, sep = "", width = "100%")),
        .switch("normalize_ts", "Ecosim series as relative index (NORMALIZE_TS)", "Off = raw density (t/km\u00b2) instead of first value = 1.", TRUE),
        tags$h6(class = "mt-3", "Groups the surveys can't measure (01b)"),
        selectInput("phyto_src", "Phytoplankton B and P/B (PHYTO_SOURCE)", width = "100%",
                    choices = c("ISIMIP COBALT2 1995 (B), EcoBase (P/B)" = "isimip",
                                "Copernicus, from 05 environment (needs 05 run first)" = "copernicus"), selected = "isimip"),
        tags$div(class = "wm-help mb-2", "The biomass is set in 01b; the P/B goes to the workbook in 03."))),
    tags$hr(),
    tags$h6("Upload biomass from any survey"),
    tags$div(class = "wm-help", "Any survey and any species: a csv with ",
             tags$code("species"), " (scientific name), ", tags$code("year"), " and ", tags$code("biomass_t_km2"),
             " (t wet weight per km\u00b2 of the model area); optionally ", tags$code("area_km2"),
             " (when the biomass holds for a smaller area) and ", tags$code("reference"), ". ",
             tags$strong("A species in this file takes its biomass only from it, in every year:"),
             " its MEDITS/MEDIAS value is removed from its FG, the file's values are used in the years it gives, and in other years the species adds nothing. Ecopath B = the mean of its rows in the Ecopath years (if none, of its first three years). An FG name instead of a species replaces the whole FG."),
    fileInput("other_survey_upload", NULL, accept = ".csv", width = "100%", buttonLabel = "Upload survey biomass (csv)",
              placeholder = "species, year, biomass_t_km2 [, area_km2, reference]"),
    uiOutput("other_survey_info"),
    downloadButton("dl_other_survey_template", "Download an empty template", class = "btn-sm btn-outline-secondary"),
    tags$div(class = "wm-help mt-2", "Runs with an uploaded survey go to their own output folder (\u2026__survey-own), so the default results are kept.")),

  ## 4b. Fisheries settings (02) -----------------------------------------------
  .card("4b", "Fisheries settings (02 fisheries)", "\U0001F3A3",
    tags$div(class = "wm-help mb-2", "Catch, landings, discards and effort follow the years of card 3, for the GSAs of card 1."),
    tags$div(class = "wm-help mb-2", tags$strong("FGs with high F"), " are reported, not changed: F above 1, or above the highest F in the stock assessments of the FG's species. Each one gets a recommendation in high_F_REVIEW.csv (Results), e.g. a trawl survey that hardly samples the group, missing catchability (card 4a), or catch to check. Ecopath_B is left as it is.")),

  ## 4c. Environment (05) and Ecospace maps (05b) ---------------------------------
  .card("4c", "Environment and Ecospace settings (05 environment, 05b Ecospace maps)", "\U0001F30A",
    tags$div(class = "wm-help mb-2",
      "05 environment gets monthly temperature, salinity, NPP and phytoplankton from Copernicus Marine (Ecosim forcing and, with PHYTO_SOURCE = Copernicus in card 4a, the Ecopath phytoplankton). ",
      "Its first run downloads several GB; later runs only fetch new years. It needs a Copernicus Marine account: ",
      actionLink("cm_login_link", "enter or change the login"), "."),
    tags$script(HTML("document.addEventListener('toggle', function (e) { if (e.target.classList && e.target.classList.contains('wm-esp') && e.target.open && window.jQuery) { jQuery(e.target).trigger('shown'); jQuery(window).trigger('resize'); } }, true);")),
    tags$details(class = "wm-esp",
      tags$summary(tags$strong("Open the Ecospace drivers"), tags$span(class = "wm-help ms-2",
        "monthly ASCII maps of temperature, salinity, NPP and phytoplankton on your Ecospace grid: resolution, output folder and map viewer")),
      tags$div(class = "mt-2",
        tags$div(class = "wm-help mt-1", "05b regrids the monthly maps of 05 (temperature, salinity, bottom temperature, NPP, phytoplankton carbon) onto the Ecospace base map: same rows, columns, corner and cell size, coastal gaps filled from the neighbouring cells, land = NODATA. No download: rerun 05b alone for another resolution."),
        radioButtons("esp_grid_mode", "Base map", inline = TRUE, width = "100%",
                     choices = c("Make it at a resolution" = "cell", "Use my depth .asc" = "file"), selected = "cell"),
        conditionalPanel("input.esp_grid_mode == 'cell'",
          fluidRow(column(6, numericInput("esp_cell_arcmin", "Cell size (arc-minutes)", value = 10, min = 0.5, max = 60, step = 0.5, width = "100%")),
                   column(6, tags$div(class = "wm-help mt-4", textOutput("esp_cell_km", inline = TRUE)))),
          tags$div(class = "wm-help", "Depth from the EMODnet bathymetry (1 arc-min, data/Bathymetry): a cell is water when at least half of it is below sea level, with the mean depth of that part; cells outside the model area are land. The depth map is written next to the drivers (depth_used.asc): load it as the Ecospace depth map.")),
        conditionalPanel("input.esp_grid_mode == 'file'",
          fileInput("ecospace_depth_upload", NULL, multiple = TRUE, accept = c(".asc", ".prj"), width = "100%",
                    buttonLabel = "Upload depth .asc", placeholder = "depth map (+ .prj if projected)"),
          uiOutput("ecospace_depth_info")),
        uiOutput("esp_out_where"),
        tags$hr(),
        tags$h6("View the maps"),
        fluidRow(
          column(4, selectInput("esp_view_map", "Base map", choices = NULL, width = "100%")),
          column(4, selectInput("esp_view_var", "Variable", choices = NULL, width = "100%")),
          column(4, radioButtons("esp_view_scale", "Colour scale", inline = TRUE, width = "100%",
                                 choices = c("Same for all years" = "fixed", "This year / month" = "month"), selected = "fixed"))),
        fluidRow(
          column(6, radioButtons("esp_view_mode", NULL, inline = TRUE,
                                 choices = c("A year: its 12 months" = "year", "One month" = "month"), selected = "year")),
          column(6, radioButtons("esp_view_values", NULL, inline = TRUE,
                                 choices = c("Values" = "value", "Difference from the average year" = "anom"), selected = "value"))),
        uiOutput("esp_view_slider"),
        uiOutput("esp_view_plot_ui"),
        tags$div(class = "mt-2 mb-1",
          downloadButton("dl_esp_fig", "Download this figure (.png)", class = "btn-sm btn-outline-secondary"),
          downloadButton("dl_esp_zip", "Download all the maps of this base map (.zip of .asc)", class = "btn-sm btn-outline-secondary ms-2")),
        tags$div(class = "wm-help", "Press \u25B6 under the slider to play the years (or months). Grey = land (NODATA). One colour scale for all panels. The changes between years are small next to the seasonal cycle, so with \u201cValues\u201d the years look alike: \u201cDifference from the average year\u201d shows each map minus the mean of that month over all years (red = warmer / higher, blue = colder / lower). The line under the single-month map is the mean over the water cells, with the month shown marked."),
        fluidRow(column(6, selectInput("esp_step", "Time step", c("Monthly" = "monthly", "Annual mean" = "annual"), width = "100%")),
                 column(6, selectInput("esp_scaling", "Values", c("Absolute (deg C, PSU, g C m-2 ...)" = "absolute",
                                                                  "Relative to the base year (= 1)" = "relative"), width = "100%"))),
    tags$div(class = "wm-help", "Tick \u201c05b Ecospace maps\u201d in Scripts to run (card 4), or use the link \u201conly 05b\u201d there. 05 must have run once.")))),

  ## 4. Run ----------------------------------------------------------------
  .card("5", "Run", "\u25B6\ufe0f",
    fluidRow(
      column(5,
        actionButton("run", "Run pipeline", class = "btn-primary", icon = icon("play")),
        actionButton("stop", "Stop", class = "btn-outline-danger ms-1", icon = icon("stop")),
        tags$div(class = "alert alert-secondary py-1 px-2 mt-2 mb-2 small",
          tags$strong("Run time: "), "01-04 + 06 about 10-30 min; the first run on a new computer up to about 70 min ",
          "(it downloads and caches bathymetry, FishBase/SeaLifeBase and taxonomy). ",
          "05 environment: the first run downloads several GB from Copernicus and can take a few hours; ",
          "later runs about 15 min. Internet needed. Keep this page and R open until it finishes."),
        uiOutput("run_progress"),
        tags$style(HTML("#run_status { font-size: .9rem; line-height: 1.35; max-height: 460px; overflow-y: auto; white-space: pre-wrap; background: #0f172a; color: #e2e8f0; border-radius: 8px; padding: .7rem .9rem; }")),
        tags$script(HTML("$(document).on('shiny:value', function(e) { if (e.name === 'run_status') setTimeout(function() { var el = document.getElementById('run_status'); if (el) el.scrollTop = el.scrollHeight; }, 50); });")),
        verbatimTextOutput("run_status")),
      column(7,
        tags$div(class = "d-flex justify-content-between align-items-center mb-1",
          tags$strong("Settings block"),
          tags$div(
            tags$button(type = "button", id = "wm-copy", class = "btn btn-sm btn-outline-secondary", "Copy"),
            downloadButton("dl_config", "Download .R", class = "btn-sm btn-outline-secondary"))),
        fileInput("settings_upload", NULL, accept = c(".R", ".r"), width = "100%", buttonLabel = "Load settings (.R)",
                  placeholder = "a settings file from 'Download .R': sets the inputs above again"),
        textOutput("config_text", container = function(...) tags$pre(class = "wm-config", ...))))),

  ## Results ---------------------------------------------------------------
  tags$div(id = "wm-results-card"),
  .card("6", "Results", "\U0001F4CA",
    tags$div(class = "mb-2",
      selectInput("results_run", "Show the results of", choices = NULL, width = "100%"),
      tags$div(class = "wm-help", "Each set of inputs (area, years, FG table) has its own output folder: the default inputs (GSAs 1-12, Ecopath 1994-1996, Ecosim 1995-2023, FG_WMed_2026.csv) in the main output folder, any other in runs/<area>__EP<years>_TS<years>. Runs with different inputs never overwrite each other; the same inputs again replace their own results."),
      uiOutput("results_run_note")),
    tags$div(class = "wm-downloads mb-3",
      tags$strong("Download the outputs: "),
      downloadButton("dl_workbook", "Ecopath/Ecosim inputs (.xlsx)", class = "btn-sm btn-primary"),
      downloadButton("dl_diet", "Diet matrices (.xlsx)", class = "btn-sm btn-outline-primary"),
      downloadButton("dl_review", "REVIEW tables (.zip)", class = "btn-sm btn-outline-secondary"),
      downloadButton("dl_all", "All tables (.zip)", class = "btn-sm btn-outline-secondary"),
      downloadButton("dl_logs", "Logs of the last run (.zip)", class = "btn-sm btn-outline-secondary"),
      uiOutput("dl_info", inline = TRUE)),
    uiOutput("run_summary"),
    tags$details(class = "card mb-3", open = NA, id = "wm-fg-explorer",
      tags$summary(class = "card-header py-2", tags$strong("FG explorer"), " - one group's Ecosim series from the workbook"),
      tags$div(class = "card-body py-2",
        div(class = "d-flex flex-wrap align-items-center gap-3",
            selectInput("fg_explore", NULL, choices = NULL, width = "420px"),
            radioButtons("fg_explore_scale", NULL, inline = TRUE,
                         choices = c("Relative to Ecopath years" = "rel", "Absolute (t/km\u00b2)" = "abs"), selected = "rel")),
        fluidRow(
          column(6, tags$div(style = "position: relative;",
            plotOutput("fg_explore_plot", height = "340px",
                       hover = hoverOpts("fg_explore_hover", delay = 80, delayType = "throttle", nullOutside = TRUE)),
            uiOutput("fg_explore_tip"))),
          column(6, plotOutput("fg_explore_plot_catch", height = "340px"))),
        uiOutput("fg_explore_info"),
        tags$div(class = "wm-help", "Band: survey sampling uncertainty of the index (index x (1 \u00b1 1.96 CV)), for the FGs measured by MEDITS. Grey: the Ecopath baseline years; their B (t/km\u00b2) is in the subtitle and below, with its source. Point the mouse at a year of the biomass plot to see the share of each species in the FG biomass that year (survey species from MEDITS and MEDIAS; in the Ecopath years, and for groups without a survey split, the Ecopath baseline composition from all sources)."))),
    tags$div(class = "wm-results", uiOutput("results"))),

  tags$script(type = "application/json", id = "wm-gsa-geojson", HTML(.gsa_geojson)),
  tags$script(HTML(r"---(
(function () {
  var UNSURVEYED = [3, 4, 12];
  function checked() {
    return $('input[name="gsas"]:checked').map(function () { return +this.value; }).get();
  }
  function setGsas(list) {
    var boxes = $('input[name="gsas"]');
    boxes.each(function () { $(this).prop('checked', list.indexOf(+this.value) !== -1); });
    boxes.first().trigger('change');
  }
  var tries = 0;
  function init() {
    if (typeof L === 'undefined' || !document.getElementById('wm-map')) {
      if (++tries === 50 && document.getElementById('wm-map'))   // 10 s without Leaflet: say so instead of a blank box
        document.getElementById('wm-map').innerHTML = '<div style="padding:1rem;color:#8a4b00">The map library (Leaflet) did not load: scripts/leaflet/leaflet.js is missing and the CDN could not be reached. Use the GSA checkboxes instead.</div>';
      return setTimeout(init, 200);
    }
    var map = L.map('wm-map', { scrollWheelZoom: false, zoomSnap: 0.25 });
    // Esri Ocean basemap (no API key); falls back to OpenStreetMap if its tiles fail
    var base = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}', {
      attribution: 'Tiles &copy; Esri - GEBCO, NOAA, Garmin', maxZoom: 10 }).addTo(map);
    var fails = 0;
    base.on('tileerror', function () {
      if (++fails === 3) {
        map.removeLayer(base);
        L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
          attribution: '&copy; OpenStreetMap contributors', maxZoom: 10 }).addTo(map);
      }
    });
    var data = null;
    try { data = JSON.parse(document.getElementById('wm-gsa-geojson').textContent); } catch (e) {}
    var layer = null, rect = null;
    var CUSTOM_BG = { fillOpacity: 0.05, color: '#b8c4cc', weight: 1, fillColor: '#d5dde3' };
    function isCustom() { return $('input[name="area_mode"]:checked').val() === 'custom'; }
    function style(f) {
      var g = f.properties.gsa_num, on = checked().indexOf(g) !== -1, uns = UNSURVEYED.indexOf(g) !== -1;
      return { color: on ? '#0b4f6c' : '#8a99a6', weight: on ? 1.5 : 1,
               fillColor: on ? (uns ? '#f0a202' : '#1b7f9e') : '#d5dde3', fillOpacity: on ? 0.5 : 0.15 };
    }
    if (data) {
      layer = L.geoJSON(data, {
        style: style,
        onEachFeature: function (f, l) {
          var g = f.properties.gsa_num;
          l.bindTooltip('GSA ' + g + ' - ' + f.properties.SMU_NAME +
                        (UNSURVEYED.indexOf(g) !== -1 ? ' (no MEDITS)' : ''), { sticky: true });
          l.on('click', function () {
            if ($('input[name="area_mode"]:checked').val() !== 'westmed') return;
            var s = checked(), i = s.indexOf(g);
            if (i === -1) s.push(g); else s.splice(i, 1);
            setGsas(s);
          });
          // Custom region: GSAs are only a faint background - hovering must not
          // repaint them in the GSA-selection colours (resetStyle used the
          // selection style, so hovered GSAs looked selected)
          l.on('mouseover', function () { if (!isCustom()) l.setStyle({ weight: 3 }); });
          l.on('mouseout', function () { l.setStyle(isCustom() ? CUSTOM_BG : style(f)); });
        }
      }).addTo(map);
      map.fitBounds(layer.getBounds(), { padding: [10, 10] });
      var legend = L.control({ position: 'topright' });
      legend.onAdd = function () {
        var d = L.DomUtil.create('div', 'wm-legend');
        d.innerHTML = '<i style="background:#1b7f9e"></i>In the model, MEDITS survey<br>' +
                      '<i style="background:#f0a202"></i>In the model, no survey (GSA 3, 4, 12)<br>' +
                      '<i style="background:#d5dde3"></i>Not in the model';
        return d;
      };
      legend.addTo(map);
    } else { map.setView([39, 5], 5); }
    var shapeLayer = null, shapeData = null;
    function drawBox() {
      if (rect) { map.removeLayer(rect); rect = null; }
      if (shapeLayer) { map.removeLayer(shapeLayer); shapeLayer = null; }
      var custom = $('input[name="area_mode"]:checked').val() === 'custom';
      if (layer) layer.setStyle(custom ? CUSTOM_BG : style);
      if (!custom) { if (layer) layer.setStyle(style); return; }
      if ($('input[name="custom_type"]:checked').val() === 'shapefile') {
        if (shapeData) {
          shapeLayer = L.geoJSON(shapeData, { style: { color: '#d9480f', weight: 2, fillOpacity: 0.25 } }).addTo(map);
          map.fitBounds(shapeLayer.getBounds(), { padding: [20, 20], maxZoom: 9 });
        }
        return;
      }
      var v = ['bbox_xmin', 'bbox_xmax', 'bbox_ymin', 'bbox_ymax'].map(function (id) { return parseFloat($('#' + id).val()); });
      if (v.every(function (x) { return !isNaN(x); }) && v[1] > v[0] && v[3] > v[2]) {
        rect = L.rectangle([[v[2], v[0]], [v[3], v[1]]], { color: '#d9480f', weight: 2, fillOpacity: 0.15 }).addTo(map);
      }
    }
    $(document).on('change', 'input[name="gsas"]', function () { if (layer && !isCustom()) layer.setStyle(style); });
    $(document).on('change', 'input[name="area_mode"], input[name="custom_type"]', drawBox);
    function onShape(m) { shapeData = m.geojson ? JSON.parse(m.geojson) : null; drawBox(); }
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('wm-custom-shape', onShape);
    $(document).on('input change', '#bbox_xmin, #bbox_xmax, #bbox_ymin, #bbox_ymax', drawBox);
    $(document).on('click', '.wm-quick .btn', function () {
      var s = String($(this).data('gsas'));
      setGsas(s ? s.split(',').map(Number) : []);
    });
    $(document).on('click', '#wm-copy', function () {
      var btn = this;
      navigator.clipboard.writeText($('#config_text').text()).then(function () {
        btn.textContent = 'Copied!'; setTimeout(function () { btn.textContent = 'Copy'; }, 1200);
      });
    });
    setTimeout(function () { map.invalidateSize(); if (layer) map.fitBounds(layer.getBounds(), { padding: [10, 10] }); }, 400);
  }
  $(init);

  // --- Zoomable plot viewer: click a plot, wheel or +/- to zoom, drag to move, Esc to close
  var lb, img, sc = 1, tx = 0, ty = 0, drag = null, seq = [], pos = 0;
  // Left/right arrows (or the < > buttons) go to the previous/next plot of
  // the same set: the visible thumbnails of All plots, the Key plots, or the
  // figures in the text.
  function showAt(i) {
    if (!seq.length) return;
    pos = (i + seq.length) % seq.length;
    var el = seq[pos];
    img.onload = fit; img.src = el.currentSrc || el.src;
    lb.find('.wm-lb-name').text(($(el).attr('alt') || '') + (seq.length > 1 ? '   (' + (pos + 1) + ' / ' + seq.length + ')' : ''));
    lb.find('[data-z="prev"], [data-z="next"]').toggle(seq.length > 1);
    lb.show(); if (img.complete) fit();
  }
  function apply() { img.style.transform = 'translate(' + tx + 'px,' + ty + 'px) scale(' + sc + ')'; }
  function fit() {
    var w = img.naturalWidth, h = img.naturalHeight, W = window.innerWidth, H = window.innerHeight;
    sc = Math.min(W / w, (H - 60) / h, 1); tx = (W - w * sc) / 2; ty = (H - h * sc) / 2 + 20; apply();
  }
  function zoomAt(f, cx, cy) {
    var ns = Math.min(Math.max(sc * f, 0.1), 8);
    tx = cx - (cx - tx) * ns / sc; ty = cy - (cy - ty) * ns / sc; sc = ns; apply();
  }
  function build() {
    lb = $('<div id="wm-lightbox"><div class="wm-lb-bar">' +
           '<button data-z="prev" title="Previous (left arrow)">&lsaquo;</button><button data-z="next" title="Next (right arrow)">&rsaquo;</button>' +
           '<button data-z="in">+</button><button data-z="out">&minus;</button>' +
           '<button data-z="fit">Fit</button><button data-z="full">100%</button><button data-z="close">Close &times;</button></div>' +
           '<div class="wm-lb-name"></div>' +
           '<img alt=""><div class="wm-lb-hint">Left/right arrows: previous/next plot - mouse wheel or +/- to zoom - drag to move - Esc to close</div></div>').appendTo('body');
    img = lb.find('img')[0];
    lb.on('click', 'button', function () {
      var z = $(this).data('z'), W = window.innerWidth / 2, H = window.innerHeight / 2;
      if (z === 'in') zoomAt(1.4, W, H); else if (z === 'out') zoomAt(1 / 1.4, W, H);
      else if (z === 'fit') fit(); else if (z === 'full') zoomAt(1 / sc, W, H);
      else if (z === 'prev') showAt(pos - 1); else if (z === 'next') showAt(pos + 1); else lb.hide();
    });
    lb[0].addEventListener('wheel', function (e) { e.preventDefault(); zoomAt(e.deltaY < 0 ? 1.15 : 1 / 1.15, e.clientX, e.clientY); }, { passive: false });
    $(img).on('mousedown', function (e) { e.preventDefault(); drag = { x: e.clientX - tx, y: e.clientY - ty }; img.style.cursor = 'grabbing'; });
    $(document).on('mousemove', function (e) { if (drag) { tx = e.clientX - drag.x; ty = e.clientY - drag.y; apply(); } });
    $(document).on('mouseup', function () { drag = null; if (img) img.style.cursor = 'grab'; });
    $(document).on('keydown', function (e) {
      if (!lb || !lb.is(':visible')) return;
      if (e.key === 'Escape') lb.hide();
      else if (e.key === 'ArrowRight') { e.preventDefault(); showAt(pos + 1); }
      else if (e.key === 'ArrowLeft') { e.preventDefault(); showAt(pos - 1); }
    });
  }
  function wmFilter() {
    var f = $('.wm-filter .btn.active').data('folder') || '', q = ($('#wm-search').val() || '').toLowerCase();
    $('.wm-thumb').each(function () {
      var ok = (!f || $(this).data('folder') === f) && (!q || String($(this).data('name')).indexOf(q) !== -1);
      $(this).toggle(ok);
    });
  }
  $(document).on('click', '.wm-filter .btn', function () {
    $('.wm-filter .btn').removeClass('active'); $(this).addClass('active'); wmFilter();
  });
  $(document).on('input', '#wm-search', wmFilter);
  // Figures in the text: their image comes from out_dir through the Shiny session
  function wmFillFiles(m) {
    $('img[data-wmrepo]').each(function () {
      var el = this;
      el.onerror = function () {
        $(el).replaceWith($('<div class="alert alert-secondary small"></div>').text('Image not found in the repo: ' + $(el).data('wmrepo')));
      };
      el.src = m.repo + '&f=' + encodeURIComponent($(el).data('wmrepo')) + '&v=' + m.v; el.style.cursor = 'zoom-in';
    });
    $('[data-wmfile]').each(function () {
      var el = this, url = m.base + '&f=' + encodeURIComponent($(el).data('wmfile')) + '&v=' + m.v;
      if (el.tagName === 'IMG') {
        el.onerror = function () {
          $(el).replaceWith($('<div class="alert alert-secondary small"></div>').text(
            'Figure not yet available (' + $(el).data('wmfile') + '): run ' + ($(el).data('wmhint') || 'the pipeline') + ' first.'));
        };
        el.src = url; el.style.cursor = 'zoom-in';
      } else { el.href = url; }
    });
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('wmfile-base', wmFillFiles);
  else $(document).on('shiny:connected', function () { Shiny.addCustomMessageHandler('wmfile-base', wmFillFiles); });
  $(document).on('click', 'img[data-wmfile], img[data-wmrepo]', function () {
    if (!this.naturalWidth || this.naturalWidth < 2) return;
    if (!lb) build();
    seq = $('img[data-wmfile], img[data-wmrepo]').filter(function () { return this.naturalWidth > 1; }).toArray();
    showAt(Math.max(seq.indexOf(this), 0));
  });
  $(document).on('click', '.wm-results img', function () {
    if (!this.src || this.naturalWidth === 0) return;
    if (!lb) build();
    var grid = $(this).closest('.wm-grid');
    seq = (grid.length ? grid.find('.wm-thumb:visible img') : $(this).closest('.tab-pane').find('img.wm-key-img')).toArray();
    if (seq.indexOf(this) < 0) seq = [this];
    showAt(seq.indexOf(this));
  });
})();
)---"))
)
