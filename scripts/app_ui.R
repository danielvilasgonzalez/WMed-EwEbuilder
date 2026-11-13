## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: app_ui.R
## =================================================================
## app_ui.R
## Created by: Daniel Vilas
##
## The page's controls (cards 0-7): built once when the page renders; the last
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
## Land for the survey hauls map (Natural Earth 1:10m land, public domain,
## clipped to 8W-18E, 33-46.5N and simplified), so the coast shows even
## without the online basemap.
.land_geojson <- if (file.exists("land_westmed.geojson"))
  paste(readLines("land_westmed.geojson", warn = FALSE), collapse = "") else "null"
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
        if (this.id === 'wm-card-7') {
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
      actionButton("cm_install", "Install the Copernicus tool", icon = icon("download"), class = "btn-sm btn-outline-secondary ms-1",
                   title = "Only for 05 environment: installs copernicusmarine in its own Python environment (~/.copernicusmarine-venv)"),
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
    uiOutput("fg_summary"),
    div(class = "d-flex flex-wrap align-items-end gap-3 mt-3",
        radioButtons("fw_view", "Show", inline = TRUE, choices = c("By FG" = "fg", "By species" = "sp"), selected = "fg"),
        selectInput("fw_fg", "Functional group", choices = c("All FGs" = ""), width = "300px"),
        textInput("fw_search", "Search (species or FG name)", width = "240px", placeholder = "e.g. Merluccius, hake"),
        tags$div(class = "mb-3", downloadButton("dl_fw_sp", "Download the FG table with biomass and q (csv)", class = "btn-sm btn-outline-secondary"))),
    uiOutput("fg_table_view"),
    tags$div(class = "wm-help", "Biomass of the last run (Ecopath years). By FG: B with q = the FG's Ecopath B; B without q = the sum of its species without catchability. By species: q, B with and without q and the species' share of its FG (B proportion). B without q: for an FG whose B is a MEDITS value, the FG B scaled by the raw / q-corrected survey biomass and split by the raw shares; for other FGs (acoustic series, stock assessment, literature) q only changes the split, not the FG total. MEDIAS acoustic has no q. The download is the FG table with these columns added (B_with_q_t_km2, B_without_q_t_km2, prop_with_q, prop_without_q)."),
    tags$hr(),
    tags$h6("Diet matrix (04)"),
    checkboxGroupInput("diet_sources", "Diet data sources, in order of priority", inline = TRUE,
                       c("dietcalcR (diet calculator, internal diet database)" = "dietcalc",
                         "MAISHA" = "maisha", "FishBase / SeaLifeBase" = "fishbase", "EcoBase models" = "ecobase"),
                       selected = c("dietcalc", "maisha", "fishbase", "ecobase")),
    conditionalPanel("input.diet_sources.indexOf('dietcalc') > -1",
      fluidRow(column(4, textInput("dietcalc_dir", "dietcalcR folder", value = "", width = "100%", placeholder = "pCloud data/diet/dietcalcR")),
               column(4, textInput("dietcalc_db", "Diet database (.accdb, .sqlite or folder of csv)", value = "", width = "100%", placeholder = "pCloud data/diet/dietcalc_db")),
               column(4, textInput("dietcalc_config", "Compatibility tables folder", value = "", width = "100%", placeholder = "pCloud data/diet/dietcalc_config"))),
      fluidRow(column(4, selectInput("dietcalc_region", "Region of the model in the database", width = "100%",
                                     c("Western Mediterranean Sea", "North-western Mediterranean", "Mediterranean Sea", "Catalan Sea",
                                       "Balearic Sea", "Lion Gulf", "Ligurian Sea", "Tyrrhenian Sea"))),
               column(4, selectInput("dietcalc_nobio", "Prey with no FG of the model", width = "100%",
                                     c("Drop (renormalise the column)" = "drop", "Imports" = "imports"))))),
    tags$div(class = "mb-2", downloadButton("dl_diet_src", "Download the diet matrix of each source (.zip)", class = "btn-sm btn-outline-secondary")),
    tags$div(class = "wm-help", "Each ticked source builds its own predator FG \u00d7 prey FG matrix (diet/dietcalc, diet/MAISHA, diet/FishBase), all in the download and in the Diet explorer (Results). The model\u2019s matrix takes each predator FG from the first ticked source that has it: dietcalcR, then MAISHA and FishBase/SeaLifeBase (MAISHA first per species), then EcoBase and the literature table; diet_provenance_by_predator_fg.csv says which. If dietcalcR or its database is not found, the run says so in the 04 log and uses the other sources. dietcalcR (iMARES diet calculator) scores every record of the diet database by region, years (the Ecopath years), type of data and method, selects and merges the studies of each predator species and weights the species of an FG by their biomass in the model (01). Its diets replace the pipeline\u2019s for the predator FGs it covers; the pipeline fills the rest. Empty boxes = the default pCloud folders. An Access database needs the Access driver (Windows) or mdbtools on a Mac; otherwise export its tables (Diets_data, Species_data, Region, Collecting_data_codes, Diet_method) to SQLite or comma-separated csv files in one folder. Outputs: diet/dietcalc/ and diet_provenance_by_predator_fg.csv.")),

  ## 3. Years ----------------------------------------------------------------
  .card("3", "Years", "\U0001F4C5",
    fluidRow(
      column(6, sliderInput("ecopath_years", "Ecopath baseline (YEAR_ECOPATH)", min = 1990, max = 2023, value = c(1994, 1996), step = 1, sep = "", width = "100%")),
      column(6, sliderInput("ts_years", "Ecosim time series (TS_YEARS)", min = 1990, max = 2023, value = c(1995, 2023), step = 1, sep = "", width = "100%"))),
    tags$div(class = "wm-help", "Used by every script: biomass (01), catch, landings, discards and effort (02, from the first Ecopath or Ecosim year to the last Ecosim year), P/B and Q/B (03), diets (04) and the environmental drivers (05, 05b).")),

  ## 4. Biomass settings (01, 01b) ---------------------------------------------
  .card("4", "Biomass settings (01 biomass, 01b unsurveyed groups)", "\U0001F4E5",
    tags$div(class = "wm-help mb-2", "Biomass comes from the MEDITS trawl and MEDIAS acoustic surveys (2024 MEDBS data) and, for groups they cannot measure, from literature and models (01b)."),
    fluidRow(
      column(6,
        tags$h6("Survey density (MEDITS, MEDIAS)"),
        .switch("strata", "Strata-weighted density (STRATA)", "Off = simple mean density instead of the depth-stratum (area-weighted) estimator.", TRUE),
        .switch("drop_outliers", "Remove flagged outlier hauls (DROP_OUTLIERS)", "Off = outliers are only reported in survey_outliers_flagged.csv.", TRUE),
        .switch("extra_data", "Add the extra survey and assessment files (USE_EXTRA_DATA)",
                "Files in data/extra_data on pCloud fill what the standard data lack: MEDITS hauls for GSA-years missing from the JRC files (Spain GSA 1, 2 and 6 in 2022, GSA 6 in 2023), MEDIAS acoustic biomass (GSA 6 2022, GSA 7 2002) and newer GFCM STAR assessments (anchovy and sardine GSA 7). What was added: extra_medits_hauls_added.csv and star_extra_REVIEW.csv.", TRUE),
        .switch("catch_q", "Catchability correction (APPLY_CATCHABILITY)",
                "Divides MEDITS density by the species catchability q. Default table: catchability_factors_ecotrans_medits_2021_spp.csv (pCloud), from Lloret-Lloret (2023, PhD thesis, Universitat de Barcelona): Fraser et al. (2007) for fish, Fiorentino et al. (2013) for sardine and anchovy, stock assessments (STECF 2021) for hake, red mullet, deep-water rose shrimp and Norway lobster, 0.2 for cephalopods. Upload your own below (columns ScientificName, q).", TRUE),
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
             " its part of its FG is removed whatever the source (MEDITS/MEDIAS, stock assessment, acoustic series, literature: the FG value \u00d7 the species' share of the FG survey biomass, or the whole FG if it is the only species), the file's values are used in the years it gives, and in other years the species adds nothing. Ecopath B = the mean of its rows in the Ecopath years (if none, of its first three years). An FG name instead of a species replaces the whole FG."),
    fileInput("other_survey_upload", NULL, accept = ".csv", width = "100%", buttonLabel = "Upload survey biomass (csv)",
              placeholder = "species, year, biomass_t_km2 [, area_km2, reference]"),
    uiOutput("other_survey_info"),
    downloadButton("dl_other_survey_template", "Download an empty template", class = "btn-sm btn-outline-secondary"),
    tags$div(class = "wm-help mt-2", "Runs with an uploaded survey go to their own output folder (\u2026__survey-own), so the default results are kept.")),

  ## 5. Environment (05) and Ecospace maps (05b) ---------------------------------
  .card("5", "Environment and Ecospace settings (05 environment, 05b Ecospace maps)", "\U0001F30A",
    tags$div(class = "wm-help mb-2",
      "05 environment gets monthly temperature, salinity, NPP and phytoplankton from Copernicus Marine (Ecosim forcing and, with PHYTO_SOURCE = Copernicus in card 4, the Ecopath phytoplankton). ",
      "Its first run downloads several GB; later runs only fetch new years. It needs a Copernicus Marine account: ",
      actionLink("cm_login_link", "enter or change the login"), "."),
    tags$script(HTML("document.addEventListener('toggle', function (e) { if (e.target.open && window.jQuery) { jQuery(e.target).trigger('shown'); jQuery(window).trigger('resize'); } }, true);")),
    tags$details(class = "wm-esp",
      tags$summary(tags$strong("\u25B8 Ecospace maps (05b): settings"), tags$span(class = "wm-help ms-2",
        "click to open - monthly ASCII maps of temperature, salinity, NPP and phytoplankton on your Ecospace grid")),
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
        fluidRow(column(6, selectInput("esp_step", "Time step", c("Monthly" = "monthly", "Annual mean" = "annual"), width = "100%")),
                 column(6, selectInput("esp_scaling", "Values", c("Absolute (deg C, PSU, g C m-2 ...)" = "absolute",
                                                                  "Relative to the base year (= 1)" = "relative"), width = "100%"))),
    tags$div(class = "wm-help", "Tick \u201c05b Ecospace maps\u201d in Scripts to run (card 6, Run), or use the link \u201conly 05b\u201d there. 05 must have run once."))),
    tags$details(class = "wm-esp mt-3",
      tags$summary(tags$strong("\u25B8 Ecospace response functions (05c): settings"), tags$span(class = "wm-help ms-2",
        "click to open - response of each FG to depth, SST, SBT, salinity and NPP, from AquaMaps and GLOSSA")),
      tags$div(class = "mt-2",
    fluidRow(column(6, checkboxGroupInput("resp_src", "Sources", inline = TRUE, choices = c("AquaMaps" = "aquamaps", "GLOSSA" = "glossa"),
                                          selected = c("aquamaps", "glossa"))),
             column(6, selectInput("glossa_mode", "GLOSSA", width = "100%",
                                   c("Run GLOSSA here if installed, else prepare its inputs" = "auto", "Prepare the inputs for the GLOSSA app" = "prepare",
                                     "Run GLOSSA here (slow)" = "run", "Read the results exported from GLOSSA" = "read")))),
    tags$div(class = "wm-help", "One function per FG and driver (depth, bottom and surface temperature, salinity, NPP), 1200 values between X min and X max, maximum 1, as EwE keeps them (environment/ecospace_responses/). AquaMaps: the species\u2019 envelopes (trapezoids) weighted by their share of the FG biomass. GLOSSA (BART species distribution models, iMARES): the functional response of the FG\u2019s presence in the MEDITS hauls; \u201cPrepare\u201d writes the occurrences and fit_layers.zip for the GLOSSA app, then save GLOSSA\u2019s exported functional responses in glossa_results/ and run 05c again with \u201cRead\u201d.")))),

  ## 6. Run ----------------------------------------------------------------
  .card("6", "Run", "\u25B6\ufe0f",
    tags$div(class = "mb-2 pb-2 border-bottom",
      checkboxGroupInput("steps_chk", "Scripts to run (RUN_STEPS)", inline = TRUE,
                         choices = c("01 biomass" = 1, "02 fisheries" = 2, "03 P/B, Q/B" = 3, "04 diets" = 4, "05 environment" = 5,
                                     "05b Ecospace maps + 05c responses" = 6, "06 diagnostics" = 7),
                         selected = c(1:4, 7)),
      tags$div(class = "mb-2",
        actionLink("steps_all", "all 01-06"), " \u00b7 ", actionLink("steps_main", "01-04 + 06"), " \u00b7 ", actionLink("steps_none", "none"), " \u00b7 ",
        actionLink("steps_after1", "02-04 + 06 (keep biomass)"), " \u00b7 ", actionLink("steps_only_diag", "only 06"), " \u00b7 ", actionLink("steps_only_env", "only 05 + 05b"), " \u00b7 ", actionLink("steps_only_esp", "only 05b (new base map)")),
      tags$small(class = "text-muted d-block mb-1",
        "The scripts run in this order; 06 diagnostics runs last, so it checks the outputs of all the others. ",
        "The settings of each script are in the cards above (4 biomass, 5 environment and Ecospace); 02 fisheries, 03 and 04 have none of their own (catch and effort follow the years of card 3 and the GSAs of card 1)."),
      uiOutput("steps_check")),
    fluidRow(
      column(5,
        actionButton("run", "Run pipeline", class = "btn-primary btn-lg", icon = icon("play")),
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
  .card("7", "Results", "\U0001F4CA",
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
    tags$div(class = "wm-ewe border rounded p-2 mb-3",
      tags$strong("EwE import files"), tags$span(class = "wm-help ms-2", "the Ecopath inputs in the layout and names of EwE's own export, ready to import in EwE"),
      div(class = "d-flex flex-wrap align-items-end gap-3 mt-1",
          textInput("ewe_model", "Model name (start of the file names)", value = "Western_Mediterranean_Sea", width = "280px"),
          radioButtons("ewe_fmt", "Format", inline = TRUE, choices = c("csv files (.zip)" = "csv", "One xlsx" = "xlsx"), selected = "csv"),
          radioButtons("ewe_dec", "Decimal separator (csv)", inline = TRUE, choices = c("Comma 0,5 (columns ;)" = ",", "Point 0.5 (columns ,)" = "."), selected = ","),
          tags$div(class = "mb-3", downloadButton("dl_ewe", "Download EwE files", class = "btn-sm btn-primary"))),
      tags$div(class = "wm-help", "Tables: Basic_input, Diet_composition, Landings, Discards, Definition_of_fleets, Pedigree, Traits (e.g. Western_Mediterranean_Sea-Basic_input.csv). Use the decimal separator EwE uses on the computer that runs it (comma for Spanish, French or Italian settings; then columns are separated by \";\"). Text in double quotes, UTF-8. Multi-stanza groups (e.g. European hake juv. + adult) get their stanza row, Z as Total mortality and their own Q/B; the Stanzas file lists what EwE\u2019s multi-stanza form asks for (age at start, Z, K, Wmat/Winf, BA/B, leading stanza). Pedigree: the pipeline's confidence index (0-1) of each input, -1 when not set. The pipeline also writes these files at the end of each run, to the output folder ewe_import/.")),
    uiOutput("run_summary"),
    tags$div(class = "wm-help mb-2", tags$strong("FGs with high F"), " are only reported, never changed: F above 1, or above the highest F in the stock assessments of the FG's species. Each one gets a recommendation in high_F_REVIEW.csv (e.g. a trawl survey that hardly samples the group, missing catchability (card 4), or catch to check)."),
    tags$details(class = "card mb-3", id = "wm-haul-map-box",
      tags$summary(class = "card-header py-2", tags$strong("Survey hauls map"), " - MEDITS density in each haul, by year: zoom, move and filter"),
      tags$div(class = "card-body py-2",
        div(class = "d-flex flex-wrap align-items-end gap-3",
            selectInput("hm_fg", "Functional group", choices = NULL, width = "300px"),
            selectInput("hm_sp", "Species", choices = c("All species of the FG" = ""), width = "280px"),
            radioButtons("hm_q", "Density", inline = TRUE, choices = c("With q" = "q", "Without q (raw)" = "raw"), selected = "q"),
            checkboxInput("hm_zero", "Show hauls without it", TRUE)),
        div(class = "d-flex flex-wrap align-items-end gap-3",
            div(style = "min-width:320px; flex:1;", sliderInput("hm_year", "Years (drag both ends for a range; same year twice = one year)", min = 1994, max = 2023, value = c(1995, 1995), step = 1, sep = "", width = "100%",
                                                                 animate = animationOptions(interval = 1500, loop = FALSE))),
            checkboxInput("hm_all_years", "All years together", FALSE),
            div(style = "min-width:260px;", sliderInput("hm_depth", "Depth (m)", min = 0, max = 800, value = c(0, 800), step = 10, width = "100%")),
            selectizeInput("hm_gsa", "GSAs", choices = NULL, multiple = TRUE, width = "220px", options = list(placeholder = "all"))),
        tags$div(id = "wm-haul-map", style = "height: 520px; border-radius: 6px; border: 1px solid #c7d3dc; background: #dbe8f1;"),
        uiOutput("hm_info"),
        plotOutput("hm_trend", height = "180px"),
        tags$div(class = "wm-help", "Each circle is a haul: area ∝ density (t/km² swept), colour on one log scale for all years of the FG or species, so the years can be compared (press ▶ under the year slider to play them). Grey dots: hauls without it. Dashed lines: GSA boundaries; land from Natural Earth (shown also without internet). Click a haul for its density and its species. With q = the density used by the estimates (raw / q); without q = what the trawl caught. Outlier hauls removed by the pipeline are not shown. The line below: mean density per haul each year for the hauls in the filter, with the selected year marked. File: biomass/haul_density.rds (01)."))),
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
          column(6, tags$div(style = "position: relative;",
            plotOutput("fg_explore_plot_catch", height = "340px",
                       hover = hoverOpts("fg_explore_hover_catch", delay = 80, delayType = "throttle", nullOutside = TRUE)),
            uiOutput("fg_explore_tip_catch")))),
        uiOutput("fg_explore_info"),
        tags$div(class = "wm-help", "Band: survey sampling uncertainty of the index (index x (1 \u00b1 1.96 CV)), for the FGs measured by MEDITS. Grey: the Ecopath baseline years; their B (t/km\u00b2) is in the subtitle and below, with its source. Point the mouse at a year of the biomass plot to see the share of each species in the FG biomass that year (survey species from MEDITS and MEDIAS; in the Ecopath years, and for groups without a survey split, the Ecopath baseline composition from all sources). On the catches plot, the species in that year\u2019s landings or discards (STECF FDI from 2014 for Spain, France and Italy; else GFCM landings, with NEI groups as reported)."))),
    tags$details(class = "card mb-3", id = "wm-fleet-explorer",
      tags$summary(class = "card-header py-2", tags$strong("Fleet explorer"), " - one fleet's landings, discards and effort, or one FG's catch by fleet"),
      tags$div(class = "card-body py-2",
        div(class = "d-flex flex-wrap align-items-center gap-3",
            radioButtons("fleet_view_mode", NULL, inline = TRUE, c("One fleet: its catch by FG" = "fleet", "One FG: its catch by fleet" = "fg")),
            selectizeInput("fleet_explore", NULL, choices = NULL, multiple = TRUE, width = "520px", options = list(placeholder = "pick one or several", plugins = list("remove_button"))),
            radioButtons("fleet_view_what", NULL, inline = TRUE, c("Landings + discards" = "both", "Landings" = "L", "Discards" = "D")),
            conditionalPanel("input.fleet_view_mode == 'fg'",
              radioButtons("fleet_view_group", NULL, inline = TRUE, c("By fleet" = "fleet", "By country (all its fleets)" = "country")))),
        fluidRow(column(7, plotOutput("fleet_plot_catch", height = "460px")),
                 column(5, plotOutput("fleet_plot_effort", height = "460px"))),
        tags$div(class = "wm-help", "From the workbook of the last run (Ecopath_L and Ecopath_Di, t/km\u00b2/year in the Ecopath years) and the Ecosim effort (index, 1 = mean of the Ecopath years). Pick one or several fleets: the FGs they catch, largest first, with one colour per fleet (one fleet alone: landings and discards), and the effort of each fleet. Pick one or several FGs: the fleets that catch them (or, by country, the sum of all the fleets of each country, e.g. all Spain), one colour per FG, and the effort of the fleets that catch most."))),
    tags$details(class = "card mb-3", id = "wm-diet-explorer",
      tags$summary(class = "card-header py-2", tags$strong("Diet explorer"), " - the diet of a predator FG according to each source, or the whole matrix of one source"),
      tags$div(class = "card-body py-2",
        fluidRow(column(4, radioButtons("diet_view_mode", NULL, inline = TRUE, c("One predator, all sources" = "pred", "One source, whole matrix" = "matrix", "dietcalcR outputs" = "dc"))),
                 column(4, conditionalPanel("input.diet_view_mode == 'pred'", selectInput("diet_view_pred", "Predator FG", choices = NULL, width = "100%")),
                           conditionalPanel("input.diet_view_mode == 'matrix'", selectInput("diet_view_src1", "Source", choices = NULL, width = "100%"))),
                 column(4, conditionalPanel("input.diet_view_mode == 'pred'",
                                            checkboxGroupInput("diet_view_src", "Sources", inline = TRUE, choices = NULL)))),
        conditionalPanel("input.diet_view_mode == 'dc'",
          fluidRow(column(5, selectInput("diet_dc_what", "dietcalcR output", width = "100%",
                            c("Run summary" = "summary", "Diet pedigree by FG" = "pedigree", "Coverage of the predator FGs" = "coverage",
                              "Species matching (FG table \u2194 diet database)" = "matching", "Selected studies" = "selected",
                              "Study scores" = "study_scores", "Record scores" = "record_scores", "Merged diets of the selected studies" = "merged",
                              "EwE diet table (dietcalcR)" = "ewe_table", "FG table vs 01 biomass" = "mismatch"))),
                   column(4, selectInput("diet_dc_fg", "Functional group", choices = c("All" = ""), width = "100%")),
                   column(3, tags$div(style = "margin-top:28px", textOutput("diet_dc_n", inline = TRUE)))),
          uiOutput("diet_dc_ui")),
        conditionalPanel("input.diet_view_mode != 'dc'", uiOutput("diet_view_ui")),
        tags$div(class = "wm-help", "One predator: the share of each prey FG in its diet according to each source (prey with at least 1% in any source); \u201cModel\u201d is the matrix that goes to Ecopath, with the source it was taken from in the title. Whole matrix: predators in columns, prey in rows, darker = larger share. dietcalcR outputs: the tables dietcalcR writes in diet/dietcalc/ (run summary, diet pedigree, scores, selected studies, merged diets, its EwE diet table), with the FG of each species from the app\u2019s FG table; pick an FG to see only its species."))),
    tags$details(class = "card mb-3", id = "wm-resp-explorer",
      tags$summary(class = "card-header py-2", tags$strong("Responses explorer"), " - Ecospace response functions of the FGs (AquaMaps and GLOSSA) by driver"),
      tags$div(class = "card-body py-2",
    fluidRow(column(3, selectInput("resp_view_drv", "Driver", width = "100%",
                                   c("Depth" = "depth", "SST (surface temperature)" = "temp_surf", "SBT (bottom temperature)" = "temp_bot",
                                     "Salinity (surface)" = "sal_surf", "NPP" = "npp"))),
             column(5, selectizeInput("resp_view_fg", "Functional groups", choices = NULL, multiple = TRUE, width = "100%",
                                      options = list(placeholder = "all FGs with a function"))),
             column(4, checkboxGroupInput("resp_view_src", "Sources", inline = TRUE, choices = c("AquaMaps", "GLOSSA"), selected = c("AquaMaps", "GLOSSA")),
                       checkboxInput("resp_view_both", "Only FGs with both sources (compare)", FALSE))),
    uiOutput("resp_view_ui"),
    tags$details(class = "mt-2", open = NA, tags$summary(tags$strong("AquaMaps vs GLOSSA for this driver")),
      tags$div(class = "wm-help", "Per FG with both functions: the range where each gives at least half of its maximum capacity, the depth/temperature/salinity of each one\u2019s peak, and how much the two curves agree over their common range: overlap (Schoener\u2019s D, 0 = no overlap, 1 = the same curve, computed on the curves scaled to sum 1) and correlation."),
      tableOutput("resp_compare")),
    tags$div(class = "mt-2 mb-1",
      downloadButton("dl_resp_fig", "Download this figure (.png)", class = "btn-sm btn-outline-secondary"),
      downloadButton("dl_resp_zip", "Download the response functions (.zip of csv)", class = "btn-sm btn-outline-secondary ms-2")),
    tags$div(class = "wm-help", "Blue: AquaMaps (species envelopes weighted by biomass in the FG). Red: GLOSSA (BART functional response of the FG in the MEDITS hauls) with its 95% band. Each curve is relative habitat capacity (maximum 1) over the range EwE uses for that function (X min to X max)."))),
    tags$details(class = "card mb-3", id = "wm-esp-explorer",
      tags$summary(class = "card-header py-2", tags$strong("Ecospace maps explorer"), " - the monthly driver maps of 05b on the Ecospace base map"),
      tags$div(class = "card-body py-2",
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
        tags$div(class = "wm-help", "Press \u25B6 under the slider to play the years (or months). Grey = land (NODATA). One colour scale for all panels. The changes between years are small next to the seasonal cycle, so with \u201cValues\u201d the years look alike: \u201cDifference from the average year\u201d shows each map minus the mean of that month over all years (red = warmer / higher, blue = colder / lower). The line under the single-month map is the mean over the water cells, with the month shown marked."))),
    tags$div(class = "wm-results", uiOutput("results"))),

  ## Survey hauls map (Results): Leaflet, drawn from the server's
  ## "wm_haul_map" message; created when its box is first opened.
  tags$script(HTML(r"---(
(function () {
  var map = null, layer = null, legend = null, pending = null, fitted = false;
  var st = document.createElement('style');
  st.textContent = '.wm-gsa-lab{font:600 11px/1 system-ui,sans-serif;color:#0b4f6c;white-space:nowrap;' +
    'text-shadow:0 0 3px #fff,0 0 3px #fff;transform:translate(-50%,-50%);}';
  document.head.appendChild(st);
  var STOPS = [[68,1,84],[59,82,139],[33,145,140],[94,201,98],[253,231,37]];
  function col(t) {
    t = Math.max(0, Math.min(1, t)) * (STOPS.length - 1);
    var i = Math.min(Math.floor(t), STOPS.length - 2), f = t - i, a = STOPS[i], b = STOPS[i + 1];
    return 'rgb(' + [0,1,2].map(function (k) { return Math.round(a[k] + f * (b[k] - a[k])); }).join(',') + ')';
  }
  function fmt(v) { return v >= 100 ? v.toFixed(0) : v >= 1 ? v.toPrecision(3) : v.toPrecision(2); }
  function visible() { var b = document.getElementById('wm-haul-map-box'); return b && b.open; }
  function ensure() {
    if (map) return true;
    if (typeof L === 'undefined' || !document.getElementById('wm-haul-map') || !visible()) return false;
    map = L.map('wm-haul-map', { zoomSnap: 0.25 });
    var base = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}', {
      attribution: 'Tiles &copy; Esri - GEBCO, NOAA, Garmin', maxZoom: 10 }).addTo(map);
    var fails = 0;
    base.on('tileerror', function () { if (++fails === 3) { map.removeLayer(base);
      L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', { attribution: '&copy; OpenStreetMap contributors', maxZoom: 12 }).addTo(map); } });
    map.setView([40, 4], 5);
    // land (always drawn, also offline) and the GSA boundaries with their numbers
    map.createPane('wmLand'); map.getPane('wmLand').style.zIndex = 380;
    map.createPane('wmGsa');  map.getPane('wmGsa').style.zIndex = 390;
    try {
      var land = JSON.parse(document.getElementById('wm-land-geojson').textContent);
      if (land) L.geoJSON(land, { pane: 'wmLand', interactive: false,
        style: { color: '#7a7466', weight: 0.6, fillColor: '#e9e4d8', fillOpacity: 1 } }).addTo(map);
    } catch (e) {}
    try {
      var gsa = JSON.parse(document.getElementById('wm-gsa-geojson').textContent);
      if (gsa) {
        var gl = L.geoJSON(gsa, { pane: 'wmGsa', interactive: false,
          style: { color: '#0b4f6c', weight: 1.2, dashArray: '5 4', fill: false } }).addTo(map);
        var seen = {};
        gl.eachLayer(function (l) {
          var g = l.feature.properties.gsa_num;
          if (seen[g]) return; seen[g] = true;
          L.marker(l.getBounds().getCenter(), { interactive: false, pane: 'wmGsa',
            icon: L.divIcon({ className: 'wm-gsa-lab', html: 'GSA ' + g, iconSize: null }) }).addTo(map);
        });
      }
    } catch (e) {}
    legend = L.control({ position: 'bottomright' });
    legend.onAdd = function () { var d = L.DomUtil.create('div', 'wm-legend'); d.id = 'wm-haul-legend'; return d; };
    legend.addTo(map);
    return true;
  }
  function draw(m) {
    pending = m;
    if (!ensure()) return;
    if (layer) map.removeLayer(layer);
    layer = L.featureGroup();
    var l0 = Math.log10(m.vmin), l1 = Math.log10(m.vmax), idx = [], n = (m.lat || []).length;
    for (var i = 0; i < n; i++) idx.push(i);
    idx.sort(function (a, b) { return m.d[a] - m.d[b]; });       // big circles on top
    idx.forEach(function (i) {
      var d = m.d[i], o;
      if (!(d > 0)) {
        if (!m.zero) return;
        o = { radius: 2, stroke: false, fillColor: '#7d8b96', fillOpacity: 0.55 };
      } else {
        var t = (Math.log10(d) - l0) / ((l1 - l0) || 1);
        o = { radius: 3 + 11 * Math.sqrt(Math.min(d / m.vmax, 1)), color: '#1f2a33', weight: 0.5, fillColor: col(t), fillOpacity: 0.85 };
      }
      L.circleMarker([m.lat[i], m.lon[i]], o).bindPopup(m.tip[i]).addTo(layer);
    });
    layer.addTo(map);
    if (!fitted && n) { map.fitBounds(layer.getBounds(), { padding: [12, 12] }); fitted = true; }
    var lg = document.getElementById('wm-haul-legend');
    if (lg) lg.innerHTML = '<div style="font-weight:600;margin-bottom:3px">' + m.label + '</div>' +
      '<div style="width:170px;height:10px;border:1px solid #000;background:linear-gradient(to right,' +
      [0, .25, .5, .75, 1].map(col).join(',') + ')"></div>' +
      '<div style="display:flex;justify-content:space-between;width:172px;font-size:11px"><span>' + fmt(m.vmin) +
      '</span><span>' + fmt(Math.sqrt(m.vmin * m.vmax)) + '</span><span>' + fmt(m.vmax) + '+</span></div>' +
      (m.zero ? '<div style="margin-top:3px"><i style="background:#7d8b96;border-radius:50%;width:8px;height:8px;display:inline-block;margin-right:4px"></i>without it</div>' : '');
  }
  document.addEventListener('toggle', function (e) {
    if (e.target && e.target.id === 'wm-haul-map-box' && e.target.open)
      setTimeout(function () { if (ensure()) { map.invalidateSize(); if (pending) draw(pending); } }, 50);
  }, true);
  function reg() { Shiny.addCustomMessageHandler('wm_haul_map', draw); }
  if (window.Shiny && Shiny.addCustomMessageHandler) reg(); else $(document).on('shiny:connected', reg);
})();
)---")),
  ## Run status that stays in view (bottom right): where the Run button is,
  ## and while a run goes, which script and how long; click = go to the
  ## Run card (or to the Results when finished).
  uiOutput("run_pill"),
  tags$style(HTML("
    #run_pill { position: fixed; right: 18px; bottom: 18px; z-index: 1060; }
    .wm-pill { display: inline-flex; align-items: center; gap: .55rem; padding: .6rem 1.05rem; border-radius: 999px; font-weight: 600;
               box-shadow: 0 4px 14px rgba(16,42,67,.28); cursor: pointer; text-decoration: none !important; font-size: .98rem; }
    .wm-pill-idle { background: #0d6efd; color: #fff !important; }
    .wm-pill-running { background: #0b4f6c; color: #fff !important; }
    .wm-pill-done { background: #198754; color: #fff !important; }
    .wm-pill-error { background: #dc3545; color: #fff !important; }
    .wm-pill .spinner-border { width: 1.05rem; height: 1.05rem; border-width: .17em; }
    @keyframes wmPulse { 0% { box-shadow: 0 0 0 0 rgba(13,110,253,.7); } 70% { box-shadow: 0 0 0 18px rgba(13,110,253,0); } 100% { box-shadow: 0 0 0 0 rgba(13,110,253,0); } }
    .wm-pulse { animation: wmPulse 1.2s ease-out 3; }
  ")),
  tags$script(HTML("
    $(document).on('click', '.wm-pill', function (e) {
      e.preventDefault();
      var t = document.getElementById($(this).data('target') || 'wm-card-6');
      if (t) t.scrollIntoView({ behavior: 'smooth', block: 'start' });
      if (($(this).data('target') || 'wm-card-6') === 'wm-card-6') {
        var b = document.getElementById('run');
        if (b) { b.classList.remove('wm-pulse'); void b.offsetWidth; b.classList.add('wm-pulse'); }
      }
    });
  ")),
  tags$script(type = "application/json", id = "wm-gsa-geojson", HTML(.gsa_geojson)),
  tags$script(type = "application/json", id = "wm-land-geojson", HTML(.land_geojson)),
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
