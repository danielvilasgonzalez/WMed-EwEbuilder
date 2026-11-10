# WMed-EwEbuilder

<img src="scripts/images/wmed_ewebuilder_logo_map4.png" alt="WMed-EwEbuilder" width="560">

*Building Ecopath with Ecosim inputs for the Western Mediterranean: biomass, fisheries, P/B, Q/B, traits, diets and environmental drivers.*

WMed-EwEbuilder is an R pipeline that turns survey, fisheries, literature and ocean-model data into Ecopath with Ecosim (EwE) inputs for the Western Mediterranean (GSAs 1–12: GSAs 1–11 plus GSA 12, Northern Tunisia, as in the MSFD Western Mediterranean subregion), or for any region inside it. It writes one workbook, `ecopath_ecosim_inputs.xlsx`, ready to import into EwE, plus a CSV and a plot for every step, a validation report and a record of each run.

Created by: Daniel Vilas (iMARES, ICM-CSIC)

**Contents:** [What you need](#what-you-need) · [Quick start](#quick-start) · [Interactive page](#interactive-page) · [Pipeline steps](#pipeline-steps) · [Main options](#main-options) · [First run](#first-run-downloads-and-time) · [Outputs](#outputs-under-out_dir) · [Reproducibility](#reproducibility) · [Troubleshooting](#troubleshooting) · [Repository structure](#repository-structure) · [Known limitations](#known-limitations) · [How to cite](#how-to-cite)

## What you need

- **This repository**: the code.
- **The shared pCloud folder "EwE Western Med 2026"**: the input data. Ask Daniel Vilas for access, and install pCloud Drive so the folder appears as a drive. Without access, a frozen copy of the inputs that can be shared may be deposited on Zenodo (see [Reproducibility](#reproducibility)).
- **R** (≥ 4.3) and **RStudio**.
- **Quarto** (for the interactive page; it comes with recent RStudio versions).
- **Internet** on the first run: several sources are downloaded once and cached.
- **A free Copernicus Marine account**, only for step 05 (environmental drivers): <https://data.marine.copernicus.eu/register>.

## Quick start

1. Clone the repo and open `WMed_EwE.Rproj` in RStudio.
2. Install the packages (once, then restart R):
   ```r
   source("scripts/install_packages.R")
   ```
3. Either open the interactive page (recommended, see below), or run the pipeline from the console:
   ```r
   setwd("scripts")
   source("run_pipeline_demo.R")
   ```

You don't need to edit any paths. `scripts/wmed_paths.R` finds the three folders on each computer:

| Folder | What it is | How it is found (first one that exists wins) |
|---|---|---|
| `git_dir` | this repo | the folder the scripts are in |
| `pcloud_dir` | the shared data folder | `config.local.R` → `config.R` → environment variable `WMED_PCLOUD_DIR` → the standard pCloud Drive place (macOS `~/pCloud Drive/EwE Western Med 2026`, Windows `P:/EwE Western Med 2026`, Linux `~/pCloudDrive/EwE Western Med 2026`) |
| `out_dir` | where the outputs go (your own computer) | `config.local.R` → `config.R` → `WMED_OUT_DIR` → `~/WMed_EwE_output` (created if missing) |

**Local copy of the data.** The pipeline never reads the pCloud folder directly. The first run (or *Make local copy of the data* on the page, Settings) copies its `data/` folder (~2.5 GB) to `~/EwE Western Med 2026`, and `pcloud_dir` then points to that copy, so a run cannot change the shared data. *Update local data* copies only new or changed files. Another place: `local_data_dir` in `config.local.R` or the environment variable `WMED_LOCAL_DATA_DIR`.

If the pCloud folder is somewhere else, the page (card 0) or RStudio asks you to pick it once and saves your answer to `config.local.R`. To set the folders by hand, copy `config.R.example` to `config.local.R` and edit it. `config.R` and `config.local.R` are git-ignored, so your paths never reach other users.

## Interactive page

`scripts/WMed-EwEbuilder.qmd` is a Quarto/Shiny page to set up, run and check the pipeline without editing code. It also documents every method, equation, assumption and source.

**Start it:**

> **Render the page, do not run it.** Open it with **Run Document** / **Render** (or `quarto serve`). Running its code chunks one by one (Run, Ctrl/Cmd+Enter, Run All) gives errors in the R session: most chunks are documentation and need objects from a pipeline run. To run only the pipeline: `source("scripts/run_pipeline_demo.R")`.

- RStudio: open `scripts/WMed-EwEbuilder.qmd`, switch the editor to **Source** mode (not Visual: it rewraps the file), and click **Run Document**. Click *Show in new window* to open it in your browser.
- Terminal:
  ```bash
  cd scripts
  quarto serve WMed-EwEbuilder.qmd
  ```
  then open the `http://localhost:...` link it prints. Ctrl+C stops it.

On a new computer only `rmarkdown`, `knitr` and `shiny` must be installed before the page can start (`install.packages(c("rmarkdown", "knitr", "shiny"))`); the page installs the rest. A plain HTML file opened from disk shows the page but cannot run R.

**On the page:**

| Card | What you do |
|---|---|
| 0 Folders | Check or change the three folders. **Check setup** tests the folders, FG table, packages, internet and the Copernicus login, and says what to fix. |
| 1 Model area | Click GSAs on the map, or a custom region (upload a shapefile, GeoJSON, GeoPackage or KML, or draw a lon/lat box). |
| 2 Food web | Pick or upload the species → FG table (`FG_number`, `FG_name`, `species`; optional `FG_group`). The default is `FG_WMed_2026.csv` (80 FGs). Below it, the biomass of each species of the last run, with and without catchability (B with q, B without q, B proportion in the FG with and without q), and a download of the FG table with these columns. |
| 3 Years | Ecopath baseline and Ecosim years, used by every script. Catch and effort years follow them. |
| 4 Biomass settings (01, 01b) | Survey estimator, outliers, catchability (and your own q table), filling missing Ecopath B, Ecosim normalisation, phytoplankton source, and the upload of biomass from any survey. |
| 5 Environment and Ecospace (05, 05b) | Your Copernicus login for step 05, and (collapsed) the Ecospace base map, resolution, map viewer and downloads for 05b. |
| 6 Run | Tick the scripts to run (01–06; they run in order and 06 diagnostics runs last), then **Run pipeline** / **Stop**, with progress and time left per step. The *Settings block* shows the same run as R code: copy or download it, and **Load settings (.R)** fills the page again from a downloaded file (the file is read, never run). |
| 7 Results | Download the workbook, diet matrices, review tables, all tables or the logs. A summary of the last run (errors, warnings, FGs with F > 1, FGs without Ecopath B). FGs with high F are only reported, with a recommendation each (high_F_REVIEW.csv). The **FG explorer** (one group's Ecosim biomass with the survey uncertainty band and the same series without catchability q, and its catches; point at a year to see the species shares), the key plots and every plot, with zoom. |

The floating **Run / Results / Methods** bar jumps around the page; **Hide methods** hides the documentation while you work.

## Pipeline steps

Each step reads files the earlier ones wrote. `run_pipeline_demo.R` runs the steps in `RUN_STEPS` in this order: 1–4 = 01–04, 5 = 05 environment, 6 = 05b Ecospace maps, 7 = 06 diagnostics (always last). Default `c(1:4, 7)`; 5 and 6 are optional.

| Step | Script | What it does |
|---|---|---|
| 1 | `01_biomass.R` (sources `01b_biomass_unsurveyed.R`) | **Biomass** from the MEDITS bottom-trawl and MEDIAS acoustic surveys, strata-weighted, with the sampling CV of every index. AquaMaps depth pseudo-strata cover 0–10 m, 800–1000 m and 1000 m to the seafloor (3,785 m). Stock assessments for assessed single-species FGs and hake stanzas. Small pelagics back-cast to 1994–96. Writes Ecopath_B, the Ecosim biomass series, their weights from the survey CV, and FG_spp. |
| 1b | `01b_biomass_unsurveyed.R` | **Groups the surveys can't measure.** Seagrass, macroalgae and corals: density × mapped area × occupancy. Plankton: ISIMIP COBALT model, or Copernicus from step 05 (`PHYTO_SOURCE`). Detritus: Pauly et al. 1993. Macrozooplankton: MAREDAT. Cetaceans, seabirds, turtles and monk seal: survey density × body mass. Bluefin tuna and swordfish: ICCAT. Some FGs are left for Ecopath to estimate (EE = 0.95). |
| 2 | `02_fisheries.R` | **Catch, landings, discards, effort and F** by country, fleet and FG. Catch from GFCM, STECF FDI, Sea Around Us and ICCAT; effort from FishMIP and FDI, with technology creep. F by country, to find where a high F comes from. |
| 3 | `03_pbqb-traits.R` | **P/B and Q/B** per FG from FishBase/SeaLifeBase traits and empirical equations, plus EcoBase. |
| 4 | `04_diets.R` | **Diet matrix.** Sources in order: Mediterranean metaweb → MAISHA → FishBase/SeaLifeBase → EcoBase → literature fallback. FG order follows the FG table. |
| 6 | `05_environment.R` (optional) | **Environmental drivers** from Copernicus Marine (Mediterranean reanalyses; global model scaled to the Med for 1994–98): bottom and surface temperature, surface salinity, NPP. Monthly maps on the native grid, time series with the 10–90 % range over the area, Ecosim forcing series, and phytoplankton B and P/B for Ecopath. |
| 7 | `05b_ecospace_drivers.R` (optional) | **Ecospace driver maps**: the monthly maps of 05 regridded onto your Ecospace depth `.asc` (any resolution or projection), coastal gaps filled, one `.asc` per variable and month plus an index file. No download; rerun alone for a new base map. |
| 5 | `06_diagnostics.R` | **Validation.** Compares with the 1995 Western Med model and runs the EwE plausibility checks (P/Q, diet sums, cannibalism, PREBAL, F). Writes one summary table and one PDF of all plots. |

Every method, equation, assumption and reference is in `scripts/WMed-EwEbuilder.qmd`.

## Main options

Set these on the page, or at the top of `scripts/run_pipeline_demo.R` (every option has a comment there).

| Option | Default | Meaning |
|---|---|---|
| `RUN_STEPS` | `c(1:4, 7)` | Scripts to run, e.g. `7` (only redraw the validation), `c(2:4, 7)` (keep the biomass), `5` (only the environment), `6` (only the Ecospace maps). |
| `MODEL_GSAS` | `1:12` | GSAs in the model area. A subset filters area, survey data and catch countries together. |
| `MODEL_AREA_BASIS` | `"whole_gsa"` | `"whole_gsa"` = coast to bottom; `"medits_10_800"` = MEDITS 10–800 m strata only. |
| `AREA_MODE` | `"westmed"` | `"custom"` with `AREA_NAME` and `CUSTOM_AREA_TYPE = "shapefile"` (`CUSTOM_SHAPEFILE_PATH`) or `"bbox"` (`CUSTOM_BBOX`). Runs all steps for that region; outputs in `out_dir/<AREA_NAME>/`; catch from the GSA(s) it overlaps. |
| `YEAR_ECOPATH` / `TS_YEARS` | `1994:1996` / `1995:2023` | Ecopath baseline / Ecosim years. |
| `ECOPATH_B_FILL_MODE`, `ECOPATH_B_FILL_YEARS` | `"window_mean"`, `1995:2000` | FGs with no biomass in the Ecopath years take this mean; "Expanding" FGs get 0.00001. |
| `HIGH_F_B_FIX` | `"none"` | FGs with F > 1: `"none"` reports them (`high_F_REVIEW.csv`); `"min_from_catch"` raises B to catch / 1; `"ecopath_estimate"` leaves B to Ecopath (EE = 0.95). |
| `PHYTO_SOURCE` | `"isimip"` | Phytoplankton B and P/B: `"isimip"` (COBALT 1995, EcoBase P/B) or `"copernicus"` (from step 05; run 05 first). |
| `STOCK_ASSESSMENT_SOURCE` | `"GFCM"` | `"GFCM"` = the newest GFCM Stock Assessment Forms, downloaded and read automatically (check `biomass/gfcm_saf_parse_REVIEW.csv`); `"RAM"` = RAM Legacy. |
| `STRATA`, `DROP_OUTLIERS`, `NORMALIZE_TS` | `TRUE` | Strata-weighted density; remove flagged outlier hauls; Ecosim series as a relative index. |
| `APPLY_CATCHABILITY` | `TRUE` | Divide MEDITS density by the species catchability (default table in pCloud, or your own `CATCHABILITY_FILE`). |
| `FG_SPECIES_FILE` | `data/FG_WMed_2026.csv` | Species → FG table. |
| `PLOT_DPI` | `300` | Resolution of every PNG. |

## First run: downloads and time

Steps 01–06 take about 10–30 minutes. The first run on a new computer can take up to about 70 minutes, because some sources are downloaded once and cached:

| What | Size | Used by | Notes |
|---|---|---|---|
| AquaMaps database (`aquamapsdata`) | ~2 GB download, ~10 GB unpacked | 01, 01b | depth suitability for the pseudo-strata and habitat depth |
| Bathymetry (EMODnet, NOAA ETOPO as fallback) | small, per area | 01, 01b | stratum and habitat depths |
| FishBase / SeaLifeBase (`rfishbase`) | small | 01, 03, 04 | taxonomy, traits, diets |
| ISIMIP3a COBALT plankton fields | hundreds of MB | 01b | plankton and primary production |
| EUSeaMap 2025 geodatabase | > 1 GB | 01b | put it in `data/Complementary data/EUSeaMap_2025/` |
| GFCM Stock Assessment Forms (PDF) | ~1 MB each, ~70 stocks | 01b, 02 | cached in `data/fisheries/GFCM_SAF/pdf/` |
| Copernicus Marine | several GB | 05 | the first 05 run can take a few hours; later runs only fetch new years (~15 min) |

The GitHub-only packages (`rfishbase`, `duckdbfs`, `FishLife`) are installed by `install_packages.R`.

**Copernicus login.** Your own login is asked for on the page (card 5) and saved by the `copernicusmarine` toolbox in your user folder (`~/.copernicusmarine`), or set it in your own `~/.Renviron`:
```
COPERNICUSMARINE_SERVICE_USERNAME=your_user
COPERNICUSMARINE_SERVICE_PASSWORD=your_password
```
Never write the login in the code or in `config.R`.

## Data in the pCloud folder

The main inputs, under `EwE Western Med 2026/data/`:

- `FG_WMed_2026.csv`: species → functional groups (80 FGs).
- `Medits_Medias_JRC2026/`: MEDITS and MEDIAS survey data.
- `fisheries/`: GFCM, FDI, Sea Around Us, FishMIP, ICCAT, RousseauEtAl2023, STAR_RAMLegacy, GFCM_SAF.
- `Complementary data/`: reference tables with a citation on every row (habitat biomass, megafauna, unit conversions, diet tables, literature fallbacks).
- `diet/`: metaweb and MAISHA files.
- `Bathymetry/`: `depth_wmed.asc`, the model grid used for the environmental maps.
- `WMed_EwE.xlsx`: the 1995 Western Med model, used for validation.
- `data_sources.csv` (optional): source, citation, years, licence and URL of each input file (columns `file,source,citation,years,licence,url`). Each run joins it to `data_catalogue.csv`.

## Outputs (under `out_dir`)

```
ecopath_ecosim_inputs.xlsx      # the EwE workbook: info, FGs, Fleets, FG_spp, Ecopath_B, Ecopath_L,
                                #   Ecopath_Di, Ecopath_PBQB, Ecopath_traits, Ecopath_diet, Ecosim_ts,
                                #   FG_References, Ecobase
biomass/ fisheries/ pbqb-traits/ diet/ validation/ environment/
                                # CSVs of each step; *_REVIEW.csv tables are the ones to check
plots/                          # PNGs by step
<date>_0N_<step>_log.txt        # one log per script and run
run_record_<time>.json          # settings, minutes per step, git commit, R and package versions, input files
data_catalogue.csv              # every input file used: size, date, md5, scripts, source and licence
previous_runs/<time>/           # the workbook, logs and validation summary of the last 5 runs
```

## Reproducibility

- **Run record.** Each run writes `run_record_<time>.json` and adds the git commit and R version to the workbook's `info` sheet, so any workbook can be traced to its code, settings and inputs.
- **Previous runs.** Before each run, the last results are copied to `previous_runs/` (5 kept), so two runs can be compared.
- **Package versions.** `scripts/package_versions.csv` holds the versions of a computer where the pipeline runs well; `install_packages.R` warns when yours are older. Update it with:
  ```r
  setwd("scripts"); source("lib_run_record.R"); wmed_save_package_versions(normalizePath(".."))
  ```
- **Input snapshot.** `source("scripts/make_input_snapshot.R")` packs the inputs the scripts use, with `data_catalogue.csv` and a README, into `out_dir/WMed_EwE_inputs_<date>.zip`, to deposit on Zenodo. Check each source's licence first: survey data from the MEDITS data call and some catch data may not be redistributable. Leave those out with `SNAPSHOT_EXCLUDE <- c("TA.csv", "TB.csv", "TC.csv", ...)` and cite where to request them.
- **Checks before committing.** `source("scripts/check_pipeline.R")` (from `scripts/`) checks that every R file and the page parse, that every sourced script exists, and that no home folders, e-mails or passwords are in the code. `QUICK_RUN <- TRUE` first adds a short real run over a small box in GSA 6 (~10 min).

## Troubleshooting

| Problem | What to do |
|---|---|
| The page is grey or a list on it is empty | Close the `.qmd` tab in RStudio with **Don't Save** (RStudio may save an old copy over the file), reopen it in **Source** mode and click Run Document again. |
| "Folders not found" | Card 0 of the page, or copy `config.R.example` to `config.local.R` and set the paths. |
| Errors after running chunks of the `.qmd` in the console | The page must be rendered (Run Document), not run chunk by chunk. Restart R and click Run Document. |
| A step failed | Results → **Logs of the last run (.zip)**, or the `<date>_0N_..._log.txt` files in `out_dir`. The run summary on the page shows the first error of each script. |
| 05 downloads nothing | Card 0 → **Check setup** (Copernicus tool and login), then card 5 → *enter or change the login*. |
| Packages older than the reference | `source("scripts/install_packages.R")` and restart R. |
| The input snapshot or first run is slow | The first run copies the shared data to the local copy (~2.5 GB); later runs read the local copy. |

## Repository structure

```
scripts/
  WMed-EwEbuilder.qmd              # interactive page + full method documentation
  app_ui.R  app_server.R           # the page's controls and server code
  run_pipeline_demo.R              # runs the steps; all options
  wmed_paths.R                     # finds git_dir, pcloud_dir, out_dir on any computer
  install_packages.R  package_versions.csv
  01_biomass.R  01b_biomass_unsurveyed.R  02_fisheries.R  03_pbqb-traits.R  04_diets.R
  06_diagnostics.R  05_environment.R  05b_ecospace_drivers.R
  03b_ecobase.R                    # EcoBase fallbacks
  lib_*.R                          # shared functions (survey density, AquaMaps, FishBase diets,
                                   #   plankton, Copernicus, taxonomy, run record)
  check_pipeline.R                 # checks before committing or sharing
  make_input_snapshot.R            # zip of the inputs for Zenodo
  references.bib  ieee.csl         # every reference of the pipeline
  gsa_boundaries_westmed.geojson   # GSA 1-12 polygons for the page map
  example_regions/                 # example custom region (cetacean migration corridor MPA)
  leaflet/  images/                # map library and logos of the page
  additional/                      # one-off helpers, not run by the pipeline
config.R.example                   # template for config.local.R (your folders; git-ignored)
ewe_infography.jpg                 # workflow figure
ewe_infography_references.md       # references by block of the workflow figure
```

## Known limitations

- The survey density of GSAs 1, 2 and 5–11 is applied to the unsurveyed GSAs 3, 4 and 12.
- Several groups rest on literature values from one site or on surveys from 2011–2019 (flagged as temporal mismatches in the review tables).
- Some groups the trawl survey hardly sees (coastal, rocky-bottom, pelagic) get F > 1; see the *F by country* figure and the `HIGH_F_B_FIX` option.
- The GFCM fleet register file is not parsed correctly yet. The vessel-based fleet split for Morocco, Algeria and Tunisia falls back to the GFCM fleet taxonomy or Sea Around Us.
- The literature diet fallback for corals and gorgonians, meso- and microzooplankton and suprabenthos has no proportions yet.
- Package versions are recorded and checked, not pinned (no `renv` lockfile).

See the "Known limitations" section of the interactive page for the full list.

## How to cite

Cite the code with the metadata in `CITATION.cff` (GitHub shows it under **Cite this repository**), and the data sources listed in `data_catalogue.csv` and on the interactive page.

## Contact

Daniel Vilas, iMARES group, Institut de Ciències del Mar (ICM-CSIC), Barcelona. Questions and bugs: open an issue on GitHub.
