# WMed-EwEbuilder

<img src="scripts/images/wmed_ewebuilder_logo_map4.png" alt="WMed-EwEbuilder" width="560">

*Building Ecopath with Ecosim inputs for the Western Mediterranean: biomass, fisheries, P/B, Q/B, traits, diets and environmental drivers.*

WMed-EwEbuilder is an R pipeline that turns survey, fisheries, literature and ocean-model data into Ecopath with Ecosim (EwE) inputs for the Western Mediterranean (GSAs 1–12: GSAs 1–11 plus GSA 12, Northern Tunisia, as in the MSFD Western Mediterranean subregion), or for any region inside it. It writes one workbook, `ecopath_ecosim_inputs.xlsx`, ready to import into EwE, plus a CSV and a plot for every step, a validation report and a record of each run.

Created by: Daniel Vilas (iMARES, ICM-CSIC)

**Contents:** [What you need](#what-you-need) · [How to run it](#how-to-run-it) · [Interactive page](#interactive-page) · [Pipeline steps](#pipeline-steps) · [Main options](#main-options) · [First run](#first-run-downloads-and-time) · [Outputs](#outputs-under-out_dir) · [Reproducibility](#reproducibility) · [Troubleshooting](#troubleshooting) · [Repository structure](#repository-structure) · [Known limitations](#known-limitations) · [How to cite](#how-to-cite)

## What you need

- **This repository**: the code.
- **The shared pCloud folder "EwE Western Med 2026"**: the input data. Ask Daniel Vilas for access, and install pCloud Drive so the folder appears as a drive. Without access, a frozen copy of the inputs that can be shared may be deposited on Zenodo (see [Reproducibility](#reproducibility)).
- **R** (≥ 4.3) and **RStudio**.
- **Quarto** (for the interactive page; it comes with recent RStudio versions).
- **Internet** on the first run: several sources are downloaded once and cached.
- **A free Copernicus Marine account** (<https://data.marine.copernicus.eu/register>): for step 05 (environmental drivers), and by default also for the phytoplankton B and P/B (`PHYTO_SOURCE = "copernicus"`).

## How to run it

### 1. Set up a computer (once)

1. Install **R** (≥ 4.3) and **RStudio** (it includes Quarto).
2. Get access to the shared pCloud folder **"EwE Western Med 2026"** and install **pCloud Drive**. Wait until the folder shows up as a drive.
3. Get the code and open the project:
   ```bash
   git clone https://github.com/danielvilasgonzalez/WMed-EwEbuilder.git
   ```
   then double-click `WMed_EwE.Rproj` to open it in RStudio.
4. Install the packages, then restart R (Session → Restart R):
   ```r
   source("scripts/install_packages.R")
   ```
5. Make a free Copernicus Marine account (<https://data.marine.copernicus.eu/register>). It is needed for step 05 and, by default, for the phytoplankton B and P/B (MedBFM, `PHYTO_SOURCE = "copernicus"`; step 05 then runs before 01 if its outputs are missing). You give the login on the page (card 5); never write it in the code.

### 2a. Run it from the interactive page (recommended)

1. In RStudio open `scripts/WMed-EwEbuilder.qmd`, switch the editor to **Source** (not Visual) and click **Run Document**. Click *Show in new window* to use it in your browser. Keep RStudio open while it runs.
2. Go through the cards from top to bottom. The defaults reproduce the standard Western Med model, so on a first run you only need cards 0, 5 (Copernicus login) and 6:

   | Card | What to do |
   |---|---|
   | 0 Folders | Check the three folders and click **Check setup**; fix anything it reports. |
   | 1 Model area | Click the GSAs, or give a custom region. |
   | 2 Food web | Keep `FG_WMed_2026.csv` or upload your own species → FG table; tick the diet sources. |
   | 3 Years | Ecopath baseline (default 1994–1996) and Ecosim years (1995–2023). |
   | 4 Biomass | Survey and catchability options (defaults are fine). |
   | 5 Environment and Ecospace | Copernicus login (needed for 05 and, by default, for the phytoplankton B and P/B), and for 05b/05c the Ecospace base map and cell size and the climate scenarios (none by default). |
   | 6 Run | Tick the scripts (default 01–04 and 06; tick 05 for the environment, 05b + 05c for Ecospace maps and response functions) and click **Run pipeline**. The bar shows each step and the time left; **Stop** ends the run. |
   | 7 Results | Check the run summary, open the explorers, and download the workbook or the **EwE import files**. |

3. The first run copies the data from pCloud to your computer (~2.5 GB) and downloads some sources once (see [First run](#first-run-downloads-and-time)), so it is slower; later runs take 10–30 minutes for 01–04 and 06.

### 2b. Run it from the R console

From the project folder:
```r
setwd("scripts")
source("run_pipeline_demo.R")   # steps 01-04 and 06, default settings
```

To change settings, set them **before** `source()` together with `CLEAN_START <- FALSE` (otherwise the workspace, and your settings with it, is cleared at the start; only `RUN_STEPS` is always kept):
```r
setwd("scripts")
CLEAN_START  <- FALSE
RUN_STEPS    <- c(1:4, 7)        # 1-4 = 01-04, 5 = 05, 6 = 05b + 05c, 7 = 06 (always last)
MODEL_GSAS   <- c(1, 5, 6)       # a subset of GSAs
YEAR_ECOPATH <- 1994:1996
TS_YEARS     <- 1995:2023
source("run_pipeline_demo.R")
```
Every option is listed, with its default, at the top of `run_pipeline_demo.R` and in [Main options](#main-options).

**Common runs**

| I want to... | Set before `source("run_pipeline_demo.R")` |
|---|---|
| redo only the validation plots | `RUN_STEPS <- 7` |
| keep the biomass and redo fisheries, P/B-Q/B, diets | `RUN_STEPS <- c(2:4, 7)` |
| the environmental drivers and Ecospace maps | `RUN_STEPS <- c(5, 6)` (Copernicus login needed; the first 05 run can take hours, later ones ~15 min) |
| add climate scenarios | `CLEAN_START <- FALSE; ENV_SCENARIOS <- c("ssp126", "ssp370", "ssp585"); RUN_STEPS <- c(5, 6)` |
| Ecospace maps on another grid | `CLEAN_START <- FALSE; ECOSPACE_DEPTH_FILE <- "my_depth.asc"` (or `ECOSPACE_CELL_SIZE <- 1/12` for a 5′ grid); `RUN_STEPS <- 6` |
| a custom region | `CLEAN_START <- FALSE; AREA_MODE <- "custom"; AREA_NAME <- "my_area"; CUSTOM_BBOX <- c(xmin = 2, xmax = 8, ymin = 38, ymax = 42)` (or `CUSTOM_AREA_TYPE <- "shapefile"; CUSTOM_SHAPEFILE_PATH <- "..."`) |

### 3. Check the results

When the run ends, the console prints `Pipeline finished in ... min. Workbook: ...`. In the output folder (`out_dir`, see below):

1. **`validation/validation_summary_ALL.csv`** and **`plots/validation/validation_plots_ALL.pdf`**: the checks of every step (P/Q, diet sums, cannibalism, PREBAL, F, comparison with the 1995 model). Start here.
2. **`*_REVIEW.csv`** in each step's folder: what needs a decision (e.g. `fisheries/high_F_REVIEW.csv`, `diet/diet_still_missing_REVIEW.csv`).
3. **Logs**: `<date>_0N_<step>_log.txt`, one per script; the page's run summary shows the first error of each script.
4. **`ecopath_ecosim_inputs.xlsx`**: all the inputs in one workbook.
5. **`ewe_import/`**: the same inputs in the layout and names of EwE's own export files (Basic_input, Diet_composition, Landings, Discards, Definition_of_fleets, Off_vessel_price, Discard_fate, Pedigree, Traits, Stanzas), ready to bring into EwE. `EWE_DECIMAL` (default `","`) and `EWE_EXPORT_FORMAT` (`"csv"`, `"xlsx"` or `"both"`) match them to your EwE settings.

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
| 4 Biomass settings (01, 01b) | Survey estimator, outliers, catchability (and your own q table), filling missing Ecopath B, Ecosim normalisation, phytoplankton source, and the upload of biomass from any survey (per row: species or FG, year, biomass, and `overwrite` = replace that year or only fill a year without other biomass). |
| 5 Environment and Ecospace (05, 05b, 05c) | Your Copernicus login for step 05, and (collapsed) the Ecospace base map, resolution, map viewer and downloads for 05b, and the Ecospace response functions of 05c (AquaMaps). |
| 6 Run | Tick the scripts to run (01–06; they run in order and 06 diagnostics runs last), then **Run pipeline** / **Stop**, with progress and time left per step. The *Settings block* shows the same run as R code: copy or download it, and **Load settings (.R)** fills the page again from a downloaded file (the file is read, never run). |
| 7 Results | Download the workbook, diet matrices, review tables, all tables or the logs, and the **EwE import files** (Basic_input, Diet_composition, Landings, Discards, Definition_of_fleets, Off_vessel_price, Discard_fate, Pedigree, Traits in the layout and names of EwE's own export, plus Stanzas with the multi-stanza inputs; csv files or one xlsx, decimal comma (columns ";") or point (columns ","), text in double quotes, UTF-8; also written to `ewe_import/` at the end of each run). A summary of the last run (errors, warnings, FGs with F > 1, FGs without Ecopath B). FGs with high F are only reported, with a recommendation each (high_F_REVIEW.csv). Explorers, each opening on click: **Survey hauls explorer**, **Fleet explorer** (a fleet's landings, discards and effort, or an FG's catch by fleet), **Diet explorer** (a predator's diet by source, or one source's matrix), **Ecospace maps explorer** (environmental drivers by month, historical or climate scenario, with the map-mean series; habitat drivers: depth and EUSeaMap habitats), **Responses explorer** (AquaMaps response functions over the driver values of the Ecospace maps), and the **FG explorer** (one group's Ecosim biomass with the survey uncertainty band and the same series without catchability q, and its catches; point at a year to see the species shares), the key plots and every plot, with zoom. |

The floating **Run / Results / Methods** bar jumps around the page; **Hide methods** hides the documentation while you work.

## Pipeline steps

Each step reads files the earlier ones wrote. `run_pipeline_demo.R` runs the steps in `RUN_STEPS` in this order: 1–4 = 01–04, 5 = 05 environment, 6 = 05b Ecospace maps + 05c response functions, 7 = 06 diagnostics (always last). Default `c(1:4, 7)`; 5 and 6 are optional.

| Step | Script | What it does |
|---|---|---|
| 1 | `01_biomass.R` (sources `01b_biomass_unsurveyed.R`) | **Biomass** from the MEDITS bottom-trawl and MEDIAS acoustic surveys, strata-weighted, with the sampling CV of every index. MEDITS densities are divided by the species catchability q (a species without its own q takes the mean q of its FG; q = 1 outside MEDITS). Gaps in the JRC survey files are filled from `data/extra_data` (`USE_EXTRA_DATA`). AquaMaps depth pseudo-strata cover 0–10 m, 800–1000 m and 1000 m to the seafloor (3,785 m). Stock assessments for assessed single-species FGs and hake stanzas. Small pelagics back-cast to 1994–96. Writes Ecopath_B, the Ecosim biomass series, their weights from the survey CV, and FG_spp. |
| 1b | `01b_biomass_unsurveyed.R` | **Groups the surveys can't measure.** Seagrass, macroalgae and corals: density × mapped area × occupancy. Phytoplankton: Copernicus MedBFM from step 05 (`PHYTO_SOURCE`, default), the same source as the Ecosim NPP forcing and the Ecospace maps; zooplankton: ISIMIP COBALT model. Detritus: Pauly et al. 1993. Macrozooplankton: MAREDAT. Cetaceans, seabirds, turtles and monk seal: survey density × body mass. Bluefin tuna and swordfish: ICCAT. Some FGs are left for Ecopath to estimate (EE = 0.95). |
| 2 | `02_fisheries.R` | **Catch, landings, discards, effort and F** by country, fleet and FG. Catch from GFCM, STECF FDI, Sea Around Us and ICCAT; effort from FishMIP and FDI, with technology creep and cleaning of gaps and of years with an unusual share of unclassified effort (`effort_cleaning_REVIEW.csv`). Landings and discards by the model's 35 EwE fleets (29 commercial, country × gear, and one recreational fleet per country; `EWE_FLEETS`); catch with no gear is shared among the country's fleets (`fleet_*_REVIEW.csv`). Recreational catch = commercial catch × the Sea Around Us recreational:commercial ratio of the country and FG, every year (assumption; `recreational_share_by_country_REVIEW.csv`); it is part of `Catches_Ecopath`/`Catches_Ecosim` and, for the EU countries, of the Ecosim catch series. Ecosim effort for the same fleets (`effort_to_ewe_fleet_REVIEW.csv`). Off-vessel prices from the FDI landing values (`Ecopath_price`, `offvessel_price_REVIEW.csv`) and fleet cost shares by gear from the STECF Annual Economic Reports (`data/fisheries/fleet_costs_ewe.csv`). F by country, to find where a high F comes from; FGs with F > 1 are reported, not changed (`high_F_REVIEW.csv`). |
| 3 | `03_pbqb-traits.R` | **P/B and Q/B** per FG from FishBase/SeaLifeBase traits and empirical equations, plus EcoBase. Multi-stanza groups (hake): P/B = Z of each stanza, Q/B of each stanza from von Bertalanffy growth, and the stanza inputs (age at start, Z, K, Wmat/Winf, BA/B) in `Ecopath_multistanza`. |
| 4 | `04_diets.R` | **Diet matrix.** Sources in order: dietcalcR (the iMARES diet calculator on the Mediterranean diet database; `DIET_ENGINE = "dietcalc"`, `lib_dietcalc.R`) → MAISHA → FishBase/SeaLifeBase → EcoBase (no manual diet entries). FG order follows the FG table. |
| 5 | `05_environment.R` (runs before 01 when the phytoplankton comes from it, `PHYTO_SOURCE = "copernicus"`) | **Environmental drivers** from Copernicus Marine (MedPHY and MedBFM reanalyses; for 1994–98, PISCES global scaled to MedBFM): bottom and surface temperature, surface salinity, NPP and phytoplankton carbon, NPP and carbon integrated over the water column (surface to seafloor, at most 200 m). Large and Small phytoplankton: the carbon × the satellite microphytoplankton share per cell and calendar month (2002–2004 climatology, applied to every year back to 1994). Monthly maps on the native grid, time series with the 10–90 % range over the area, Ecosim forcing series, and the Ecopath phytoplankton B (mean of the Ecopath years) and P/B (NPP / carbon). With `ENV_SCENARIOS`, `05s_environment_scenarios.R` continues the drivers to `ENV_SCEN_END` under climate scenarios: the ISIMIP3b GFDL-ESM4 and IPSL-CM6A-LR change, bias-adjusted to the Copernicus hindcast per map cell and calendar month (mean, year-to-year spread, and a join so each scenario starts from the last historical year; `environment_scenarios_continuity.csv`). Monthly maps and Ecosim forcing per scenario in `environment/scenarios/`, and 05b makes their Ecospace maps. |
| 6 | `05b_ecospace_drivers.R` (optional) | **Ecospace driver maps**: the monthly maps of 05 (historical and each scenario) regridded onto your Ecospace depth `.asc` (any resolution or projection), coastal gaps filled, one `.asc` per variable and month plus an index file. **Habitat drivers** (static): depth and the share of each cell covered by seagrass, coralligenous, rock, other biogenic, gravel, mixed sediment, sand and mud, from EUSeaMap (2025, else 2023; EUNIS 2019 codes). No download; rerun alone for a new base map. |
| 6 | `05c_habitat_responses.R` (optional, with 05b) | **Ecospace response functions** of every FG to depth, bottom and surface temperature, salinity and NPP, as EwE keeps them (1200 values between X min and X max, maximum 1), from **AquaMaps** (species envelopes weighted by their share of the FG biomass), plus the values each driver takes in the Ecospace maps of 05b (`ecospace_driver_distribution.csv`), drawn behind the curves. |
| 7 | `06_diagnostics.R` | **Validation.** Compares with the 1995 Western Med model and runs the EwE plausibility checks (P/Q, diet sums, cannibalism, PREBAL, F). Writes one summary table and one PDF of all plots. |

Every method, equation, assumption and reference is in `scripts/WMed-EwEbuilder.qmd`.

## Main options

Set these on the page, or at the top of `scripts/run_pipeline_demo.R` (every option has a comment there).

| Option | Default | Meaning |
|---|---|---|
| `RUN_STEPS` | `c(1:4, 7)` (05 is added before 01 when `PHYTO_SOURCE = "copernicus"` and its outputs are missing) | Scripts to run, e.g. `7` (only redraw the validation), `c(2:4, 7)` (keep the biomass), `5` (only the environment), `6` (only the Ecospace maps). |
| `MODEL_GSAS` | `1:12` | GSAs in the model area. A subset filters area, survey data and catch countries together. |
| `MODEL_AREA_BASIS` | `"whole_gsa"` | `"whole_gsa"` = coast to bottom; `"medits_10_800"` = MEDITS 10–800 m strata only. |
| `AREA_MODE` | `"westmed"` | `"custom"` with `AREA_NAME` and `CUSTOM_AREA_TYPE = "shapefile"` (`CUSTOM_SHAPEFILE_PATH`) or `"bbox"` (`CUSTOM_BBOX`). Runs all steps for that region; outputs in `out_dir/<AREA_NAME>/`; catch from the GSA(s) it overlaps. |
| `YEAR_ECOPATH` / `TS_YEARS` | `1994:1996` / `1995:2023` | Ecopath baseline / Ecosim years. |
| `ECOPATH_B_FILL_MODE`, `ECOPATH_B_FILL_YEARS` | `"window_mean"`, `1995:2000` | FGs with no biomass in the Ecopath years take this mean; "Expanding" FGs get 0.00001. |
| `F_MAX` | `1` | FGs with F above this (or above their stock-assessment F) are reported with a recommendation each (`high_F_REVIEW.csv`); Ecopath B is not changed. |
| `PHYTO_SOURCE` | `"copernicus"` | Phytoplankton B and P/B: `"copernicus"` = Copernicus MedBFM from step 05 (B = phytoplankton carbon, P/B = NPP / carbon, mean of the Ecopath years; 05 runs before 01 when its outputs are missing, so a Copernicus login is needed) or `"isimip"` (COBALT 1995, EcoBase P/B). |
| `ENV_SCENARIOS`, `ENV_SCEN_END` | none, `2050` | Optional climate scenarios of the drivers (05s, 05b; about 20 min or more the first time; none = historical only): any of `"ssp126"`, `"ssp245"`, `"ssp370"`, `"ssp585"` (ISIMIP3b has 126, 370 and 585; a scenario without files is reported and skipped), until that year. Also `ENV_SCEN_MODELS`, `ENV_SCEN_REF_N` (reference = last 20 hindcast years), `ENV_SCEN_SMOOTH_YEARS` (11-year running mean of the model change), `ENV_SCEN_VAR_SCALE`, `ENV_SCEN_JOIN_YEARS` (1) and `ENV_SCEN_BLEND_YEARS` (10; how fast the last historical year's anomaly fades). |
| `STOCK_ASSESSMENT_SOURCE` | `"GFCM"` | `"GFCM"` = the newest GFCM Stock Assessment Forms, downloaded and read automatically (check `biomass/gfcm_saf_parse_REVIEW.csv`); `"RAM"` = RAM Legacy. |
| `STRATA`, `DROP_OUTLIERS`, `NORMALIZE_TS` | `TRUE` | Strata-weighted density; remove flagged outlier hauls; Ecosim series as a relative index. |
| `USE_EXTRA_DATA`, `EXTRA_DATA_DIR` | `TRUE`, `data/extra_data` | Add the extra MEDITS hauls, MEDIAS rows and GFCM STAR workbooks in that folder, only where the standard data have nothing (see its README; `biomass/extra_medits_hauls_added.csv`, `star_extra_REVIEW.csv`). |
| `APPLY_CATCHABILITY` | `TRUE` | Divide MEDITS density by the species catchability (default table in pCloud, from Lloret-Lloret 2023, or your own `CATCHABILITY_FILE`). A MEDITS species with no q of its own takes the mean q of its FG's species; groups not sampled by MEDITS keep q = 1. |
| `EWE_FLEETS` | the 35 model fleets | EwE fleets, in order (ALG, FRA, ITA, MOR, SPA, TUN × gear, then one recreational fleet per country). |
| `INCLUDE_RECREATIONAL_FLEET`, `RECREATIONAL_RATIO_MANUAL`, `RECREATIONAL_EFFORT_MANUAL` | `TRUE`, none, none | Recreational catch and effort (assumptions, see step 2); the manual tables replace the ratio or the effort index where a survey value exists. |
| `PRICE_COUNTRY_FACTOR`, `RECREATIONAL_PRICE_FACTOR` | `1` | Off-vessel prices of Morocco, Algeria and Tunisia, and of the recreational fleets, as a multiple of the EU (FDI) price of the same FG. |
| `DIET_FALLBACK_ENABLE_MAISHA`, `..._FISHBASE`, `..._ECOBASE` | `TRUE` | Diet data sources to use besides dietcalcR (page card 2); each source also writes its own matrix (`diet/dietcalc`, `diet/MAISHA`, `diet/FishBase`), compared in the Diet explorer. |
| `DIET_ENGINE` | `"pipeline"` | Diet matrix (04): `"pipeline"` (MAISHA → FishBase/SeaLifeBase → EcoBase) or `"dietcalc"`: dietcalcR (iMARES diet calculator) scores the records of a diet database (`DIETCALC_DB`: Access, SQLite or a folder of csv) by region, years, data type and method, and its diets replace the pipeline's for the predator FGs it covers. Also `DIETCALC_DIR` (dietcalcR folder), `DIETCALC_CONFIG_DIR` (compatibility tables), `DIETCALC_REGION`. FG membership comes from the FG table (as edited in the page); the 01 biomass only weights the species (`dietcalc_fg_vs_01_biomass_REVIEW.csv` lists differences). dietcalcR is run with its own workflow (`dietcalc_run_scenario()`: scoring, study selection and merging, `build_ewe_diet_matrix()`), and its EwE diet table is used as it comes (columns sum to 1; prey outside the model go to the EwE Import row). Outputs in `diet/dietcalc/`: the same tables as dietcalcR's workflow (`01_studies_table` … `08_ewe_matrix_column_sums_verification`), the run summary, species matching, record and study scores, selected studies, merged diets, its EwE diet table and matrix, and the diet pedigree (used for the Diet pedigree of those FGs in 06); all shown in the Diet explorer ("dietcalcR outputs"). |
| `RESPONSE_DRIVERS` | `c("depth", "temp_bot", "temp_surf", "sal_surf", "npp")` | Drivers of the Ecospace response functions (05c, AquaMaps). |
| `FG_SPECIES_FILE` | `data/FG_WMed_2026.csv` | Species → FG table. |
| `PLOT_DPI` | `300` | Resolution of every PNG. |

## First run: downloads and time

Steps 01–06 take about 10–30 minutes. The first run on a new computer can take up to about 70 minutes, because some sources are downloaded once and cached:

| What | Size | Used by | Notes |
|---|---|---|---|
| AquaMaps database (`aquamapsdata`) | ~2 GB download, ~10 GB unpacked | 01, 01b, 05c | depth suitability for the pseudo-strata and habitat depth; envelopes for the response functions. Only species missing from `data/aquamaps_cache/` need it |
| Bathymetry (EMODnet, NOAA ETOPO as fallback) | small, per area | 01, 01b | stratum and habitat depths |
| FishBase / SeaLifeBase (`rfishbase`) | small | 01, 03, 04 | taxonomy, traits, diets |
| ISIMIP3a COBALT plankton fields | hundreds of MB | 01b | plankton and primary production |
| EUSeaMap geodatabase (2025, or 2023) | > 1 GB | 01b, 05b | put it in `data/Complementary data/EUSeaMap_2025/` (or `EUSeaMap_2023/`) |
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
- `extra_data/`: survey and assessment files shared outside the JRC data call (IEO MEDITS for Spain, MEDITS/MEDIAS data call for GSAs 6–7, GFCM STAR workbooks); used to fill gaps (`USE_EXTRA_DATA`).
- `fisheries/`: GFCM, FDI, Sea Around Us (`sau_raw_combined_west_med.csv`), FishMIP, ICCAT, RousseauEtAl2023, STAR_RAMLegacy, GFCM_SAF, and `fleet_costs_ewe.csv` (fleet cost shares by gear).
- `Complementary data/`: reference tables with a citation on every row (habitat biomass, megafauna, unit conversions, diet tables).
- `diet/`: the dietcalcR diet database and configuration (`dietcalc_db/`, `dietcalc_config/`), MAISHA files and the FishBase trophic snapshot.
- `Bathymetry/`: `depth_wmed.asc` (the Ecospace base map) and `EMODnet_WMed_1arcmin.tif` (EMODnet bathymetry for the strata, the 05b grids).
- `aquamaps_cache/`: AquaMaps species IDs and envelopes already looked up, so most runs never open the AquaMaps database.
- `WMed_EwE.xlsx`: the 1995 Western Med model, used for validation.
- `data_sources.csv` (optional): source, citation, years, licence and URL of each input file (columns `file,source,citation,years,licence,url`). Each run joins it to `data_catalogue.csv`.

## Outputs (under `out_dir`)

Every csv is written with `;` between columns, `,` as decimal separator, text in double quotes and UTF-8 (with BOM), so it opens directly in Excel with Spanish/European settings (`options(wmed.csv_euro = FALSE)` before a run gives `,` and `.`). The pipeline reads both formats.

```
ecopath_ecosim_inputs.xlsx      # the EwE workbook: info, FGs, Fleets, FG_spp, Ecopath_B, Ecopath_L,
                                #   Ecopath_Di, Ecopath_price, Ecopath_PBQB, Ecopath_multistanza,
                                #   Ecopath_traits, Ecopath_diet, Ecosim_ts, FG_References, Ecobase, Pedigree (06)
ewe_import/                     # the same inputs as EwE import files (csv per table, or one xlsx), incl.
                                #   Definition_of_fleets (costs), Off_vessel_price and Discard_fate
environment/scenarios/          # climate scenarios of the drivers: maps and Ecosim forcing per scenario (05s)
environment/ecospace_drivers/   # Ecospace driver maps (.asc), per base map (05b): <base map>/historical/ and <base map>/<scenario>/,
                                #   each with sst/ sbt/ sss/ pp/ phyc/ phyl/ phys/ (total, Large, Small phytoplankton);
                                #   <base map>/habitat/ with depth and the habitat layers
environment/ecospace_responses/ # Ecospace response functions (AquaMaps) and driver values of the maps (05c)
biomass/ fisheries/ pbqb-traits/ diet/ validation/ environment/
                                # CSVs of each step; *_REVIEW.csv tables are the ones to check
plots/                          # PNGs by step
<date>_0N_<step>_log.txt        # one log per script and run
run_record_<time>.json          # settings, minutes per step, git commit, R and package versions, input files
data_catalogue.csv              # every input file used: size, date, md5, scripts, source and licence
previous_runs/<time>/           # the workbook, logs and validation summary of the last 5 runs
runs/<area>__EP<years>_TS<years>/  # a run with other area, years or FG table than the defaults gets its own folder
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
  01_biomass.R  01b_biomass_unsurveyed.R  02_fisheries.R  02b_sau_extraction.R  03_pbqb-traits.R  04_diets.R
  05_environment.R  05s_environment_scenarios.R  05b_ecospace_drivers.R  05c_habitat_responses.R  06_diagnostics.R
  03b_ecobase.R                    # EcoBase fallbacks
  lib_*.R                          # shared functions (survey density, AquaMaps, FishBase/MAISHA diets, dietcalcR,
                                   #   plankton, gelatinous/macrozooplankton, GFCM SAF, Copernicus, taxonomy,
                                   #   extra data, EwE export, output tidying, run record)
  phyto.R  posidonia.R  coral_gorgonian.R  emodnet_benthic_habitat_biomass_extraction.R
  combine_STAR_RAMlegacy.R  export_gsa_boundaries.R  environmental_pipeline.R
                                   # standalone helpers used to build reference tables; not run by the pipeline
  medbs_pipeline_methodology.qmd  pipeline_documentation.*   # older documentation (the page is the current one)
  downloads/  figures/             # example outputs and figures of the documentation
  check_pipeline.R                 # checks before committing or sharing
  make_input_snapshot.R            # zip of the inputs for Zenodo
  references.bib  ieee.csl         # every reference of the pipeline
  gsa_boundaries_westmed.geojson   # GSA 1-12 polygons for the page map
  land_westmed.geojson             # coastline for the survey hauls map (Natural Earth)
  example_regions/                 # example custom region (cetacean migration corridor MPA)
  leaflet/  images/                # map library and logos of the page
  additional/                      # one-off helpers, not run by the pipeline
gsa_export/                        # GFCM GSA shapefile and GeoJSON (used by the helpers)
config.R.example                   # template for config.local.R (your folders; git-ignored)
CITATION.cff                       # how to cite the code
ewe_infography.jpg                 # workflow figure
ewe_infography_references.md       # references by block of the workflow figure
```

## Known limitations

- The survey density of GSAs 1, 2 and 5–11 is applied to the unsurveyed GSAs 3, 4 and 12.
- Several groups rest on literature values from one site or on surveys from 2011–2019 (flagged as temporal mismatches in the review tables).
- Some groups the trawl survey hardly sees (coastal, rocky-bottom, pelagic) get F > 1; see the *F by country* figure and the `HIGH_F_B_FIX` option.
- Corals and gorgonians, meso- and microzooplankton and suprabenthos have no diet until the full diet database (dietcalcR) covers them (`diet_still_missing_REVIEW.csv`); no diets are entered by hand.
- Ecosim fishing effort mixes units between countries (STECF FDI fishing days for Spain, France and Italy; FishMIP kW-days for Morocco, Algeria and Tunisia), so each fleet's series is only used as an index.
- Recreational catch and effort have no direct source: catch from the Sea Around Us recreational:commercial ratio (Tunisia: the median of Algeria and Morocco), effort from the Sea Around Us recreational catch of the country.
- Fleet costs are Mediterranean values by gear (STECF AER), the same for every country; prices of Morocco, Algeria and Tunisia are EU (FDI) prices unless `PRICE_COUNTRY_FACTOR` is set.
- AquaMaps envelopes come from the species match in `data/aquamaps_cache/`; a wrong or genus-level match gives a wrong curve (check `environment/ecospace_responses/aquamaps_envelopes_by_species.csv` and the response plots).
- Package versions are recorded and checked, not pinned (no `renv` lockfile).

See the "Known limitations" section of the interactive page for the full list.

## How to cite

Cite the code with the metadata in `CITATION.cff` (GitHub shows it under **Cite this repository**), and the data sources listed in `data_catalogue.csv` and on the interactive page.

## Contact

Daniel Vilas, iMARES group, Institut de Ciències del Mar (ICM-CSIC), Barcelona. Questions and bugs: open an issue on GitHub.
