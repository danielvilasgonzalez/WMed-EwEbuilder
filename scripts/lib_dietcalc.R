## =================================================================
## lib_dietcalc.R
## Created by: Daniel Vilas
##
## Connection to dietcalcR (iMARES diet calculator: J. Steenbeek, M. Coll,
## J. Mestre-Tomás, J. E. Leon; github.com/iMARES-group/dietcalcR) for
## 04_diets.R (DIET_ENGINE = "dietcalc").
##
## dietcalcR scores every diet record of its diet database by how well it
## fits the model (region, years, type of data, method; compatibility
## tables and field weights), selects and merges the studies of each
## predator species, and builds the EwE diet matrix with predator species
## weighted by their biomass within the FG. This file gives it the
## model's own FGs and species biomass:
##   groups         FG_num, FG_name, category ("consumer" for predator FGs,
##                  else "producer"/"detritus"), ewe_order = FG_num
##   group_species  FG_num x the diet database's species code of each
##                  species of the FG table (matched by scientific name;
##                  database prey with no exact match: by genus, to the FG
##                  with most biomass of that genus)
##   biomass        species code x species biomass (t/km2;
##                  biomass/biomass_proportion_by_species_fg.csv, 01)
## and returns the FG diet as predator_fg, prey_fg, weight (columns sum to
## 1) plus dietcalcR's diet pedigree per predator FG.
##
## Inputs (settings, pCloud data/diet/ by default):
##   DIETCALC_DIR         dietcalcR source folder (or the installed package
##                        "dietcalc"); the R files are sourced, no install
##   DIETCALC_DB          the diet database: Access (.accdb/.mdb; needs the
##                        Access ODBC driver, Windows, or mdbtools'
##                        mdb-export), SQLite (.sqlite/.db) or a folder of
##                        CSV/TSV files, with the tables Diets_data,
##                        Species_data, Region, Collecting_data_codes and
##                        Diet_method
##   DIETCALC_CONFIG_DIR  RegionCompatibility.xlsx, MethodPedigree.xlsx,
##                        DataCompatibility.xlsx, FieldWeights.xlsx; when
##                        absent, the compatibility tables shipped with
##                        dietcalcR (dietcalc_toy) are used
##   DIETCALC_REGION      region of the model in the database's Region
##                        table (default "Western Mediterranean Sea")
##   DIETCALC_YEARS       model years for the time score (default the
##                        Ecopath years)
##   DIETCALC_NO_BIOMASS_PREY  prey with no FG of the model: "drop"
##                        (default; the column is renormalised), "imports"
## =================================================================

## dietcalcR functions: the installed package, else the source folder
.dietcalc_env <- function(dir = NULL) {
  if (requireNamespace("dietcalc", quietly = TRUE)) {
    ns <- asNamespace("dietcalc")
    env <- new.env(parent = ns)
    for (nm in ls(ns, all.names = TRUE)) assign(nm, get(nm, envir = ns), envir = env)
    try(utils::data("dietcalc_toy", package = "dietcalc", envir = env), silent = TRUE)
    return(env)
  }
  if (is.null(dir) || is.na(dir) || !dir.exists(file.path(dir, "R")))
    stop("dietcalcR not found: install it or set DIETCALC_DIR to its folder (the one with R/ and DESCRIPTION).")
  for (p in c("dplyr", "cli", "tibble", "readxl")) if (!requireNamespace(p, quietly = TRUE)) stop("dietcalcR needs the package ", p)
  env <- new.env(parent = globalenv())
  for (f in sort(list.files(file.path(dir, "R"), pattern = "\\.R$", full.names = TRUE))) sys.source(f, envir = env)
  toy <- file.path(dir, "data", "dietcalc_toy.rda")
  if (file.exists(toy)) load(toy, envir = env)
  env
}

## Access database on a computer without the Access ODBC driver: its
## tables exported to CSV with mdbtools (mdb-export), when installed
.dietcalc_access_to_csv <- function(db, tables) {
  if (!nzchar(Sys.which("mdb-export"))) return(NULL)
  d <- file.path(tempdir(), paste0("dietcalc_db_", tools::md5sum(db)))
  dir.create(d, showWarnings = FALSE)
  for (t in tables) {
    f <- file.path(d, paste0(t, ".csv"))
    if (!file.exists(f)) system2("mdb-export", c(shQuote(db), shQuote(t)), stdout = f)
  }
  d
}

dietcalc_fg_diet <- function(group_table, species_biomass,
                             dietcalc_dir = get0("DIETCALC_DIR", ifnotfound = NULL),
                             db = get0("DIETCALC_DB", ifnotfound = NULL),
                             config_dir = get0("DIETCALC_CONFIG_DIR", ifnotfound = NULL),
                             region = get0("DIETCALC_REGION", ifnotfound = "Western Mediterranean Sea"),
                             years = get0("DIETCALC_YEARS", ifnotfound = get0("YEAR_ECOPATH", ifnotfound = 1995)),
                             no_biomass_prey = get0("DIETCALC_NO_BIOMASS_PREY", ifnotfound = "drop")) {
  dc <- .dietcalc_env(dietcalc_dir)
  if (is.null(db) || is.na(db) || !(file.exists(db) || dir.exists(db)))
    stop("diet database not found (DIETCALC_DB = ", if (is.null(db)) "NULL" else db, ").")
  tabs <- c("Diets_data", "Species_data", "Region", "Collecting_data_codes", "Diet_method")
  src <- db
  if (grepl("\\.(accdb|mdb)$", db, ignore.case = TRUE)) {
    ok <- tryCatch({ dc$read_access_table(db, "Region"); TRUE }, error = function(e) FALSE)
    if (!ok) {
      src <- .dietcalc_access_to_csv(db, tabs)
      if (is.null(src)) stop("cannot read the Access database here (no Access ODBC driver, no mdb-export). ",
                             "Export its tables ", paste(tabs, collapse = ", "), " to CSV files in one folder (or to SQLite) and set DIETCALC_DB to it.")
      message("[dietcalc] Access database read through mdb-export.")
    }
  }
  rd <- function(fun) fun(src, reader = dc$read_scenario_table)
  diets <- rd(dc$load_diets); species_info <- rd(dc$load_species_info)
  data_codes <- tryCatch(rd(dc$load_collecting_data_codes), error = function(e) NULL)
  methods <- tryCatch(rd(dc$load_diet_method), error = function(e) NULL)
  region_lookup <- tryCatch(rd(dc$load_region_lookup), error = function(e) dc$default_region_lookup())
  region_code <- dc$resolve_region_code(region, region_lookup)
  comp <- if (!is.null(config_dir) && !is.na(config_dir) && file.exists(file.path(config_dir, "RegionCompatibility.xlsx"))) {
    dc$load_config_xlsx(config_dir)
  } else if (exists("dietcalc_toy", envir = dc)) {
    message("[dietcalc] No compatibility tables in DIETCALC_CONFIG_DIR - using the ones shipped with dietcalcR.")
    get("dietcalc_toy", envir = dc)$compatibility
  } else stop("no compatibility tables (DIETCALC_CONFIG_DIR).")

  ## model species -> database species codes (by scientific name)
  key <- function(x) tolower(gsub("[^A-Za-z]+", " ", trimws(x)))
  si <- as.data.table(species_info)[, k := key(species_name)]
  sb <- as.data.table(species_biomass)[, .(FG_num = as.integer(FG_num), Species, B = as.numeric(Biomass_t_km2))][!is.na(FG_num)]
  sb[, k := key(Species)]
  sb[, species_code := si$species_code[match(k, si$k)]]
  n_match <- sb[!is.na(species_code), uniqueN(Species)]
  sb[is.na(species_code), species_code := paste0("wmed_", gsub("[^A-Za-z0-9]+", "_", Species))]
  gs <- unique(sb[, .(group_code = as.character(FG_num), species_code)])
  ## database prey not in the FG table: by genus, to that genus' FG with most biomass
  prey <- setdiff(unique(diets$prey_code), gs$species_code)
  pi <- si[species_code %in% prey & !is.na(species_name)]
  pi[, genus := sub(" .*$", "", key(species_name))]
  sb[, genus := sub(" .*$", "", k)]
  gfg <- sb[is.finite(B), .(B = sum(B)), by = .(genus, FG_num)][order(genus, -B)][, .SD[1], by = genus]
  pi <- merge(pi, gfg[, .(genus, FG_num)], by = "genus")
  if (nrow(pi)) gs <- unique(rbind(gs, pi[, .(group_code = as.character(FG_num), species_code)]))
  gt <- as.data.table(group_table)
  groups <- data.frame(group_code = as.character(gt$group_number), group_name = gt$group_name,
                       category = ifelse(gt$is_predator %in% TRUE, "consumer",
                                         ifelse(grepl("detritus|discard", gt$group_name, ignore.case = TRUE), "detritus", "producer")),
                       ewe_order = as.integer(gt$group_number), stringsAsFactors = FALSE)
  bio <- unique(sb[is.finite(B) & B > 0, .(species_code, biomass = B)], by = "species_code")
  message("[dietcalc] ", nrow(diets), " diet records of ", uniqueN(diets$reference), " studies; ", n_match, " of ",
          uniqueN(sb$Species), " FG-table species found in the diet database; ", nrow(pi), " more prey by genus; region ",
          region, " (code ", region_code, "), years ", min(years), "-", max(years), ".")
  p <- dc$dietcalc_run_scenario(diets = diets, biomass = as.data.frame(bio), group_species = as.data.frame(gs), groups = groups,
                                species_info = species_info, compatibility = comp, region = region_code,
                                year_start = as.integer(min(years)), year_end = as.integer(max(years)),
                                collecting_data_codes = data_codes, diet_method = methods, no_biomass_prey = no_biomass_prey)
  m <- p$outputs$ewe_diet_matrix
  if (is.null(m) || !ncol(m)) return(list(fg_diet = data.table(), pedigree = data.table(), project = p))
  nm <- setNames(gt$group_name, as.character(gt$group_number))
  long <- as.data.table(as.table(m))[N > 0]
  setnames(long, c("prey_code", "pred_code", "weight"))
  long[, `:=`(predator_fg = unname(nm[pred_code]), prey_fg = fifelse(prey_code %in% names(nm), unname(nm[prey_code]), prey_code))]
  long <- long[!is.na(predator_fg)]
  long[, weight := weight / sum(weight), by = predator_fg]
  ped <- as.data.table(p$outputs$ewe_diet_pedigree)
  list(fg_diet = long[, .(predator_fg, prey_fg, weight)], pedigree = ped, project = p,
       records = as.data.table(p$scores$record), studies = as.data.table(p$selection$studies))
}
