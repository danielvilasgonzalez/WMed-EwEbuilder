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
##   DIETCALC_DB          the diet database: its data-entry workbook
##                        (data_entry_metaweb*.xlsx), Access (.accdb/.mdb; needs the
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
  ## RStudio on a Mac does not see Homebrew's folder in PATH: look there too
  exe <- c(Sys.which("mdb-export"), "/opt/homebrew/bin/mdb-export", "/usr/local/bin/mdb-export")
  exe <- exe[nzchar(exe) & file.exists(exe)][1]
  if (is.na(exe)) return(NULL)
  d <- file.path(tempdir(), paste0("dietcalc_db_", tools::md5sum(db)))
  dir.create(d, showWarnings = FALSE)
  for (t in tables) {
    f <- file.path(d, paste0(t, ".csv"))
    if (!file.exists(f) || file.mtime(f) < file.mtime(db)) system2(exe, c(shQuote(db), shQuote(t)), stdout = f)
  }
  d
}

## The data-entry workbook of the Mediterranean diet database
## (data_entry_metaweb*.xlsx): DATA_ENTRY -> diet records, Taxonomic_codes ->
## species, Region -> regions; data and method codes as in its
## Description_tabs (Data: 0 unknown, 1 adults and juveniles, 2 adults,
## 3 juveniles, 4 larvae; Method: 0 unknown, 1 stable isotopes, 2 visual
## observations, 3 stomach content, 4 others, 5 metabarcoding). Diet value:
## "both_prey" when filled, else %W, %Vol, %N, IRI, %F (first available).
.dietcalc_read_entry_xlsx <- function(f) {
  rx <- function(sh, skip = 0) { x <- as.data.frame(readxl::read_excel(f, sheet = sh, skip = skip, .name_repair = "minimal"), stringsAsFactors = FALSE)
    names(x) <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(names(x))))); x }
  d <- rx("DATA_ENTRY")
  d <- d[!is.na(d$code_predator) & !is.na(d$code_prey), , drop = FALSE]
  num <- function(v) suppressWarnings(as.numeric(v))
  val <- rep(NA_real_, nrow(d))
  for (cc in c("both_prey", "weight", "volume", "number", "iri", "frequency")) if (cc %in% names(d)) val <- ifelse(is.na(val), num(d[[cc]]), val)
  reg <- rx("Region")
  rl <- data.frame(region_code = as.integer(num(reg$code_region)), region_name = as.character(reg$region_0), stringsAsFactors = FALSE)
  rl <- rl[!is.na(rl$region_code) & !is.na(rl$region_name), ]
  rcode <- rl$region_code[match(tolower(trimws(d$region_0)), tolower(trimws(rl$region_name)))]
  diets <- data.frame(record_id = as.character(seq_len(nrow(d))), predator_code = as.character(d$code_predator), prey_code = as.character(d$code_prey),
                      diet_value = val, data_code = as.integer(num(d$data)), method_code = as.integer(num(d$method)),
                      year_start = as.integer(num(d$beginning_year)), year_end = as.integer(num(d$end_year)), region_code = as.integer(rcode),
                      reference = as.character(d$reference), stringsAsFactors = FALSE)
  diets <- diets[is.finite(diets$diet_value) & diets$diet_value > 0, , drop = FALSE]
  tx <- rx("Taxonomic_codes")
  si <- data.frame(species_code = as.character(tx$valid_code), species_name = as.character(tx$valid_name), phylum = as.character(tx$phylum),
                   class_name = as.character(tx$class), order_name = as.character(tx$order), family = as.character(tx$family), stringsAsFactors = FALSE)
  list(diets = diets, species_info = si[!is.na(si$species_code), ], region_lookup = rl,
       data_codes = data.frame(code_data = 0:4, data_collected = c("Unknown", "Adults and juveniles", "Adults", "Juveniles", "Larvae")),
       methods = data.frame(method_code = 0:5, method_name = c("Unknown", "Stable isotopes", "Visual observations", "Stomach content", "Others", "Metabarcoding")))
}

dietcalc_fg_diet <- function(group_table, species_biomass, fg_species = NULL,
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
      if (is.null(src)) stop("cannot read the Access database here (no Access ODBC driver, no mdb-export; on a Mac: brew install mdbtools). ",
                             "Export its tables ", paste(tabs, collapse = ", "), " to CSV files in one folder (or to SQLite) and set DIETCALC_DB to it.")
      message("[dietcalc] Access database read through mdb-export.")
    }
  }
  if (grepl("\\.xlsx$", db, ignore.case = TRUE)) {
    x <- .dietcalc_read_entry_xlsx(db)
    diets <- x$diets; species_info <- x$species_info; data_codes <- x$data_codes; methods <- x$methods; region_lookup <- x$region_lookup
  } else {
    rd <- function(fun) fun(src, reader = dc$read_scenario_table)
    diets <- rd(dc$load_diets); species_info <- rd(dc$load_species_info)
    data_codes <- tryCatch(rd(dc$load_collecting_data_codes), error = function(e) NULL)
    methods <- tryCatch(rd(dc$load_diet_method), error = function(e) NULL)
    region_lookup <- tryCatch(rd(dc$load_region_lookup), error = function(e) dc$default_region_lookup())
  }
  if (!nrow(diets)) stop("the diet database has no diet records yet (", basename(db), ").")
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
  ## Coverage: dietcalcR scores records (region, years, data, method) but
  ## does not drop them, so any record of a predator counts; a predator
  ## species is only used when it is in an FG with biomass > 0. To use every
  ## record the database has:
  ##  - every FG-table species gets a biomass: its own (01), else its share of
  ##    the FG's Ecopath B, else a tiny one (1e-9 t/km2: used only when no
  ##    species of the FG with biomass has a diet);
  ##  - database species not in the FG table (predators and prey) join the FG
  ##    of their genus (the FG with most biomass of that genus), else of their
  ##    family, with the tiny biomass;
  ##  - records with no sampling years (dropped by dietcalcR) get the
  ##    years of the other records of their study, else are reported.
  sp <- as.data.table(species_biomass)
  num <- function(v) suppressWarnings(as.numeric(v))
  b01 <- sp[, .(FG_num = as.integer(FG_num),
                FG_name = if ("FG_name" %in% names(sp)) as.character(FG_name) else NA_character_,
                Species, B = num(Biomass_t_km2),
                Bfg = if ("Ecopath_B_Biomass" %in% names(sp)) num(Ecopath_B_Biomass) else NA_real_,
                w = if ("prop_sp_fg" %in% names(sp)) num(prop_sp_fg) else NA_real_)][!is.na(FG_num)]
  TINY <- 1e-9
  fg_mismatch <- NULL
  if (!is.null(fg_species) && nrow(fg_species)) {
    ## FG membership = the current FG table of the app (it can be edited
    ## there); the biomass file of 01 only gives the weights. A species keeps
    ## its own 01 biomass (times its share in the FG); the FG-share fallback
    ## is used only where its FG is unchanged.
    gt <- as.data.table(group_table)
    fs <- unique(as.data.table(fg_species)[!is.na(species) & species != "",
                 .(Species = as.character(species), FG_name = as.character(fg_name),
                   prop = { p <- num(proportion); fifelse(is.finite(p) & p > 0, p, 1) })])
    fs[, FG_num := as.integer(gt$group_number[match(FG_name, gt$group_name)])]
    fs <- fs[!is.na(FG_num)]
    nk <- function(x) tolower(gsub("[^A-Za-z]+", " ", trimws(x)))
    fs[, k := nk(Species)]; b01[, k := nk(Species)]
    bsp <- b01[is.finite(B) & B > 0, .(Bsp = sum(B)), by = k]
    bfg <- b01[, .(Bfg = Bfg[1], w = sum(w, na.rm = TRUE)), by = .(k, FG_name)]
    fs <- merge(fs, bsp, by = "k", all.x = TRUE)
    fs <- merge(fs, bfg, by = c("k", "FG_name"), all.x = TRUE)
    sb <- fs[, .(FG_num, Species, B = Bsp * prop, Bfg, w)]
    ## review: species whose FG differs between the 01 biomass file and the FG table
    o1 <- unique(b01[, .(k, Species_01 = Species, FG_01 = FG_name)])
    o2 <- unique(fs[, .(k, Species, FG_app = FG_name)])
    m <- merge(o2[, .(FG_app = paste(sort(unique(FG_app)), collapse = " | ")), by = .(k, Species)],
               o1[, .(FG_01 = paste(sort(unique(FG_01)), collapse = " | ")), by = k], by = "k", all = TRUE)
    m <- m[is.na(FG_app) | is.na(FG_01) | FG_app != FG_01]
    if (nrow(m)) {
      m[, Species := fifelse(is.na(Species), o1$Species_01[match(k, o1$k)], Species)]
      m[, issue := fifelse(is.na(FG_01), "in the FG table, not in the 01 biomass (tiny biomass used)",
                   fifelse(is.na(FG_app), "in the 01 biomass, not in the FG table (not used)",
                           "FG changed since 01 (own biomass kept)"))]
      fg_mismatch <- m[, .(Species, FG_app, FG_01, issue)][order(issue, Species)]
      message("[dietcalc] ", nrow(fg_mismatch), " species differ between the FG table and the 01 biomass file ",
              "(dietcalcR uses the FG table; rerun 01 to update the biomass weights).")
    }
  } else {
    sb <- b01[, .(FG_num, Species, B, Bfg, w)]
  }
  sb[!(is.finite(B) & B > 0) & is.finite(Bfg) & Bfg > 0 & is.finite(w) & w > 0, B := Bfg * w]
  sb[!(is.finite(B) & B > 0), B := TINY]
  sb[, k := key(Species)]
  sb[, species_code := si$species_code[match(k, si$k)]]
  n_match <- sb[!is.na(species_code), uniqueN(Species)]
  sb[is.na(species_code), species_code := paste0("wmed_", gsub("[^A-Za-z0-9]+", "_", Species))]
  gs <- unique(sb[, .(group_code = as.character(FG_num), species_code)])
  sb[, genus := sub(" .*$", "", k)]
  if (!"family" %in% names(si)) si[, family := NA_character_]
  sb[, family := tolower(si$family[match(species_code, si$species_code)])]
  gfg <- sb[B > TINY, .(B = sum(B)), by = .(genus, FG_num)][order(genus, -B)][, .SD[1], by = genus]
  ffg <- sb[B > TINY & !is.na(family), .(B = sum(B)), by = .(family, FG_num)][order(family, -B)][, .SD[1], by = family]
  extra <- setdiff(unique(c(diets$predator_code, diets$prey_code)), gs$species_code)
  pi <- si[species_code %in% extra & !is.na(species_name)]
  pi[, `:=`(genus = sub(" .*$", "", key(species_name)), family = tolower(family))]
  pi[, FG_num := gfg$FG_num[match(genus, gfg$genus)]]
  pi[is.na(FG_num), FG_num := ffg$FG_num[match(family[is.na(FG_num)], ffg$family)]]
  pi[, by_rank := fifelse(genus %in% gfg$genus, "genus", "family")]
  pi <- pi[!is.na(FG_num)]
  if (nrow(pi)) gs <- unique(rbind(gs, pi[, .(group_code = as.character(FG_num), species_code)]))
  ## records with no years: the years of their study
  if (!"study_id" %in% names(diets)) diets$study_key <- diets$reference
  dd <- as.data.table(diets)
  dd[, `:=`(ys = suppressWarnings(min(year_start, na.rm = TRUE)), ye = suppressWarnings(max(year_end, na.rm = TRUE))), by = reference]
  n_fill <- dd[(is.na(year_start) | is.na(year_end)) & is.finite(ys) & is.finite(ye), .N]
  dd[is.na(year_start) & is.finite(ys), year_start := as.integer(ys)]; dd[is.na(year_end) & is.finite(ye), year_end := as.integer(ye)]
  n_noyear <- dd[is.na(year_start) | is.na(year_end), .N]
  diets <- as.data.frame(dd[, !c("ys", "ye")]); diets$study_key <- NULL
  if (n_fill || n_noyear) message("[dietcalc] ", n_fill, " records without years took the years of their study; ",
                                  n_noyear, " records with no years at all (dietcalcR leaves them out).")
  gt <- as.data.table(group_table)
  groups <- data.frame(group_code = as.character(gt$group_number), group_name = gt$group_name,
                       category = ifelse(gt$is_predator %in% TRUE, "consumer",
                                         ifelse(grepl("detritus|discard", gt$group_name, ignore.case = TRUE), "detritus", "producer")),
                       ewe_order = as.integer(gt$group_number), stringsAsFactors = FALSE)
  bio <- unique(rbind(sb[, .(species_code, biomass = B)], pi[, .(species_code, biomass = TINY)]), by = "species_code")
  message("[dietcalc] ", nrow(diets), " diet records of ", uniqueN(diets$reference), " studies; ", n_match, " of ",
          uniqueN(sb$Species), " FG-table species found in the diet database; ", pi[by_rank == "genus", .N], " more species by genus and ",
          pi[by_rank == "family", .N], " by family; region ",
          region, " (code ", region_code, "), years ", min(years), "-", max(years), ".")
  p <- dc$dietcalc_run_scenario(diets = diets, biomass = as.data.frame(bio), group_species = as.data.frame(gs), groups = groups,
                                species_info = species_info, compatibility = comp, region = region_code,
                                year_start = as.integer(min(years)), year_end = as.integer(max(years)),
                                collecting_data_codes = data_codes, diet_method = methods, no_biomass_prey = no_biomass_prey)
  m <- p$outputs$ewe_diet_matrix
  if (is.null(m) || !ncol(m)) m <- matrix(numeric(0), 0, 0)
  nm <- setNames(gt$group_name, as.character(gt$group_number))
  long <- if (length(m)) as.data.table(as.table(m))[N > 0] else data.table(V1 = character(0), V2 = character(0), N = numeric(0))
  setnames(long, c("prey_code", "pred_code", "weight"))
  long[, `:=`(predator_fg = unname(nm[pred_code]), prey_fg = fifelse(prey_code %in% names(nm), unname(nm[prey_code]), prey_code))]
  long <- long[!is.na(predator_fg)]
  long[, weight := weight / sum(weight), by = predator_fg]
  ped <- as.data.table(p$outputs$ewe_diet_pedigree)
  ## coverage per predator FG: why an FG has (no) dietcalcR diet
  prd <- unique(as.data.table(diets)[, .(species_code = predator_code, dated = !is.na(year_start) & !is.na(year_end))])
  gsd <- merge(as.data.table(gs), prd[, .(any_rec = TRUE, dated = any(dated)), by = species_code], by = "species_code", all.x = TRUE)
  cov <- gsd[, .(species_in_db = sum(!grepl("^wmed_", species_code)), predators_with_records = sum(any_rec %in% TRUE),
                 predators_with_dated_records = sum(dated %in% TRUE)), by = .(FG_num = as.integer(group_code))]
  cov <- merge(gt[is_predator %in% TRUE, .(FG_num = as.integer(group_number), FG_name = group_name)], cov, by = "FG_num", all.x = TRUE)
  for (cc in c("species_in_db", "predators_with_records", "predators_with_dated_records")) cov[is.na(get(cc)), (cc) := 0L]
  cov[, dietcalc_diet := FG_name %in% long$predator_fg]
  cov[, why := fifelse(dietcalc_diet, "", fifelse(species_in_db == 0, "none of its species (or their genus/family) is in the database",
                fifelse(predators_with_records == 0, "its species are in the database only as prey (no diet records)",
                fifelse(predators_with_dated_records == 0, "its diet records have no sampling years", "its records give no prey of the model"))))]
  list(fg_diet = long[, .(predator_fg, prey_fg, weight)], pedigree = ped, project = p, coverage = cov[order(FG_num)],
       records = as.data.table(p$scores$record), studies = as.data.table(p$selection$studies),
       fg_mismatch = fg_mismatch)
}
