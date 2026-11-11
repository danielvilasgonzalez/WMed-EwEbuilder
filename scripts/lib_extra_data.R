## ==================================================================
## Created by: Daniel Vilas
## Extra survey and stock-assessment files that fill gaps in the
## standard inputs (JRC MEDITS TA/TB, JRC MEDIAS, GFCM SAF table).
## Everything here is read from one folder, EXTRA_DATA_DIR (default
## <pcloud_dir>/data/extra_data), searched recursively; see the README
## in that folder. Nothing replaces the standard data: each source only
## adds what the standard data do not have.
##
##  1. MEDITS hauls (01_biomass.R): GSA-year pairs with no haul in the
##     JRC TA file are filled, in this order,
##       a. from MEDITS data-call TA/TB pairs (TA*.csv + TB*.csv with the
##          same suffix, e.g. TA_67.csv / TB_67.csv; standard MEDITS
##          columns, ";"-separated; R write.table output with row names
##          is read too),
##       b. from IEO (Spain) haul-by-species tables: Abundancia_biomasa*.xlsx
##          and MEDITS_<year>.CSV (YEAR, SECTOR, LANCE, LONGITUD/LATITUD
##          INI, ESPECIE, KILOS, AREA BARRIDA, PROF.MED; decimal degrees,
##          km2 swept area). IEO sectors -> GSA: IEO_SECTOR_GSA.
##  2. MEDIAS acoustic biomass (01_biomass.R): BIOMASS*.csv (length
##     classes) and ABUND_BIOM*.csv (age groups) - GSA-year-species rows
##     the JRC MEDIAS files do not have.
##  3. Stock assessments (01b_biomass_unsurveyed.R): GFCM STAR workbooks
##     (STAR_<SPC>_<GSA>_RefY<year>.xlsx). A STAR stock replaces the
##     stock in use for the same species and GSAs when its reference year
##     is newer (or the one in use is a RAM Legacy / hand-transcribed
##     fill); it never replaces a stock covering more GSAs.
## ==================================================================

if (!exists("USE_EXTRA_DATA", envir = .GlobalEnv, inherits = FALSE)) USE_EXTRA_DATA <- TRUE
if (!exists("EXTRA_DATA_DIR", envir = .GlobalEnv, inherits = FALSE) || !nzchar(get0("EXTRA_DATA_DIR", ifnotfound = "")))
  EXTRA_DATA_DIR <- if (exists("pcloud_dir")) file.path(pcloud_dir, "data", "extra_data") else ""
## IEO MEDITS sectors (Spanish Mediterranean) -> GFCM GSA
if (!exists("IEO_SECTOR_GSA", envir = .GlobalEnv, inherits = FALSE))
  IEO_SECTOR_GSA <- c("110" = 2, "111" = 1, "112" = 6, "113" = 6)

.extra_on <- function() isTRUE(USE_EXTRA_DATA) && nzchar(EXTRA_DATA_DIR) && dir.exists(EXTRA_DATA_DIR)
.extra_files <- function(pattern) {
  if (!.extra_on()) return(character(0))
  sort(list.files(EXTRA_DATA_DIR, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE))
}

## ";"-separated csv that may have been written by R's write.table with
## row names (header one field shorter than the rows)
read_semicolon_csv <- function(path, encoding = "UTF-8") {
  first <- readLines(path, n = 1, warn = FALSE, encoding = encoding)
  sep <- if (grepl(";", first, fixed = TRUE)) ";" else ","
  hdr <- gsub('^"|"$', "", trimws(strsplit(first, sep, fixed = TRUE)[[1]]))
  hdr <- trimws(gsub("\t", "", hdr))
  d <- fread(path, skip = 1, header = FALSE, sep = sep, na.strings = c("", "NA", "NULL"),
             encoding = if (identical(encoding, "latin1")) "Latin-1" else "UTF-8", showProgress = FALSE)
  if (ncol(d) == length(hdr) + 1) d[, 1 := NULL]
  if (ncol(d) != length(hdr)) stop("columns do not match the header in ", basename(path))
  setnames(d, hdr)
  d
}

## ------------------------------------------------------------------
## 1. MEDITS hauls
## ------------------------------------------------------------------
## have: data.table(AreaID, Year) already in the JRC data.
## Returns list(data = rows in dataframe1's columns, log = what was added).
extra_medits_hauls <- function(have, tm_lookup) {
  empty <- list(data = NULL, log = data.table())
  if (!.extra_on()) return(empty)
  have <- unique(as.data.table(have)[, .(AreaID = as.numeric(AreaID), Year = as.integer(Year))])
  added <- list(); log <- list()
  take <- function(d, source) {      # keep only GSA-years nobody has yet
    if (is.null(d) || !nrow(d)) return(invisible(NULL))
    d <- d[!is.na(AreaID) & !is.na(Year)]
    ## a MEDITS tow sweeps ~0.03-0.15 km2: hauls outside 0.005-0.5 km2 are typos
    bad <- d[is.na(Effort) | Effort < 0.005 | Effort > 0.5, unique(SampleID)]
    if (length(bad)) { message("[Extra MEDITS] ", source, ": ", length(bad), " haul(s) dropped (swept area missing or outside 0.005-0.5 km2): ",
                               paste(head(bad, 5), collapse = ", ")); d <- d[!SampleID %in% bad] }
    have_now <- rbindlist(c(list(have), lapply(added, function(x) unique(x[, .(AreaID, Year)]))))
    d <- d[!have_now, on = c("AreaID", "Year")]
    if (!nrow(d)) return(invisible(NULL))
    added[[length(added) + 1]] <<- d
    log[[length(log) + 1]] <<- d[, .(n_hauls = uniqueN(SampleID), biomass_t = sum(Biomass, na.rm = TRUE), source = source), by = .(GSA = AreaID, Year)]
  }
  ## a. data-call TA/TB pairs
  for (ta_f in .extra_files("^TA(_[^.]*)?\\.csv$")) {
    tb_f <- file.path(dirname(ta_f), sub("^TA", "TB", basename(ta_f), ignore.case = TRUE))
    if (!file.exists(tb_f)) { message("[Extra MEDITS] ", basename(ta_f), " has no matching TB file - skipped"); next }
    d <- tryCatch({
      ta <- read_semicolon_csv(ta_f); tb <- read_semicolon_csv(tb_f)
      ta[, `:=`(wing_opening_m = if (WING_OPENING_UNIT == "decimetres") wing_opening / 10 else wing_opening,
                SampleID = paste("X", country, area, vessel, year, haul_number, month, day, sep = "_"))]
      ta[, `:=`(swept_area_km2 = distance * wing_opening_m / 1e6, mean_depth = (shooting_depth + hauling_depth) / 2,
                Lat = convert_ddmm_to_decimal(shooting_latitude),
                Lon = fix_alboran_longitude_sign(convert_ddmm_to_decimal(shooting_longitude), area))]
      ta <- unique(ta, by = "SampleID")
      tb[, `:=`(species_code = paste0(genus, species), ptot = fifelse(ptot < 0, NA_real_, as.numeric(ptot)),
                SampleID = paste("X", country, area, vessel, year, haul_number, month, day, sep = "_"))]
      x <- merge(merge(tb, tm_lookup, by = "species_code", all.x = TRUE),
                 ta[, .(SampleID, swept_area_km2, mean_depth, Lat, Lon)], by = "SampleID")
      x[, .(species_code, ScientificName, Biomass = ptot / 1e6, Year = as.integer(year), AreaID = as.numeric(area),
            Depth = mean_depth, SampleID, Effort = swept_area_km2, Lat, Lon)]
    }, error = function(e) { message("[Extra MEDITS] ", basename(ta_f), " not read: ", conditionMessage(e)); NULL })
    take(d, paste0("MEDITS data call (", basename(ta_f), ")"))
  }
  ## b. IEO haul-by-species tables
  for (f in .extra_files("(^abundancia_biomasa.*\\.xlsx$)|(^MEDITS_[0-9]{4}\\.csv$)")) {
    d <- tryCatch({
      x <- if (grepl("\\.xlsx$", f, ignore.case = TRUE)) as.data.table(readxl::read_excel(f, sheet = 1)) else read_semicolon_csv(f, encoding = "latin1")
      setnames(x, toupper(gsub("[^A-Za-z]", "", names(x))))
      need <- c("YEAR", "SECTOR", "LANCE", "LONGITUDINI", "LATITUDINI", "ESPECIE", "KILOS", "AREABARRIDA", "PROFMED")
      if (!all(need %in% names(x))) stop("missing column(s) ", paste(setdiff(need, names(x)), collapse = ", "))
      num <- function(v) suppressWarnings(as.numeric(gsub(",", ".", as.character(v))))
      x <- x[, .(Year = as.integer(num(YEAR)), sector = as.character(as.integer(num(SECTOR))), haul = as.character(LANCE),
                 Lon = num(LONGITUDINI), Lat = num(LATITUDINI), ScientificName = trimws(as.character(ESPECIE)),
                 Biomass = num(KILOS) / 1000, Effort = num(AREABARRIDA), Depth = num(PROFMED))]
      x[, AreaID := unname(IEO_SECTOR_GSA[sector])]
      x <- x[!is.na(AreaID)]
      x[, SampleID := paste("IEO", sector, Year, haul, sep = "_")]
      ## hauls with impossible positions (typos) are dropped
      bad <- x[!(between(Lat, 30, 46) & between(Lon, -10, 20)), unique(SampleID)]
      if (length(bad)) message("[Extra MEDITS] ", basename(f), ": ", length(bad), " haul(s) dropped (position not valid): ", paste(head(bad, 5), collapse = ", "))
      x <- x[!SampleID %in% bad]
      x[, .(species_code = NA_character_, ScientificName, Biomass, Year, AreaID, Depth, SampleID, Effort, Lat, Lon)]
    }, error = function(e) { message("[Extra MEDITS] ", basename(f), " not read: ", conditionMessage(e)); NULL })
    take(d, paste0("IEO MEDITS (", basename(f), ")"))
  }
  if (!length(added)) { message("[Extra MEDITS] nothing to add (every GSA-year in the extra files is already in the JRC data)."); return(empty) }
  log <- rbindlist(log)[order(GSA, Year)]
  message("[Extra MEDITS] hauls added for GSA-years missing from the JRC data: ",
          paste0("GSA ", log$GSA, " ", log$Year, " (", log$n_hauls, ", ", sub(" \\(.*", "", log$source), ")", collapse = "; "))
  list(data = rbindlist(added, use.names = TRUE), log = log)
}

## ------------------------------------------------------------------
## 2. MEDIAS
## ------------------------------------------------------------------
## One total per survey, GSA, year and species: the combined-sex row (C)
## when there is one, otherwise the sum of the sexes (F + M + U).
medias_collapse_sex <- function(dt) {
  dt <- as.data.table(dt)
  if (!"name_survey" %in% names(dt)) dt[, name_survey := ""]
  dt[is.na(sex), sex := "C"]
  dt[, .has_c := any(sex == "C"), by = .(country, gsa, year, species, name_survey)]
  dt[(.has_c & sex == "C") | !.has_c,
     .(total_value = if (all(is.na(total_value))) NA_real_ else sum(total_value, na.rm = TRUE)),
     by = .(country, gsa, year, species, name_survey)]
}

## Extra MEDIAS rows (biomass and abundance) as country/gsa/year/species/
## name_survey/total_value tables, before any comparison with JRC.
extra_medias_raw <- function() {
  tot <- function(d, cols) { m <- as.matrix(d[, ..cols]); m <- suppressWarnings(matrix(as.numeric(m), nrow(m))); m[m < 0] <- NA
    r <- rowSums(m, na.rm = TRUE); r[rowSums(!is.na(m)) == 0] <- NA; r }
  rd <- function(files, kind) rbindlist(lapply(files, function(f) tryCatch({
    d <- read_semicolon_csv(f)
    cols <- if (kind == "biomass_lc") grep("^lengthclass", names(d), value = TRUE)
            else grep(if (kind == "abund") "^agegroup.*abund$" else "^agegroup.*biom$", names(d), value = TRUE)
    if (!length(cols)) stop("no length-class or age-group columns")
    d[, total_value := tot(d, cols)]
    d[, gsa := as.numeric(gsub("\\D", "", area))]
    d[, .(country, gsa, year = as.integer(year), species, sex, name_survey = if ("name_survey" %in% names(d)) name_survey else "", total_value)]
  }, error = function(e) { message("[Extra MEDIAS] ", basename(f), " not read: ", conditionMessage(e)); NULL })), fill = TRUE)
  list(biomass = rd(.extra_files("^BIOMASS[^.]*\\.csv$"), "biomass_lc"),
       abundance = rd(.extra_files("^ABUND_BIOM[^.]*\\.csv$"), "abund"))
}

## ------------------------------------------------------------------
## 3. GFCM STAR workbooks
## ------------------------------------------------------------------
## One STAR workbook -> rows in the GFCM SAF table's columns. Biomass is
## the total stock biomass (TSB) when the workbook gives it, else SSB.
read_star_xlsx <- function(f) {
  md <- readxl::read_excel(f, sheet = "Metadata", col_names = FALSE, .name_repair = "minimal")
  md <- as.data.frame(md, stringsAsFactors = FALSE)
  cell <- function(lbl) { i <- which(trimws(as.character(md[[1]])) == lbl); if (length(i)) as.character(md[i[1], 2]) else NA_character_ }
  i_sp <- which(trimws(as.character(md[[1]])) == "Information")[1]
  species <- trimws(as.character(md[i_sp, 2]))
  gsas <- trimws(as.character(unlist(md[i_sp, 3:min(ncol(md), 20)])))
  gsas <- gsas[!is.na(gsas) & grepl("^[0-9]+(\\.[0-9]+)?$", gsas)]   # drops the template's placeholder text
  ref_year <- suppressWarnings(as.integer(cell("Reference Year")))
  ts <- as.data.frame(readxl::read_excel(f, sheet = "TimeSeries", col_names = FALSE, .name_repair = "minimal"), stringsAsFactors = FALSE)
  hrow <- which(trimws(as.character(ts[[1]])) == "Year")[1]
  if (is.na(hrow)) stop("no 'Year' header in TimeSeries")
  ind <- function(col) { i <- which(trimws(as.character(ts[[col]])) == "Indicator"); if (length(i)) toupper(trimws(as.character(ts[i[1], col + 1]))) else NA_character_ }
  ind1 <- ind(5); ind2 <- ind(8)               # Stock size indicator #1 (col F) and #2 (col I)
  hdr <- trimws(as.character(unlist(ts[hrow, ])))
  body <- ts[(hrow + 1):nrow(ts), , drop = FALSE]
  names(body) <- make.unique(ifelse(is.na(hdr) | !nzchar(hdr), "x", hdr))
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  body <- data.table(year = as.integer(num(body$Year)), s1 = num(body$Stock1), s2 = num(body$Stock2),
                     catches = num(body$Catches), landings = num(body$Landings))
  body <- body[!is.na(year)]
  pick <- if (identical(ind2, "TSB") && !identical(ind1, "TSB")) "s2" else if (ind1 %in% c("TSB", "SSB", "B")) "s1" else if (ind2 %in% c("TSB", "SSB", "B")) "s2" else NA_character_
  if (is.na(pick)) stop("no TSB/SSB stock-size indicator")
  btype <- if (pick == "s1") ind1 else ind2
  out <- body[!is.na(get(pick)), .(year, biomass = get(pick), catches = fifelse(is.na(catches), landings, catches), landings)]
  code <- sub("^STAR_([A-Za-z]{3}).*", "\\1", basename(f))
  g <- paste(gsas, collapse = ",")
  out[, `:=`(source = "GFCM STAR (extra_data)", stock_key = paste0(toupper(code), "_", gsub(",", "_", g)), species = species,
             common_name = NA_character_, gsa = g, subregion = "", biomass_type = btype, landings_flag = NA_character_,
             model = NA_character_, ref_year = ref_year,
             reference = paste0("GFCM STAR workbook: ", species, " in GSA ", gsub(",", "-", g), ", reference year ", ref_year,
                                " (", btype, "; ", basename(f), ")"),
             url = basename(f), use_for_biomass = TRUE, parse_status = "STAR workbook")]
  out
}

## Adds the STAR workbooks in EXTRA_DATA_DIR to a GFCM SAF table (the
## gfcm_saf_stock_assessments_ALL.csv rows) and switches off the rows they
## supersede. Writes star_extra_REVIEW.csv to review_dir.
add_extra_star_assessments <- function(sa, model_gsas, review_dir = NULL) {
  files <- .extra_files("^STAR_.*\\.xlsx$")
  if (!length(files)) return(sa)
  gi <- function(g) floor(suppressWarnings(as.numeric(trimws(unlist(strsplit(as.character(g), "[,;]"))))))
  sa <- as.data.table(copy(sa))
  if (!"use_for_biomass" %in% names(sa)) sa[, use_for_biomass := TRUE]
  if (!"ref_year" %in% names(sa)) sa[, ref_year := NA_integer_]
  if (!"parse_status" %in% names(sa)) sa[, parse_status := NA_character_]
  rev <- list()
  for (f in files) {
    st <- tryCatch(read_star_xlsx(f), error = function(e) { message("[Extra STAR] ", basename(f), " not read: ", conditionMessage(e)); NULL })
    if (is.null(st) || !nrow(st)) next
    g <- gi(st$gsa[1]); sp <- st$species[1]; ry <- st$ref_year[1]
    if (!length(g) || anyNA(g) || !all(g %in% model_gsas)) { rev[[length(rev) + 1]] <- data.table(file = basename(f), species = sp, gsa = st$gsa[1], ref_year = ry, decision = "outside the model GSAs"); next }
    cur <- if (nrow(sa)) sa[species == sp & use_for_biomass %in% TRUE] else sa
    cur <- cur[vapply(as.character(gsa), function(z) any(gi(z) %in% g), logical(1))]
    cur_st <- unique(cur[, .(stock_key, gsa, ref_year, parse_status, source)])
    larger <- cur_st[vapply(as.character(gsa), function(z) !all(gi(z) %in% g), logical(1))]
    older <- nrow(cur_st) == 0 || all(is.na(cur_st$ref_year) | cur_st$ref_year < ry | !(cur_st$parse_status %in% "ok"))
    if (nrow(larger)) {
      st[, use_for_biomass := FALSE]
      dec <- paste0("not used: the stock in use covers more GSAs (", paste(larger$stock_key, collapse = ", "), ")")
    } else if (older) {
      if (nrow(cur_st)) sa[stock_key %in% cur_st$stock_key, use_for_biomass := FALSE]
      dec <- if (nrow(cur_st)) paste0("used; replaces ", paste0(cur_st$stock_key, " (ref. year ", cur_st$ref_year, ", ", cur_st$source, ")", collapse = ", ")) else "used (no other assessment of this stock)"
    } else {
      st[, use_for_biomass := FALSE]
      dec <- paste0("not used: the stock in use is as recent (", paste0(cur_st$stock_key, " ref. year ", cur_st$ref_year, collapse = ", "), ")")
    }
    sa <- rbindlist(list(sa[stock_key != st$stock_key[1] | !(source %in% st$source[1])], st), fill = TRUE)
    rev[[length(rev) + 1]] <- data.table(file = basename(f), species = sp, gsa = st$gsa[1], ref_year = ry, biomass_type = st$biomass_type[1],
                                         first_year = min(st$year), last_year = max(st$year), decision = dec)
  }
  rev <- rbindlist(rev, fill = TRUE)
  if (nrow(rev)) {
    message("[Extra STAR] ", sum(grepl("^used", rev$decision)), " of ", nrow(rev), " STAR workbook(s) used: ",
            paste0(rev$species, " GSA ", rev$gsa, " (", sub(";.*", "", rev$decision), ")", collapse = "; "))
    if (!is.null(review_dir)) tryCatch({ dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
      fwrite(rev, file.path(review_dir, "star_extra_REVIEW.csv")) }, error = function(e) NULL)
  }
  sa
}
