## =============================================================================
## lib_ewe_export.R
## Created by: Daniel Vilas
##
## The Ecopath inputs of a run (ecopath_ecosim_inputs.xlsx) as the tables the
## EwE software reads, with the same layout, column names and file names as
## EwE's own csv export ("<model>-Basic_input.csv", ...):
##   Basic_input, Diet_composition, Landings, Discards, Definition_of_fleets,
##   Pedigree, Traits (+ Stanzas: the inputs of EwE's multi-stanza form)
## as csv files (one per table) and/or one xlsx with a sheet per table.
##
## Layout rules copied from EwE's export:
##   - first column = group number, then "Group name"; multi-stanza groups
##     get a header row with the stanza name and no number (Basic_input,
##     Landings, Discards) and one row for the whole stanza (Traits)
##   - Diet_composition: prey rows (all groups) x predator columns
##     (consumers, by number), then the rows Import, Sum and (1 - Sum)
##   - Landings / Discards: one column per fleet "<fleet> (t/km^2/year)",
##     Total, and a Sum row
##   - csv: decimal separator "," (computer set to Spanish, French,
##     Italian...) with ";" between columns, or "." (English settings) with
##     ","; text in double quotes, UTF-8, CRLF line ends, no BOM (EwE reads
##     the first column name as it is). Pick the one of the computer that
##     runs EwE.
## Stanza groups: FGs whose names differ only by a life-stage word (juv.,
## juvenile, adult, recruit, sub-adult) and have consecutive numbers, e.g.
## "European hake juv." + "European hake adult" -> stanza "European hake".
## =============================================================================

.ewe_num <- function(x, dec = ",", digits = 6) {
  out <- vapply(seq_along(x), function(i) if (is.finite(x[i])) format(signif(x[i], digits), scientific = FALSE, drop0trailing = TRUE, trim = TRUE) else "", "")
  if (dec != ".") out <- gsub(".", dec, out, fixed = TRUE)
  out
}
.ewe_fix <- function(x, d = 3, dec = ",") {
  out <- ifelse(is.finite(x), formatC(x, format = "f", digits = d), "")
  if (dec != ".") out <- gsub(".", dec, out, fixed = TRUE)
  out
}
## one csv line: text in double quotes, numbers (with the chosen decimal
## separator) and empty cells as they are
.ewe_line <- function(v, sep = ";", dec = ",") {
  v <- ifelse(is.na(v), "", as.character(v))
  num <- grepl(if (dec == ",") "^-?[0-9]+(,[0-9]+)?([eE][-+]?[0-9]+)?$" else "^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$", v)
  q <- nzchar(v) & !num
  v[q] <- paste0("\"", gsub("\"", "\"\"", v[q]), "\"")
  paste(v, collapse = sep)
}
.ewe_write_csv <- function(tab, file, dec = ",") {
  sep <- if (dec == ",") ";" else ","
  lines <- c(.ewe_line(names(tab), sep, dec), apply(as.matrix(tab), 1, .ewe_line, sep = sep, dec = dec))
  con <- file(file, open = "wb"); on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
}

## stanza groups from the FG names
ewe_stanza_groups <- function(fg) {
  stage <- "(?i)[ _-]*\\b(juv(enile)?s?|adults?|recruits?|sub-?adults?|larva[e]?|young)\\b\\.?\\s*$"
  base <- trimws(sub(stage, "", fg$FG_name, perl = TRUE))
  is_st <- grepl(stage, fg$FG_name, perl = TRUE)
  out <- data.frame(FG_num = fg$FG_num, stanza = NA_character_, first = FALSE)
  for (b in unique(base[is_st])) {
    k <- which(is_st & base == b)
    if (length(k) >= 2 && all(diff(sort(fg$FG_num[k])) == 1)) {
      out$stanza[k] <- b
      out$first[k[which.min(fg$FG_num[k])]] <- TRUE
    }
  }
  out
}

## group type: consumer / producer / detritus
.ewe_type <- function(name, qb) {
  ifelse(grepl("detritus|discard", name, ignore.case = TRUE), "detritus",
    ifelse((is.finite(qb) & qb == 0) | grepl("phytoplankton|seagrass|posidonia|cymodocea|macroalga|algae|zoostera|zostera", name, ignore.case = TRUE),
           "producer", "consumer"))
}

## Fleet costs for EwE's "Definition of fleets" (% of the value of the
## catch): Fixed cost = capital costs (depreciation, interest);
## Effort-related cost = intermediate consumption (energy, repair and
## maintenance, other variable and non-variable costs); Sailing-related
## cost = crew / labour costs; Profit = net profit margin. One row per
## fleet type (regex on the fleet name, e.g. "Bottom trawls"), optionally
## per Country ("*" = any); the first matching row is used; a fleet with
## no row keeps EwE's defaults (0 / 40 / 40 / 20). The file
## data/fisheries/fleet_costs_ewe.csv (Country, Fleet_pattern,
## Fixed_cost_pct, Effort_cost_pct, Sailing_cost_pct, Profit_pct,
## Reference) replaces the built-in rows below.
## Built-in rows: STECF (2023) Annual Economic Report on the EU fishing
## fleet (STECF 23-07), section 3.5 Mediterranean Sea, 2021:
##   small-scale coastal fleet: GVA/revenue 65%, gross profit margin 23%,
##   net profit margin 10.7% -> labour 42, capital 12.3, intermediate 35;
##   whole Mediterranean fleet: revenue EUR 1.49 bn, GVA 0.89 bn, gross
##   profit 389 M, net profit 191 M -> intermediate 40.3, labour 33.6,
##   capital 13.3, net profit 12.8 (used for the large-scale gears: the
##   report gives no cost split by gear for the Mediterranean).
EWE_FLEET_COSTS_DEFAULT <- data.frame(
  Country = "*",
  Fleet_pattern = c("Recreational", "Artisanal|small.scale", "."),
  Fixed_cost_pct = c(0, 12.3, 13.3), Effort_cost_pct = c(40, 35.0, 40.3),
  Sailing_cost_pct = c(40, 42.0, 33.6), Profit_pct = c(20, 10.7, 12.8),
  Reference = c("EwE default (no cost data for recreational fishing)",
                "STECF 2023 AER (STECF 23-07), Mediterranean small-scale coastal fleet 2021: GVA/revenue 65%, gross profit margin 23%, net profit margin 10.7%",
                "STECF 2023 AER (STECF 23-07), whole Mediterranean fleet 2021: revenue EUR 1.49 bn, GVA 0.89 bn, gross profit 389 M, net profit 191 M"),
  stringsAsFactors = FALSE)

ewe_fleet_costs <- function(fleets, cost_file = NULL) {
  tab <- EWE_FLEET_COSTS_DEFAULT
  if (!is.null(cost_file) && !is.na(cost_file) && file.exists(cost_file)) {
    x <- tryCatch(as.data.frame(get0("fread", envir = .GlobalEnv, mode = "function", ifnotfound = data.table::fread)(cost_file)),
                  error = function(e) NULL)
    if (!is.null(x) && all(c("Fleet_pattern", "Fixed_cost_pct", "Effort_cost_pct", "Sailing_cost_pct", "Profit_pct") %in% names(x))) {
      if (!"Country" %in% names(x)) x$Country <- "*"
      if (!"Reference" %in% names(x)) x$Reference <- basename(cost_file)
      tab <- x[, names(EWE_FLEET_COSTS_DEFAULT)]
    }
  }
  ## fleet names "<Country> - <type>" or "<code> <type>" (EwE fleets, e.g. "SPA bottom trawlers")
  .codes <- c(ALG = "Algeria", FRA = "France", ITA = "Italy", MOR = "Morocco", SPA = "Spain", TUN = "Tunisia")
  dash <- grepl(" - ", fleets)
  ctry <- ifelse(dash, trimws(sub(" - .*$", "", fleets)), ifelse(sub(" .*$", "", fleets) %in% names(.codes), .codes[sub(" .*$", "", fleets)], ""))
  ftyp <- ifelse(dash, trimws(sub("^[^-]* - ", "", fleets)), ifelse(nzchar(ctry), sub("^[A-Z]{3} ", "", fleets), fleets))
  out <- t(vapply(seq_along(fleets), function(i) {
    k <- which((tab$Country %in% c("*", "", NA) | tolower(tab$Country) == tolower(ctry[i])) &
               vapply(tab$Fleet_pattern, function(p) grepl(p, ftyp[i], ignore.case = TRUE), logical(1)))
    if (!length(k)) return(c(0, 40, 40, 20, NA))
    k <- k[order(!(tolower(tab$Country[k]) == tolower(ctry[i])))][1]   # a country row before a "*" row
    c(as.numeric(tab[k, 3:6]), k)
  }, numeric(5)))
  data.frame(Fleet = fleets, Fixed = out[, 1], Effort = out[, 2], Sailing = out[, 3], Profit = out[, 4],
             Reference = ifelse(is.na(out[, 5]), "EwE default", tab$Reference[out[, 5]]), stringsAsFactors = FALSE)
}

## Build the EwE tables (a named list of data.frames, all columns text)
ewe_tables <- function(wb_path, dec = ",", fleet_cost_file = NULL) {
  rd <- function(s) tryCatch(openxlsx::read.xlsx(wb_path, sheet = s, check.names = FALSE, sep.names = " "), error = function(e) NULL)
  fg <- rd("FGs"); if (is.null(fg)) stop("no FGs sheet in ", wb_path)
  fg <- fg[order(fg$FG_num), c("FG_num", "FG_name")]
  bb <- rd("Ecopath_B"); pq <- rd("Ecopath_PBQB"); dm <- rd("Ecopath_diet")
  ll <- rd("Ecopath_L"); di <- rd("Ecopath_Di"); fl <- rd("Fleets"); pe <- rd("Pedigree"); tr <- rd("Ecopath_traits")
  g <- function(tab, col) if (!is.null(tab) && col %in% names(tab)) suppressWarnings(as.numeric(tab[[col]][match(fg$FG_num, tab$FG_num)])) else rep(NA_real_, nrow(fg))
  B <- g(bb, "Biomass"); EE <- g(bb, "EE_input"); PB <- g(pq, "PB_FG"); QB <- g(pq, "QB_FG")
  type <- .ewe_type(fg$FG_name, QB)
  st <- ewe_stanza_groups(fg)
  n <- nrow(fg); blank <- rep("", n)
  ## multi-stanza FGs: Total mortality = Z of the stanza, P/B left blank
  ## (EwE takes P/B = Z), Q/B of each stanza (Ecopath_multistanza, 03)
  ms <- rd("Ecopath_multistanza")
  Zs <- if (!is.null(ms) && all(c("FG_num", "Z") %in% names(ms))) suppressWarnings(as.numeric(ms$Z[match(fg$FG_num, ms$FG_num)])) else rep(NA_real_, n)
  if (!is.null(ms) && "QB_stanza" %in% names(ms)) { q2 <- suppressWarnings(as.numeric(ms$QB_stanza[match(fg$FG_num, ms$FG_num)])); QB <- ifelse(is.finite(q2), q2, QB) }
  is_ms <- !is.na(st$stanza) & is.finite(Zs)
  ## insert the stanza header rows (no number) before the first stanza FG
  with_stanza <- function(tab) {
    rows <- list()
    for (i in seq_len(n)) {
      if (isTRUE(st$first[i])) { h <- tab[i, , drop = FALSE]; h[1, ] <- ""; h[1, 2] <- st$stanza[i]; rows[[length(rows) + 1]] <- h }
      rows[[length(rows) + 1]] <- tab[i, , drop = FALSE]
    }
    do.call(rbind, rows)
  }
  out <- list()

  ## Basic input
  cons <- type == "consumer"
  bi <- data.frame(
    a = as.character(fg$FG_num), `Group name` = fg$FG_name,
    `Hab area (proportion)` = .ewe_fix(rep(1, n), 3, dec),
    `Biomass in habitat area (t/km^2)` = .ewe_num(ifelse(type == "detritus" & !grepl("detritus", fg$FG_name, ignore.case = TRUE), NA, B), dec),
    `Total mortality (/year)` = ifelse(is_ms, .ewe_num(Zs, dec), ""),
    `Production / biomass (/year)` = .ewe_num(ifelse(type == "detritus" | is_ms, NA, PB), dec),
    `Consumption / biomass (/year)` = .ewe_num(ifelse(cons, QB, NA), dec),
    `Ecotrophic Efficiency` = .ewe_num(ifelse(type == "detritus", NA, EE), dec),
    `Other mortality` = blank, `Production / consumption` = blank,
    `Unassim. consumption` = ifelse(cons, .ewe_fix(rep(0.2, n), 3, dec), ""),
    `Detritus import (t/km^2/year)` = ifelse(type == "detritus", .ewe_fix(rep(0, n), 3, dec), ""),
    check.names = FALSE, stringsAsFactors = FALSE)
  names(bi)[1] <- ""
  out$Basic_input <- with_stanza(bi)

  ## Diet composition: prey rows x consumer columns
  pred <- fg$FG_num[cons]
  D <- matrix(NA_real_, n, length(pred))
  if (!is.null(dm)) {
    pc <- grep("^Predator_", names(dm), value = TRUE)
    pnum <- suppressWarnings(as.integer(sub("^Predator_([0-9]+)_.*$", "\\1", pc)))
    for (j in seq_along(pred)) {
      k <- which(pnum == pred[j]); if (!length(k)) next
      D[, j] <- suppressWarnings(as.numeric(dm[[pc[k[1]]]][match(fg$FG_num, dm$Prey_FG_num)]))
    }
  }
  dd <- data.frame(a = as.character(fg$FG_num), b = fg$FG_name, stringsAsFactors = FALSE)
  for (j in seq_along(pred)) dd[[as.character(pred[j])]] <- ifelse(is.finite(D[, j]) & D[, j] > 0, .ewe_num(D[, j], dec), "")
  imp <- rep(0, length(pred)); sm <- colSums(D, na.rm = TRUE)
  tail3 <- data.frame(a = c("", "", ""), b = c("Import", "Sum", "(1 - Sum)"), stringsAsFactors = FALSE)
  for (j in seq_along(pred)) tail3[[as.character(pred[j])]] <- .ewe_fix(round(c(imp[j], sm[j], 1 - sm[j]), 3) + 0, 3, dec)
  dd <- rbind(dd, tail3); names(dd)[1:2] <- c("", "Prey \\ predator")
  out$Diet_composition <- dd

  ## Landings and Discards
  fleets <- if (!is.null(fl)) fl$Fleet_name[order(fl$Fleet_num)] else setdiff(names(ll), c("FG_num", "FG_name", "Reference", "Total"))
  catch_tab <- function(x) {
    M <- sapply(fleets, function(f) if (!is.null(x) && f %in% names(x)) suppressWarnings(as.numeric(x[[f]][match(fg$FG_num, x$FG_num)])) else rep(NA_real_, n))
    M <- matrix(M, nrow = n)
    t <- data.frame(a = as.character(fg$FG_num), b = fg$FG_name, stringsAsFactors = FALSE)
    for (j in seq_along(fleets)) t[[paste0(fleets[j], " (t/km^2/year)")]] <- ifelse(is.finite(M[, j]) & M[, j] > 0, .ewe_num(M[, j], dec), "")
    tot <- rowSums(M, na.rm = TRUE)
    t$Total <- ifelse(tot > 0, .ewe_num(tot, dec), .ewe_fix(0, 3, dec))
    t <- with_stanza(t)
    cs <- c(colSums(M, na.rm = TRUE), sum(M, na.rm = TRUE))
    s <- c(as.character(n + 2), "Sum", ifelse(cs > 0, .ewe_num(cs, dec), .ewe_fix(0, 3, dec)))
    t <- rbind(t, setNames(as.list(s), names(t)))
    names(t)[1:2] <- c("", "Group name"); t
  }
  out$Landings <- catch_tab(ll)
  out$Discards <- catch_tab(di)

  ## Definition of fleets: costs per fleet from the literature (ewe_fleet_costs)
  k <- length(fleets); fc <- ewe_fleet_costs(fleets, fleet_cost_file)
  out$Definition_of_fleets <- setNames(data.frame(as.character(seq_len(k)), fleets, .ewe_fix(fc$Fixed, 3, dec), .ewe_fix(fc$Effort, 2, dec),
                                                  .ewe_fix(fc$Sailing, 2, dec), .ewe_fix(fc$Profit, 3, dec),
                                                  .ewe_fix(fc$Fixed + fc$Effort + fc$Sailing + fc$Profit, 3, dec), stringsAsFactors = FALSE),
                                       c("", "Name", "Fixed cost (%)", "EffortCost (%)", "Cost of sailing (%)", "Profit (%)", "Total value (%)"))
  attr(out, "fleet_cost_refs") <- fc

  ## Pedigree (index 0-1 of each input; -1 = not set)
  pg <- function(col) { v <- g(pe, col); ifelse(is.finite(v), .ewe_num(v, dec), "-1") }
  pcatch <- { a <- g(pe, "L"); b <- g(pe, "Di"); ifelse(is.finite(a), a, b) }
  out$Pedigree <- setNames(data.frame(as.character(fg$FG_num), fg$FG_name, pg("B"),
                                      ifelse(type == "detritus", "", pg("PB")), ifelse(cons, pg("QB"), ""),
                                      ifelse(cons, pg("Diet"), ""), ifelse(is.finite(pcatch), .ewe_num(pcatch, dec), ""), stringsAsFactors = FALSE),
                           c("", "Group name", "Biomass in habitat area", "Production / biomass", "Consumption / biomass", "Diet", "Catch"))

  ## Traits: one header row per group ("n: name"; one row per stanza), its species below
  tcols <- c(Organism = "Organism", Ecology = "Ecology", `Occurrence status` = "Occurrence_status", `Biomass contribution` = "Biomass_contribution",
             `Catch contribution` = "Catch_contribution", `IUCN conservation status` = "IUCN_conservation_status", `Exploitation status` = "Exploitation_status",
             `Vulnerability index (0-100)` = "Vulnerability_index", `Mean length (cm)` = "Mean_length", `Max length (cm)` = "Max_length",
             `Mean weight (kg)` = "Mean_weight", `Mean life span (year)` = "Mean_lifespan_years")
  numc <- c("Biomass_contribution", "Catch_contribution", "Vulnerability_index", "Mean_length", "Max_length", "Mean_weight", "Mean_lifespan_years")
  rows <- list(); done_stanza <- character(0)
  sp_of <- list()
  if (!is.null(tr) && "Species" %in% names(tr)) {
    hdr <- grepl("^[0-9]+: ", tr$Species) & is.na(tr$row_index)
    cur <- NA_integer_
    for (i in seq_len(nrow(tr))) {
      if (hdr[i]) { cur <- as.integer(sub(":.*$", "", tr$Species[i])); next }
      if (!is.na(cur)) sp_of[[as.character(cur)]] <- rbind(sp_of[[as.character(cur)]], tr[i, , drop = FALSE])
    }
  }
  mk <- function(first, species, src = NULL) {
    v <- c(first, species, vapply(tcols, function(cc) {
      if (is.null(src) || !cc %in% names(src)) return("")
      x <- src[[cc]]
      if (cc %in% numc) { xn <- suppressWarnings(as.numeric(x)); if (is.finite(xn)) .ewe_num(xn, dec) else "" } else if (is.na(x)) "" else as.character(x)
    }, ""))
    as.list(v)
  }
  for (i in seq_len(n)) {
    s <- st$stanza[i]
    if (!is.na(s)) { if (s %in% done_stanza) next; done_stanza <- c(done_stanza, s); rows[[length(rows) + 1]] <- mk("", s) }
    else rows[[length(rows) + 1]] <- mk("", paste0(fg$FG_num[i], ": ", fg$FG_name[i]))
    sp <- sp_of[[as.character(fg$FG_num[i])]]
    if (!is.null(sp)) for (r in seq_len(nrow(sp))) rows[[length(rows) + 1]] <- mk(as.character(sp$row_index[r]), sp$Species[r], sp[r, , drop = FALSE])
  }
  trt <- as.data.frame(do.call(rbind, lapply(rows, unlist)), stringsAsFactors = FALSE)
  names(trt) <- c("", "Species", names(tcols))
  out$Traits <- trt
  ## Stanzas: what EwE's multi-stanza form asks for, one row per stanza
  ## (stanza group, its life stages, age at start, Z, leading stanza,
  ## K, Wmat/Winf, BA/B, recruitment power 1). Not an EwE export table:
  ## typed into Edit multi-stanza groups.
  if (!is.null(ms) && all(c("FG_num", "Stanza", "Z") %in% names(ms))) {
    m <- ms[!is.na(suppressWarnings(as.numeric(ms$FG_num))), , drop = FALSE]
    if (nrow(m)) {
      num <- function(col) if (col %in% names(m)) suppressWarnings(as.numeric(m[[col]])) else rep(NA_real_, nrow(m))
      ba <- if ("BA_B_reference" %in% names(m)) num("BA_B_reference") else rep(NA_real_, nrow(m))
      grp <- st$stanza[match(as.numeric(m$FG_num), fg$FG_num)]
      out$Stanzas <- data.frame(`Stanza group` = ifelse(is.na(grp), m$Species, grp), `Group` = as.character(m$FG_num),
                                `Group name` = m$FG_name, `Leading stanza` = ifelse(m$Leading_stanza %in% TRUE, "yes", ""),
                                `Age start (months)` = .ewe_num(num("Age_start_months"), dec), `Z (/year)` = .ewe_num(num("Z"), dec),
                                `Q/B (/year)` = .ewe_num(num("QB_stanza"), dec), `K (VBGF, /year)` = .ewe_num(num("K_VBGF"), dec),
                                `Wmat/Winf` = .ewe_num(num("Wmat_Winf"), dec), `BA/B (/year)` = .ewe_num(ba, dec),
                                `Recruitment power` = .ewe_fix(rep(1, nrow(m)), 3, dec),
                                check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  out
}

## Write the tables: format "csv" (one file per table), "xlsx" (one workbook,
## a sheet per table) or "both". Returns the paths written.
write_ewe_tables <- function(wb_path, out_dir, model_name = "Western_Mediterranean_Sea", format = c("csv", "xlsx", "both"), dec = ",",
                             fleet_cost_file = NULL) {
  format <- match.arg(format)
  tabs <- ewe_tables(wb_path, dec = if (format == "xlsx") "." else dec, fleet_cost_file = fleet_cost_file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  model_name <- gsub("[^A-Za-z0-9_.-]+", "_", model_name)
  paths <- character(0)
  if (format %in% c("csv", "both")) {
    tc <- if (format == "both" && dec != ".") ewe_tables(wb_path, dec = dec, fleet_cost_file = fleet_cost_file) else tabs
    for (nm in names(tc)) {
      f <- file.path(out_dir, paste0(model_name, "-", nm, ".csv")); .ewe_write_csv(tc[[nm]], f, dec = dec); paths <- c(paths, f)
    }
  }
  if (format %in% c("xlsx", "both")) {
    tx <- if (format == "both") ewe_tables(wb_path, dec = ".", fleet_cost_file = fleet_cost_file) else tabs
    wb <- openxlsx::createWorkbook()
    for (nm in names(tx)) {
      sh <- gsub("_", " ", nm); openxlsx::addWorksheet(wb, sh)
      x <- tx[[nm]]
      ## numbers as numbers in Excel (text that parses as a number)
      for (j in seq_along(x)) { v <- suppressWarnings(as.numeric(x[[j]])); if (j > 1 && any(nzchar(x[[j]])) && all(is.finite(v[nzchar(x[[j]])]))) x[[j]] <- ifelse(nzchar(x[[j]]), v, NA) }
      if (is.character(x[[1]])) { v1 <- suppressWarnings(as.numeric(x[[1]])); x[[1]] <- ifelse(nzchar(x[[1]]), v1, NA) }
      openxlsx::writeData(wb, sh, x, keepNA = FALSE)
      openxlsx::setColWidths(wb, sh, cols = 2, widths = 34)
    }
    fr <- attr(tx, "fleet_cost_refs")
    if (!is.null(fr)) { openxlsx::addWorksheet(wb, "Fleet cost sources"); openxlsx::writeData(wb, "Fleet cost sources", fr) }
    f <- file.path(out_dir, paste0(model_name, "-EwE_input.xlsx"))
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE); paths <- c(paths, f)
  }
  paths
}
