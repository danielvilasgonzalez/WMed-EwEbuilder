## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: check_pipeline.R
## =================================================================
## check_pipeline.R
## Created by: Daniel Vilas
##
## Checks to run before committing or sharing the code:
##   source("check_pipeline.R")                      # quick checks, ~10 s
##   QUICK_RUN <- TRUE; source("check_pipeline.R")    # + a small real run, ~10 min
## (from the scripts/ folder; nothing in the normal outputs is touched)
##
## Quick checks:
##  1. every R file and every R chunk of WMed-EwEbuilder.qmd parses;
##  2. every script that is source()d exists;
##  3. no personal information in the code: home folders (/Users/<name>,
##     C:/Users/<name>), e-mail addresses, passwords or Copernicus logins
##     written in the code;
##  4. lines of a .qmd starting with "(@" (Quarto reads them as an example
##     list; it emptied two items of the page once).
## Quick run (QUICK_RUN = TRUE): 01-05 for a small custom region (a box
## over GSA 6, outputs in out_dir/check_run/), then every log is scanned
## for errors. Finds the problems a full run would, in a fraction of the time.
## =================================================================

.chk_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!file.exists(file.path(.chk_dir, "01_biomass.R"))) .chk_dir <- getwd()
.problems <- character(0)
.say <- function(ok, what) { message(if (ok) "  ok    " else "  FAIL  ", what); if (!ok) .problems <<- c(.problems, what) }

message("1. Parsing the R files and the page")
for (f in list.files(.chk_dir, "\\.R$", full.names = TRUE)) {
  r <- tryCatch({ parse(f); "" }, error = function(e) conditionMessage(e))
  .say(!nzchar(r), paste0(basename(f), if (nzchar(r)) paste(":", r) else ""))
}
for (q in list.files(.chk_dir, "\\.qmd$", full.names = TRUE)) {
  x <- readLines(q, warn = FALSE, encoding = "UTF-8"); st <- grep("^```\\{r", x); en <- grep("^```\\s*$", x); bad <- 0
  for (a in st) {
    b <- min(en[en > a])
    ok <- tryCatch({ parse(text = x[(a + 1):(b - 1)]); TRUE }, error = function(e) FALSE)
    if (!ok) bad <- bad + 1
  }
  .say(bad == 0, sprintf("%s: %d R chunks parse", basename(q), length(st) - bad))
  ex <- grep("^\\s*\\(@", x)
  .say(!length(ex), sprintf("%s: no line starts with '(@'%s", basename(q), if (length(ex)) paste0(" (lines ", paste(ex, collapse = ", "), ")") else ""))
}

message("2. Sourced scripts exist")
srcd <- unique(unlist(lapply(list.files(.chk_dir, "\\.(R|qmd)$", full.names = TRUE), function(f) {
  l <- readLines(f, warn = FALSE)
  unlist(regmatches(l, gregexpr("\"[A-Za-z0-9_.-]+\\.R\"", l)))
})))
srcd <- gsub("\"", "", srcd)
srcd <- srcd[!srcd %in% c("config.R", "config.local.R", "run.R")]
missing <- srcd[!file.exists(file.path(.chk_dir, srcd)) & grepl("^(0[1-6][a-z]?_|lib_|install_|app_|run_pipeline|wmed_paths[.]R$)", srcd)]
.say(!length(missing), paste("scripts named in the code are in scripts/", if (length(missing)) paste(":", paste(missing, collapse = ", ")) else ""))

message("3. No personal information in the code")
pat <- c(home = "(/Users/|/home/|[A-Za-z]:[/\\\\]+Users[/\\\\]+)[A-Za-z][A-Za-z0-9._-]+",
         email = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[a-z]{2,}",
         password = "(PASSWORD|password|passwd)\\s*(=|<-)\\s*[\"'][^\"']+[\"']")
allow <- "pat <- c\\(|password = \"\\(|Users/<name>|Users/<you>|/Users/\\.\\.\\.|noreply@|example\\.(com|org)|@[0-9]|@sec-|data\\.marine|copernicus\\.eu"
for (f in c(list.files(.chk_dir, "\\.(R|qmd|Rmd|md)$", full.names = TRUE), file.path(dirname(.chk_dir), c("README.md", "config.R.example")))) {
  if (!file.exists(f)) next
  l <- readLines(f, warn = FALSE)
  for (k in names(pat)) {
    h <- grep(pat[[k]], l, perl = TRUE)
    h <- h[!grepl(allow, l[h], perl = TRUE)]
    if (length(h)) .say(FALSE, sprintf("%s: possible %s on line(s) %s", basename(f), k, paste(head(h, 5), collapse = ", ")))
  }
}
if (!any(grepl("possible", .problems))) .say(TRUE, "no home folders, e-mails or passwords found")

if (isTRUE(get0("QUICK_RUN", envir = .GlobalEnv, ifnotfound = FALSE))) {
  message("4. Quick run: 01-05 for a box over GSA 6 (outputs in out_dir/check_run)")
  rs <- file.path(tempdir(), "wmed_check_run.R")
  writeLines(c("CLEAN_START <- FALSE; RUN_STEPS <- 1:5",
               "AREA_MODE <- 'custom'; AREA_NAME <- 'check_run'; CUSTOM_AREA_TYPE <- 'bbox'",
               "CUSTOM_BBOX <- c(xmin = 0, xmax = 3.5, ymin = 38.5, ymax = 41.5)",
               "YEAR_ECOPATH <- 1994:1996; TS_YEARS <- 1995:2000",
               sprintf("setwd('%s'); source('run_pipeline_demo.R')", gsub("\\\\", "/", .chk_dir))), rs)
  t0 <- Sys.time()
  st <- system2(file.path(R.home("bin"), "Rscript"), shQuote(rs), stdout = TRUE, stderr = TRUE)
  message(sprintf("  finished in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  od <- sub("^.*outputs in (.*) ===.*$", "\\1", grep("outputs in .* ===", st, value = TRUE)[1])
  logs <- if (!is.na(od) && dir.exists(od)) list.files(od, "_log\\.txt$", full.names = TRUE) else character(0)
  .say(length(logs) >= 4, sprintf("logs written (%d)", length(logs)))
  for (f in logs) {
    l <- readLines(f, warn = FALSE)
    e <- grep("^ERROR|ERROR:|Execution halted|=== RESULT: FAILED", l, value = TRUE)
    .say(!length(e), paste0(basename(f), if (length(e)) paste(":", substr(e[1], 1, 150)) else ": no errors"))
  }
  if (any(grepl("failed|Error", tail(st, 5)))) .say(FALSE, paste("run:", paste(tail(st, 3), collapse = " | ")))
}

message(if (length(.problems)) sprintf("\n%d problem(s) - fix them before committing:\n  %s", length(.problems), paste(.problems, collapse = "\n  "))
        else "\nAll checks passed.")
invisible(.problems)
