# Paths and packages. All paths are relative to the replication folder.
pkgs <- c("data.table", "dplyr", "tidyr", "stringr", "readxl", "haven", "fixest", "AER", "sandwich")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Install these R packages first: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(tidyr); library(stringr)
  library(readxl); library(haven); library(fixest); library(AER); library(sandwich)
})
invisible(try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE))
options(scipen = 999, width = 200)

ROOT     <- normalizePath(".")
RAW_CFPS <- file.path(ROOT, "data", "raw", "cfps")
RAW_PROV <- file.path(ROOT, "data", "raw", "province")
DERIVED  <- file.path(ROOT, "data", "derived")
OUT      <- file.path(ROOT, "output")
dir.create(DERIVED, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
