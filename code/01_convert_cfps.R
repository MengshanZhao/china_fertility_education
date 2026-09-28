# Convert the official CFPS Stata files to CSV, keeping value labels as text.
# 2018 files: English-labelled release (ecfps2018*); 2020 files: Chinese-labelled release.
# Missing values are written as empty cells.
if (!exists("ROOT")) source("code/00_setup.R")

cfps_files <- c(CFPS_2018     = "ecfps2018person_202012.dta",
                CFPS_2018_fam = "ecfps2018famconf_202008.dta",
                CFPS_2020     = "cfps2020person_202112.dta",
                CFPS_2020_fam = "cfps2020famconf_202301.dta")
for (nm in names(cfps_files)) {
  d <- as_factor(read_dta(file.path(RAW_CFPS, cfps_files[[nm]])), levels = "default")
  d[] <- lapply(d, function(x) if (is.factor(x)) trimws(as.character(x)) else as.vector(x))
  fwrite(d, file.path(DERIVED, paste0(nm, ".csv")), na = "")
  cat("converted", cfps_files[[nm]], ":", nrow(d), "rows\n")
}
