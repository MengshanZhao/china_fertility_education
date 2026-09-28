# Runs the full replication from raw inputs to all regression tables.
# Usage: from this folder, run   Rscript run_all.R
# Raw inputs are read from data/raw/cfps/ and data/raw/province/ (file names are set in scripts 01, 03, 04).
source("code/00_setup.R")
source("code/01_convert_cfps.R")      # CFPS .dta -> labelled .csv
source("code/02_prepare_cfps.R")      # women's fertility histories, 2018-2020 panel
source("code/03_province_panel.R")    # higher-education institutions by province-year
source("code/04_analysis_sample.R")   # analysis sample, instrument, outcomes, controls
source("code/05_tables.R")            # Tables 3-9, A1, A2 and numbers quoted in the text
