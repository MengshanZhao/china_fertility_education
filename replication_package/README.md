# Replication code

**Paper:** "Family Plans and Planning Policy: The Role of Women's Human Capital in Shaping China's Fertility Trends"

These scripts reproduce every regression table in the paper (Tables 3–9 and Appendix Tables A1–A2) and the regression numbers quoted in the text. They start from the raw CFPS files and province-level statistics. The descriptive tables (Tables 1–2) and the figures are not included.

## Software

- R 4.2.1. Packages (versions used): data.table 1.18.0, dplyr 1.1.4, tidyr 1.3.1, stringr 1.5.1, readxl 1.4.1, haven 2.5.3, fixest 0.11.1, AER 1.2.10, sandwich 3.0.2.

## How to run

Put the raw input files in `data/raw/cfps/` and `data/raw/province/`; the file names are set in scripts 01, 03 and 04. Then, from this folder, run:

```
Rscript run_all.R
```

The full run takes about 20–30 minutes. Most of that time is spent converting the CFPS files and running the 300 bootstrap replications. The scripts run in this order, and each can also be run on its own once the earlier steps have been run:

| Script | What it does | Main output (`data/derived/` or `output/`) |
|---|---|---|
| `code/00_setup.R` | Loads packages and defines relative paths | – |
| `code/01_convert_cfps.R` | Converts the CFPS Stata files to CSV, keeping value labels as text | `CFPS_2018*.csv`, `CFPS_2020*.csv` |
| `code/02_prepare_cfps.R` | Builds women's fertility histories from the 2018–2020 family rosters and merges the individual questionnaires | `cfps_1820_female.csv` |
| `code/03_province_panel.R` | Builds the number of higher education institutions by province and year | `province_college.csv` |
| `code/04_analysis_sample.R` | Builds the analysis sample: the instrument, outcomes, controls and the policy-strictness index | `analysis_objects.rds` |
| `code/05_tables.R` | Estimates all regression tables | `output/*.csv` |

### Output files

| Paper | File in `output/` |
|---|---|
| Table 3 | `table3_main.csv` |
| Table 4, Panels A–D | `table4_iv_diagnostics.csv` |
| Table 5 | `table5_province_correlates.csv` |
| Table 6 | `table6_parity.csv` |
| Tables 7–8 | `table7_8_strictness_interactions.csv` |
| Table 9 | `table9_mechanisms.csv` |
| Table A1 | `tableA1_province_controls.csv` |
| Table A2 | `tableA2_strictness_direct.csv` |
| Numbers in Section 5.5 (Likert item; gap between desired and actual children) | `text_numbers.csv` |

## Estimation details

- **Sample:** women observed in both the 2018 and 2020 CFPS waves and born 1980–1990. Records with a first birth before age 12 or after 50 are dropped. Women born in Xinjiang, Ningxia, Inner Mongolia or Hainan are excluded. Each regression also drops observations with missing values in its own variables.
- **Instrument (PCA):** the number of regular higher education institutions per 10,000 residents in the woman's birth province, measured in her college-entry year.
- **College-entry year:** an approximation of the actual or potential gaokao year, inferred from the year the woman completed her highest level of schooling (`kw2y`):
  - college graduates: `kw2y − 4`
  - 12–15 years of schooling: `kw2y`
  - middle-school graduates: `kw2y + 3`
  - otherwise, or if `kw2y` is missing: birth year + 17
- **Specification:** all 2SLS and OLS models include family socioeconomic status at age 14, birth-cohort fixed effects, birth-province fixed effects and province-specific linear trends. The strictness models (Tables 7, 8 and A2) include cohort and province fixed effects only.
- **Standard errors** are clustered by birth province (24 clusters in the estimation samples).
- **Bootstrap:** the standard deviations reported in brackets come from 300 individual-level case-resampling replications (seed 20240624).
- **Table 4:**
  - Panel B's Anderson–Rubin confidence sets are obtained by inverting cluster-robust Anderson–Rubin tests on a grid.
  - Panel D follows the union-of-confidence-intervals approach of Conley, Hansen and Rossi (2012). The assumed direct effect of PCA equals a fraction (grid step 0.05) of the reduced-form effect of PCA on the outcome. The reported breakdown point is the smallest fraction at which the 95% confidence interval for the education effect includes zero.

## Variable codebook (constructed variables)

| Variable | Definition | CFPS source |
|---|---|---|
| `cfps2020eduy` | Years of schooling. Missing values are filled from the highest level completed (university 16, junior college 15, senior/vocational high school 12, middle school 9, primary 6, none 0). | `cfps2020eduy`, `cfps2020edu` |
| `provd_born` | Birth province: the reported birth province for women born in another province, otherwise the 2018 province of residence | `qa401`, `qa401a_code`, `provcd18` |
| `prov_ratio` | PCA, the instrument (see above) | Province data |
| `college_entry_y` | Year in which PCA is measured (see above) | `kw2y`, `tb1y_a_p` |
| `qv04` | Family socioeconomic status at age 14 (1–5) | `qv04` |
| `no_of_child` | Number of children ever born | Family roster, children's birth years |
| `age_first_birth` | Age at first birth (mothers only) | Mother's and oldest child's birth years |
| `dummy_no_25`, `dummy_no_30` | No child before age 25 / 30 | Constructed |
| `no_of_child_after14` | Children born after 2014 (aged 1–5 in 2020); `before14` = children aged 6 or older in 2020 | Constructed |
| `total_fert_with_intent` | Expected children: current children plus 1 if the woman plans a birth within two years | `qka205` (2020) |
| `disobey` | Births above the policy limit: equals 1 unless the woman has no child, one child, or two children with at least one born after 2014. Women born 1981–1985 who plan a birth within two years are coded 0. | Constructed |
| `ge2_before14`, `b_lt3to3`, etc. | Parity indicators before 2014 and transitions after 2014 (Table 6) | Constructed |
| `q_c`, `q_c_agree` | Agreement that "a woman is complete only when she has children" (1–5); indicator for agree or strongly agree (4–5) | `qm1103` (2020) |
| `qka202` | Ideal number of children | `qka202` (2018) |
| `help` | Grandparents provide childcare | `qf703_a_1`, `qf703_a_2` |
| `govern_work` | Holds an official establishment post (*bianzhi*) in government, a public institution or a state-owned enterprise | `qg2032` (2020) |
| `non_agri_work` | Works in a non-agricultural job | `qg101` (2020) |
| `be_employed` | Employee, not self-employed | `jobclass` (2020) |
| `strict` | Policy-strictness index: rule score (one-child = 2, 1.5-child = 1, two-child = 0) + urban hukou + standardized fine rate at age 10 | `qa301`; `fines.dta` |
