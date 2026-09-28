# Analysis sample: women born 1980-1990, instrument (PCA), outcomes, controls, strictness index.
# Output: data/derived/analysis_objects.rds (analysis sample + province-year inputs)
if (!exists("ROOT")) source("code/00_setup.R")

# ---------- individual data ----------
df <- as.data.frame(fread(file.path(DERIVED, "cfps_1820_female.csv"), encoding = "UTF-8"))
df <- df %>% mutate(
  educ = case_when(cfps2018eduy >= 9 & cfps2018eduy < 12 ~ "middle school",
                   cfps2018eduy >= 12 & cfps2018eduy < 16 ~ "high school",
                   cfps2018eduy >= 16 ~ "college", TRUE ~ "less than middle school"),
  no_of_child_after14 = no_of_child_after16 + no_of_child_1416) %>%
  # Birth pattern relative to policy limits; 5 = births above the limit ("disobey")
  mutate(birth = case_when(
    age_first_birth <= 25 & no_of_child == 1 ~ 1,
    age_first_birth >  25 & no_of_child == 1 ~ 2,
    no_of_child_after14 == 1 & no_of_child == 2 ~ 3,
    no_of_child == 2 & no_of_child_after14 == 2 ~ 4,
    no_of_child == 0 ~ 0, TRUE ~ 5)) %>%
  mutate(birth = case_when(group == 3 & want_kids == 1 ~ 2, group == 5 & want_kids == 1 ~ 2, TRUE ~ birth))
# Province of birth (qa401 = 6: born in another province than the current one)
df$provd_born <- ifelse(df$qa401.x == 6, df$qa401a_code.x, df$provcd18)
df2 <- df[!(df$provd_born %in% c("Xinjiang Uygur Autonomous Region", "Ningxia Hui Autonomous", "Inner mongolia", "", "Hainan")), ]

# College-entry year used to date the instrument: approximate actual or potential gaokao year,
# inferred from the year the highest level of schooling was completed (kw2y); age 17 if unavailable.
df2 <- df2 %>% mutate(kw2y_numeric = suppressWarnings(as.numeric(kw2y)),
  college_entry_y = case_when(is.na(kw2y_numeric) ~ tb1y_a_p + 17,
                              educ == "college" ~ kw2y_numeric - 4,
                              educ == "high school" ~ kw2y_numeric,
                              educ == "middle school" ~ kw2y_numeric + 3,
                              TRUE ~ tb1y_a_p + 17))
df2$qv04[!grepl("^[1-5]$", df2$qv04)] <- NA; df2$qv04 <- as.numeric(df2$qv04)   # family SES at age 14 (1-5)
edu_map <- c(16, 12, 9, 6, 15, 0)
names(edu_map) <- c("大学本科", "高中/中专/技校/职高", "初中", "小学", "大专", "文盲/半文盲")
df2 <- df2 %>% mutate(
  cfps2020eduy = ifelse(is.na(cfps2020eduy), edu_map[cfps2020edu], cfps2020eduy),   # years of schooling
  dummy_no_25 = ifelse(age_first_birth > 25 | is.na(age_first_birth), 1, 0),
  dummy_no_30 = ifelse(age_first_birth > 30 | is.na(age_first_birth), 1, 0),
  disobey = ifelse(birth == 5, 1, 0),
  non_agri_work = ifelse(enc2utf8(qg101) == "非农工作", 1, 0),      # non-agricultural work
  be_employed   = ifelse(enc2utf8(jobclass) == "受雇", 1, 0),              # employee (not self-employed)
  govern_work   = ifelse(enc2utf8(qg2032) == "是", 1, 0),                      # establishment post (bianzhi)
  q_c_agree = ifelse(!is.na(q_c) & q_c %in% c(4, 5), 1, ifelse(!is.na(q_c), 0, NA)),
  qka202 = suppressWarnings(as.numeric(qka202)),
  total_fert_with_intent = no_of_child + want_kids,                                  # expected number of children
  trend = as.numeric(tb1y_a_p))

# ---------- province-year inputs ----------
college <- read.csv(file.path(DERIVED, "province_college.csv"))
pc   <- read.csv(file.path(RAW_PROV, "gdp_control_province.csv"), check.names = FALSE); names(pc)[1] <- "Year"
grad <- read.csv(file.path(RAW_PROV, "graduate.csv"))
panel <- pc %>% inner_join(college, by = c("Province_EN" = "Province", "Year")) %>%
  left_join(grad %>% select(Province_EN, Year, high_grad), by = c("Province_EN", "Year")) %>%
  mutate(prov_ratio = no_college / population_all, ln_col = log(no_college), ln_pop = log(population_all), ln_hs = log(high_grad)) %>%
  select(Province_EN, Year, prov_ratio, ln_col, ln_pop, ln_hs)

# PCA: institutions per 10,000 residents in the birth province in the college-entry year
df3 <- df2 %>% left_join(panel %>% select(Province_EN, Year, prov_ratio), by = c("provd_born" = "Province_EN", "college_entry_y" = "Year"))

# Province controls (Table A1), lagged one year where indicated
ctrl <- pc %>% select(Province_EN, Year, gdp_percapita, population_all, second_share, gov_share, tech_share, educ_share) %>%
  left_join(grad %>% select(Province_EN, Year, high_grad), by = c("Province_EN", "Year")) %>%
  mutate(pop_all_million = population_all / 100, high_grad_pop = high_grad / pop_all_million) %>%
  arrange(Province_EN, Year) %>% group_by(Province_EN) %>%
  mutate(lag_gdp_percapita = dplyr::lag(gdp_percapita), lag_pop_all_million = dplyr::lag(pop_all_million),
         high_grad_pop_chg = high_grad_pop - dplyr::lag(high_grad_pop),
         lag_gdp_percapita_chg = lag_gdp_percapita - dplyr::lag(lag_gdp_percapita),
         lag_high_grad_pop_chg = dplyr::lag(high_grad_pop) - dplyr::lag(high_grad_pop, 2),
         lag_second_share = dplyr::lag(second_share), lag_gov_share = dplyr::lag(gov_share),
         lag_tech_share = dplyr::lag(tech_share), lag_educ_share = dplyr::lag(educ_share)) %>% ungroup() %>%
  select(Province_EN, Year, lag_gdp_percapita, lag_pop_all_million, high_grad_pop_chg, lag_gdp_percapita_chg, lag_high_grad_pop_chg,
         lag_second_share, lag_gov_share, lag_tech_share, lag_educ_share)
# High school graduates per 100 residents when the cohort was 12 (1992 population filled with 1993)
pop93 <- pc %>% filter(Year == 1993) %>% select(Province_EN, pop93 = population_all)
hs12 <- grad %>% select(Province_EN, Year, high_grad) %>% left_join(pc %>% select(Province_EN, Year, population_all), by = c("Province_EN", "Year")) %>%
  left_join(pop93, by = "Province_EN") %>%
  mutate(population_all = ifelse(is.na(population_all) & Year < 1993, pop93, population_all),
         hs_pc_age12b = high_grad / (population_all / 100)) %>% select(Province_EN, Year, hs_pc_age12b)

# Birth-control fines and policy rules at age 10 (Ebenstein 2010 replication files)
fines <- read_dta(file.path(RAW_PROV, "fines.dta"))
code_map <- data.frame(code = c(11,12,13,14,15,21,22,23,31,32,33,34,35,36,37,41,42,43,44,45,46,50,51,52,53,54,61,62,63,64,65),
  english_name = c("Beijing","Tianjin","Hebei","Shanxi","Inner Mongolia","Liaoning","Jilin","Heilongjiang","Shanghai","Jiangsu","Zhejiang","Anhui",
                   "Fujian","Jiangxi","Shandong","Henan","Hubei","Hunan","Guangdong","Guangxi Zhuang Autonomous Region","Hainan","Chongqing",
                   "Sichuan","Guizhou","Yunnan","Tibet","Shaanxi","Gansu","Qinghai","Ningxia Hui Autonomous","Xinjiang Uygur Autonomous Region"),
  stringsAsFactors = FALSE)
pfines <- fines %>% left_join(code_map, by = c("province" = "code")) %>% select(english_name, birthyear, fine, policy)

df3 <- df3 %>% left_join(ctrl, by = c("provd_born" = "Province_EN", "college_entry_y" = "Year")) %>%
  left_join(panel %>% select(Province_EN, Year, ln_pop_entry = ln_pop), by = c("provd_born" = "Province_EN", "college_entry_y" = "Year")) %>%
  mutate(y12 = tb1y_a_p + 12, year_10 = tb1y_a_p + 10) %>%
  left_join(hs12, by = c("provd_born" = "Province_EN", "y12" = "Year")) %>%
  left_join(panel %>% select(Province_EN, Year, ln_hs_age12 = ln_hs), by = c("provd_born" = "Province_EN", "y12" = "Year")) %>%
  left_join(pfines, by = c("provd_born" = "english_name", "year_10" = "birthyear")) %>%
  filter(tb1y_a_p >= 1980 & tb1y_a_p <= 1990) %>%
  mutate(rural = ifelse(enc2utf8(qa301) == "农业户口", 1, 0), urban = 1 - rural,    # agricultural hukou
         policy = as.numeric(policy),
         rule_score = case_when(policy == 1 ~ 2, policy == 1.5 ~ 1, policy == 2 ~ 0, TRUE ~ NA_real_),
         fine_z = as.numeric(scale(fine)),
         strict = rule_score + urban + fine_z,                                               # equal-weight strictness index
         before14 = no_of_child - no_of_child_after14,
         ge1_before14 = as.numeric(before14 >= 1), ge2_before14 = as.numeric(before14 >= 2), ge3_before14 = as.numeric(before14 >= 3),
         b_0to1 = ifelse(before14 == 0, as.numeric(no_of_child_after14 >= 1), NA),
         b_0to2_exact = ifelse(before14 == 0, as.numeric(no_of_child == 2), NA),
         b_1to2_exact = ifelse(before14 == 1, as.numeric(no_of_child == 2), NA),
         b_lt3to3 = ifelse(before14 < 3, as.numeric(no_of_child >= 3), NA),
         fertility_gap = qka202 - no_of_child)
# Composite index of lagged province characteristics (Table A1, column 7)
pv <- c("lag_gdp_percapita_chg", "lag_pop_all_million", "lag_high_grad_pop_chg", "lag_second_share", "lag_gov_share", "lag_tech_share", "lag_educ_share")
cc <- complete.cases(df3[, pv]); df3$prov_pca1 <- NA_real_; df3$prov_pca1[cc] <- prcomp(df3[cc, pv], scale. = TRUE)$x[, 1]

saveRDS(list(df3 = df3, pc = pc, grad = grad, college = college), file.path(DERIVED, "analysis_objects.rds"))
cat("analysis sample (women born 1980-1990, before per-model missing values):", nrow(df3), "rows;", n_distinct(df3$provd_born), "birth provinces\n")
