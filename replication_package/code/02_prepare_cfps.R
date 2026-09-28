# Build the women's file: fertility histories from the 2018/2020 family rosters,
# merged with the 2018/2020 individual questionnaires.
# Output: data/derived/cfps_1820_female.csv
if (!exists("ROOT")) source("code/00_setup.R")

rd <- function(f) read.csv(file.path(DERIVED, f), na.strings = "NA")
df_18_fam <- rd("CFPS_2018_fam.csv"); df_20_fam <- rd("CFPS_2020_fam.csv")
df_18     <- rd("CFPS_2018.csv");     df_20     <- rd("CFPS_2020.csv")

# Respondents present in both waves (family roster and individual file)
df  <- inner_join(df_18_fam, df_20_fam, by = "pid")
df1 <- inner_join(df_18, df_20, by = "pid")
# Duplicated variable names: keep the 2020 version (".y") under the plain name
df <- df %>% rename_with(~ gsub("\\.y$", "", .x))
for (k in 1:10) df[[paste0("tb1y_a_c", k)]] <- suppressWarnings(as.numeric(df[[paste0("tb1y_a_c", k)]]))  # children's birth years
df <- df %>% mutate(group = ifelse(tb1y_a_p >= 1970 & tb1y_a_p <= 1975, 1,
                            ifelse(tb1y_a_p > 1975 & tb1y_a_p <= 1980, 2,
                            ifelse(tb1y_a_p > 1980 & tb1y_a_p <= 1985, 3,
                            ifelse(tb1y_a_p > 1985 & tb1y_a_p <= 1990, 4,
                            ifelse(tb1y_a_p > 1990 & tb1y_a_p <= 1995, 5, 6))))))

# Women (2020 labels are in Chinese: "女" = female)
df_female <- df %>% filter(tb2_a_p == "女") %>%
  mutate(tb1y_a_p = suppressWarnings(as.numeric(tb1y_a_p))) %>% drop_na(tb1y_a_p)

# Children's ages in 2020, number of children, age at first birth
age_cols <- paste0("child_", 1:10, "_age")
for (k in 1:10) df_female[[age_cols[k]]] <- 2020 - df_female[[paste0("tb1y_a_c", k)]]
df_female$no_of_child <- rowSums(!is.na(df_female[, age_cols]))
df_female[, age_cols] <- lapply(df_female[, age_cols], function(x) replace(x, is.na(x), 0))
df_female$oldest_child_age   <- ifelse(df_female$no_of_child == 0, 0, apply(df_female[, age_cols], 1, max))
df_female$youngest_child_age <- apply(df_female[, age_cols], 1, function(x) if (all(x == 0)) 0 else min(x[x > 0]))
df_female <- df_female %>% filter(tb1y_a_p >= 1970 & tb1y_a_p <= 2000) %>%
  mutate(age_first_birth = ifelse(no_of_child > 0, 2020 - tb1y_a_p - oldest_child_age, NA))
# Drop biologically implausible records (first birth before 12 or after 50)
df_female <- subset(df_female, age_first_birth >= 12 & age_first_birth <= 50 | is.na(age_first_birth))
df_female <- df_female %>% mutate(no_25 = ifelse(age_first_birth >= 25 | no_of_child == 0, 1, 0),
                                  no_30 = ifelse(age_first_birth >= 30 | no_of_child == 0, 1, 0))
# Births by period: before 2014 (age >= 6 in 2020), 2014-2016 (age 4-5), after 2016 (age 1-3)
ages <- df_female[, age_cols]
df_female$no_of_child_before14 <- rowSums(ages >= 6)
df_female$no_of_child_1416     <- rowSums(ages >= 4 & ages < 6)
df_female$no_of_child_after16  <- rowSums(ages < 4 & ages > 0)

# Merge individual questionnaires
df1 <- df1 %>% rename_with(~ gsub("\\.y$", "", .x))
df2 <- merge(x = df, y = df1, by = "pid"); df2 <- df2[, !duplicated(colnames(df2))]
df2_male <- df2 %>% filter(tb2_a_p == "男") %>%
  mutate(qg6 = suppressWarnings(as.numeric(qg6)), qg12 = suppressWarnings(as.numeric(qg12)), hourly_income = qg12 / (4 * 12 * qg6))
df2_female <- merge(x = df_female, y = df1, by = "pid"); df2_female <- df2_female[, !duplicated(colnames(df2_female))]
df2_female <- df2_female %>%
  mutate(qg6 = suppressWarnings(as.numeric(qg6)), qg12 = suppressWarnings(as.numeric(qg12)), hourly_income = qg12 / (4 * 12 * qg6))
df2_female$husband_hourly_income <- df2_male$hourly_income[match(df2_female$pid_a_s, df2_male$pid)]
df2_female <- df2_female %>% mutate(income = suppressWarnings(as.numeric(income))) %>%
  filter(tb1y_a_p >= 1970 & tb1y_a_p <= 2000)

# Education ("缺失" = missing)
df2_female <- df2_female %>%
  mutate(cfps2020eduy = suppressWarnings(as.numeric(ifelse(cfps2020eduy != "缺失", cfps2020eduy, NA))),
         middle_school = ifelse(cfps2020eduy >= 9 & cfps2020eduy < 12, 1, 0),
         high_school   = ifelse(cfps2020eduy >= 12 & cfps2020eduy < 16, 1, 0),
         college       = ifelse(cfps2020eduy >= 16, 1, 0))

# Preferences, childcare and attitudes ("是" = yes, "否" = no, "不适用" = not applicable)
df2_female <- df2_female %>%
  mutate(qka202     = suppressWarnings(as.numeric(qka202)),                                   # ideal number of children (2018)
         want_kids  = ifelse(qka205 == "是", 1, ifelse(qka205 == "否", 0, NA)),       # plans a birth within two years (2020)
         help       = ifelse(qf703_a_1 == "是" | qf703_a_2 == "是", 1,
                             ifelse(qf703_a_1 == "否" & qf703_a_2 == "否", 0, NA)),   # grandparents provide childcare
         time_child = suppressWarnings(as.numeric(ifelse(qq9013 == "不适用", NA, qq9013))),
         q_a = suppressWarnings(as.numeric(ifelse(qm1101 == "不适用", NA, qm1101))),
         q_b = suppressWarnings(as.numeric(ifelse(qm1102 == "不适用", NA, qm1102))),
         q_c = suppressWarnings(as.numeric(ifelse(qm1103 == "不适用", NA, qm1103))))  # "a woman is complete only with children"
write.csv(df2_female, file.path(DERIVED, "cfps_1820_female.csv"))
cat("women's file:", nrow(df2_female), "rows\n")
