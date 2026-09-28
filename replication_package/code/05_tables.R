# All regression tables. Standard errors are clustered by province of birth throughout.
# Outputs (output/): table3_main.csv, table4_iv_diagnostics.csv, table5_province_correlates.csv,
#   table6_parity.csv, table7_8_strictness_interactions.csv, table9_mechanisms.csv,
#   tableA1_province_controls.csv, tableA2_strictness_direct.csv, text_numbers.csv
# Runtime: about 10-15 minutes (300 bootstrap replications for Tables 3 and 9).
if (!exists("ROOT")) source("code/00_setup.R")
obj <- readRDS(file.path(DERIVED, "analysis_objects.rds"))
df3 <- obj$df3; pc <- obj$pc; grad <- obj$grad; college <- obj$college

FE <- "tb1y_a_p + provd_born[trend]"        # birth-cohort FE, birth-province FE, province-specific linear trends
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))))
fmt <- function(b, s, p) sprintf("%.3f%s (%.3f)", b, stars(p), s)
samp <- function(y, extra = character(0), Z = "prov_ratio") df3[complete.cases(df3[, c(y, "cfps2020eduy", "qv04", Z, extra)]), ]

# ---------- 2SLS and OLS on the same sample; optional individual-level bootstrap ----------
est <- function(y, extra = character(0), boot = 0) {
  d <- samp(y, extra); rhs <- paste(c("qv04", extra), collapse = " + ")
  f_iv <- as.formula(sprintf("%s ~ %s | %s | cfps2020eduy ~ prov_ratio", y, rhs, FE))
  f_ol <- as.formula(sprintf("%s ~ cfps2020eduy + %s | %s", y, rhs, FE))
  iv <- feols(f_iv, d, cluster = ~provd_born); ol <- feols(f_ol, d, cluster = ~provd_born)
  fs  <- feols(as.formula(sprintf("cfps2020eduy ~ prov_ratio + %s | %s", rhs, FE)), d, cluster = ~provd_born)
  fs0 <- feols(as.formula(sprintf("cfps2020eduy ~ %s | %s", rhs, FE)), d)
  k <- "fit_cfps2020eduy"
  out <- data.frame(outcome = y, N = nobs(iv), mean = mean(d[[y]]),
    IV = coef(iv)[k], IV_se = se(iv)[k], IV_p = pvalue(iv)[k],
    OLS = coef(ol)["cfps2020eduy"], OLS_se = se(ol)["cfps2020eduy"], OLS_p = pvalue(ol)["cfps2020eduy"],
    first_stage = coef(fs)["prov_ratio"], first_stage_se = se(fs)["prov_ratio"],
    F_cluster = (coef(fs)["prov_ratio"] / se(fs)["prov_ratio"])^2,
    partial_R2 = 1 - sum(resid(fs)^2) / sum(resid(fs0)^2), IV_boot_sd = NA_real_, OLS_boot_sd = NA_real_, row.names = NULL)
  if (boot > 0) {
    set.seed(20240624); bi <- bo <- rep(NA_real_, boot)
    for (r in seq_len(boot)) {
      db <- d[sample.int(nrow(d), replace = TRUE), ]
      bi[r] <- tryCatch(coef(feols(f_iv, db, vcov = "iid", notes = FALSE))[k], error = function(e) NA)
      bo[r] <- tryCatch(coef(feols(f_ol, db, vcov = "iid", notes = FALSE))["cfps2020eduy"], error = function(e) NA)
    }
    out$IV_boot_sd <- sd(bi, na.rm = TRUE); out$OLS_boot_sd <- sd(bo, na.rm = TRUE)
  }
  out
}
pretty <- function(t) t %>% mutate(IV_pct_of_mean = 100 * IV / mean, IV_cell = fmt(IV, IV_se, IV_p), OLS_cell = fmt(OLS, OLS_se, OLS_p))

# ---------- Table 3: main results ----------
T3 <- pretty(bind_rows(lapply(c("age_first_birth", "dummy_no_25", "dummy_no_30", "no_of_child", "total_fert_with_intent", "disobey"), est, boot = 300)))
write.csv(T3, file.path(OUT, "table3_main.csv"), row.names = FALSE)
cat("\nTable 3\n"); print(T3 %>% select(outcome, N, IV_cell, IV_boot_sd, OLS_cell, OLS_boot_sd, F_cluster, partial_R2), row.names = FALSE)

# ---------- Table 9: mechanisms ----------
T9 <- pretty(bind_rows(lapply(c("q_c_agree", "qka202", "help", "govern_work", "non_agri_work", "be_employed"), est, boot = 300)))
write.csv(T9, file.path(OUT, "table9_mechanisms.csv"), row.names = FALSE)
cat("\nTable 9\n"); print(T9 %>% select(outcome, N, mean, IV_cell, IV_boot_sd, OLS_cell, OLS_boot_sd, F_cluster), row.names = FALSE)

# ---------- Table 6: parity before and after the 2014 relaxation ----------
T6 <- pretty(bind_rows(lapply(c("ge1_before14", "ge2_before14", "ge3_before14", "b_0to1", "b_0to2_exact", "b_1to2_exact", "b_lt3to3"), est)))
write.csv(T6, file.path(OUT, "table6_parity.csv"), row.names = FALSE)
cat("\nTable 6\n"); print(T6 %>% select(outcome, N, mean, IV_cell, OLS_cell, IV_pct_of_mean, F_cluster), row.names = FALSE)

# ---------- Table 4: first stage, weak-IV-robust inference, exclusion checks ----------
ar_set <- function(y, b, s, level = 0.95) {          # Anderson-Rubin set by test inversion, cluster-robust
  d <- samp(y); grid <- seq(b - 12 * s, b + 12 * s, length.out = 4801); crit <- qnorm(1 - (1 - level) / 2)
  acc <- vapply(grid, function(g) { d$yg <- d[[y]] - g * d$cfps2020eduy
    m <- feols(as.formula(sprintf("yg ~ prov_ratio + qv04 | %s", FE)), d, cluster = ~provd_born, notes = FALSE)
    abs(coef(m)["prov_ratio"] / se(m)["prov_ratio"]) < crit }, logical(1))
  a <- grid[acc]
  c(ar_lo = min(a), ar_hi = max(a), unbounded = (min(a) == min(grid)) | (max(a) == max(grid)))
}
T4B <- bind_rows(T3 %>% filter(outcome != "dummy_no_30"), T6 %>% filter(outcome %in% c("ge2_before14", "b_lt3to3")))
T4B <- T4B %>% mutate(ci_lo = IV - qnorm(.975) * IV_se, ci_hi = IV + qnorm(.975) * IV_se)
T4B <- bind_cols(T4B, as.data.frame(t(mapply(ar_set, T4B$outcome, T4B$IV, T4B$IV_se))))
dA <- samp("age_first_birth")
pc_c <- feols(as.formula(sprintf("age_first_birth ~ cfps2020eduy + prov_ratio + qv04 | %s", FE)), dA, cluster = ~provd_born)

# Panel D: Conley, Hansen and Rossi (2012) union-of-confidence-intervals. gamma = frac x reduced-form effect of PCA;
# breakdown = smallest |frac| (grid step 0.05) at which the 95% CI for the education effect includes zero.
CTRL <- "qv04 + as.character(tb1y_a_p) + provd_born + provd_born:as.numeric(tb1y_a_p)"
conley_breakdown <- function(y) {
  d <- df3[complete.cases(df3[, c(y, "cfps2020eduy", "qv04", "prov_ratio")]), ]
  delta <- unname(coef(lm(as.formula(paste(y, "~ prov_ratio +", CTRL)), data = d))["prov_ratio"])
  res <- t(sapply(seq(-1, 1, by = 0.05), function(f) {
    d$ytilde <- d[[y]] - f * delta * d$prov_ratio
    m <- ivreg(as.formula(paste("ytilde ~ cfps2020eduy +", CTRL, "| prov_ratio +", CTRL)), data = d)
    s <- unname(sqrt(diag(vcovCL(m, cluster = m$model[["provd_born"]], type = "HC1")))["cfps2020eduy"])
    b <- unname(coef(m)["cfps2020eduy"]); c(frac = f, lo = b - qnorm(.975) * s, hi = b + qnorm(.975) * s) }))
  inc0 <- res[, "lo"] <= 0 & res[, "hi"] >= 0; cand <- res[inc0, "frac"]
  data.frame(outcome = y, reduced_form = delta, breakdown_frac = cand[which.min(abs(cand))])
}
T4D <- bind_rows(lapply(c("age_first_birth", "no_of_child", "disobey"), conley_breakdown))
fsA <- T3 %>% filter(outcome == "no_of_child")
T4 <- bind_rows(
  data.frame(panel = "A", item = c("first_stage", "first_stage_se", "F_cluster"), value = c(fsA$first_stage, fsA$first_stage_se, fsA$F_cluster)),
  T4B %>% transmute(panel = "B", item = outcome, value = IV, ci_lo, ci_hi, ar_lo, ar_hi),
  data.frame(panel = "C", item = c("education", "PCA"), value = coef(pc_c)[1:2], se = se(pc_c)[1:2], p = pvalue(pc_c)[1:2]),
  T4D %>% transmute(panel = "D", item = outcome, value = breakdown_frac, reduced_form))
write.csv(T4, file.path(OUT, "table4_iv_diagnostics.csv"), row.names = FALSE)
cat("\nTable 4\n"); print(T4, row.names = FALSE)

# ---------- Table A1: province-level controls ----------
specs <- list(`(1)` = character(0), `(2)` = "lag_gdp_percapita", `(3)` = "lag_pop_all_million", `(4)` = "high_grad_pop_chg",
              `(5)` = "hs_pc_age12b", `(6)` = c("ln_hs_age12", "ln_pop_entry"), `(7)` = "prov_pca1")
A1 <- bind_rows(lapply(names(specs), function(s) bind_rows(lapply(
  c("age_first_birth", "dummy_no_25", "no_of_child", "no_of_child_after14", "disobey", "govern_work"),
  function(y) est(y, specs[[s]]) %>% mutate(spec = s)))))
A1 <- pretty(A1); write.csv(A1, file.path(OUT, "tableA1_province_controls.csv"), row.names = FALSE)
cat("\nTable A1\n"); print(A1 %>% select(spec, outcome, IV_cell) %>% pivot_wider(names_from = spec, values_from = IV_cell) %>% as.data.frame(), row.names = FALSE)
print(A1 %>% filter(outcome == "no_of_child") %>% select(spec, N, F_cluster), row.names = FALSE)

# ---------- Tables 7-8 (strictness interactions) and A2 (direct effect): cohort + province FE ----------
FE2 <- "tb1y_a_p + provd_born"
S <- bind_rows(lapply(c("age_first_birth", "dummy_no_25", "dummy_no_30", "no_of_child", "total_fert_with_intent", "disobey", "q_c_agree", "qka202", "help"), function(y) {
  d  <- df3[complete.cases(df3[, c(y, "cfps2020eduy", "prov_ratio", "strict", "qv04")]), ]
  ol <- feols(as.formula(sprintf("%s ~ cfps2020eduy + strict + cfps2020eduy:strict + qv04 | %s", y, FE2)), d, cluster = ~provd_born)
  iv <- feols(as.formula(sprintf("%s ~ strict + qv04 | %s | cfps2020eduy + cfps2020eduy:strict ~ prov_ratio + prov_ratio:strict", y, FE2)), d, cluster = ~provd_born)
  fs <- feols(as.formula(sprintf("cfps2020eduy ~ prov_ratio + prov_ratio:strict + strict + qv04 | %s", FE2)), d, cluster = ~provd_born)
  kk <- c("prov_ratio", "prov_ratio:strict"); bq <- coef(fs)[kk]
  dd <- df3[complete.cases(df3[, c(y, "cfps2020eduy", "strict", "qv04")]), ]
  di <- feols(as.formula(sprintf("%s ~ strict + cfps2020eduy + qv04 | %s", y, FE2)), dd, cluster = ~provd_born)
  g <- function(m, k) fmt(coef(m)[k], se(m)[k], pvalue(m)[k])
  data.frame(outcome = y, N = nobs(iv), F_joint_cluster = as.numeric(t(bq) %*% solve(vcov(fs)[kk, kk]) %*% bq) / 2,
             T7_OLS_interaction = g(ol, "cfps2020eduy:strict"), T7_OLS_strictness = g(ol, "strict"), T7_OLS_education = g(ol, "cfps2020eduy"),
             T8_IV_interaction = g(iv, "fit_cfps2020eduy:strict"), T8_IV_strictness = g(iv, "strict"), T8_IV_education = g(iv, "fit_cfps2020eduy"),
             A2_N = nobs(di), A2_strictness = g(di, "strict"), A2_education = g(di, "cfps2020eduy"))
}))
write.csv(S %>% select(outcome, N, F_joint_cluster, starts_with("T7"), starts_with("T8")), file.path(OUT, "table7_8_strictness_interactions.csv"), row.names = FALSE)
write.csv(S %>% select(outcome, A2_N, A2_strictness, A2_education), file.path(OUT, "tableA2_strictness_direct.csv"), row.names = FALSE)
cat("\nTables 7, 8, A2\n"); print(S, row.names = FALSE)

# ---------- Table 5: PCA and province characteristics (province-year panel, 1998-2010) ----------
pop93 <- pc %>% filter(Year == 1993) %>% select(Province_EN, pop93 = population_all)
hs_lag5 <- grad %>% select(Province_EN, Year, high_grad) %>% left_join(pc %>% select(Province_EN, Year, population_all), by = c("Province_EN", "Year")) %>%
  left_join(pop93, by = "Province_EN") %>% mutate(population_all = ifelse(is.na(population_all) & Year < 1993, pop93, population_all),
  hs_pc = high_grad / (population_all / 100), Year = Year + 5) %>% select(Province_EN, Year, hs_pc_lag5 = hs_pc)
p5 <- pc %>% inner_join(college, by = c("Province_EN" = "Province", "Year")) %>% left_join(hs_lag5, by = c("Province_EN", "Year")) %>%
  mutate(college_ratio = no_college / population_all, pop_all_million = population_all / 100, trend = Year) %>%
  arrange(Province_EN, Year) %>% group_by(Province_EN) %>%
  mutate(across(c(gdp_percapita, pop_all_million, second_share, gov_share, educ_share, tech_share), ~ dplyr::lag(.x), .names = "lag_{.col}")) %>%
  ungroup() %>% filter(Year >= 1998, Year <= 2010)
v5 <- c("gdp_percapita", "pop_all_million", "second_share", "gov_share", "educ_share", "tech_share",
        paste0("lag_", c("gdp_percapita", "pop_all_million", "second_share", "gov_share", "educ_share", "tech_share")), "hs_pc_lag5")
T5 <- bind_rows(lapply(v5, function(x) { m <- feols(as.formula(sprintf("college_ratio ~ %s | Province_EN[trend] + Year", x)), p5, cluster = ~Province_EN)
  data.frame(variable = x, coef = coef(m)[x], se = se(m)[x], p = pvalue(m)[x], N = nobs(m)) }))
write.csv(T5, file.path(OUT, "table5_province_correlates.csv"), row.names = FALSE)
cat("\nTable 5\n"); print(T5, row.names = FALSE)

# ---------- Numbers quoted in the text ----------
TX <- pretty(bind_rows(lapply(c("q_c", "fertility_gap"), est)))
write.csv(TX, file.path(OUT, "text_numbers.csv"), row.names = FALSE)
cat("\nText: Likert item and desired-minus-actual gap\n"); print(TX %>% select(outcome, N, IV_cell, OLS_cell), row.names = FALSE)
