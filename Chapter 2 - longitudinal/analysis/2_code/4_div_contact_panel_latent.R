# ------------
# Introduction
# ------------

## Run  cross-lagged model

NAME <- '4_div_contact_panel_latent' 

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, magrittr, here, lavaan, readr, tidyr)

pacman::p_install_version(c("haven", "stringr", "purrr", "gt"),
                          c("2.3.1", "1.4.0", "0.3.4", "0.2.1"))
pacman::p_load_gh("lukaswallrich/timesaveR")

source(here("1_tools/managementFunctions.R"))


#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "0_data"
pipelinedir <- "3_pipeline"

notes <- character()
notes <- take_note("Note created:", timestamp(quiet = TRUE))

AllCasesWhite <- read_rds(here(pipelinedir, "1_scales/out/WhiteCasesWScales.RDS"))

# ------------
# Model
# ------------

AllCasesWhite %<>% select(-c(ValDiv_2, BlackPOS_2, BlackNEG_2, 
                             ValDiv_1, BlackPOS_1, BlackNEG_1))

xlag.measurement.model <- ("
              ValDiv_1 =~ NA*ValDiv1 + ValDiv2 + ValDiv3  
              ValDiv_2 =~ NA*ValDiv1_2 + ValDiv2_2 + ValDiv3_2
              BlackNEG_1 =~ NA*BlackRidiculed + BlackUnwanted + BlackVerbAbused + BlackIntimidated + BlackThreatened
              BlackNEG_2 =~ NA*BlackRidiculed_2 + BlackUnwanted_2 + BlackVerbAbused_2 + BlackIntimidated_2 + BlackThreatened_2
              BlackPOS_1 =~ NA*BlackComplimented + BlackBefriended + BlackWelcome + BlackSupported + BlackHelped
              BlackPOS_2 =~ NA*BlackComplimented_2 + BlackBefriended_2 + BlackWelcome_2 + BlackSupported_2 + BlackHelped_2

              ValDiv_2 ~~ 1*ValDiv_2
              BlackPOS_2 ~~ 1*BlackPOS_2
              BlackNEG_2 ~~ 1*BlackNEG_2
              ValDiv_1 ~~ 1*ValDiv_1
              BlackPOS_1 ~~ 1*BlackPOS_1
              BlackNEG_1 ~~ 1*BlackNEG_1
              
              ValDiv1_2 ~~ ValDiv1
              ValDiv2_2 ~~ ValDiv2
              ValDiv3_2 ~~ ValDiv3
              
              BlackComplimented ~~ BlackComplimented_2
              BlackBefriended ~~ BlackBefriended_2
              BlackWelcome ~~ BlackWelcome_2
              BlackSupported ~~ BlackSupported_2
              BlackHelped ~~ BlackHelped_2
              BlackRidiculed ~~ BlackRidiculed_2
              BlackUnwanted ~~ BlackUnwanted_2
              BlackVerbAbused ~~ BlackVerbAbused_2
              BlackIntimidated ~~ BlackIntimidated_2
              BlackThreatened ~~ BlackThreatened_2
              
                           ")

fit_measurement <- sem(xlag.measurement.model, data=AllCasesWhite, missing = "fiml", std.lv = TRUE)

fitm <- fitmeasures(fit_measurement, fit.measures = c("cfi", "rmsea", "srmr", "chisq", "df"))

take_note("Fitmeasures measurement model\n",
          fitm %>% c(chisq_df = fitm["chisq"]/fitm["df"]) %>% round(3))


xlag.measurement.model_invariant <- ("
              ValDiv_1 =~   v1*ValDiv1 +   v2*ValDiv2 +   v3*ValDiv3  
              ValDiv_2 =~   v1*ValDiv1_2 + v2*ValDiv2_2 + v3*ValDiv3_2
              BlackPOS_1 =~ p1*BlackComplimented +   p2*BlackBefriended +   p3*BlackWelcome +   p4*BlackSupported +   p5*BlackHelped
              BlackPOS_2 =~ p1*BlackComplimented_2 + p2*BlackBefriended_2 + p3*BlackWelcome_2 + p4*BlackSupported_2 + p5*BlackHelped_2
              BlackNEG_1 =~ n1*BlackRidiculed +   n2*BlackUnwanted +   n3*BlackVerbAbused +   n4*BlackIntimidated +   n5*BlackThreatened
              BlackNEG_2 =~ n1*BlackRidiculed_2 + n2*BlackUnwanted_2 + n3*BlackVerbAbused_2 + n4*BlackIntimidated_2 + n5*BlackThreatened_2

              ValDiv_2 ~~ 1*ValDiv_2
              BlackPOS_2 ~~ 1*BlackPOS_2
              BlackNEG_2 ~~ 1*BlackNEG_2
              ValDiv_1 ~~ 1*ValDiv_1
              BlackPOS_1 ~~ 1*BlackPOS_1
              BlackNEG_1 ~~ 1*BlackNEG_1
              
              ValDiv1_2 ~~ ValDiv1
              ValDiv2_2 ~~ ValDiv2
              ValDiv3_2 ~~ ValDiv3
              
              BlackComplimented ~~ BlackComplimented_2
              BlackBefriended ~~ BlackBefriended_2
              BlackWelcome ~~ BlackWelcome_2
              BlackSupported ~~ BlackSupported_2
              BlackHelped ~~ BlackHelped_2
              BlackRidiculed ~~ BlackRidiculed_2
              BlackUnwanted ~~ BlackUnwanted_2
              BlackVerbAbused ~~ BlackVerbAbused_2
              BlackIntimidated ~~ BlackIntimidated_2
              BlackThreatened ~~ BlackThreatened_2
              
                           ")

fit_measurement_invariant <- sem(xlag.measurement.model_invariant, data=AllCasesWhite, missing = "fiml", std.lv = TRUE)

fit_comparison <- list(free = fit_measurement, invariant = fit_measurement_invariant) %>% 
  purrr::map_dfr(~fitmeasures(.x, fit.measures = c("aic", "bic", "cfi")), .id = "model") %>%
  mutate(across(2:4, as.numeric))

fit_comparison %<>% select(-model) %>% summarise_all(diff) %>% 
  mutate(model = "delta") %>%
  bind_rows(fit_comparison, .)

an <- anova(fit_measurement, fit_measurement_invariant) 
fit_comparison %>% gt::gt() %>% gt::tab_source_note(glue_warn("Model comparison: chisq({an$`Df diff`[2]}) = {round_(an$`Chisq diff`[2])}, p {fmt_p(an$`Pr(>Chisq)`[2])}")) %>%
  gt_apa_style() %>% gt::fmt_number(2:3, decimals = 1) %>%  gt::fmt_number(4, decimals = 3) %>% gt::gtsave(here(pipeline, "out", "measurement_invariance_tests.html"))

xlag.M.model <- ("
              ValDiv_2 =~ ValDiv1_2 + ValDiv2_2 + ValDiv3_2
              BlackPOS_2 =~ BlackComplimented + BlackBefriended + BlackWelcome + BlackSupported + BlackHelped
              BlackNEG_2 =~ BlackRidiculed_2 + BlackUnwanted_2 + BlackVerbAbused_2 + BlackIntimidated_2 + BlackThreatened_2
              ValDiv_1 =~ ValDiv1 + ValDiv2 + ValDiv3  
              BlackPOS_1 =~ BlackComplimented_2 + BlackBefriended_2 + BlackWelcome_2 + BlackSupported_2 + BlackHelped_2
              BlackNEG_1 =~ BlackRidiculed + BlackUnwanted + BlackVerbAbused + BlackIntimidated + BlackThreatened
              
              ValDiv_2 ~ ValDiv_1 + BlackPOS_1 + BlackNEG_1
              BlackPOS_2 ~ ValDiv_1 + BlackPOS_1 + BlackNEG_1 
              BlackNEG_2 ~ ValDiv_1 + BlackPOS_1 + BlackNEG_1


             ValDiv_1 ~~ BlackPOS_1 + BlackNEG_1 
             BlackNEG_1 ~~ BlackPOS_1 
             
           ")


AllCasesWhite$src %<>% haven::as_factor()

fit_2groups <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE, group = "src")
fit_2groups_equal <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE, group = "src", group.equal = c("intercepts", "regressions", "residual.covariances"))

anova(fit_2groups, fit_2groups_equal) %>% timesaveR:::report_anova() %>% take_note("Model comparison two-groups:", .)

#Add parameters to compare effects
xlag.M.model <- ("
              ValDiv_2 =~ ValDiv1_2 + ValDiv2_2 + ValDiv3_2
              BlackPOS_2 =~ BlackComplimented + BlackBefriended + BlackWelcome + BlackSupported + BlackHelped
              BlackNEG_2 =~ BlackRidiculed_2 + BlackUnwanted_2 + BlackVerbAbused_2 + BlackIntimidated_2 + BlackThreatened_2
              ValDiv_1 =~ ValDiv1 + ValDiv2 + ValDiv3  
              BlackPOS_1 =~ BlackComplimented_2 + BlackBefriended_2 + BlackWelcome_2 + BlackSupported_2 + BlackHelped_2
              BlackNEG_1 =~ BlackRidiculed + BlackUnwanted + BlackVerbAbused + BlackIntimidated + BlackThreatened
              
              ValDiv_2 ~ ValDiv_1 + posdiv*BlackPOS_1 + negdiv*BlackNEG_1
              BlackPOS_2 ~ divpos*ValDiv_1 + BlackPOS_1 + BlackNEG_1 
              BlackNEG_2 ~ divneg*ValDiv_1 + BlackPOS_1 + BlackNEG_1


             ValDiv_1 ~~ BlackPOS_1 + BlackNEG_1 
             BlackNEG_1 ~~ BlackPOS_1 
             
             #Differences in effect size
             posdiv_vs_divpos := posdiv - divpos
             negdiv_vs_divneg := negdiv - divneg
             
           ")


fit_std <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE)


write_rds(fit_std, here(pipeline, "out/CrossLaggedModel.RDS"))

parameterestimates(fit_std) %>%
  filter(op == ":=") %>%
  transmute(fmt = glue::glue("{lhs}: diff: {round_(est, 3)}, z = {round_(z)},  p {fmt_p(pvalue)}, {fmt_ci(ci.lower, ci.upper)}\n\n")) %>%
  pull() %>%
  take_note("Tests for differences in directional paths\n", .)

res_coefs <- parameterestimates(fit_std, ci = TRUE) %>% filter(op == "~") %>% 
  mutate(beta = paste(round_(est), fmt_ci(ci.lower, ci.upper)), p = fmt_p(pvalue) %>% stringr::str_replace("=", " ")) %>%
  select(lhs, rhs, beta, p)

res_coefs %<>% build_wider_spec(names_from = lhs, values_from = c(beta, p)) %>% arrange(desc(lhs)) %>% mutate(.name = paste0(lhs, "_", .value)) %>% 
  pivot_wider_spec(spec = ., data = res_coefs) 

res_coefs$rhs <- c("Valuing diversity (T1)", "Positive contact (T1)", "Negative contact (T1)")


res_coefs %>% gt::gt() %>% gt::tab_spanner("Valuing diversity (T2)", 2:3) %>% 
  gt::tab_spanner("Positive contact (T2)", 4:5) %>% 
  gt::tab_spanner("Negative contact (T2)", 6:7) %>% 
  gt::cols_label(
    rhs = "Predictor",
    ValDiv_2_beta = gt::md("&beta; [95% CI]"), 
    ValDiv_2_p = gt::md("*p*"),
    BlackPOS_2_beta = gt::md("&beta; [95% CI]"), 
    BlackPOS_2_p = gt::md("*p*"),
    BlackNEG_2_beta = gt::md("&beta; [95% CI]"), 
    BlackNEG_2_p = gt::md("*p*")
  ) %>% gt::gtsave(here(pipeline, "out/longit_coefficients.html"))

parameterestimates(fit_std, ci = TRUE) %>% filter(op == "~~", lhs != rhs) %>% 
  mutate(beta = paste(round_(est), fmt_ci(ci.lower, ci.upper)), p = fmt_p(pvalue) %>% stringr::str_replace("=", " ")) %>%
  select(lhs, rhs, beta, p) %>% gt::gt() %>% gt::tab_row_group("T1", rows=1:3, others = "T2")  %>% 
  gt::cols_label(
    rhs = "", lhs = "",
    beta = gt::md("&beta; [95% CI]"), 
    p = gt::md("*p*")
  ) %>% gt::gtsave(here(pipeline, "out/longit_covars.html"))

writeLines(notes, here(pipeline, "out/notes.txt")) 

