# ------------
# Introduction
# ------------

## Run  cross-lagged model

NAME <- '4_div_approach_panel' 

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, magrittr, here, lavaan, readr, tidyr)

pacman::p_install_version(c("haven", "stringr", "purrr", "gt"),
                          c("2.3.1", "1.4.0", "0.3.4", "0.2.1"))
pacman::p_install_version_gh(c("lukaswallrich/rNuggets"),
                             c("0.1.8"))

source(here("empirical/1_tools/managementFunctions.R"))


#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "empirical/0_data"
pipelinedir <- "empirical/3_pipeline"


AllCasesWhite <- read_rds(here(pipelinedir, "1_scales/out/WhiteCasesWScales.RDS"))

# ------------
# Model
# ------------

xlag.M.model <- ("
           
              ValDiv_2 ~ ValDiv_1 + ApproachBlack_1
              ApproachBlack_2 ~ ValDiv_1 + ApproachBlack_1

             ValDiv_1 ~~ ApproachBlack_1 
           ")


fit_std <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE)

AllCasesWhite$src %<>% haven::as_factor()

fit_2groups <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE, group = "src")
fit_2groups_equal <- sem(xlag.M.model, data=AllCasesWhite, missing = "fiml", std.ov = TRUE, group = "src", group.equal = c("intercepts", "regressions", "residual.covariances"))
anova(fit_2groups, fit_2groups_equal)


write_rds(fit_std, here(pipeline, "out/CrossLaggedModel.RDS"))

res_coefs <- parameterestimates(fit_std, ci = TRUE) %>% filter(op == "~") %>% 
  mutate(beta = paste(sprintf("%.2f", est), rNuggets:::.fmt_ci(ci.lower, ci.upper)), p = rNuggets::fmt_p(pvalue) %>% stringr::str_replace("=", " ")) %>%
  select(lhs, rhs, beta, p)
  
res_coefs %<>% build_wider_spec(names_from = lhs, values_from = c(beta, p)) %>% arrange(desc(lhs)) %>% mutate(.name = paste0(lhs, "_", .value)) %>% 
  pivot_wider_spec(spec = ., data = res_coefs) 

res_coefs$rhs <- c("Valuing diversity (T1)", "Approach intentions (T1)")


res_coefs %>% gt::gt() %>% gt::tab_spanner("Valuing diversity (T2)", 2:3) %>% 
  gt::tab_spanner("Approach intentions (T2)", 4:5) %>% 
  gt::cols_label(
    rhs = "Predictor",
    ValDiv_2_beta = gt::md("&beta; [95% CI]"), 
    ValDiv_2_p = gt::md("*p*"),
    ApproachBlack_2_beta = gt::md("&beta; [95% CI]"), 
    ApproachBlack_2_p = gt::md("*p*")
  ) %>% gt::gtsave(here(pipeline, "out/longit_coefficients.html"))

parameterestimates(fit_std, ci = TRUE) %>% filter(op == "~~", lhs != rhs) %>% 
  mutate(beta = paste(sprintf("%.2f", est), rNuggets:::.fmt_ci(ci.lower, ci.upper)), p = rNuggets::fmt_p(pvalue) %>% stringr::str_replace("=", " ")) %>%
  select(lhs, rhs, beta, p) %>% gt::gt()  %>% 
  gt::cols_label(
    rhs = "", lhs = "",
    beta = gt::md("&beta; [95% CI]"), 
    p = gt::md("*p*")
  ) %>% gt::gtsave(here(pipeline, "out/longit_covars.html"))


