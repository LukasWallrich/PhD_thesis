# ------------
# Introduction
# ------------

NAME <- '3_models' ## Name of the R file goes here (without the file extension!)

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, here, foreign, survey, srvyr, lavaan, lavaan.survey, mice)
pacman::p_load_gh("lukaswallrich/rNuggets")
pacman::p_load_gh("lukaswallrich/timesaveR")

source(here("managementFunctions.R"))

#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "0_data"
pipelinedir <- "2_pipeline"

# ----------------------
# Load data
# ----------------------

imp_long_list <- read_rds(here(pipelinedir, "1_data_prep", "imp_long_list.RDS"))
imp_long_mids <- read_rds(here(pipelinedir, "1_data_prep", "imp_long_mids.RDS"))
ac_svy <- read_rds(here(pipelinedir, "1_data_prep", "ac_svy.RDS"))

# ----------------------
# Regression model
# ----------------------

fitimp <- with(imp_long_mids,
               lm(nb_shareY ~ divprefinstr + for_att + leftright + eastwest +  age  + sex + educN, weights = wt))

fitimp_std <- with(imp_long_mids,
               lm_std(nb_shareY ~ divprefinstr + for_att + leftright + eastwest +  age  + sex + educN, weights = wt))


fit <- ac_svy$variables %>% mutate(across(c(sex, eastwest), factor)) %>% lm(nb_shareY ~ divprefinstr + for_att + leftright + eastwest +  age  + sex + educN, ., weights = ac_svy$variables$wt)
fit_std <- ac_svy$variables %>% mutate(across(c(sex, eastwest), factor)) %>% lm_std(nb_shareY ~ divprefinstr + for_att + leftright + eastwest +  age  + sex + educN, data = ., weights = ac_svy$variables$wt)

coef_names <- c(
  `(Intercept)` = "(Intercept)",
  divprefinstr = "Valuing diversity",
  for_att = "Attitude towards foreigners",
  leftright = "Political orientation (right-wing)",
  age = "Age",
  sex2 = "Gender (female)",
  sexFRAU = "Gender (female)",
  eastwest2 = "Region (East)",
  `eastwestNEUE BUNDESLAENDER` = "Region (East)",
  educN = "Education"
)

report_lm_with_std(mod = list(fitimp, fit), mod_std = list(fitimp_std, fit_std), 
                   model_names = c("Multiple imputation", "Listwise deletion"), 
                   coef_map = coef_names, 
                   filename = here(pipeline, "First OLS model.html"))


# ----------------------
# Mediation model
# ----------------------
imp_long_list_with_dummy <- imp_long_list %>% map(function(x) x$foreign_neighbours %>% forcats::fct_recode(none = "(Almost) no foreigners", some = "Some foreigners", many = "Many foreigners", mostly = "Mostly foreigners") %>% psych::dummy.code() %>% data.frame() %>% select(-none) %>% cbind(x, .)) 

ac_svy_dummy <- ac_svy$variables$foreign_neighbours %>% forcats::fct_recode(none = "(Almost) no foreigners", some = "Some foreigners", many = "Many foreigners", mostly = "Mostly foreigners") %>% psych::dummy.code() %>% data.frame() %>% select(-none) %>% cbind(ac_svy$variables, .)

names_col <- names(imp_long_list_with_dummy[[1]])

imp_long_list_with_dummy_sd <- imp_long_list_with_dummy %>% map(function(x) x %>% mutate_all(as.numeric) %>% mutate_at(vars(-.id, -.imp, -wt), rNuggets::scale_blank))

ac_svy_dummy_sd <- ac_svy_dummy %>% haven::zap_labels() %>% mutate_all(as.numeric) %>% mutate_at(vars(-wt), rNuggets::scale_blank)

model <- (' # direct effect
             nb_shareY ~ some + many + mostly + c1*posCont + c2*negCont + b1*divprefinstr + eastwest +  age  + sex + educN + leftright + b2*for_att
             
           # mediator
             divprefinstr ~ some + many + mostly + a1*posCont + a2*negCont + eastwest +  age  + sex + educN + leftright 
             for_att ~ some + many + mostly + d1*posCont + d2*negCont + eastwest +  age  + sex + educN + leftright 

           # direct effect
             direct_pos := c1
             direct_neg := c2
            
           # indirect effect div (a*b1)
             ind_pos_div := a1*b1
             ind_neg_div := a2*b1

           # indirect effect att (d*b2)
             ind_pos_att := d1*b2
             ind_neg_att := d2*b2


           # total effect
             total_pos := a1*b1 + d1*b2 + c1
             total_neg := a2*b1 + d2*b2 + c2
         ')

mod_complete <-lavaan::sem(model, ac_svy_dummy_sd)

imp_svy <- svydesign(ids = ~1, weights = imp_long_list_with_dummy_sd[[1]]$wt, data = mitools::imputationList(imp_long_list_with_dummy_sd))

mod_weighted <- lavaan.survey::lavaan.survey(mod_complete, imp_svy)


ind_effects <- ("ind_pos_div := a1*b1
             ind_neg_div := a2*b1
             ind_pos_att := d1*b2
             ind_neg_att := d2*b2")

ind_effects %<>% str_split("\n") %>% unlist()

ind_effects<-map(ind_effects, function(x) {
  parts <- str_split(x, ":=") %>% map(str_trim) %>% unlist()
  tibble(name = parts[1], term = parts[2], params = list(str_split(term, "\\*|\\+") %>% map(str_trim)))
})

ind_CIs <- map_dfr(ind_effects, function(x) {
  myCoefs <- coef(mod_weighted)[x$params %>% unlist()]
  myACM <- vcov(mod_weighted)[x$params %>% unlist(), x$params %>% unlist()]
  y<-semTools::monteCarloMed(x$term, myCoefs, ACM = myACM, outputValues = TRUE)
  tibble(name = x$name, est.std = y[[1]]$`Point Estimate`, ci.lower = y[[1]]$`95% Confidence Interval`[1], ci.upper = y[[1]]$`95% Confidence Interval`[2], pvalue=mean(abs(y[[2]]-mean(y[[2]]))>abs(y[[1]]$`Point Estimate`)))
})

res <- parameterestimates(mod_weighted, ci = TRUE) %>%
  filter(str_detect(lhs, ("^direct|^ind|^total"))) %>%
  select(name = lhs, est.std = est, ci.lower, ci.upper, pvalue) %>%
  filter(!name %in% ind_CIs$name) %>%
  rbind(ind_CIs)

res_tbl <- res %>%
  tidyr::separate(name, c("type", "pred", "mod"), fill = "right") %>%
  mutate(type = coalesce(mod, type), fmt = paste(sprintf("%.2f", est.std, 2), sigstars(pvalue), fmt_ci(ci.lower, ci.upper, 2))) %>%
  select(type, pred, fmt) %>%
  spread(type, fmt) %>%
  select(pred, direct, everything(), total) %>%
  gt::gt() %>%
  gt::cols_label(pred = "Measure") %>%
  gt::tab_spanner(gt::md("**Paths** (std. coefficients)"), 2:ncol(.[["_data"]])) %>%
  gt::fmt_markdown(everything()) %>%
  gt::tab_source_note(gt::md(timesaveR:::.make_stars_note()))

res_tbl

res_tbl %>% gt::gtsave(filename = here(pipeline, "mediation.html"))

graph_parameters <- parameterestimates(mod_weighted, ci = TRUE) %>% filter(nchar(label)>0)

write_rds(graph_parameters, here(pipeline, "graph_parameters.RDS"))
