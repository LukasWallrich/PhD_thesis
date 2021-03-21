# ------------
# Introduction
# ------------

# Run meta-analysis

NAME <- "mediation_meta"

# ------------
# Sources
# ------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, here, dplyr, readr, metaSEM, metafor)
pacman::p_load_gh("LukasWallrich/timesaveR")

source(here("1_tools/management_functions.R"))

#Set up pipeline folder if missing
pipeline <- create_pipeline_dir(NAME)
datadir <- "0_data"
pipelinedir <- "3_outputs"

notes <- character()
notes <- c(notes, "Note created:", timestamp(quiet = TRUE))

meta_data <- read_rds(here(pipelinedir, "mediation_data_prep/mediation_meta_data.RDS"))

meta_table <- meta_data %>%  group_by(study) %>%  mutate(N = max(N)) %>% select(-inv_N) %>% tidyr::pivot_wider(names_from = pair, values_from = r)

rename_studies <- c("DE_mediation" = "Ch. 3: Mediation DE",  "India" = "Suppl.: India", "longit" = "Ch. 1: Longitudinal",  "NCS_2018" = "Ch. 4: NCS 2018", "NCS_2019" = "Ch. 5: NCS 2019", "UK_mediation" = "Ch. 2: Mediation UK")
meta_table$study <-  rename_studies[meta_table$study]

meta_table %>% select(study, measure, N, pos_neg, pos_div, neg_div, pos_att, neg_att, div_att) %>% ungroup() %>% arrange(study) %>% gt::gt() %>% gt::tab_spanner("Correlations (Pearson's r)", 4:9) %>% gt::fmt(4:9, fns = fmt_cor) %>% gt::fmt_missing(4:9, missing_text = "") %>% gt_apa_style() %>% gt::gtsave(here(pipeline, "meta_analysis_input.html"))

# ------------
# Two-stage SEM - 1) create cor matrix
# ------------

meta_cor_est <-rma.mv(yi=r, V=inv_N,
                      data=meta_data,
                      random=list(~pair|study, ~pair|measure),
                      struct="DIAG",
                      method="REML", test = "t",
                      mods=~factor(pair)%>%forcats::fct_rev()-1)
summary(meta_cor_est)


# Variance - from summary(meta_cor_est)
#           estim    sqrt  k.lvl  fixed    level 
# tau^2.1    0.0414  0.2035     12     no  div_att 
# tau^2.2    0.0000  0.0000      9     no  neg_att 
# tau^2.3    0.0010  0.0321     10     no  neg_div 
# tau^2.4    0.0125  0.1118      9     no  pos_att 
# tau^2.5    0.0000  0.0000     10     no  pos_div 
# tau^2.6    0.0179  0.1336      7     no  pos_neg 

tau <-tibble(
  level = c("div_att", "neg_att", "neg_div", "pos_att", "pos_div", "pos_neg"),
  tau = c(0.0414, 0.0000, 0.0010, 0.0125, 0.0000, 0.0179)
)

est <- coef(meta_cor_est) %>%
  set_names(names(.) %>% stringr::str_remove("^.*\\(\\)")) %>%
  tibble(est = ., level = names(.)) %>%
  left_join(tau) %>%
  mutate(tau_share = tau / est)

meta_acov <- meta_cor_est$vb

meta_cor_matrix <- vec2symMat(meta_cor_est$b[,1], diag = FALSE)
meta_cor_matrix_print <- vec2symMat(paste(fmt_cor(meta_cor_est$b[,1]),sigstars(meta_cor_est$pval),  "\n", fmt_ci(meta_cor_est$ci.lb, meta_cor_est$ci.ub)), diag = FALSE)

rownames(meta_cor_matrix) <- colnames(meta_cor_matrix) <- c("pos", "neg", "div", "att")
rownames(meta_cor_matrix_print) <- colnames(meta_cor_matrix_print) <- c("pos", "neg", "div", "att")

meta_cor_matrix_print[upper.tri(meta_cor_matrix_print)] <- ""
diag(meta_cor_matrix_print) <- ""
gt::gt(meta_cor_matrix_print %>% as_tibble(rownames = "var")) %>% gt_apa_style() %>% gt::gtsave(here(pipeline, "meta_analysis_correlations.html"))


# ------------
# Run second stage SEM to test mediation
# ------------

model <- "att ~ c1*pos + c2*neg + b*div
          div ~ a1*pos + a2*neg
          pos ~~ 1*pos
          neg ~~ 1*neg
pos ~~ neg"

RAM1 <- lavaan2RAM(model, obs.variables=c("pos", "neg", "div", "att")) #Order obs. var must match dataset

N_tot <- meta_data %>% group_by(study) %>% summarise(n=max(N)) %>% summarise(sum(n)) %>% pull()

meta_SEM <-wls(Cov=meta_cor_matrix,aCov=meta_acov,n=N_tot,
               RAM = RAM1,intervals.type = "LB", cor.analysis=TRUE, diag.constraints = FALSE,
               mx.algebras = list(ind_pos=mxAlgebra(a1*b, name="ind_pos"),
                   ind_neg=mxAlgebra(a2*b, name="ind_neg"),
                   dir_pos=mxAlgebra(c1, name="dir_pos"),
                   dir_neg=mxAlgebra(c2, name="dir_neg")))                                         

summary(meta_SEM)

summary(meta_SEM) %>%
  extract2("mx.algebras") %>%
  as_tibble(rownames = "param") %>%
  tidyr::separate(param, into = c("effect", "valence")) %>%
  group_by(valence) %>%
  mutate(share = Estimate / sum(Estimate), total = sum(Estimate))

graph_params <- summary(meta_SEM) %>% extract2("coefficients") %>% as_tibble(rownames = "label") %>%
  select(label, est = Estimate, ci.lower = lbound, ci.upper = ubound)

#Add p-values (Wald test)
graph_params <- wls(Cov=meta_cor_matrix,aCov=meta_acov,n=N_tot,
               RAM = RAM1, cor.analysis=TRUE, diag.constraints = FALSE) %>% 
  summary() %>% extract2("coefficients") %>%
  as_tibble(rownames = "label") %>% select(label, pvalue = `Pr(>|z|)`) %>%
  left_join(graph_params)

readr::write_rds(graph_params, here(pipeline, "graph_params.RDS"))


####
### India moderation?

meta_data$India <- FALSE
meta_data$India[meta_data$study == "India"] <- TRUE


meta_cor_est_India <-rma.mv(yi=r, V=inv_N,
                      data=meta_data,
                      random=list(~pair|study, ~pair|measure),
                      struct="DIAG",
                      method="REML", test = "t", btt = "India",
                      mods=~factor(pair)%>%forcats::fct_rev()+India-1)
summary(meta_cor_est_India)

meta_cor_est_India <-rma.mv(yi=r, V=inv_N,
                      data=meta_data %>% filter(stringr::str_detect(pair, "div")),
                      random=list(~pair|study, ~pair|measure),
                      struct="DIAG",
                      method="REML", test = "t", btt = "India",
                      mods=~factor(pair)%>%forcats::fct_rev()+India-1)

  summary(meta_cor_est_India)

# ------------
# Save outputs
# ------------


#writeLines(notes, here(pipeline, "notes.txt"))
