# ------------
# Introduction
# ------------

# Create datafile for mediation meta-analysis

NAME <- "1_mediation_data_prep"

# ------------
# Sources
# ------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, here, dplyr, haven, readxl, readr, stringr, lavaan)
pacman::p_load_gh("LukasWallrich/timesaveR")

source(here("1_tools/management_functions.R"))

#Set up pipeline folder if missing
pipeline <- create_pipeline_dir(NAME)
datadir <- "0_data"
pipelinedir <- "3_outputs"

notes <- character()
notes <- c(notes, "Note created:", timestamp(quiet = TRUE))


# ------------
# Create dataset
# ------------

# From chapters 2 to 4

meta_data <- tibble::tribble(
  ~study, ~measure, ~pair, ~r, ~N,
  "longit", "T1", "pos_div", .22, 211,
  "longit", "T1", "pos_neg", .16, 211,
  "longit", "T1", "neg_div", -.02, 211,
  "longit", "T2", "pos_div", .33, 211,
  "longit", "T2", "pos_neg", -.05, 211,
  "longit", "T2", "neg_div", -.28, 211,
  "UK_mediation", "only", "neg_div",  -.30, 224,
  "UK_mediation", "only", "pos_div", .43, 224,
  "UK_mediation", "only", "pos_neg", -.01, 224,
  "UK_mediation", "affective", "pos_att", -.38, 224,
  "UK_mediation", "cognitive", "pos_att", -.20, 224,
  "UK_mediation", "affective", "div_att", -.44, 224,
  "UK_mediation", "cognitive", "div_att", -.55, 224,
  "UK_mediation", "affective", "neg_att",  .18, 224,
  "UK_mediation", "cognitive", "neg_att",  .21, 224,
  "DE_mediation", "only", "pos_div", .34, 2618,
  "DE_mediation", "only", "neg_div",  -.17, 2618,
  "DE_mediation", "only", "div_att",  -.52, 2618,
  "DE_mediation", "only", "pos_neg",  -.27, 2618,
  "DE_mediation", "only", "pos_att",  -.42, 2618,
  "DE_mediation", "only", "neg_att",  .27, 2618
)

# ------------
# Calculate attitude correlations longit
# ------------

#Longit: add attitudes

df_longit <- haven::read_spss("../Chapter 2 - longitudinal/analysis/0_data/full/CombinedData_all_vars.sav")

df_longit %<>% rename(
  ValDiv1 = DivVal4, ValDiv2 = DivVal5, ValDiv3 = DivVal6,
  ValDiv1_2 = DivVal4_2, ValDiv2_2 = DivVal5_2, ValDiv3_2 = DivVal6_2
)

df_longit %<>% mutate_at(c("Ethn"), haven::as_factor)

df_longit %<>% dplyr::filter(Ethn == 'White British' | Ethn == 'Other White')

scalesList <- list("BlackPOS_1" = c("BlackComplimented", "BlackBefriended", "BlackWelcome",
                                    "BlackSupported", "BlackHelped"),
                   "BlackPOS_2" =  c("BlackComplimented_2", "BlackBefriended_2",
                                     "BlackWelcome_2", "BlackSupported_2", "BlackHelped_2"),
                   "BlackNEG_1" = c("BlackRidiculed", "BlackUnwanted", "BlackVerbAbused",
                                    "BlackIntimidated", "BlackThreatened"),
                   "BlackNEG_2" = c("BlackRidiculed_2", "BlackUnwanted_2", "BlackVerbAbused_2",
                                    "BlackIntimidated_2", "BlackThreatened_2"),
                   "ValDiv_1" = c("ValDiv1", "ValDiv2", "ValDiv3"),
                   "ValDiv_2" = c("ValDiv1_2", "ValDiv2_2", "ValDiv3_2"),
                   "Prej_2" = c("Prej1", "Prej2")) #Prej only collected at T2


scales <- make_scales(df_longit, scalesList, print_hist = F, print_desc = F)              

df_longit <- cbind(df_longit, scales$scores)


cors <- df_longit %>%
  select(Prej_2, BlackPOS_2, BlackNEG_2, BlackPOS_1, BlackNEG_1, ValDiv_1, ValDiv_2, ThermBlack_2, ThermBlack_1 = ThermBlack) %>%
  cor_matrix(missing = "fiml")

cors <- tidy(cors)
  

meta_data <- cors %>% select(column1, column2, estimate, n) %>%
  filter(str_detect(column1, "Prej|Therm"), !str_detect(column2, "Prej|Therm")) %>% 
  mutate(time1 = str_sub(column1, start = -1), time2 = str_sub(column2, start = -1), 
         across(c(column1, column2), str_remove_all, "_[1-2]?|Black|Val"), 
         across(c(column1, column2), str_to_lower)) %>%
  filter(time1 == time2) %>%
  transmute(study = "longit", measure = paste0("T", time1, "_", column1), pair = paste0(column2, "_att"), 
            r = ifelse(str_detect(column1, "therm"), -1*estimate, estimate), #Recode so that pos correlation indicates greater prejudice
            N = n) %>%
  rbind(meta_data, .)


# ------------
# Calculate India correlations
# ------------

df_India <- haven::read_sav(here::here(datadir, "IndiaDatawScales.sav"))
cors <- df_India %>% select(DivVal, DivValInstr,ctPosFreq, ctNegFreq, Attitudes) %>% mutate_all(as.numeric) %>% cor_matrix() %>% tidy()


meta_data <- cors %>%
  mutate(
    pair = paste(column1, column2, sep = "_"),
    measure = ifelse(str_detect(pair, "Instr"), "divval_instr", "divval_pref"),
    pair = pair %>% str_remove_all("Val|ct|Freq|itudes|Instr") %>% str_to_lower(),
    study = "India",
    r = ifelse(str_detect(pair, "att"), -1*estimate, estimate)
  ) %>%
  filter(pair %in% meta_data$pair) %>% rename(N = n) %>%
  select(all_of(names(meta_data))) %>%
  rbind(meta_data, .)
                                
# ------------
# Calculate TCN correlations
# ------------

#### 2018 - include White only to maintain majority status perspective?

# Attitude measure: single-item - but similar to "social distance" measures that made up large part of Pettigrew & Tropp dataset (2005)
df_TCN_2018 <- readxl::read_excel(here::here("../Chapter 5 - contact intervention//Matched 2018 Summer impact responses 081118.xlsx")) %>%
  select(ctPosFreq = Q13.1_START_E, ctNegFreq = Q13.2_START_E, div_prefer = Q9.1_START, div_better = Q7.1_START, attitude = Q10_START, Ethnicity)

df_TCN_2018 %<>% mutate(Ethn_type = case_when(
  str_detect(Ethnicity, "White|Irish") & !str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Chinese|Mixed")) ~ "White",
  !str_detect(Ethnicity, "White|Irish|Mixed") & str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Chinese|Any other")) ~ "ME",
  str_detect(Ethnicity, "White|Irish|Mixed") & str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Mixed|Chinese")) ~ "Mixed",
  TRUE ~ NA_character_
) %>% factor())

# Test whether correlations differ by ethnic status
lav_cor <- ("
            ctPosFreq ~~ ctNegFreq + div_prefer + div_better + attitude
            ctNegFreq ~~ div_prefer + div_better + attitude
            div_prefer ~~ div_better + attitude
            div_better ~~ attitude
            ")

library(lavaan)
cors_group <- lavaan::sem(lav_cor, df_TCN_2018 %>% tidyr::drop_na(), meanstructure = TRUE, group = "Ethn_type")
cors_group_equal <- lavaan::sem(lav_cor, df_TCN_2018 %>% tidyr::drop_na(), meanstructure = TRUE, group = "Ethn_type", group.equal = c("residual.covariances"))

take_note("TCN 2018: Two group solution by ethnic status")
anova(cors_group, cors_group_equal) %>% timesaveR:::report_anova() %>% take_note()

cors <- df_TCN_2018 %>% cor_matrix() %>% tidy()

meta_data <- cors %>% mutate(pair = paste(column1, column2, sep = "_"),
                                measure = ifelse(str_detect(pair, "better"), "divval_better", "divval_pref"),
                                pair = pair %>% str_remove_all("_prefer|ct|Freq|itude|_better") %>% str_to_lower(),
                             study = "NCS_2018", r = ifelse(str_detect(pair, "att"), -1*estimate, estimate)) %>%
  filter(pair %in% meta_data$pair) %>% rename(N = n) %>%
  select(all_of(names(meta_data))) %>% 
  rbind(meta_data, .)

#### 
# 2019
####

#Better single item on social distance & new items on diversity in society
df_TCN_2019 <- read_rds(here::here("../Chapter 6 - diversity field experiment/data_with_conditions.RDS")) %>% 
  select(`Q21-START-E`, `Q22-START-E`, ctPosFreq = `Q16.1-START-E`, ctNegFreq = `Q16.2-START-E`, div_prefer = `Q12-START`, 
         div_better = `Q8-START`, attitude = `Q10-START-E`, Ethnicity = `INFO:E-START`)

df_TCN_2019 %<>% mutate(Ethn_type = case_when(
  str_detect(Ethnicity, "White|Irish") & !str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Chinese|Mixed")) ~ "White",
  !str_detect(Ethnicity, "White|Irish|Mixed") & str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Chinese|Any other")) ~ "ME",
  str_detect(Ethnicity, "White|Irish|Mixed") & str_detect(Ethnicity, c("Black|Arab|Asian|Pakistani|Bangladeshi|Indian|Mixed|Chinese")) ~ "Mixed",
  TRUE ~ NA_character_
) %>% factor())

recode_vector <- c("Never" = "1", "Rarely" = "2", "Sometimes" = "3", "Very Often" = "5", "Often" = "4")
df_TCN_2019$ctPosFreq <- recode_vector[df_TCN_2019$ctPosFreq %>% as.character()] %>% as.numeric()
df_TCN_2019$ctNegFreq <- recode_vector[df_TCN_2019$ctNegFreq %>% as.character()] %>% as.numeric()
recode_vector <- c("Very happy" = "1", "Happy" = "2", "Neutral" = "3", "Very unhappy" = "5", "Unhappy" = "4")
df_TCN_2019$attitude <- recode_vector[df_TCN_2019$attitude %>% as.character()] %>% as.numeric()

df_TCN_2019$`Q21-START-E`[df_TCN_2019$`Q21-START-E` == 0] <- NA
df_TCN_2019$`Q21-START-E`[df_TCN_2019$`Q21-START-E` == 0] <- NA
df_TCN_2019$div_prefer[df_TCN_2019$div_prefer == 0] <- NA
df_TCN_2019$div_better[df_TCN_2019$div_better == 0] <- NA


df_TCN_2019$val_div_society <- make_scale(df_TCN_2019, c("Q21-START-E", "Q22-START-E"), "Valuing diversity in society")
df_TCN_2019$val_div_teams <- make_scale(df_TCN_2019, c("div_prefer", "div_better"), "Valuing diversity in teams")


lav_cor <- ("
            ctPosFreq ~~ ctNegFreq + val_div_teams + val_div_society + attitude
            ctNegFreq ~~ val_div_teams + val_div_society + attitude
            val_div_teams ~~ val_div_society + attitude
            val_div_society ~~ attitude
            ")

library(lavaan)
cors_group <- lavaan::sem(lav_cor, df_TCN_2019 %>% tidyr::drop_na(), group = "Ethn_type")
cors_group_equal <- lavaan::sem(lav_cor, df_TCN_2019 %>% tidyr::drop_na(), group = "Ethn_type", group.equal = c("residual.covariances"))

take_note("TCN 2019: Two group solution by ethnic status")
anova(cors_group, cors_group_equal) %>% timesaveR:::report_anova() %>% take_note()

cors <- df_TCN_2019 %>% select(ctPosFreq, ctNegFreq, val_div_teams, val_div_society, attitude) %>% cor_matrix() %>% tidy()

meta_data <- cors %>% mutate(pair = paste(column1, column2, sep = "_"),
                            measure = ifelse(str_detect(pair, "society"), "divval_society", "divval_teams"),
                            pair = pair %>% str_remove_all("val_|_society|_teams|_prefer|ct|Freq|itude|_better") %>% str_to_lower(),
                    study = "NCS_2019") %>%  filter(pair %in% meta_data$pair) %>% rename(N = n, r = estimate) %>%
  select(all_of(names(meta_data))) %>% 
  rbind(meta_data, .)
  


# ------------
# Save full dataset
# ------------

#Add weights
meta_data$inv_N <- 1/meta_data$N

readr::write_rds(meta_data, here(pipeline, "mediation_meta_data.RDS"))

writeLines(notes, here(pipeline, "notes.txt"))
