# ------------
# Introduction
# ------------

NAME <- '1_data_prep' ## Name of the R file goes here (without the file extension!)

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, here, foreign, survey, srvyr, haven)
pacman::p_install_version(c("jtools", "modelsummary", "stringr", "purrr", "gt"),
                          c("2.1.0", "0.5.1", "1.4.0", "0.3.4", "0.2.1"))
pacman::p_load_gh("lukaswallrich/timesaveR")

source(here("managementFunctions.R"))

#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "0_data"
pipelinedir <- "2_pipeline"

# ----------------------
# Data import and prep
# ----------------------
#Load Allbus data
allbus_file <- here(datadir, "ZA5250_v2-1-0.sav")
allbus_all_df <- haven::read_sav(allbus_file)
allbus_all_df_NA <- haven::read_sav(allbus_file, user_na = TRUE)


#Retain different types of missingness for contact variables
ac <- allbus_all_df_NA %>% select(respid, mc09_with_NAs = mc09, mc10_with_NAs = mc10) %>% right_join(allbus_all_df)

notes <- character()
take_note(paste0("Notes from ", NAME), "---------------------------------")

# ------------------------------------------------------
# Coding neighbourhood choice
# ------------------------------------------------------

neighborhoods_Y <- c(paste0("ms0", 1:9), paste0("ms", 10:13))
neighborhoods_N <- paste0("ms", 14:26)

# ------------------------------------------------------
# ... for supplementary plot
# ------------------------------------------------------

resp_Y <- ac %>%
  select(one_of(neighborhoods_Y), respid) %>%
  gather(-respid, key = "neighborhood", value = "resp", factor_key = T) %>%
  mutate(resp = factor(resp))

resp_Y$Q <- "Would want to live there"

resp_N <- ac %>%
  select(one_of(neighborhoods_N), respid) %>%
  gather(-respid, key = "neighborhood", value = "resp", factor_key = T) %>%
  mutate(resp = factor(resp))

resp_N$Q <- "Would NOT want to live there"

recode_values <- neighborhoods_N
names(recode_values) <- neighborhoods_Y

#Neighbourshoods ms01 to ms13 are the same as ms14 to ms26
# - former are choices, latter are rejections
resp_N$neighborhood %<>% forcats::fct_recode(!!!recode_values) 

resp_B <- rbind(resp_N, resp_Y)

resp_B$resp %<>% recode(GENANNT = "MENTIONED", `NICHT GENANNT` = "NOT MENTIONED")

resp_B %>% write_csv(here(pipeline, "neighbourhood_choices.csv"))

# -----------------------------
# ... for analysis
# -----------------------------

neighbourhood_scores_Y <- ac %>%
  select(one_of(c("respid", all_of(neighborhoods_Y)))) %>%
  gather(all_of(neighborhoods_Y), key = "neighborhood", value = "resp") %>%
  group_by(respid) %>%
  mutate(n = row_number()) %>%
  filter(resp == 1) %>%
  summarise(nb_scoreY = max(n))

neighbourhood_scores_N <- ac %>%
  select(one_of(c("respid", all_of(neighborhoods_N)))) %>%
  gather(all_of(neighborhoods_N), key = "neighborhood", value = "resp") %>%
  group_by(respid) %>%
  mutate(n = row_number())

neighbourhood_scores_N %<>% left_join(neighbourhood_scores_Y)

neighbourhood_scores_N %<>% filter(resp == 1, n > nb_scoreY) %>% summarise(nb_scoreN = min(n))

neighbourhood_scores <- full_join(neighbourhood_scores_Y, neighbourhood_scores_N)

ac %<>% left_join(neighbourhood_scores)

ac %<>% mutate(nb_scoreN = 15-ifelse(nb_scoreY == 13, 14, nb_scoreN))

ac %<>% mutate(nb_shareY = (nb_scoreY-1) * 4/49 , nb_shareN = (nb_scoreN - 1) * 4/49)

# -----------------------------
# Filter & create survey object
# -----------------------------

ac_de <- ac %>% mutate(german = as_factor(german)) %>% filter(german != "NEIN")


#Both (known) parents born in (former) Germany - 
acF  <- ac_de %>% mutate(across(c(mdm01a, fdm01a), as_factor)) %>% filter((
  mdm01a %in% c("DEUTSCHLAND", "FRUEHERE DT.OSTGEB.") & fdm01a %in% c("DEUTSCHLAND", "FRUEHERE DT.OSTGEB.")) |
    (is.na(mdm01a) & fdm01a %in% c("DEUTSCHLAND", "FRUEHERE DT.OSTGEB.")) |
     (mdm01a %in% c("DEUTSCHLAND", "FRUEHERE DT.OSTGEB.") & is.na(fdm01a)))


ac_with_contact <- acF %>% mutate(across(c(mc09_with_NAs, mc10_with_NAs), haven::as_factor)) %>%
  filter(!(mc09_with_NAs=="KEINE KONTAKTE"&mc10_with_NAs=="KEINE KONTAKTE"))

take_note("Number of  exclusions due to citizenship:", round(sum(ac$wghtpew) - sum(ac_de$wghtpew)))
take_note("Number of additional exclusions due to parentage:", round(sum(ac_de$wghtpew) - sum(acF$wghtpew)))
take_note("Number of additional exclusions due to no contact:", round(sum(acF$wghtpew) - sum(ac_with_contact$wghtpew)))

acF <- ac_with_contact

take_note("\nNumber of responses considered (all weighted):", round(sum(acF$wghtpew)))


acF$wt <- 0.53335
acF$wt[as_factor(acF$wghtpew) == "ALTE BUNDESLAENDER"] <- 1.23382

ac.w <- acF %>% as_survey(ids = 1, weights = wt)


# ----------------------
# Create scales
# ----------------------

## Diversity opinions
# ma12: Es ist besser für ein Land, wenn alle Menschen einer gemeinsamen Kultur angehören
# ma13:  Eine Gesellschaft mit einem hohen Ausmaß an kultureller Vielfalt ist eher befähigt, neue Probleme in Angriff zu nehmen

# Smaller numbers: higher agreement - so reverse items
ac.w$variables$ma12 <- 5 - ac.w$variables$ma12
ac.w$variables$ma13 <- 5 - ac.w$variables$ma13

#Instrumental value of diversity
scale_items <- c("ma12", "ma13")
reversed <- c("ma12")

take_note(paste(capture.output(ac.w <- svy_make_scale(ac.w, scale_items, reversed = reversed, scale_name = "divprefinstr", 
                                                        print_hist = FALSE, scale_title = "Instrumental value for diversity")), collapse = "\n"))

#Calc SB reliability

ac.w$variables$ma12num <- as.numeric(ac.w$variables$ma12)
ac.w$variables$ma13num <- as.numeric(ac.w$variables$ma13)

cor_value <- jtools::svycor(~ma12num+ma13num, ac.w, na.rm = T)[[1]][1,2]
SB_value <- (abs(cor_value) * 2)/(1 + abs(cor_value))

take_note(paste0("Spearman-Brown correlation for valuing diversity: ", round_(SB_value, 3)))

#General attitude towards foreigners - did not include items referring to diversity

scale_items <- c("mp02", "mp05", "mp06", "mp07", "mp08", "mp11", "mp12")
reversed <- c( "mp05", "mp08", "mp12") 

take_note(paste(capture.output(ac.w <- svy_make_scale(ac.w, scale_items, scale_name = "for_att", 
                                                      print_hist = FALSE, scale_title = "Instrumental value for diversity", 
                                                      reversed = reversed, r_key = -1)), collapse = "\n"))
# ----------------------
# Code covariates
# ----------------------

ac.w$variables$leftright <- 11-as.numeric(ac.w$variables$pa01)

#Generate education covariate
##Code education for those still in school
ac.w %<>% mutate(across(c(educ, isced97), as_factor),
  isced97A = factor(case_when(
  educ == "NOCH SCHUELER" ~ "UPPER SECONDARY",
  T ~ as.character(isced97))))
ac.w$variables$isced97A %<>% factor(levels = levels(as_factor(ac.w$variables$isced97)))
ac.w %<>% mutate(educN = 7 - (8 - as.numeric(isced97A)))

ac.w$variables$posCont <- 6 - as.numeric(ac.w$variables$mc09)
ac.w$variables$negCont <- 6 - as.numeric(ac.w$variables$mc10)

#Foreigners in neighbourhood - merge questions from split survey experiment
set.seed(300688) #Ties are randomly resolved
ac.w$variables$mp15bc <- cut_p(ac.w$variables$mp15b %>% as.numeric(), 
                               rev(ac.w$variables$mp15a %>% table() %>% prop.table() %>% as.numeric()), 
                               fct_levels = levels(as_factor(ac.w$variables$mp15a))[6:3])

ac.w$variables$foreign_neighbours <- coalesce(as_factor(ac.w$variables$mp15a), ac.w$variables$mp15bc) %>%
  droplevels() %>%
  factor(labels = rev(c("(Almost) no foreigners", "Some foreigners", "Many foreigners", "Mostly foreigners")), ordered = T)

# ----------------------
# Missing data analysis
# ----------------------

take_note(ac.w %>% svy_miss_var_summary(sex, age, educN, divprefinstr, for_att, leftright, 
                              nb_shareY, eastwest, foreign_neighbours, posCont, negCont))


# ----------------------
# Multiple imputation
# ----------------------

vars_all <- ac.w$variables %>% select(sex, age, educN, divprefinstr, for_att, leftright, 
                                      nb_shareN, nb_shareY, eastwest, foreign_neighbours, 
                                      posCont, negCont, wt, ma13num, ma12num)

library(mice)

vars_all$educN %<>% factor()
vars_all$foreign_neighbours %<>% factor()
vars_all$sex %<>% as_factor()
vars_all$eastwest %<>% as_factor()

vars_all <- vars_all %>% mutate(across(c(age, divprefinstr, for_att, leftright, 
                                         nb_shareN, nb_shareY, 
                                         posCont, negCont, wt, ma13num, ma12num), as.numeric))


imp <- mice(vars_all, maxit=0, seed = 270491)

predM <- imp$predictorMatrix
meth <- imp$method

# Ordered categorical variables 
poly <- c("educN", "foreign_neighbours")

# Dichotomous variable
#log <- c("") #None missing on sex

# Unordered categorical variable 
#poly2 <- c("") #None 

meth[poly] = "polr"
#meth[log] = "logreg"
#meth[poly2] = "polyreg"


imp2 <- mice(vars_all, m = 8, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE, seed = 270491)

imp_long <- mice::complete(imp2, action="long", include = TRUE)

imp_long$educN <- with(imp_long, as.integer(imp_long$educN))

as.mids(imp_long) %>% write_rds(here(pipeline, "imp_long_mids.RDS"))

imp_long %>% filter(.imp>0) %>% dplyr::group_split(.imp) %>% 
  write_rds(here(pipeline, "imp_long_list.RDS"))

ac.w %>% write_rds(here(pipeline, "ac_svy.RDS"))

writeLines(notes, here(pipeline, "notes incl reliability.txt"))


