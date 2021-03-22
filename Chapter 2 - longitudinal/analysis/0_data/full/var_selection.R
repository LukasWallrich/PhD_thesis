pacman::p_load(tidyverse, magrittr, here, haven)
datadir <- "empirical/0_data"
AllCases <- read_sav(here(datadir, "full/CombinedDataT2.sav"))
AllCases %<>% rowid_to_column(var = "respid")
AllCases %>% write_sav(here(datadir, "full/CombinedData_all_vars.sav"))
AllCases %>% select(respid, Ethn, src, Gender, Age, BlackComplimented, BlackBefriended, BlackWelcome, BlackSupported, BlackHelped, 
                    BlackRidiculed, BlackUnwanted, BlackVerbAbused, BlackIntimidated, BlackThreatened, 
                    ValDiv1 = DivVal4, ValDiv2 = DivVal5, ValDiv3 = DivVal6, 
                    Ethn_2, BlackComplimented_2, BlackBefriended_2, BlackWelcome_2, BlackSupported_2, BlackHelped_2, 
                    BlackRidiculed_2, BlackUnwanted_2, BlackVerbAbused_2, BlackIntimidated_2, BlackThreatened_2, 
                    ValDiv1_2 = DivVal4_2, ValDiv2_2 = DivVal5_2, ValDiv3_2 = DivVal6_2, BothWaves, 
                    ApproachBlack2 = AvoidBlack2, ApproachBlack5 = AvoidBlack5, ApproachBlack2_2 = AvoidBlack2_2, ApproachBlack5_2 = AvoidBlack5_2) %>% write_sav(here(datadir, "CombinedData.sav"))
