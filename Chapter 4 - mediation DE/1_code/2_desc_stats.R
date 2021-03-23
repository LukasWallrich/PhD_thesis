# ------------
# Introduction
# ------------

NAME <- '2_desc_stats' ## Name of the R file goes here (without the file extension!)

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, here, foreign, survey, srvyr, scatterpie, mitools)
pacman::p_install_version_gh(c("lukaswallrich/rNuggets"),
                             c("0.1.9.1000"))
pacman::p_load_gh(c("lukaswallrich/timesaveR"))

source(here("managementFunctions.R"))

#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "0_data"
pipelinedir <- "2_pipeline"

# ----------------------
# Load data
# ----------------------

resp_B <- read_csv(here(pipelinedir, "1_data_prep", "neighbourhood_choices.csv"))
ac_svy <- read_rds(here(pipelinedir, "1_data_prep",  "ac_svy.RDS"))
imp_long_list <- read_rds(here(pipelinedir, "1_data_prep","imp_long_list.RDS"))
imp_long_mids <- read_rds(here(pipelinedir, "1_data_prep", "imp_long_mids.RDS"))

ac_svy_MI <- svydesign(id=~1, weight=~wt, data=mitools::imputationList(imp_long_list))

# ----------------------
# Sample descriptives based on MI
# ----------------------

svy_mi_mean_sd <- function(design, variable) {
  mean <- with(design,svymean(as.formula(paste0("~", variable)))) %>% mitools::MIcombine() %>% .$coefficients %>% set_names("")
  sd <- with(design,svyvar(as.formula(paste0("~", variable)))) %>% mitools::MIcombine() %>% .$coefficients %>% sqrt() %>% set_names("")
  rNuggets:::.named_list(variable, mean, sd)
}


sink(here(pipeline, "sample_descriptives.txt"))
paste("N=",round(sum(svytable(~respid, design = ac_svy))))
round(prop.table(svytable(~sex, design = ac_svy)), 3)
svy_mi_mean_sd(ac_svy_MI, "age")
round(prop.table(svytable(~eastwest, design = ac_svy)), 3)
sink()


# Correlation table

var_names <- c(
"nb_shareY" = "Approach intentions", 
"divprefinstr" = "Valuing diversity",
"for_att" = "Attitude towards foreigners",
"leftright" = "Political Orientation",
"sex" = "Gender",
"age" = "Age",
"educN" = "Education",
"Region" = "eastwest", 
"posCont" = "Positive contact",
"negCont" = "Negative contact"
)

cors <- imp_long_list %>% 
  rNuggets::wtd_cor_matrix_mi(wt, var_names = var_names) 

cors %>% 
  report_cor_table(filename = here(pipeline, "corTableMultipleImputation.html"), add_title = FALSE)


# Cat descriptives table

var_names <- tribble(
  ~old, ~new,
  "sex", "Gender",
  "eastwest", "Region",
  "foreign_neighbours", "Foreigners in neighbourhood"
)

level_names <- tribble(
  ~var, ~level_old, ~level_new, 

"sex", "FRAU", "female", 
"sex", "MANN", "male", 
"eastwest", "NEUE BUNDESLAENDER", "East Germany", 
"eastwest", "ALTE BUNDESLAENDER", "West Germany", 
"foreign_neighbours", "(Almost) no foreigners", "(Almost) no foreigners", 
"foreign_neighbours", "Some foreigners", "Some foreigners", 
"foreign_neighbours", "Many foreigners", "Many foreigners", 
"foreign_neighbours", "Mostly foreigners", "Mostly foreigners"
)



rNuggets::cat_var_table_mi(imp_long_list, nb_shareY, weights = wt, sex, eastwest, foreign_neighbours, 
                 var_names = var_names, level_names = level_names, dv_name = "neighbourhood approach",
                 filename = here(pipeline, "summaryCategorical.html"))




# Chart neighbourhood choices

neighborhood <- c(paste0("ms0", 1:9), paste0("ms", 10:13))

resp_B %>% drop_na() %>% mutate(Q = factor(Q, levels=c("Would want to live there", "Would NOT want to live there"))) %>% ggplot(aes(x=neighborhood)) +
  geom_bar(aes(fill = factor(resp))) +
  labs(title = "Readiness to live in neighbourhoods", x = NULL, y = NULL, fill = "Response") +
  facet_wrap(~Q, nrow = 2) + jtools::theme_apa() + scale_fill_brewer() + scale_x_discrete(labels = 1:13)

ggsave(here(pipeline, "neighbourhood-all-selections.png"), width = 17, units = "cm")



resp_Ns <- resp_B %>% filter(str_detect(Q, "NOT"), resp == "1")

resp_Ns <- ac_svy$variables %>% select(respid, nb_scoreY, eastwest) %>% right_join(resp_Ns)

nb_counts <- resp_Ns %>% group_by(neighborhood, nb_scoreY, eastwest) %>% summarise(count = n()) %>% mutate(nb_N = as.numeric(str_remove(neighborhood, "ms"))) %>% spread(eastwest, count) %>% ungroup() %>% select(-`<NA>`, -neighborhood) %>% rename(East = `2`, West = `1`) %>% mutate(East = replace_na(East, 0), West = replace_na(West, 0), total = East + West) %>% rowid_to_column()

area <- function(x) {
  round(pi*x^2)
}

p <- ggplot() + jtools::theme_apa() +
  geom_scatterpie(aes(x = nb_scoreY * 13, y = nb_N * 13, group = rowid, r = sqrt(total) / pi), data = nb_counts, cols = c("East", "West"), color = NA) +
  coord_equal() +
  geom_scatterpie_legend((sqrt(nb_counts$total) / pi)[(sqrt(nb_counts$total) / pi) > 0], x = 180, y = 125, n = 3, labeller = area) +
  theme(legend.position = c(.86, .43), legend.title = element_blank(), legend.background = element_rect(fill = "transparent", linetype = "blank")) +
  annotate("text", x = 190, y = 142, label = "Occurences", fontface = "bold") +
  annotate("text", x = 183, y = 97, label = "Region", fontface = "bold") +
  labs(x = "Most diverse neighbourhood selected           ", y = "Neighbourhoods rejected", title = "Neighbourhoods rejected depending on \nmost diverse neighbourhood selected", subtitle = " ", caption = "Note: Neighbourhood 1 is least \ndiverse, 13 most diverse") +
  scale_x_continuous(breaks = seq.int(13, by = 13, length.out = 13), labels = 1:13, limits = c(0,210)) +
  scale_y_continuous(breaks = seq.int(13, by = 13, length.out = 13), labels = 1:13) + scale_fill_brewer(palette="Dark2") 

addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 3, spaceLegend = 0.1) {
    myPlot +
        guides(shape = guide_legend(override.aes = list(size = pointSize)),
               color = guide_legend(override.aes = list(size = pointSize))) +
        theme(legend.title = element_text(size = textSize), 
              legend.text  = element_text(size = textSize),
              legend.key.size = unit(spaceLegend, "lines"),
              legend.key.width = unit(spaceLegend, "lines"))
}

# Apply on original plot
addSmallLegend(p, 2, 10, 1) + theme(legend.title = element_blank())


ggsave(here(pipeline, "neighbourhood-rejections.png"), width = 30, height = 30, units = "cm")


