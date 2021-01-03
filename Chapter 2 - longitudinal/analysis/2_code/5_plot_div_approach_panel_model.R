# ------------
# Introduction
# ------------

## Plot cross-lagged model

NAME <- '5_plot_div_approach_panel_model' 

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, magrittr, here,  DiagrammeR, rsvg, DiagrammeRsvg, readr, lavaan)

pacman::p_install_version(c("stringr"),
                          c("1.4.0"))
pacman::p_install_version_gh(c("lukaswallrich/rNuggets"),
                             c("0.1.8"))

source(here("empirical/1_tools/managementFunctions.R"))


#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "empirical/0_data"
pipelinedir <- "empirical/3_pipeline"

fit_std <- read_rds(here(pipelinedir, "4_div_approach_panel/out/CrossLaggedModel.RDS"))



# ------------
# Plot model
# ------------

fig_coefs <- parameterestimates(fit_std, ci = TRUE) %>% filter(op %in% c("~", "~~"), lhs != rhs, pvalue <= .1) 

#Set parameters
IV1 <- "Valuing<BR />diversity (T1)"  %>% set_names("ValDiv_1")
IV2 <- "Approach <BR /> intentions (T1)"  %>% set_names("ApproachBlack_1")
DV1 <- "Valuing<BR />diversity (T2)"  %>% set_names("ValDiv_2")
DV2 <- "Approach <BR /> intentions (T2)"  %>% set_names("ApproachBlack_2")


IV1DV1c <- "0.39"
IV1DV1p <- "= .001"
IV1DV1s <- "solid"

IV2DV1c <- "0.21"
IV2DV1p <- "= .016"
IV2DV1s <- "solid"

IV1DV2c <- "0.16"
IV1DV2p <- "= .049"
IV1DV2s <- "solid"

IV2DV2c <- "0.44"
IV2DV2p <- "&lt; .001"
IV2DV2s <- "solid"

IV1IV2c <- "0.37"
IV1IV2p <- "= .001"
IV1IV2s <- "solid"

DV1DV2c <- "0.28"
DV1DV2p <- "= .001"
DV1DV2s <- "solid"



extract_coef <- function(df) {
  df %>% mutate(coef = paste(sprintf("%.2f", est), rNuggets::sigstars(pvalue))) %>% pull()
}

IV1DV1c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(IV1)) %>% extract_coef()
IV1DV1s <- "solid"


IV2DV1c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(IV2)) %>% extract_coef()
IV2DV1s <- "solid"


IV1DV2c <- fig_coefs %>% filter(lhs == names(DV2), rhs == names(IV1)) %>% extract_coef()
IV1DV2s <- "solid"

IV2DV2c <- fig_coefs %>% filter(lhs == names(DV2), rhs == names(IV2)) %>% extract_coef()
IV2DV2s <- "solid"

IV1IV2c <- fig_coefs %>% filter(lhs == names(IV1), rhs == names(IV2)) %>% extract_coef()
IV1IV2s <- "solid"

DV1DV2c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(DV2)) %>% extract_coef()
DV1DV2s <- "solid"


note <-"" 


grcode <- 
  glue::glue("digraph {{
        
        graph [layout = 'neato',
        outputorder = 'edgesfirst',
        bgcolor = 'white', rankdir=LR,]
        
        node [fontname = 'Helvetica',
        fontsize = '10',
        shape = 'circle',
        fixedsize = 'true',
        width = '0.5',
        style = 'filled',
        fillcolor = 'white',
        color = 'black',
        fontcolor = 'black']
        
        
        
        '1' [label = <{IV1}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '1,1!']
        '2' [label = <{IV2}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '1,-1!']

        '3' [label = <{DV1}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '4,1!']
        '4' [label = <{DV2}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '4,-1!']
      
        '6' [style = {IV1DV2s}, label = <{IV1DV2c}>, color = 'black', shape = 'plaintext', pos = '1.65,0.3!']   
        '5' [style = {IV2DV1s}, label = <{IV2DV1c}>, color = 'black', shape = 'plaintext', pos = '1.65,-0.3!']
        
        edge [fontname = 'Helvetica',
        fontsize = '10',
        len = '1.5',
        color = 'black',
        arrowsize = '0.5']
        
        1->2 [color = 'white', label= <{IV1IV2c}>]
        1->4  
        2->3
        3->4 [color = 'white', label= <{DV1DV2c}>]
        1->3  [style = {IV1DV1s}, label= <{IV1DV1c}>]
        2->4  [style = {IV2DV2s}, label= <{IV2DV2c}>]
        
        }}")


p <- grViz(grcode)
p

library(rsvg)
grViz(grcode) %>%
  export_svg %>% charToRaw %>% rsvg_png(here(pipeline, "out/Model1.png"), width = 850, height = 500)

