# ------------
# Introduction
# ------------

## Plot cross-lagged model

NAME <- '3_plot_div_contact_panel_model' 

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

fit_std <- read_rds(here(pipelinedir, "2_div_contact_panel/out/CrossLaggedModel.RDS"))



# ------------
# Plot model
# ------------

fig_coefs <- parameterestimates(fit_std, ci = TRUE) %>% filter(op %in% c("~", "~~"), lhs != rhs, pvalue <= .1) 


#Set parameters
IV1 <- "Valuing<BR />diversity (T1)" %>% set_names("ValDiv_1")
IV2 <- "Positive <BR /> contact (T1)" %>% set_names("BlackPOS_1")
IV3 <- "Negative <BR /> contact (T1)" %>% set_names("BlackNEG_1")

DV1 <- "Valuing<BR />diversity (T2)" %>% set_names("ValDiv_2")
DV2 <- "Positive <BR /> contact (T2)" %>% set_names("BlackPOS_2")
DV3 <- "Negative <BR /> contact (T2)" %>% set_names("BlackNEG_2")

extract_coef <- function(df) {
  df %>% mutate(coef = paste(sprintf("%.2f", est), rNuggets::sigstars(pvalue))) %>% pull()
}

IV1DV1c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(IV1)) %>% extract_coef()
IV1DV1s <- "solid"


IV2DV1c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(IV2)) %>% extract_coef()
IV2DV1s <- "solid"


IV3DV1c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(IV3)) %>% extract_coef()
IV3DV1s <- "solid"

IV1DV2c <- ""
IV1DV2s <- "invisible"

IV2DV2c <- fig_coefs %>% filter(lhs == names(DV2), rhs == names(IV2)) %>% extract_coef()
IV2DV2s <- "solid"

IV3DV3c <- fig_coefs %>% filter(lhs == names(DV3), rhs == names(IV3)) %>% extract_coef()
IV3DV3s <- "solid"

IV1IV2c <- fig_coefs %>% filter(lhs == names(IV1), rhs == names(IV2)) %>% extract_coef()
IV1IV2s <- "solid"

IV2IV3c <- fig_coefs %>% filter(lhs == names(IV2), rhs == names(IV3)) %>% extract_coef()
IV2IV3s <- "solid"

DV1DV2c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(DV2)) %>% extract_coef()
DV1DV2s <- "solid"

DV1DV3c <- fig_coefs %>% filter(lhs == names(DV1), rhs == names(DV3)) %>% extract_coef()
DV1DV3s <- "solid"

# DV2DV3c <- "DV2DV3c"
# DV2DV3p <- "= .001"
# DV2DV3s <- "solid"


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
        '2' [label = <{IV2}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '1,-0.5!']

        '3' [label = <{IV3}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '1,-2!']


        '4' [label = <{DV1}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '4,1!']
        '5' [label = <{DV2}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '4,-0.5!']
        '6' [label = <{DV3}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '4,-2!']
      
        '8' [style = {IV2DV1s}, label = <{IV2DV1c}>  color = 'black', shape = 'plaintext', pos = '2.2,-0.1!']

        '12' [style = {IV3DV1s}, label = <{IV3DV1c}>,  color = 'black', shape = 'plaintext', pos = '2.2,-1.1!']

        '13' [style = {IV1DV2s}, label = <{IV1DV2c}>,  color = 'black', shape = 'plaintext', pos = '2.2, 0.6!']

        '9' [style = {IV1IV2s}, label = <{IV1IV2c}>,  color = 'black', shape = 'plaintext', pos = '0.3,0.25!']
        
        '10' [style = {DV1DV2s}, label = <{DV1DV2c}>, color = 'black', shape = 'plaintext', pos = '4.7,0.25!']
        
        '11' [style = {DV1DV3s}, label = <{DV1DV3c}>, color = 'black', shape = 'plaintext', pos = '4.7,-1.25!']
        
        edge [fontname = 'Helvetica',
        fontsize = '10',
        len = '1.5',
        color = 'black',
        arrowsize = '0.5']
        


        1->4  [style = {IV1DV1s}, label = <{IV1DV1c}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;>]
        2->5  [style = {IV2DV2s}, label = <{IV2DV2c}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;>]
        3->6  [style = {IV3DV3s}, label = <{IV3DV3c}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;>]
        
        1->5[style = {IV1DV2s}]
        2->4[style = {IV2DV1s}]
        3->4[style = {IV3DV1s}]
        
        }}")


library(rsvg)
grViz(grcode) %>%
  export_svg %>% charToRaw %>% rsvg_png(here(pipeline, "out/Model1.png"), width = 750, height = 500)

