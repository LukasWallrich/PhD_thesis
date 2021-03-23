# ------------
# Introduction
# ------------

NAME <- '3_mediation_plot'

# ------------
# Sources
# ------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, DiagrammeR, DiagrammeRsvg, here)
pacman::p_install_version_gh(c("lukaswallrich/rNuggets"),
                             c("0.1.9.1000"))

source(here("1_tools/management_functions.R"))

#Set up pipeline folder if missing
pipeline <- create_pipeline_dir(NAME)
datadir <- "0_data"
pipelinedir <- "3_outputs"

# ----------------------
# Load data
# ----------------------

graphs <- c("standard" = "graph_params.RDS")

purrr::walk2(graphs, names(graphs), function(file, graph_name){

graph_parameters <- read_rds(here(pipelinedir, "2_mediation_meta_analysis", file))

fmt_param <- function(param) {
  paste0(
  sprintf("%.2f",  graph_parameters$est[graph_parameters$label == param]),
  rNuggets::sigstars(graph_parameters$pvalue[graph_parameters$label == param]), "<BR />",
  rNuggets:::.fmt_ci(graph_parameters$ci.lower[graph_parameters$label == param], graph_parameters$ci.upper[graph_parameters$label == param]))
}

plot_code_edited <- rNuggets::glue_warn("
                     

digraph  {{

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



'x1' [label = <Valuing <br /> diversity>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '2.5,1.2!']

'y' [label = <Negative outgroup <br /> attitudes>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '5,1.2!']

'M1' [label = <Negative <br /> contact<br />>,
     color = 'black', shape = 'rectangle', height = '0.5', width = '1.5',
     pos = '0,-0.3!']
'M2' [label = <Positive <br /> contact<br />>,
     color = 'black', shape = 'rectangle', height = '0.5', width = '1.5',
     pos = '0,2.7!']


     
'a1' [label = <{fmt_param('a1')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '1.23,1.8!']
'a2' [label = <{fmt_param('a2')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '1.23,0.65!']

'c1' [label = <{fmt_param('c1')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '3, 2.25!']
'c2' [label = <{fmt_param('c2')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '3, 0.25!']

'b' [label = <{fmt_param('b')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '3.75, 1.18!']

'cv' [label = <{fmt_param('posWITHneg')}>,
     color = 'black', shape = 'plaintext', fillcolor='transparent',
     pos = '-0.8, 1.2!']

edge [fontname = 'Helvetica',
fontsize = '10',
len = '1.5',
color = 'black',
arrowsize = '0.5']

M1->x1 [tailport = 'e']
M2->x1 [tailport = 'e']
M1->y [tailport = 'e']
M2->y [tailport = 'e']

x1->y  [style = solid]

'space'  [style = invisible, pos = '-1, 2!']


 }}
")

rNuggets:::.grViz_and_save(plot_code_edited, here(pipeline, paste0("mediation_model_",graph_name, ".svg")))
})
