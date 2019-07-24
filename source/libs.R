# Basic libraries needed - don't modify this for a library just used in one/two scripts

if(!require(pacman))install.packages("pacman")
if(!require(bbplot))devtools::install_github('bbc/bbplot')

pacman::p_load('tidyverse', 'DescTools', 'RColorBrewer',
               'ggalt', 'ggExtra', 'data.table', 
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
