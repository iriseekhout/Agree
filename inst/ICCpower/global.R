#global
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)
library(Agree)
library(plotly)
library(heatmaply)
theme_set(theme_light())


ratercolors <- RColorBrewer::brewer.pal(5, "Set2")
names(ratercolors) <- c(2:6)

ncolors <- RColorBrewer::brewer.pal(8, "Set1")
names(ncolors) <- c(10, 20, 25, 30, 40, 50, 100, 200)
