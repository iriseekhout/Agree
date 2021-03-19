#global
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(RColorBrewer)
library(Agree)

theme_set(theme_light())


ratercolors <- RColorBrewer::brewer.pal(5, "Set2")
names(ratercolors) <- c(2:6)
