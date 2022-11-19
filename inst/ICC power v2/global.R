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
library(shinyjs)
theme_set(theme_light())


ratercolors <- RColorBrewer::brewer.pal(5, "Set2")
names(ratercolors) <- c(2:6)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ncolors <- gg_color_hue(8)
names(ncolors) <- c(10, 20, 25, 30, 40, 50, 100, 200)
