library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(stringr)
library(maps)
library(mapdata)
library(ggmap)
library(broom)
library(rgdal)
library(shinythemes)
library(tidyverse)

options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
