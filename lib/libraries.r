library(knitr)
library(dplyr)
library(plotly)
library(ggplot2)
require(jsonlite)
require(httr)
require(zoo)
library(rvest)
require(plyr)
library(gsubfn)


# Uvozimo funkcije za delo z datotekami XML.
source("lib/xml.r", encoding = "UTF-8")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")