library(ggplot2)
library(ggrepel)
library(ggsn)
library(ggthemes)
library(plyr)
library(raster)
library(reshape2)
library(rgdal)
library(RSQLite)
library(scatterpie)
library(sp)
library(splitstackshape)
library(viridis)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")
# con <- dbConnect(drv, "www.github.com ... WEBVERSION")