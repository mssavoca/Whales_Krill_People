#########
# Utilities script for Whales, Krill, and People
#########


library(tidyverse)
library(ggmap)
library(ggpubr)
library(maps)
library(rgdal)
library(sf)
library(ncdf4)
library(raster)
library(mapproj)
library(viridis)
library(parzer)
library(lubridate)
library(broom)
library(readxl)


# load whaling data


load("Exploit_time_extended.RData") 

load("IWC_SO_whaling_sf.RData")
load("IWC_SO_whaling_full_df.RData")


#Useful formulas and color palettes----

abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}


SE = function(x){sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))}

# color palette for all whales species
pal <- c("B. acutorostrata" = "goldenrod2",
         "B. bonaerensis" = "firebrick3", 
         "B. borealis" = "darkgreen", 
         "B. brydei" = "darkorchid3",  
         "M. novaeangliae" = "gray30", 
         "B. physalus" = "chocolate3", 
         "B. musculus" = "dodgerblue2",
         "B. mysticetus" = "#4304ff", 
         "E. glacialis" = "#ff9f04",
         "E. australis" = "orange2",
         "E. japonica" = "darkorange3",
         "E. robustus" = "gray60",
         "P. catodon" = "palegreen4"
)

#Load data----




#CCAMLR areas and subareas, and MPA shapefiles

CCAMLR_mgmt_areas <- st_read("asd-shapefile-EPSG102020.shp")

# CCAMLR_mgmt_areas_df <- tidy(CCAMLR_mgmt_areas, region = "CCAMLR")

SO_MPAs <- st_read("mpa-shapefile-EPSG102020.shp")




