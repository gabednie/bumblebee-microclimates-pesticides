##this script converts the land use types within Ontario. 
#The land use codes are converted to min, mean,
#max application rates of 3 pesticides.

library(terra)
library(vroom)
library(dplyr)

c15 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2015_on_v3.tif")
c16 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2016_on_v3.tif")
c17 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2017_on_v4.tif")
c18 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2018_on_v3.tif")

lt <- vroom("D:/Thesis Projects/Bee Beta Diversity/crop data/pesticide_lookup_updated.csv")

lmi_min <- lt %>% dplyr::select(1,2)
lmi_max <- lt %>% dplyr::select(1,3)
lmi_mean <- lt %>% dplyr::select(1,4)

clo_min <- lt %>% dplyr::select(1,5)
clo_max <- lt %>% dplyr::select(1,6)
clo_mean <- lt %>% dplyr::select(1,7)

thi_min <- lt %>% dplyr::select(1,8)
thi_max <- lt %>% dplyr::select(1,9)
thi_mean <- lt %>% dplyr::select(1,10)

##2015
c_15_lmi_min <- classify(c15, lmi_min, include.lowest=TRUE)
c_15_lmi_max <- classify(c15, lmi_max, include.lowest=TRUE)
c_15_lmi_mean <- classify(c15, lmi_mean, include.lowest=TRUE)

c_15_clo_min <- classify(c15, clo_min, include.lowest=TRUE)
c_15_clo_max <- classify(c15, clo_max, include.lowest=TRUE)
c_15_clo_mean <- classify(c15, clo_mean, include.lowest=TRUE)

c_15_thi_min <- classify(c15, thi_min, include.lowest=TRUE)
c_15_thi_max <- classify(c15, thi_max, include.lowest=TRUE)
c_15_thi_mean <- classify(c15, thi_mean, include.lowest=TRUE)


writeRaster(c_15_lmi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_min_2015.tif",overwrite=T)
writeRaster(c_15_lmi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_max_2015.tif",overwrite=T)
writeRaster(c_15_lmi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_mean_2015.tif",overwrite=T)

writeRaster(c_15_clo_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_min_2015.tif",overwrite=T)
writeRaster(c_15_clo_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_max_2015.tif",overwrite=T)
writeRaster(c_15_clo_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_mean_2015.tif",overwrite=T)

writeRaster(c_15_thi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_min_2015.tif",overwrite=T)
writeRaster(c_15_thi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_max_2015.tif",overwrite=T)
writeRaster(c_15_thi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_mean_2015.tif",overwrite=T)

#2016
c_16_lmi_min <- classify(c16, lmi_min, include.lowest=TRUE)
c_16_lmi_max <- classify(c16, lmi_max, include.lowest=TRUE)
c_16_lmi_mean <- classify(c16, lmi_mean, include.lowest=TRUE)

c_16_clo_min <- classify(c16, clo_min, include.lowest=TRUE)
c_16_clo_max <- classify(c16, clo_max, include.lowest=TRUE)
c_16_clo_mean <- classify(c16, clo_mean, include.lowest=TRUE)

c_16_thi_min <- classify(c16, thi_min, include.lowest=TRUE)
c_16_thi_max <- classify(c16, thi_max, include.lowest=TRUE)
c_16_thi_mean <- classify(c16, thi_mean, include.lowest=TRUE)


writeRaster(c_16_lmi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_min_2016.tif",overwrite=T)
writeRaster(c_16_lmi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_max_2016.tif",overwrite=T)
writeRaster(c_16_lmi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_mean_2016.tif",overwrite=T)

writeRaster(c_16_clo_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_min_2016.tif",overwrite=T)
writeRaster(c_16_clo_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_max_2016.tif",overwrite=T)
writeRaster(c_16_clo_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_mean_2016.tif",overwrite=T)

writeRaster(c_16_thi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_min_2016.tif",overwrite=T)
writeRaster(c_16_thi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_max_2016.tif",overwrite=T)
writeRaster(c_16_thi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_mean_2016.tif",overwrite=T)

#2017
c_17_lmi_min <- classify(c17, lmi_min, include.lowest=TRUE)
c_17_lmi_max <- classify(c17, lmi_max, include.lowest=TRUE)
c_17_lmi_mean <- classify(c17, lmi_mean, include.lowest=TRUE)

c_17_clo_min <- classify(c17, clo_min, include.lowest=TRUE)
c_17_clo_max <- classify(c17, clo_max, include.lowest=TRUE)
c_17_clo_mean <- classify(c17, clo_mean, include.lowest=TRUE)

c_17_thi_min <- classify(c17, thi_min, include.lowest=TRUE)
c_17_thi_max <- classify(c17, thi_max, include.lowest=TRUE)
c_17_thi_mean <- classify(c17, thi_mean, include.lowest=TRUE)


writeRaster(c_17_lmi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_min_2017.tif",overwrite=T)
writeRaster(c_17_lmi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_max_2017.tif",overwrite=T)
writeRaster(c_17_lmi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_mean_2017.tif",overwrite=T)

writeRaster(c_17_clo_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_min_2017.tif",overwrite=T)
writeRaster(c_17_clo_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_max_2017.tif",overwrite=T)
writeRaster(c_17_clo_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_mean_2017.tif",overwrite=T)

writeRaster(c_17_thi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_min_2017.tif",overwrite=T)
writeRaster(c_17_thi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_max_2017.tif",overwrite=T)
writeRaster(c_17_thi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_mean_2017.tif",overwrite=T)

#2018
c_18_lmi_min <- classify(c18, lmi_min, include.lowest=TRUE)
c_18_lmi_max <- classify(c18, lmi_max, include.lowest=TRUE)
c_18_lmi_mean <- classify(c18, lmi_mean, include.lowest=TRUE)

c_18_clo_min <- classify(c18, clo_min, include.lowest=TRUE)
c_18_clo_max <- classify(c18, clo_max, include.lowest=TRUE)
c_18_clo_mean <- classify(c18, clo_mean, include.lowest=TRUE)

c_18_thi_min <- classify(c18, thi_min, include.lowest=TRUE)
c_18_thi_max <- classify(c18, thi_max, include.lowest=TRUE)
c_18_thi_mean <- classify(c18, thi_mean, include.lowest=TRUE)


writeRaster(c_18_lmi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_min_2018.tif",overwrite=T)
writeRaster(c_18_lmi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_max_2018.tif",overwrite=T)
writeRaster(c_18_lmi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Imidacloprid_mean_2018.tif",overwrite=T)

writeRaster(c_18_clo_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_min_2018.tif",overwrite=T)
writeRaster(c_18_clo_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_max_2018.tif",overwrite=T)
writeRaster(c_18_clo_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Clothianidin_mean_2018.tif",overwrite=T)

writeRaster(c_18_thi_min, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_min_2018.tif",overwrite=T)
writeRaster(c_18_thi_max, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_max_2018.tif",overwrite=T)
writeRaster(c_18_thi_mean, "D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps/Thiamethoxam_mean_2018.tif",overwrite=T)