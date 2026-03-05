##this script accesses observation data for bumble bee species of interest from
#GBIF. The points are then filtered and converted into convex hull polygons.
#both polygons and points are then used to generate niche limit estimatese
#for temperature and airidty index

library(dplyr)
library(vroom)
library(rgbif)
library(terra)

user <- "mwats041"
pwd <- "DmDc@@1992"
email <- "mwats041@uottawa.ca"

# taxon_keys####
bombus_impatiens <- name_backbone(name="Bombus impatiens", kingdom="animalia")
bombus_bimaculatus <- name_backbone(name="Bombus bimaculatus", kingdom="animalia")
bombus_griseocollis <- name_backbone(name="Bombus griseocollis", kingdom="animalia")
bombus_rufocinctus <- name_backbone(name="Bombus rufocinctus", kingdom="animalia")
bombus_vagans <- name_backbone(name="Bombus vagans", kingdom="animalia")
bombus_perplexus <- name_backbone(name="Bombus perplexus", kingdom="animalia")
bombus_ternarius <- name_backbone(name="Bombus ternarius", kingdom="animalia")
bombus_fervidus <- name_backbone(name="Bombus fervidus", kingdom="animalia")
bombus_borealis <- name_backbone(name="Bombus borealis", kingdom="animalia")
bombus_pensylvanicus <- name_backbone(name="Bombus pensylvanicus", kingdom="animalia")
bombus_citrinus <- name_backbone(name="Bombus citrinus", kingdom="animalia")
bombus_auricomus <- name_backbone(name="Bombus auricomus", kingdom="animalia")
bombus_terricola <- name_backbone(name="Bombus terricola", kingdom="animalia")
bombus_flavidus <- name_backbone(name="Bombus flavidus", kingdom="animalia")

#Bombus OCC - get occurance from 1975 and earlier of bombus species####
#bombus_impatiens
bombus_impatiens_occ <- occ_download(
  pred_in("taxonKey", bombus_impatiens$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_impatiens_occ)

bombus_impatiens_occ_final <- occ_download_get(bombus_impatiens_occ, overwrite = T) %>% occ_download_import()
bombus_impatiens_occ_final <- bombus_impatiens_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_impatiens_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_impatiens.csv")

#bombus_bimaculatus
bombus_bimaculatus_occ <- occ_download(
  pred_in("taxonKey", bombus_bimaculatus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_bimaculatus_occ)
bombus_bimaculatus_occ_final <- occ_download_get(bombus_bimaculatus_occ) %>% occ_download_import()
bombus_bimaculatus_occ_final <- bombus_bimaculatus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_bimaculatus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_bimaculatus.csv")

#bombus_griseocollis
bombus_griseocollis_occ <- occ_download(
  pred_in("taxonKey", bombus_griseocollis$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_griseocollis_occ)
bombus_griseocollis_occ_final <- occ_download_get(bombus_griseocollis_occ) %>% occ_download_import()
bombus_griseocollis_occ_final <- bombus_griseocollis_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_griseocollis_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_griseocollis.csv")

#bombus_rufocinctus
bombus_rufocinctus_occ <- occ_download(
  pred_in("taxonKey", bombus_rufocinctus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_rufocinctus_occ)
bombus_rufocinctus_occ_final <- occ_download_get(bombus_rufocinctus_occ) %>% occ_download_import()
bombus_rufocinctus_occ_final <- bombus_rufocinctus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_rufocinctus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_rufocinctus.csv")
#bombus_vagans
bombus_vagans_occ <- occ_download(
  pred_in("taxonKey", bombus_vagans$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_vagans_occ)
bombus_vagans_occ_final <- occ_download_get(bombus_vagans_occ) %>% occ_download_import()
bombus_vagans_occ_final <- bombus_vagans_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_vagans_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_vagans.csv")

#bombus_perplexus
bombus_perplexus_occ <- occ_download(
  pred_in("taxonKey", bombus_perplexus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_perplexus_occ)
bombus_perplexus_occ_final <- occ_download_get(bombus_perplexus_occ) %>% occ_download_import()
bombus_perplexus_occ_final <- bombus_perplexus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_perplexus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_perplexus.csv")

#bombus_ternarius
bombus_ternarius_occ <- occ_download(
  pred_in("taxonKey", bombus_ternarius$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_ternarius_occ)
bombus_ternarius_occ_final <- occ_download_get(bombus_ternarius_occ) %>% occ_download_import()
bombus_ternarius_occ_final <- bombus_ternarius_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_ternarius_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_ternarius.csv")

#bombus_fervidus
bombus_fervidus_occ <- occ_download(
  pred_in("taxonKey", bombus_fervidus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_fervidus_occ)
bombus_fervidus_occ_final <- occ_download_get(bombus_fervidus_occ) %>% occ_download_import()
bombus_fervidus_occ_final <- bombus_fervidus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_fervidus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_fervidus.csv")

#bombus_borealis
bombus_borealis_occ <- occ_download(
  pred_in("taxonKey", bombus_borealis$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_borealis_occ)
bombus_borealis_occ_final <- occ_download_get(bombus_borealis_occ) %>% occ_download_import()
bombus_borealis_occ_final <- bombus_borealis_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_borealis_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_borealis.csv")

#bombus_pensylvanicus
bombus_pensylvanicus_occ <- occ_download(
  pred_in("taxonKey", bombus_pensylvanicus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_pensylvanicus_occ)
bombus_pensylvanicus_occ_final <- occ_download_get(bombus_pensylvanicus_occ) %>% occ_download_import()
bombus_pensylvanicus_occ_final <- bombus_pensylvanicus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_pensylvanicus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_pensylvanicus.csv")

#bombus_citrinus
bombus_citrinus_occ <- occ_download(
  pred_in("taxonKey", bombus_citrinus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_citrinus_occ)
bombus_citrinus_occ_final <- occ_download_get(bombus_citrinus_occ) %>% occ_download_import()
bombus_citrinus_occ_final <- bombus_citrinus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_citrinus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_citrinus.csv")

#bombus_auricomus
bombus_auricomus_occ <- occ_download(
  pred_in("taxonKey", bombus_auricomus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_auricomus_occ)
bombus_auricomus_occ_final <- occ_download_get(bombus_auricomus_occ) %>% occ_download_import()
bombus_auricomus_occ_final <- bombus_auricomus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_auricomus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_auricomus.csv")

#bombus_terricola
bombus_terricola_occ <- occ_download(
  pred_in("taxonKey", bombus_terricola$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_terricola_occ)
bombus_terricola_occ_final <- occ_download_get(bombus_terricola_occ) %>% occ_download_import()
bombus_terricola_occ_final <- bombus_terricola_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_terricola_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_terricola.csv")

#bombus_flavidus
bombus_flavidus_occ <- occ_download(
  pred_in("taxonKey", bombus_flavidus$usageKey),
  pred_lte("year", 1975),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait(bombus_flavidus_occ)
bombus_flavidus_occ_final <- occ_download_get(bombus_flavidus_occ) %>% occ_download_import()
bombus_flavidus_occ_final <- bombus_flavidus_occ_final %>%rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

vroom_write(bombus_flavidus_occ_final, "D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_flavidus.csv")

#Make Convex Hull Range Maps####

#bombus impatiens
bombus_impatiens <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_impatiens.csv")%>%
  dplyr::select(10,22,23)%>%filter(Latitude<80)

bombus_impatiens_xy <- vect(bombus_impatiens, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_impatiens_xy)
bombus_impatiens_convhull <- convHull(bombus_impatiens_xy)
plot(bombus_impatiens_convhull)
writeVector(bombus_impatiens_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_impatiens.shp")

#bombus bimaculatus

bombus_bimaculatus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_bimaculatus.csv")%>%
  dplyr::select(10,22,23)

bombus_bimaculatus_xy <- vect(bombus_bimaculatus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_bimaculatus_xy)
bombus_bimaculatus_convhull <- convHull(bombus_bimaculatus_xy)
plot(bombus_bimaculatus_convhull)
writeVector(bombus_bimaculatus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_bimaculatus.shp")
#bombus griseocollis
bombus_griseocollis <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_griseocollis.csv")%>%
  dplyr::select(10,22,23)%>% filter(Longitude<0)

bombus_griseocollis_xy <- vect(bombus_griseocollis, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_griseocollis_xy)
bombus_griseocollis_convhull <- convHull(bombus_griseocollis_xy)
plot(bombus_griseocollis_convhull)
writeVector(bombus_griseocollis_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_griseocollis.shp")

#bombus rufocinctus
bombus_rufocinctus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_rufocinctus.csv")%>%
  dplyr::select(10,22,23)

bombus_rufocinctus_xy <- vect(bombus_rufocinctus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_rufocinctus_xy)
bombus_rufocinctus_convhull <- convHull(bombus_rufocinctus_xy)
plot(bombus_rufocinctus_convhull)
writeVector(bombus_rufocinctus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_rufocinctus.shp")

#bombus
bombus_vagans <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_vagans.csv")%>%
  dplyr::select(10,22,23)

bombus_vagans_xy <- vect(bombus_vagans, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_vagans_xy)
bombus_vagans_convhull <- convHull(bombus_vagans_xy)
plot(bombus_vagans_convhull)
writeVector(bombus_vagans_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_vagans.shp")

#bombus perplexus
bombus_perplexus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_perplexus.csv")%>%
  dplyr::select(10,22,23)

bombus_perplexus_xy <- vect(bombus_perplexus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_perplexus_xy)
bombus_perplexus_convhull <- convHull(bombus_perplexus_xy)
plot(bombus_perplexus_convhull)
writeVector(bombus_perplexus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_perplexus.shp")

#bombus ternarius
bombus_ternarius <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_ternarius.csv")%>%
  dplyr::select(10,22,23)

bombus_ternarius_xy <- vect(bombus_ternarius, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_ternarius_xy)
bombus_ternarius_convhull <- convHull(bombus_ternarius_xy)
plot(bombus_ternarius_convhull)
writeVector(bombus_ternarius_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_ternarius.shp")

#bombus fervidus
bombus_fervidus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_fervidus.csv")%>%
  dplyr::select(10,22,23)%>%filter(Longitude<0)

bombus_fervidus_xy <- vect(bombus_fervidus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_fervidus_xy)
bombus_fervidus_convhull <- convHull(bombus_fervidus_xy)
plot(bombus_fervidus_convhull)
writeVector(bombus_fervidus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_fervidus.shp")

#bombus borealis
bombus_borealis <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_borealis.csv")%>%
  dplyr::select(10,22,23)

bombus_borealis_xy <- vect(bombus_borealis, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_borealis_xy)
bombus_borealis_convhull <- convHull(bombus_borealis_xy)
plot(bombus_borealis_convhull)
writeVector(bombus_borealis_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_borealis.shp")

#bombus pens
bombus_pensylvanicus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_pensylvanicus.csv")%>%
  dplyr::select(10,22,23)%>%filter(Longitude< -20)

bombus_pensylvanicus_xy <- vect(bombus_pensylvanicus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_pensylvanicus_xy)

bombus_pensylvanicus_convhull <- convHull(bombus_pensylvanicus_xy)
plot(bombus_pensylvanicus_convhull)
writeVector(bombus_pensylvanicus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_pensylvanicus.shp")

#bombus citrinus
bombus_citrinus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_citrinus.csv")%>%
  dplyr::select(10,22,23)%>%filter(Longitude < 0)

bombus_citrinus_xy <- vect(bombus_citrinus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_citrinus_xy)

bombus_citrinus_convhull <- convHull(bombus_citrinus_xy)
plot(bombus_citrinus_convhull)
writeVector(bombus_citrinus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_citrinus.shp")

#bombus auricomus
bombus_auricomus <- vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_auricomus.csv")%>%
  dplyr::select(10,22,23)

bombus_auricomus_xy <- vect(bombus_auricomus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_auricomus_xy)

bombus_auricomus_convhull <- convHull(bombus_auricomus_xy)
plot(bombus_auricomus_convhull)
writeVector(bombus_auricomus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_auricomus.shp")

#bombus terricola
bombus_terricola <-vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_terricola.csv")%>%
  dplyr::select(10,22,23)

bombus_terricola_xy <- vect(bombus_terricola, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_terricola_xy)

bombus_terricola_convhull <- convHull(bombus_terricola_xy)
plot(bombus_terricola_convhull)
writeVector(bombus_terricola_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_terricola.shp")

#bombus flavidus
bombus_flavidus <-vroom("D:/Thesis Projects/Bee Beta Diversity/Occurance Data/bombus_flavidus.csv")%>%
  dplyr::select(10,22,23)%>%filter(Longitude < -10)

bombus_flavidus_xy <- vect(bombus_flavidus, geom=c("Longitude", "Latitude"), crs="+proj=longlat +datum=WGS84")
plot(bombus_flavidus_xy)

bombus_flavidus_convhull <- convHull(bombus_flavidus_xy)
plot(bombus_flavidus_convhull)
writeVector(bombus_flavidus_convhull, "D:/Thesis Projects/Bee Beta Diversity/shapefiles/bombus_flavidus.shp")

#get climate data####
bee_maps <- list.files("D:/Thesis Projects/Bee Beta Diversity/shapefiles", pattern = ".shp", full.names = T)

DF <- data.frame(Binomial= character(),
                 TMin = numeric(),
                 TMax = numeric(),
                 Month = character())

Maxpath <- "E:/Coding Files/Climate Data/Temperature Files/Tmax/Baseline"
Minpath <- "E:/Coding Files/Climate Data/Temperature Files/Tmin/Min"

mnth <- list("_01.tif", "_02.tif", "_03.tif", "_04.tif", "_05.tif", "_06.tif",
             "_07.tif", "_08.tif", "_09.tif","_10.tif", "_11.tif", "_12.tif")

m <- list("01", "02", "03", "04", "05", "06","07", "08", "09","10", "11", "12")

###Extracts Monthly Thermal Limits

for(i in 1:length(mnth)){
  rmax <- rast(list.files(Maxpath, pattern = mnth[[i]], full.names = T))
  rmin <- rast(list.files(Minpath, pattern = mnth[[i]], full.names = T))
  
  for (sp in bee_maps){
    polyg <- vect(sp)
    maxmean <- as.data.frame(extract(rmax, polyg, fun=max, na.rm=T))
    maxmean <- t(maxmean[is.finite(rowSums(maxmean)),])
    if (ncol(maxmean) < 2){
      mxm <- mean(maxmean[-1,])
    } else {
      mxm <- mean(colMeans(t(maxmean[-1,])))
    }
    minmean <- as.data.frame(extract(rmin, polyg, fun=min, na.rm=T))
    minmean <- t(minmean[is.finite(rowSums(minmean)),])
    if (ncol(minmean) < 2){
      mnm <- mean(minmean[-1,])
    } else {
      mnm <- mean(colMeans(t(minmean[-1,])))
    }
    sp_sub <- basename(sp)
    sp_sub <- sub("_", " ", sp_sub)
    sp_sub <- sub("bombus", "Bombus", sp_sub)
    sp_sub <- sub(".shp", "", sp_sub) 
    
    DF[nrow(DF)+1,] = c(sp_sub,mnm,mxm,m[[i]])
    rm(polyg, maskmax, maskmin, maxv,minv, meanmax, meanmin, sp_sub, mnm,mxm)
    gc()
  }
}
DF_ordered <- DF[with(DF, order(Binomial, Month)), ]
DF_ordered$TMin <- as.numeric(DF_ordered$TMin)
DF_ordered$TMax <- as.numeric(DF_ordered$TMax)
DF_ordered$Month <- as.numeric(DF_ordered$Month)
write.csv(DF_ordered, "D:/Thesis Projects/Bee Beta Diversity/Thermal_Niche_Limits.csv")

###AI

DF2 <- data.frame(Binomial= character(),
                 AMin = numeric(),
                 AMax = numeric(),
                 Month = character())

arid_path <- "E:/Coding Files/Climate Data/Environmental Maps/Baseline/"

m <- list("Jan", "Feb", "Mar", "Apr", "May", "June","July", "Aug", "Sep","Oct", "Nov", "Dec")

for(i in 1:length(m)){
  rast_arid <- rast(list.files(arid_path, pattern = m[[i]], full.names = T))
  for (sp in bee_maps){

    polyg <- vect(sp)
    quant <- crop(rast_arid, polyg)
    quant <- mask(rast_arid, polyg)
    
    lower_q <- sapply((global(quant, quantile, probs=c(0.25), na.rm=T)),as.numeric)
    upper_q <- sapply((global(quant, quantile, probs=c(0.75), na.rm=T)),as.numeric)
    
    if (ncol(lower_q) < 2){
      lower_q <- min(lower_q)
    } else {
      lower_q <- min(apply(lower_q, 1, min, na.rm=TRUE))
    }
    
    if (ncol(upper_q) < 2){
      upper_q <- max(upper_q)
    } else {
      upper_q <- max(apply(upper_q, 1, max, na.rm=TRUE))
    }
    
    IQRr <- as.data.frame(extract(rast_arid, polyg, fun=IQR, na.rm=T))[,-1]
    IQRr <- t(IQRr[is.finite(rowSums(IQRr)),])
    if (ncol(IQRr) < 2){
      IQRr <- max(IQRr)
    } else {
      IQRr <- max(apply(IQRr, 1, max, na.rm=TRUE))
    }
    
    
    maxair <- as.data.frame(extract(rast_arid, polyg, fun=max, na.rm=T))[,-1]
    maxair <- t(maxair[is.finite(rowSums(maxair)),])
    maxair[maxair > (upper_q + (1.5*IQRr))] <- (upper_q + (1.5*IQRr))
    if (ncol(maxair) < 2){
      maxair <- mean(maxair)
    } else {
      maxair <- mean(apply(maxair, 1, max, na.rm=TRUE))
    }
    
    
    minair <- as.data.frame(extract(rast_arid, polyg, fun=min, na.rm=T))[,-1]
    minair <- t(minair[is.finite(rowSums(minair)),])
    minair[minair < (lower_q - (1.5*IQRr))] <- (lower_q - (1.5*IQRr))
    if (ncol(minair) < 2){
      minair <- mean(minair)
    } else {
      minair <- mean(apply(minair, 1, min, na.rm=TRUE))
    }
    
    sp_sub <- basename(sp)
    sp_sub <- sub("_", " ", sp_sub)
    sp_sub <- sub("bombus", "Bombus", sp_sub)
    sp_sub <- sub(".shp", "", sp_sub)
    DF2[nrow(DF2)+1,] = c(sp_sub,minair,maxair,m[[i]])
    rm(polyg, quant, minair, maxair,IQRr, lower_q, upper_q, sp_sub)
    gc()
  }
}

DF_ordered2 <- DF2[with(DF2, order(Binomial, Month)), ]
DF_ordered2$AMin <- as.numeric(DF_ordered2$AMin)
DF_ordered2$AMax <- as.numeric(DF_ordered2$AMax)
DF_ordered2 <- DF_ordered2 %>% rename(Month_Code = Month)
months_list <- rep(c(4,8,12,2,1,7,6,3,5,11,10,9),14)
DF_ordered2$Month <- months_list
DF_ordered2 <- DF_ordered2 %>% arrange(Binomial,Month)

write.csv(DF_ordered2, "D:/Thesis Projects/Bee Beta Diversity/Aridity_Niche_Limits.csv")

all_lim <- left_join(DF_ordered, DF_ordered2)%>%relocate(Month, .after = AMax)
write.csv(all_lim, "D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits.csv")

#get climate data using points####

DF <- data.frame(Binomial= character(),
                 TMin = numeric(),
                 TMax = numeric(),
                 Month = character())

Maxpath <- "E:/Coding Files/Climate Data/Temperature Files/Tmax/Baseline"
Minpath <- "E:/Coding Files/Climate Data/Temperature Files/Tmin/Min"

mnth <- list("_01.tif", "_02.tif", "_03.tif", "_04.tif", "_05.tif", "_06.tif",
             "_07.tif", "_08.tif", "_09.tif","_10.tif", "_11.tif", "_12.tif")

m <- list("01", "02", "03", "04", "05", "06","07", "08", "09","10", "11", "12")
bee_list_points <- list(bombus_impatiens_xy,
                        bombus_bimaculatus_xy,
                        bombus_griseocollis_xy,
                        bombus_rufocinctus_xy,
                        bombus_vagans_xy,
                        bombus_perplexus_xy,
                        bombus_ternarius_xy,
                        bombus_fervidus_xy,
                        bombus_borealis_xy,
                        bombus_pensylvanicus_xy,
                        bombus_citrinus_xy,
                        bombus_auricomus_xy,
                        bombus_terricola_xy,
                        bombus_flavidus_xy)

bee_list_names <- list("Bombus impatiens",
                       "Bombus bimaculatus",
                       "Bombus griseocollis",
                       "Bombus rufocinctus",
                       "Bombus vagans",
                       "Bombus perplexus",
                       "Bombus ternarius",
                       "Bombus fervidus",
                       "Bombus borealis",
                       "Bombus pensylvanicus",
                       "Bombus citrinus",
                       "Bombus auricomus",
                       "Bombus terricola",
                       "Bombus flavidus")
###Extracts Monthly Thermal Limits
for(i in 1:length(mnth)){
  rmax <- rast(list.files(Maxpath, pattern = mnth[[i]], full.names = T))
  rmin <- rast(list.files(Minpath, pattern = mnth[[i]], full.names = T))
  
  for (sp in 1:length(bee_list_points)){
    polyg <- bee_list_points[[sp]]
    
    maxmean <- as.data.frame(extract(rmax, polyg, fun=max, na.rm=T))
    maxmean <- maxmean %>% dplyr::select(-1)%>%summarise_all(max, na.rm=T)
    maxmean <- t(maxmean[is.finite(rowSums(maxmean)),])
    
    if (ncol(maxmean) < 2){
      mxm <- mean(maxmean[-1,])
    } else {
      mxm <- mean(colMeans(t(maxmean[-1,])))
    }
    
    minmean <- as.data.frame(extract(rmin, polyg, fun=min, na.rm=T))
    minmean <- minmean %>% dplyr::select(-1)%>%summarise_all(min, na.rm=T)
    minmean <- t(minmean[is.finite(rowSums(minmean)),])
    
    if (ncol(minmean) < 2){
      mnm <- mean(minmean[-1,])
    } else {
      mnm <- mean(colMeans(t(minmean[-1,])))
    }
    
    DF[nrow(DF)+1,] = c(bee_list_names[sp],mnm,mxm,m[[i]])
    rm(polyg, maskmax, maskmin, maxv,minv, meanmax, meanmin, mnm,mxm)
    gc()
  }
}
DF_ordered <- DF[with(DF, order(Binomial, Month)), ]
DF_ordered$TMin <- as.numeric(DF_ordered$TMin)
DF_ordered$TMax <- as.numeric(DF_ordered$TMax)
DF_ordered$Month <- as.numeric(DF_ordered$Month)
write.csv(DF_ordered, "D:/Thesis Projects/Bee Beta Diversity/Thermal_Niche_Limits_points.csv")


##ai
DF2 <- data.frame(Binomial= character(),
                  AMin = numeric(),
                  AMax = numeric(),
                  Month = character())

arid_path <- "E:/Coding Files/Climate Data/Environmental Maps/Baseline/"

m <- list("Jan", "Feb", "Mar", "Apr", "May", "June","July", "Aug", "Sep","Oct", "Nov", "Dec")

for(i in 1:length(m)){
  rast_arid <- rast(list.files(arid_path, pattern = m[[i]], full.names = T))
  for (sp in 1:length(bee_list_points)){
    
    polyg <- bee_list_points[[sp]]
    
    maxair <- as.data.frame(extract(rast_arid, polyg, fun=max, na.rm=T))[,-1]
    is.na(maxair) <- sapply(maxair, is.infinite)
    
    maxair_max <- maxair %>% dplyr::select(-1)%>%summarise_all(max, na.rm=T)
    
    maxair_IQR <- maxair %>% dplyr::select(-1)%>%summarise_all(IQR, na.rm=T)
    maxair_lq <- (maxair %>% dplyr::select(-1)%>%summarise_all(quantile, na.rm=T))[2,]
    maxair_uq <- (maxair %>% dplyr::select(-1)%>%summarise_all(quantile, na.rm=T))[4,]
    
    maxair_max <- t(maxair_max[is.finite(rowSums(maxair_max)),])
    maxair_uq <- t(maxair_uq[is.finite(rowSums(maxair_uq)),])

    maxair_max <- cbind(maxair_max,maxair_uq,maxair_uq)
    colnames(maxair_max) <- c("amax","iqr","uq")
    maxair_max <- as.data.frame(maxair_max)
    maxair_max <- maxair_max %>% mutate(Lim = uq+(1.5*iqr))%>%
      mutate(AMAX = ifelse(amax>Lim, Lim, amax))
    
    amax <- mean(maxair_max$AMAX, na.rm=T)
    
   
    minair <- as.data.frame(extract(rast_arid, polyg, fun=min, na.rm=T))[,-1]
    is.na(minair) <- sapply(minair, is.infinite)
    minair_min <- minair %>% dplyr::select(-1)%>%summarise_all(min, na.rm=T)
    
    minair_IQR <- minair %>% dplyr::select(-1)%>%summarise_all(IQR, na.rm=T)
    minair_lq <- (minair %>% dplyr::select(-1)%>%summarise_all(quantile, na.rm=T))[2,]
    minair_uq <- (minair %>% dplyr::select(-1)%>%summarise_all(quantile, na.rm=T))[4,]
    
    minair_min <- t(minair_min[is.finite(rowSums(minair_min)),])
    amin <- mean(minair_min[,1], na.rm=T)
    
    DF2[nrow(DF2)+1,] = c(bee_list_names[sp],amin,amax,m[[i]])
    rm(polyg, amin, amax)
    gc()
  }
}

DF_ordered2 <- DF2[with(DF2, order(Binomial, Month)), ]
DF_ordered2$AMin <- as.numeric(DF_ordered2$AMin)
DF_ordered2$AMax <- as.numeric(DF_ordered2$AMax)
DF_ordered2 <- DF_ordered2 %>% rename(Month_Code = Month)
months_list <- rep(c(4,8,12,2,1,7,6,3,5,11,10,9),14)
DF_ordered2$Month <- months_list
DF_ordered2 <- DF_ordered2 %>% arrange(Binomial,Month)

write.csv(DF_ordered2, "D:/Thesis Projects/Bee Beta Diversity/Aridity_Niche_Limits_points.csv")

all_lim <- left_join(DF_ordered, DF_ordered2)%>%relocate(Month, .after = AMax)
write.csv(all_lim, "D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits_points.csv")

#compare
all_lim_points <- vroom("D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits_points.csv")%>%
  rename(TMin_p = TMin, TMax_p = TMax, AMax_p = AMax, AMin_p = AMin)%>%dplyr::select(-1)

all_lim <- vroom("D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits.csv")%>%
  dplyr::select(-1)%>%
  left_join(all_lim_points)

cor.test(all_lim$TMin_p, all_lim$TMin)
cor.test(all_lim$AMin_p, all_lim$AMin)
cor.test(all_lim$TMax_p, all_lim$TMax)
cor.test(all_lim$AMax_p, all_lim$AMax)
