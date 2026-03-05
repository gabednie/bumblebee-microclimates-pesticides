#this script extracts aridity index (0.5 minute) grid cells for each site/year/month
 
library(dplyr)
library(vroom)
library(rgbif)
library(terra)
library(tidyr)

bee_data <- vroom("D:/Thesis Projects/Bee Beta Diversity/Bee_data.csv")%>%
  separate(date, into=c("Month","Day","Year"), sep = '/')%>%
  filter(!is.na(Month))%>%
  mutate(Month_n = ifelse(Month=="5","May",
                          ifelse(Month=="6", "June",
                                 ifelse(Month=="7", "July",
                                        ifelse(Month=="8", "Aug",
                                               ifelse(Month=="9", "Sep", "Oct"))))))

bee_sites <- bee_data %>% dplyr::select(site,lat,long, Month,Month_n, Year)%>%distinct()

sites_arid <- data.frame(NULL)

for(i in 1:nrow(bee_sites)){
  
  a_rast <- rast(paste0("E:/Coding Files/Climate Data/Environmental Maps/", bee_sites[i,"Year"],
                        "/Aritity_",bee_sites[i,"Month_n"],"_",bee_sites[i,"Year"],".tif"))
  
  point_xy <- vect(bee_sites[i,], geom=c("long", "lat"), crs="+proj=longlat +datum=WGS84")

    df <- data.frame(site = bee_sites[i,"site"],
                   Month = bee_sites[i,"Month"],
                   Year = bee_sites[i,"Year"],
                   AI = terra::extract(a_rast, point_xy)[1,2])
    
    sites_arid <- rbind(sites_arid, df)
}

bee_data <- left_join(bee_data,sites_arid)

vroom_write(bee_data, "D:/Thesis Projects/Bee Beta Diversity/all_years_arid.csv")
