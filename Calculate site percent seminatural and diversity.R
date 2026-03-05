#this script exracts the diversity of land use type around each site
#and the percent of each sites seminatural area within 500m 1500m and 3000m
#buffers

library(terra)
library(vroom)
library(dplyr)

c15 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2015_on_v3.tif")
c16 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2016_on_v3.tif")
c17 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2017_on_v4.tif")
c18 <- rast("D:/Thesis Projects/Bee Beta Diversity/crop data/aci_2018_on_v3.tif")

bee <- vroom("D:/Thesis Projects/Bee Beta Diversity/bee_data.csv")

xy <- bee %>% dplyr::select(site, long, lat) %>% distinct()

xy_vec <- vect(xy, geom = c("long","lat"), "+proj=longlat")
c15_p <- project(c15, xy_vec, method="near")
c16_p <- project(c16, xy_vec, method="near")
c17_p <- project(c17, xy_vec, method="near")
c18_p <- project(c18, xy_vec, method="near")

df_final_500 <- data.frame(NULL)
df_final_1500 <- data.frame(NULL)
df_final_3000 <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  c15_500 <- extract(c15_p, b_500)
  c15_1500 <- extract(c15_p, b_1500)
  c15_3000 <- extract(c15_p, b_3000)
  
  c15_500 <- c15_500 %>% group_by(aci_2015_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c15_500))%>%
    mutate(Year = 2015,Site = xy$site[i])%>%rename(landcode= aci_2015_on_v3)
  
  c15_1500 <- c15_1500 %>% group_by(aci_2015_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c15_1500))%>%
    mutate(Year = 2015,Site = xy$site[i])%>%rename(landcode= aci_2015_on_v3)
  
  c15_3000 <- c15_3000 %>% group_by(aci_2015_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c15_3000))%>%
    mutate(Year = 2015,Site = xy$site[i])%>%rename(landcode= aci_2015_on_v3)
  
  c16_500 <- extract(c16_p, b_500)
  c16_1500 <- extract(c16_p, b_1500)
  c16_3000 <- extract(c16_p, b_3000)
  
  c16_500 <- c16_500 %>% group_by(aci_2016_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c16_500))%>%
    mutate(Year = 2016,Site = xy$site[i])%>%rename(landcode= aci_2016_on_v3)
  
  c16_1500 <- c16_1500 %>% group_by(aci_2016_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c16_1500))%>%
    mutate(Year = 2016,Site = xy$site[i])%>%rename(landcode= aci_2016_on_v3)
  
  c16_3000 <- c16_3000 %>% group_by(aci_2016_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c16_3000))%>%
    mutate(Year = 2016,Site = xy$site[i])%>%rename(landcode= aci_2016_on_v3)
  
  c17_500 <- extract(c17_p, b_500)
  c17_1500 <- extract(c17_p, b_1500)
  c17_3000 <- extract(c17_p, b_3000)
  
  c17_500 <- c17_500 %>% group_by(aci_2017_on_v4)%>%summarise(Count = n(),
                                                                Total = nrow(c17_500))%>%
    mutate(Year = 2017,Site = xy$site[i])%>%rename(landcode= aci_2017_on_v4)
  
  c17_1500 <- c17_1500 %>% group_by(aci_2017_on_v4)%>%summarise(Count = n(),
                                                                Total = nrow(c17_1500))%>%
    mutate(Year = 2017,Site = xy$site[i])%>%rename(landcode= aci_2017_on_v4)
  
  c17_3000 <- c17_3000 %>% group_by(aci_2017_on_v4)%>%summarise(Count = n(),
                                                                Total = nrow(c17_3000))%>%
    mutate(Year = 2017,Site = xy$site[i])%>%rename(landcode= aci_2017_on_v4)
  
  c18_500 <- extract(c18_p, b_500)
  c18_1500 <- extract(c18_p, b_1500)
  c18_3000 <- extract(c18_p, b_3000)
  
  c18_500 <- c18_500 %>% group_by(aci_2018_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c18_500))%>%
    mutate(Year = 2018,Site = xy$site[i])%>%rename(landcode= aci_2018_on_v3)
  
  c18_1500 <- c18_1500 %>% group_by(aci_2018_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c18_1500))%>%
    mutate(Year = 2018,Site = xy$site[i])%>%rename(landcode= aci_2018_on_v3)
  
  c18_3000 <- c18_3000 %>% group_by(aci_2018_on_v3)%>%summarise(Count = n(),
                                                                Total = nrow(c18_3000))%>%
    mutate(Year = 2018,Site = xy$site[i])%>%rename(landcode= aci_2018_on_v3)
  
  all_codes_500 <- rbind(c15_500,c16_500,c17_500,c18_500)
  all_codes_1500 <- rbind(c15_1500,c16_1500,c17_1500,c18_1500)
  all_codes_3000 <- rbind(c15_3000,c16_3000,c17_3000,c18_3000)
  
  df_final_500 <- rbind(df_final_500,all_codes_500)
  df_final_1500 <- rbind(df_final_1500,all_codes_1500)
  df_final_3000 <- rbind(df_final_3000,all_codes_3000)
  
}

landcodes <- vroom("D:/Thesis Projects/Bee Beta Diversity/crop data/land_code_types2.csv")

df_final_500_f <- left_join(df_final_500, landcodes)
df_final_1500_f <- left_join(df_final_1500, landcodes)
df_final_3000_f <- left_join(df_final_3000, landcodes)

df_final_500_f_percent <- df_final_500_f %>% group_by(Site, Year, Class, Total)%>%
  summarise(Count = sum(Count))%>%ungroup()

df_semi <- df_final_500_f_percent %>% filter(Class=="Semi Natural")%>%
  mutate(Percent_Semi_Natural = Count/Total)%>%dplyr::select(Site, Year, Percent_Semi_Natural)

df_no_semi <- df_final_500_f_percent %>% filter(!Class == "Water") %>% group_by(Site,Year) %>% mutate(N = n())%>%
  ungroup()%>%filter(Class == "Non Natural" & N == 1)%>%mutate(Percent_Semi_Natural = 0)%>%select(Site,Year,Percent_Semi_Natural)

df_final_500_f_percent <- rbind(df_semi,df_no_semi)

df_final_500_f_diversity <- df_final_500_f %>% mutate(sp_proportion = (Count/Total) * log(Count/Total))%>%
  group_by(Site, Year) %>% summarise(Shannon_index = -1 * sum(sp_proportion))%>%
  mutate(Hill_Shannon = exp(Shannon_index))


df_final_1500_f_percent <- df_final_1500_f %>% group_by(Site, Year, Class, Total)%>%
  summarise(Count = sum(Count))%>%ungroup()%>%filter(Class=="Semi Natural")%>%
  mutate(Percent_Semi_Natural = Count/Total)%>%dplyr::select(Site, Year, Percent_Semi_Natural)

df_final_3000_f_percent <- df_final_3000_f %>% group_by(Site, Year, Class, Total)%>%
  summarise(Count = sum(Count))%>%ungroup()%>%filter(Class=="Semi Natural")%>%
  mutate(Percent_Semi_Natural = Count/Total)%>%dplyr::select(Site, Year, Percent_Semi_Natural)


df_final_500_f_diversity <- df_final_500_f %>% mutate(sp_proportion = (Count/Total) * log(Count/Total))%>%
  group_by(Site, Year) %>% summarise(Shannon_index = -1 * sum(sp_proportion))%>%
  mutate(Hill_Shannon = exp(Shannon_index))

df_final_1500_f_diversity <- df_final_1500_f %>% mutate(sp_proportion = (Count/Total) * log(Count/Total))%>%
  group_by(Site, Year) %>% summarise(Shannon_index = -1 * sum(sp_proportion))%>%
  mutate(Hill_Shannon = exp(Shannon_index))

df_final_3000_f_diversity <- df_final_3000_f %>% mutate(sp_proportion = (Count/Total) * log(Count/Total))%>%
  group_by(Site, Year) %>% summarise(Shannon_index = -1 * sum(sp_proportion))%>%
  mutate(Hill_Shannon = exp(Shannon_index))


df_500 <- left_join(df_final_500_f_percent, df_final_500_f_diversity)
df_1500 <- left_join(df_final_1500_f_percent, df_final_1500_f_diversity)
df_3000 <- left_join(df_final_3000_f_percent, df_final_3000_f_diversity)

vroom_write(df_500, "D:/Thesis Projects/Bee Beta Diversity/crop data/landcover diversity/Seminatural_shannon_buffers_500.csv")
vroom_write(df_1500, "D:/Thesis Projects/Bee Beta Diversity/crop data/landcover diversity/Seminatural_shannon_buffers_1500.csv")
vroom_write(df_3000, "D:/Thesis Projects/Bee Beta Diversity/crop data/landcover diversity/Seminatural_shannon_buffers_3000.csv")

