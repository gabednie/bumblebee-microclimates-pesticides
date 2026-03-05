#this script combines pesticide rasters and calculates the total amount of 
#pesticide applications within buffers and calucates a proportional pesticide 
#to area estimate.

library(terra)
library(vroom)
library(dplyr)

bee <- vroom("D:/Thesis Projects/Bee Beta Diversity/bee_data.csv")

xy <- bee %>% dplyr::select(site, long, lat) %>% distinct()

xy_vec <- vect(xy, geom = c("long","lat"), crs= "+proj=longlat")

pesticides <- list.files("D:/Thesis Projects/Bee Beta Diversity/crop data/Pesticide Maps", pattern = ".tif", full.names = T)



#2015 max####
combined_rast <- sum(rast(pesticides[1]), rast(pesticides[13]) , rast(pesticides[25]), na.rm=T)

combined_rast_2015_max <- project(combined_rast, "+proj=longlat", method="near")

df_2015_max <- data.frame(NULL)

for(i in 1:nrow(xy)){
    cur_pt <- xy_vec[i]
    b_500 <- buffer(cur_pt, width = 500)
    b_1500 <- buffer(cur_pt, width = 1500)
    b_3000 <- buffer(cur_pt, width = 3000)
    
    m_500 <- crop(combined_rast_2015_max, b_500, mask=T)
    m_1500 <- crop(combined_rast_2015_max, b_1500, mask=T)
    m_3000 <- crop(combined_rast_2015_max, b_3000, mask=T)
    
    s_500 <- global(m_500, "sum", na.rm=T)
    s_1500 <- global(m_1500, "sum", na.rm=T)
    s_3000 <- global(m_3000, "sum", na.rm=T)
    
    df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                     buffer_size = c(500,1500,3000),
                     Area = c(expanse(b_500, unit="m"),
                              expanse(b_1500, unit="m"),
                              expanse(b_3000, unit="m")),
                     Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                     Year = c(2015, 2015, 2015),
                     Variable = c("Max","Max","Max"))
    
    df_2015_max <- rbind(df_2015_max, df)
}
df_2015_max <- df_2015_max %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2016 max#### 
combined_rast_2016_max <- sum(rast(pesticides[2]), rast(pesticides[14]) , rast(pesticides[26]), na.rm=T)

combined_rast_2016_max <- project(combined_rast_2016_max, "+proj=longlat", method="near")

df_2016_max <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2016_max, b_500, mask=T)
  m_1500 <- crop(combined_rast_2016_max, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2016_max, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2016, 2016, 2016),
                   Variable = c("Max","Max","Max"))
  
  df_2016_max <- rbind(df_2016_max, df)
}
df_2016_max <- df_2016_max %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2017 max####  
combined_rast_2017_max <- sum(rast(pesticides[3]), rast(pesticides[15]) , rast(pesticides[27]), na.rm=T)

combined_rast_2017_max <- project(combined_rast_2017_max, "+proj=longlat", method="near")

df_2017_max <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2017_max, b_500, mask=T)
  m_1500 <-crop(combined_rast_2017_max, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2017_max, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2017, 2017, 2017),
                   Variable = c("Max","Max","Max"))
  
  df_2017_max <- rbind(df_2017_max, df)
}
df_2017_max <- df_2017_max %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2018 max####  
combined_rast_2018_max <- sum(rast(pesticides[4]), rast(pesticides[16]) , rast(pesticides[28]), na.rm=T)

combined_rast_2018_max <- project(combined_rast_2018_max, "+proj=longlat", method="near")

df_2018_max <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2018_max, b_500, mask=T)
  m_1500 <-crop(combined_rast_2018_max, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2018_max, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2018, 2018, 2018),
                   Variable = c("Max","Max","Max"))
  
  df_2018_max <- rbind(df_2018_max, df)
}
df_2018_max <- df_2018_max %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

Pest_Max_Buffers <- rbind(df_2015_max,df_2016_max, df_2017_max, df_2018_max)
vroom_write(Pest_Max_Buffers, "D:/Thesis Projects/Bee Beta Diversity/crop data/Site_Max_Pesticide_Buffer.csv")

#2015 mean####
combined_rast <- sum(rast(pesticides[5]), rast(pesticides[17]) , rast(pesticides[29]), na.rm=T)

combined_rast_2015_mean <- project(combined_rast, "+proj=longlat", method="near")

df_2015_mean <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2015_mean, b_500, mask=T)
  m_1500 <- crop(combined_rast_2015_mean, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2015_mean, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2015, 2015, 2015),
                   Variable = c("mean","mean","mean"))
  
  df_2015_mean <- rbind(df_2015_mean, df)
}
df_2015_mean <- df_2015_mean %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2016 mean#### 
combined_rast_2016_mean <- sum(rast(pesticides[6]), rast(pesticides[18]) , rast(pesticides[30]), na.rm=T)

combined_rast_2016_mean <- project(combined_rast_2016_mean, "+proj=longlat", method="near")

df_2016_mean <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2016_mean, b_500, mask=T)
  m_1500 <- crop(combined_rast_2016_mean, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2016_mean, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2016, 2016, 2016),
                   Variable = c("mean","mean","mean"))
  
  df_2016_mean <- rbind(df_2016_mean, df)
}
df_2016_mean <- df_2016_mean %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2017 mean####  
combined_rast_2017_mean <- sum(rast(pesticides[7]), rast(pesticides[19]) , rast(pesticides[31]), na.rm=T)

combined_rast_2017_mean <- project(combined_rast_2017_mean, "+proj=longlat", method="near")

df_2017_mean <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2017_mean, b_500, mask=T)
  m_1500 <-crop(combined_rast_2017_mean, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2017_mean, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2017, 2017, 2017),
                   Variable = c("mean","mean","mean"))
  
  df_2017_mean <- rbind(df_2017_mean, df)
}
df_2017_mean <- df_2017_mean %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2018 mean####  
combined_rast_2018_mean <- sum(rast(pesticides[8]), rast(pesticides[20]) , rast(pesticides[32]), na.rm=T)

combined_rast_2018_mean <- project(combined_rast_2018_mean, "+proj=longlat", method="near")

df_2018_mean <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2018_mean, b_500, mask=T)
  m_1500 <-crop(combined_rast_2018_mean, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2018_mean, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2018, 2018, 2018),
                   Variable = c("mean","mean","mean"))
  
  df_2018_mean <- rbind(df_2018_mean, df)
}
df_2018_mean <- df_2018_mean %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

Pest_mean_Buffers <- rbind(df_2015_mean,df_2016_mean, df_2017_mean, df_2018_mean)
vroom_write(Pest_mean_Buffers, "D:/Thesis Projects/Bee Beta Diversity/crop data/Site_Mean_Pesticide_Buffer.csv")
#2015 min####
combined_rast <- sum(rast(pesticides[9]), rast(pesticides[21]) , rast(pesticides[33]), na.rm=T)

combined_rast_2015_min <- project(combined_rast, "+proj=longlat", method="near")

df_2015_min <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2015_min, b_500, mask=T)
  m_1500 <- crop(combined_rast_2015_min, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2015_min, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2015, 2015, 2015),
                   Variable = c("min","min","min"))
  
  df_2015_min <- rbind(df_2015_min, df)
}
df_2015_min <- df_2015_min %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2016 min#### 
combined_rast_2016_min <- sum(rast(pesticides[10]), rast(pesticides[22]) , rast(pesticides[34]), na.rm=T)

combined_rast_2016_min <- project(combined_rast_2016_min, "+proj=longlat", method="near")

df_2016_min <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2016_min, b_500, mask=T)
  m_1500 <- crop(combined_rast_2016_min, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2016_min, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2016, 2016, 2016),
                   Variable = c("min","min","min"))
  
  df_2016_min <- rbind(df_2016_min, df)
}
df_2016_min <- df_2016_min %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2017 min####  
combined_rast_2017_min <- sum(rast(pesticides[11]), rast(pesticides[23]) , rast(pesticides[35]), na.rm=T)

combined_rast_2017_min <- project(combined_rast_2017_min, "+proj=longlat", method="near")

df_2017_min <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2017_min, b_500, mask=T)
  m_1500 <-crop(combined_rast_2017_min, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2017_min, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2017, 2017, 2017),
                   Variable = c("min","min","min"))
  
  df_2017_min <- rbind(df_2017_min, df)
}
df_2017_min <- df_2017_min %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

#2018 min####  
combined_rast_2018_min <- sum(rast(pesticides[12]), rast(pesticides[24]) , rast(pesticides[36]), na.rm=T)

combined_rast_2018_min <- project(combined_rast_2018_min, "+proj=longlat", method="near")

df_2018_min <- data.frame(NULL)

for(i in 1:nrow(xy)){
  cur_pt <- xy_vec[i]
  b_500 <- buffer(cur_pt, width = 500)
  b_1500 <- buffer(cur_pt, width = 1500)
  b_3000 <- buffer(cur_pt, width = 3000)
  
  m_500 <- crop(combined_rast_2018_min, b_500, mask=T)
  m_1500 <-crop(combined_rast_2018_min, b_1500, mask=T)
  m_3000 <- crop(combined_rast_2018_min, b_3000, mask=T)
  
  s_500 <- global(m_500, "sum", na.rm=T)
  s_1500 <- global(m_1500, "sum", na.rm=T)
  s_3000 <- global(m_3000, "sum", na.rm=T)
  
  df <- data.frame(site = c(xy$site[i], xy$site[i], xy$site[i]),
                   buffer_size = c(500,1500,3000),
                   Area = c(expanse(b_500, unit="m"),
                            expanse(b_1500, unit="m"),
                            expanse(b_3000, unit="m")),
                   Sum_pest = c(s_500[1,1], s_1500[1,1], s_3000[1,1]),
                   Year = c(2018, 2018, 2018),
                   Variable = c("min","min","min"))
  
  df_2018_min <- rbind(df_2018_min, df)
}
df_2018_min <- df_2018_min %>% mutate(Pesticide_Per_Area = Sum_pest/Area)

Pest_min_Buffers <- rbind(df_2015_min,df_2016_min, df_2017_min, df_2018_min)
vroom_write(Pest_min_Buffers, "D:/Thesis Projects/Bee Beta Diversity/crop data/Site_Min_Pesticide_Buffer.csv")