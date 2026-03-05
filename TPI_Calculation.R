#this loads in bee observation data with niche limits and combines it with
#temperature data then calculates Thermal Position Index.

library(vroom)
library(dplyr)
library(tidyr)

bee_data <- vroom("D:/Thesis Projects/Bee Beta Diversity/Bee_data_FINAL.csv")

temp_data_all <- vroom("D:/Thesis Projects/Bee Beta Diversity/microclima_hourly_means_advanced.csv")%>%
  separate(col = Date, into = c("Year","Month","Day"),
           sep = "-" , extra="drop")%>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month),
         Day = as.numeric(Day),
         Time = as.character(Time))%>%rename(site=Site)

bee_data_temp_all <- left_join(bee_data, temp_data_all)%>%
  mutate(TPI_range_mean = (Mean_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_points_mean = (Mean_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_range_min = (Min_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_points_min = (Min_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_range_max = (Max_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_points_max = (Max_Tair - TMin_points)/(TMax_points- TMin_points))

vroom_write(bee_data_temp_all, "D:/Thesis Projects/Bee Beta Diversity/Bee_Data_All_Climate_all_times_advanced.csv")

##

temp_data_buffer <- vroom("D:/Thesis Projects/Bee Beta Diversity/microclima_hourly_rings.csv")[,-c(7,11,15)]%>%
  separate(col = Date, into = c("Year","Month","Day"),
           sep = "-" , extra="drop")%>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month),
         Day = as.numeric(Day),
         Time = as.character(Time))%>%rename(site=Site)

bee_data_buffer <- left_join(bee_data, temp_data_buffer)%>%
  mutate(TPI_Z1_Mean_range = (Z1_Mean_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z1_Min_range = (Z1_Min_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z1_Max_range = (Z1_Max_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z2_Mean_range = (Z2_Mean_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z2_Min_range = (Z2_Min_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z2_Max_range = (Z2_Max_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z3_Mean_range = (Z3_Mean_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z3_Min_range = (Z3_Min_Tair - TMin_range)/(TMax_range- TMin_range),
         TPI_Z3_Max_range = (Z3_Max_Tair - TMin_range)/(TMax_range- TMin_range),
         
         TPI_Z1_Mean_point = (Z1_Mean_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z1_Min_point = (Z1_Min_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z1_Max_point = (Z1_Max_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z2_Mean_point = (Z2_Mean_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z2_Min_point = (Z2_Min_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z2_Max_point = (Z2_Max_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z3_Mean_point = (Z3_Mean_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z3_Min_point = (Z3_Min_Tair - TMin_points)/(TMax_points- TMin_points),
         TPI_Z3_Max_point = (Z3_Max_Tair - TMin_points)/(TMax_points- TMin_points))
  
vroom_write(bee_data_buffer, "D:/Thesis Projects/Bee Beta Diversity/Bee_Data_All_Climate_all_times_Buffer.csv")

bee_data_buffer_max <- bee_data_buffer %>% group_by(site, Binomial, Year, Month, Day)%>%
  slice_max(TPI_Z1_Max_range)%>%ungroup()
vroom_write(bee_data_buffer_max, "D:/Thesis Projects/Bee Beta Diversity/Bee_Data_All_Climate_all_times_Buffer_max_daily.csv")

