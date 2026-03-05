#combines niche limits with bee and aridity data to calcualte API

library(dplyr)
library(vroom)

bee <- vroom("D:/Thesis Projects/Bee Beta Diversity/Bee_data_AI.csv")[,c(1:12)]

niche_lims_points <- vroom("D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits_points.csv")[,-1]%>%
  rename(TMin_points = TMin,
         TMax_points = TMax,
         AMin_points = AMin,
         AMax_points = AMax)

niche_lims_range <- vroom("D:/Thesis Projects/Bee Beta Diversity/All_Niche_Limits.csv")[,-1]%>%
  rename(TMin_range = TMin,
         TMax_range = TMax,
         AMin_range = AMin,
         AMax_range = AMax)

bee <- left_join(bee,niche_lims_points)%>%left_join(niche_lims_range)%>%dplyr::select(-Month_Code)

bee_api <- bee %>% mutate(API_points = (AI - AMin_points)/(AMax_points-AMin_points),
                          API_range = (AI - AMin_range)/(AMax_range-AMin_range))
