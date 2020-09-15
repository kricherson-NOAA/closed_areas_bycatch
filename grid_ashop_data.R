#Grid the ASHOP data in 0.5X0.5 degree bins

#First, load data and libraries
library(tidyverse)
library(janitor)
library(raster)
library(viridis)
library(lubridate)

source("~/observer/Input/load_data_2020-06-24.R")

load_data("ASHOP_proc")

out_drive <- "~/observer/Output/Richerson other/hilborn_project/"

ashop <- ASOrig_Proc %>% 
  clean_names %>% 
  mutate(month = month(retrieval_date))

#Create a raster to rasterize the data. Starting with a 0.5-degree square resolution. 
ras <- raster(xmn=min(ashop$avg_lon), xmx=max(ashop$avg_lon), ymn=min(ashop$avg_lat), ymx=max(ashop$avg_lat), res=0.5, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#This is the function we will run on each year-month combination to get out the sum of target catch, selected discards, and effort in each raster grid cell
ras_fun_ashop <- function(data){ #data is the ob data, nested by year, month
  
#Not entirely sure how to get na.rm=T to work in the raster sum function, so just replacing NAs with 0s here. Adding columns with salmon and rockfish discards because it makes things easier for strata with 0 bycatch of these species. Rockfish include all Sebastes, Sebastolobus
  data <- data %>% 
    mutate(tgt_mt = ifelse(is.na(tgt_mt), 0 , tgt_mt),
           dis_mt = ifelse(is.na(dis_mt), 0 , dis_mt),
           chnk_dis_mt = ifelse(spid_eqv == "CHNK", dis_mt, 0),
           coho_dis_mt = ifelse(spid_eqv == "COHO", dis_mt, 0),
           chum_dis_mt = ifelse(spid_eqv == "CHUM", dis_mt, 0),
           sock_dis_mt = ifelse(spid_eqv == "SOCK", dis_mt, 0),
           pink_dis_mt = ifelse(spid_eqv == "PINK", dis_mt, 0),
           stlh_dis_mt = ifelse(spid_eqv == "STLH", dis_mt, 0),
           usmn_dis_mt = ifelse(spid_eqv == "USMN", dis_mt, 0),
           sal_dis_mt = ifelse(spid_eqv %in% c("SAMN",
                                               "USMN",
                                               "STLH",
                                               "SOCK",
                                               "PINK",
                                               "COHO",
                                               "CHUM",
                                               "CHNK"), dis_mt, 0),
           rfish_dis_mt = ifelse(grepl("rockfish", grouping, useBytes = TRUE) | grepl("thornyhead", grouping, useBytes = T) | spid_eqv == "POP", dis_mt, 0))
  
  #rasterize total target weight
  tgt_ras <- rasterize(cbind(data$avg_long, data$avg_lat), 
                       ras, 
                       data$tgt_mt, 
                       fun=sum)
  
  #rasterize selected discard weight
  dis_sal_ras <- rasterize(cbind(data$avg_long, data$avg_lat), 
                           ras, 
                           data$sal_dis_mt, 
                           fun=sum)
  
  dis_rfish_ras <- rasterize(cbind(data$avg_long, data$avg_lat), 
                             ras, 
                             data$rfish_dis_mt, 
                             fun=sum)
  
  #rasterize effort
  effort_ras <- rasterize(cbind(data$avg_long, data$avg_lat), 
                          ras, 
                          data$duration_in_min, 
                          fun=sum)
  
  #Make the rasters into a dataframe
  out <- stack(tgt_ras, dis_sal_ras, dis_rfish_ras, effort_ras) %>%
    rasterToPoints() %>%
    as_tibble() %>%
    rename(lon = x, lat = y, total_tgt_mt = layer.1, total_dis_sal = layer.2, total_dis_rfish = layer.3, total_haul_dur = layer.4) %>% 
    mutate(year = unique(data$year),
           month = unique(data$month))
  
  return(out)
  
}

ashop_grid <- ashop %>%
  filter(sector != "ATSEA TRIBAL" ) %>% #Question: do we want to include trips without catch?
  #dplyr::select(year, month, spid_eqv, avg_lat, avg_long, tgt_mt, dis_mt, grouping, haul_id, duration_in_min) %>% #Not sure if this is needed but might as well pare down data columns
  mutate(year_dup = year, month_dup = month) %>%  #This is a hacky way of keeping year and month in the nested data frames
  group_by(year_dup, month_dup) %>% 
  nest() %>% #This creates a row for each year-month combination, with a data column that contains a df with all the data for that year/month
  mutate(gridded = map(.x = data, # The list-column with the data for a given year-month
                       ~ ras_fun_ashop(data = .x))) #Run the rasterizing and gridding function on each group of data.

#Make the gridded data list-column into a regular data frame and format for output
ashop_grid_all <- bind_rows(ashop_grid$gridded) %>% 
  arrange(year, month) %>% 
  dplyr::select(Lat = lat, Lon = lon, Year = year, Month = month, Effort = total_haul_dur, hake = total_tgt_mt, salmon = total_dis_sal,  rockfish = total_dis_rfish)

write_csv(ashop_grid_all, paste0(out_drive, "ashop_grid.csv"))

#Make sure the gridding method produces sensible-lookin results:
example_plot<-ggplot()+
  coord_fixed(xlim = c(-127, -122.0),  ylim = c(40, 49), ratio = 1.3) +
  geom_tile(data = ashop_grid$gridded[[100]], aes(x=lon, y=lat, fill=total_haul_dur))+
  scale_fill_viridis()+
  borders()+
  NULL