
###### DATA PREPARATION TO IDENTIFY NESTS OF RED KITES BASED ON GPS TRACKING DATA ################
# original script by Ursin Beeli 6 May 2023
# includes extraction of most frequently used day and night locations, and distances between them
# creates a summary csv file that is then fed into "04.2_nest_detection_RF.R"

# updated on 9 May 2023 to include additional variables for nest success:
# n trips returning to nest in May and June
# n recursions (total n, mean, sd, min) per 2 week period from April to June


## set root folder for project
setwd("C:/Users/sop/OneDrive - Vogelwarte/REKI/Analysis/NestTool/REKI")

library(here)
library(dplyr)
library(amt)
library(sf)
library(mapview)
library(recurse)
library(ggplot2)
library(ggspatial)
library(ggpubr)
library(lubridate)



# disabling scientific notation
options(scipen=999)



# LOADING DATA -----------------------------------------------------------------
# movement data
milvus <- read.csv(here("output/02_preprocessing/03_milvus_combined.csv")) %>%
  mutate(timestamp = as.POSIXct(timestamp, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
         date = as.Date(timestamp, tz = "UTC"),
         week = as.integer(format(date, format = "%W")),
         year_week = format(date, format = "%Y-%W"),
         year_week_id = paste0(format(date, format = "%Y_%W"), "_", bird_id),
         date_id = paste0(date, "_", bird_id),
         year_day = yday(timestamp))

# validation information on home ranges and nests
validation <- read.csv(here("output/01_validation/01_validation_hr_nest.csv"))



# DATA PREPARATION -------------------------------------------------------------
# keeping only the information of relevant dates (yday 70-175)
milvus <- milvus %>%
  filter(year_day %in% c(70:175))

# joining information if individual has a nest
validation_join <- validation %>%
  select(year_id, nest = nest_id, sex) %>%
  mutate(nest = case_when(nest > 0 ~ "nest",
                          nest == 0 ~ "no nest"))
milvus <- left_join(milvus, validation_join, by = "year_id")

# counting the gps fixes per individual
milvus_grouped <- milvus %>%
  group_by(year_id) %>%
  summarise(number_of_fixes = n())

# plotting the distribution
ggplot(milvus_grouped) +
  geom_density(aes(x = number_of_fixes)) +
  labs(title = "Number of GPS Fixes of all Individuals (May-June)",
       x = "Number of GPS Fixes", y = "Proportion of Individuals") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", linewidth = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
  )
# saving plot
# ggsave(here("plots/nest/00_distribution_fixes.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)

# removing the individuals with less than 1000 fixes (about 8 fixes per day)
milvus_grouped <- milvus_grouped %>%
  filter(number_of_fixes > 800)
milvus <- milvus %>%
  filter(year_id %in% milvus_grouped$year_id)

length(unique(milvus$year_id))
table((milvus %>%
         group_by(year_id) %>%
         summarise(nest = first(nest))
)$nest
)
# <---------------------------------------------------------------------------- 787 individuals
# <---------------------------------------------------------------------------- 301 with nest
# <---------------------------------------------------------------------------- 486 without nest



# Creating a track
# 3 mins
milvus_track <- milvus %>%
  mk_track(
    .x = long_eea,
    .y = lat_eea,
    .t = timestamp,
    id = year_id,
    date_id,
    event_id,
    crs = 3035
  ) %>%
  time_of_day(include.crepuscule = T) %>% # if F, crepuscule is considered as night
  arrange(id, t_)

# # saving file
# write.csv(milvus_track, here("output/04_nest/01_milvus_track_tod.csv"),
#           row.names = F)

# creating a night and a day data frame
milvus_track_night <- milvus_track %>%
  filter(tod_ != "day") %>%
  select(-tod_)
  
milvus_track_day <- milvus_track %>%
  filter(tod_ == "day") %>%
  select(-tod_)



# RECURSIONS DURING NIGHTTIME --------------------------------------------------
# splitting track into a list with each single id grouped to an element
milvus_track_night <- as.data.frame(milvus_track_night)
milvus_track_night_list <- split(milvus_track_night, milvus_track_night$id)

# calculating recursions
milvus_night_recurse <- lapply(milvus_track_night_list, function(x)
  getRecursions(x = x[1:4], radius = 50, timeunits = "hours"))

# allocating recurse information to track data frame (1.5 mins)
milvus_track_night$revisits <- NA
milvus_track_night$residence_time <- NA
for (i in 1:length(milvus_night_recurse)) {
  milvus_track_night[milvus_track_night$id == unique(milvus_track_night$id)[i] ,]$revisits <-
    milvus_night_recurse[[i]]$revisits
  milvus_track_night[milvus_track_night$id == unique(milvus_track_night$id)[i] ,]$residence_time <-
    milvus_night_recurse[[i]]$residenceTime
}

# # saving file
# write.csv(milvus_track_night, here("output/04_nest/02_recurse_night.csv"),
#           row.names = F)



# RECURSIONS DURING DAYTIME ----------------------------------------------------
# splitting track into a list with each single id grouped to an element
milvus_track_day <- as.data.frame(milvus_track_day)
milvus_track_day_list <- split(milvus_track_day, milvus_track_day$id)

# calculating recursions (1.5 mins)
milvus_day_recurse <- lapply(milvus_track_day_list, function(x)
  getRecursions(x = x[1:4], radius = 50, timeunits = "hours"))

# allocating recurse information to track data frame (4 mins)
milvus_track_day$revisits <- NA
milvus_track_day$residence_time <- NA
for (i in 1:length(milvus_day_recurse)) {
  milvus_track_day[milvus_track_day$id == unique(milvus_track_day$id)[i] ,]$revisits <-
    milvus_day_recurse[[i]]$revisits
  milvus_track_day[milvus_track_day$id == unique(milvus_track_day$id)[i] ,]$residence_time <-
    milvus_day_recurse[[i]]$residenceTime
}

# # saving file
# write.csv(milvus_track_day, here("output/04_nest/02_recurse_day.csv"),
#           row.names = F)



# DISTANCE BETWEEN LOCATIONS WITH LONGEST RESIDENCE TIME (DAY - NIGHT)
# loading data
# milvus_track_day <- read.csv(here("output/04_nest/02_recurse_day.csv")) %>%
#   mutate(t_ = as.POSIXct(t_, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"))
# milvus_track_night <- read.csv(here("output/04_nest/02_recurse_night.csv")) %>%
#   mutate(t_ = as.POSIXct(t_, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"))

# creating a milvus data set for night locations only
milvus_night <- milvus %>%
  filter(event_id %in% milvus_track_night$event_id) %>%
  left_join(milvus_track_night[,6:8], by = "event_id")

# creating a milvus data set for night day locations only
milvus_day <- milvus %>%
  filter(event_id %in% milvus_track_day$event_id) %>%
  left_join(milvus_track_day[,6:8], by = "event_id")

# filtering the location with longest residence time during nighttime
# check whether locations with greatest number of revisits is the same?
milvus_night_max <- milvus_night %>%
  group_by(year_id) %>%
  summarise(sex = first(sex),
            residence_time_night = max(residence_time),
            revisits_night = revisits[which(residence_time == max(residence_time))],
            date_night = date[which(residence_time == max(residence_time))],
            long_night = long_eea[which(residence_time == max(residence_time))],
            lat_night = lat_eea[which(residence_time == max(residence_time))],
            nest = first(nest)) %>% 
  group_by(year_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
  summarise(sex = first(sex),
            revisits_night = max(revisits_night),
            residence_time_night = residence_time_night[which(revisits_night == max(revisits_night))],
            date_night = date_night[which(revisits_night == max(revisits_night))],
            long_night = long_night[which(revisits_night == max(revisits_night))],
            lat_night = lat_night[which(revisits_night == max(revisits_night))],
            nest = first(nest))


# milvus_night_max2 <- milvus_night %>%
#   group_by(year_id) %>%
#   summarise(sex = first(sex),
#             residence_time_night = residence_time[which(revisits == max(revisits))],
#             revisits_night = max(revisits),
#             date_night = date[which(revisits == max(revisits))],
#             long_night = long_eea[which(revisits == max(revisits))],
#             lat_night = lat_eea[which(revisits == max(revisits))],
#             nest = first(nest)) %>% 
#   group_by(year_id) %>%   ## because there are sometimes duplicates, we need to group again and take the one with max revisits for those where time is equal
#   summarise(sex = first(sex),
#             revisits_night = revisits_night[which(residence_time_night == max(residence_time_night))],
#             residence_time_night = max(residence_time_night),
#             date_night = date_night[which(residence_time_night == max(residence_time_night))],
#             long_night = long_night[which(residence_time_night == max(residence_time_night))],
#             lat_night = lat_night[which(residence_time_night == max(residence_time_night))],
#             nest = first(nest))
# 
# 
#   
# milvus_night_max2 <- milvus_night %>%
#   group_by(year_id) %>%
#   summarise(sex = first(sex),
#             revisits_night = max(revisits),
#             residence_time_night = residence_time[which(revisits_night == max(revisits))],
#             date_night = date[which(revisits == max(revisits))],
#             long_night = long_eea[which(revisits == max(revisits))],
#             lat_night = lat_eea[which(revisits == max(revisits))],
#             nest = first(nest))

# filtering the location with longest residence time during daytime
milvus_day_max <- milvus_day %>%
  group_by(year_id) %>%
  summarise(sex = first(sex),
            residence_time_day = max(residence_time),
            revisits_day = revisits[which(residence_time == max(residence_time))],
            date_day = date[which(residence_time == max(residence_time))],
            long_day = long_eea[which(residence_time == max(residence_time))],
            lat_day = lat_eea[which(residence_time == max(residence_time))],
            nest = first(nest)) %>%
  group_by(year_id) %>%
  summarise(sex = first(sex),
            revisits_day = max(revisits_day),
            residence_time_day = residence_time_day[which(revisits_day == max(revisits_day))],
            date_day = date_day[which(revisits_day == max(revisits_day))],
            long_day = long_day[which(revisits_day == max(revisits_day))],
            lat_day = lat_day[which(revisits_day == max(revisits_day))],
            nest = first(nest))

# creating sf objects for distance calculation
milvus_night_max_sf <- milvus_night_max %>%
  st_as_sf(coords = c("long_night", "lat_night"), crs = 3035)
milvus_day_max_sf <- milvus_day_max %>%
  st_as_sf(coords = c("long_day", "lat_day"), crs = 3035)

# calculating the distance from the day location to the night location
milvus_max_res_time <- milvus_day_max_sf %>%
  mutate(dist_day_to_night = as.numeric(st_distance(milvus_day_max_sf, milvus_night_max_sf, by_element = T))) %>%
  st_drop_geometry() %>%
  left_join(milvus_night_max_sf[,c(1,3,4,5)], by = "year_id") %>%
  st_drop_geometry()

# splitting the data to individuals with nest and without nest
milvus_max_res_time_nest <- milvus_max_res_time %>%
  filter(nest == "nest")
milvus_max_res_time_no_nest <- milvus_max_res_time %>%
  filter(nest == "no nest")



##### CALCULATING REVISITS TO POTENTIAL NEST SITE (day max location)
# using list apply over all individuals
# to benefit from getRecursionsAtLocations we add the pseudo-nest coordinates to each 
# add nest locations to tracking data
milvus_track <- milvus_track %>% left_join(milvus_day_max %>% select(year_id,long_day,lat_day) %>% rename(id=year_id,x=long_day,y=lat_day), by = "id")
milvus_track_list <- split(milvus_track, milvus_track$id)

## test the function and the output it produces
b<-getRecursionsAtLocations(x = as.data.frame(milvus_track_list[[1]][,1:4]), locations = as.data.frame(milvus_track_list[[1]][,8:9])[1,],
                           radius = 50, timeunits = "hours")
b$revisits  
  
  
  # calculating recursions
 nest_revisits <- lapply(milvus_track_list, function(x)
    getRecursionsAtLocations(x = as.data.frame(x[1:4]), locations = as.data.frame(x[8:9])[1,],
                             radius = 50, timeunits = "hours"))
  
  




# DISTANCE PLOT OF BIRDS WITH A NEST
# arrange data by distance (increasing)
milvus_max_res_time_nest <- milvus_max_res_time_nest %>%
  arrange(dist_day_to_night)

# calculating percentiles
n_99 <- round(0.99*nrow(milvus_max_res_time_nest))
dist_99_percent <- milvus_max_res_time_nest %>%
  slice(1:n_99)
max_dist_99_percent <- max(dist_99_percent$dist_day_to_night)

n_95 <- round(0.95*nrow(milvus_max_res_time_nest))
dist_95_percent <- milvus_max_res_time_nest %>%
  slice(1:n_95)
max_dist_95_percent <- max(dist_95_percent$dist_day_to_night)

n_50 <- round(0.50*nrow(milvus_max_res_time_nest))
dist_50_percent <- milvus_max_res_time_nest %>%
  slice(1:n_50)
max_dist_50_percent <- max(dist_50_percent$dist_day_to_night)

# plotting
milvus_max_res_time_nest %>%
  ggplot(aes(x = round(dist_day_to_night))) +
  geom_bar(width = 1, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent + 17, y = 8,
           label = paste0("50% Percentile (", round(max_dist_50_percent), " m)"),
           angle = 90, color = "orange") +
  geom_vline(xintercept = max_dist_95_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_95_percent + 10, y = 8,
           label = paste0("95% Percentile (", round(max_dist_95_percent), " m)"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_99_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_99_percent + 10, y = 8,
           label = paste0("99% Percentile (", round(max_dist_99_percent), " m)"),
           angle = 90, color = "darkred") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) [m]",
       title = paste0("Individuals with a nest (n = ",
                      length(unique(milvus_max_res_time_nest$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,22,2)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
)

# saving plot
# ggsave(here("plots/nest/01_day_night_dist_nest.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)



# DISTANCE PLOT OF BIRDS WITHOUT A NEST
# arrange data by distance (increasing)
milvus_max_res_time_no_nest <- milvus_max_res_time_no_nest %>%
  arrange(dist_day_to_night)

# calculating percentiles
n_99 <- round(0.99*nrow(milvus_max_res_time_no_nest))
dist_99_percent <- milvus_max_res_time_no_nest %>%
  slice(1:n_99)
max_dist_99_percent <- max(dist_99_percent$dist_day_to_night)

n_95 <- round(0.95*nrow(milvus_max_res_time_no_nest))
dist_95_percent <- milvus_max_res_time_no_nest %>%
  slice(1:n_95)
max_dist_95_percent <- max(dist_95_percent$dist_day_to_night)

n_50 <- round(0.50*nrow(milvus_max_res_time_no_nest))
dist_50_percent <- milvus_max_res_time_no_nest %>%
  slice(1:n_50)
max_dist_50_percent <- max(dist_50_percent$dist_day_to_night)

# plotting
milvus_max_res_time_no_nest %>%
  ggplot(aes(x = round(dist_day_to_night))) +
  geom_bar(width = 1, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent + 20000, y = 3,
           label = paste0("50% Percentile (", round(max_dist_50_percent), " m)"),
           angle = 90, color = "orange") +
  geom_vline(xintercept = max_dist_95_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_95_percent + 20000, y = 3,
           label = paste0("95% Percentile (", round(max_dist_95_percent/1000, 1), " km)"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_99_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_99_percent + 20000, y = 3,
           label = paste0("99% Percentile (", round(max_dist_99_percent/1000, 1), " km)"),
           angle = 90, color = "darkred") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) [m]",
       title = paste0("Individuals without a nest (n = ",
                      length(unique(milvus_max_res_time_no_nest$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,6,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
)

# saving plot
# ggsave(here("plots/nest/01_day_night_dist_no_nest.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)




# DISTANCE PLOT OF BIRDS WITHOUT A NEST (LOWER 50% FOR DETAILLED INSPECTATION)
# arrange data by distance (increasing)
milvus_max_res_time_no_nest_50 <- dist_50_percent %>%
  arrange(dist_day_to_night)

# calculating percentiles
n_25 <- round(0.50*nrow(milvus_max_res_time_no_nest_50))
dist_25_percent <- milvus_max_res_time_no_nest_50 %>%
  slice(1:n_25)
max_dist_25_percent <- max(dist_25_percent$dist_day_to_night)

n_10 <- round(0.20*nrow(milvus_max_res_time_no_nest_50))
dist_10_percent <- milvus_max_res_time_no_nest_50 %>%
  slice(1:n_10)
max_dist_10_percent <- max(dist_10_percent$dist_day_to_night)

# plotting
milvus_max_res_time_no_nest_50 %>%
  ggplot(aes(x = round(dist_day_to_night))) +
  geom_bar(width = 1, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent - 15, y = 3.5,
           label = paste0("50% Percentile (", round(max_dist_50_percent), " m)"),
           angle = 90, color = "darkred") +
  geom_vline(xintercept = max_dist_25_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_25_percent + 25, y = 3.5,
           label = paste0("25% Percentile (", round(max_dist_25_percent), " m)"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_10_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_10_percent - 40, y = 3.5,
           label = paste0("10% Percentile (", round(max_dist_10_percent), " m)"),
           angle = 90, color = "orange") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) [m]",
       title = paste0("Lower 50% of individuals without a nest (n = ",
                      length(unique(milvus_max_res_time_no_nest_50$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,6,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
)

# # saving plot
# ggsave(here("plots/nest/01_day_night_dist_no_nest_50.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)










#' NORMALISING DISTANCE FROM LONGEST VISITED DAY LOCATION TO LONGEST VISITED
#' NIGHT LOCATION BY MEDIAN DISTANCE OF ALL DAY LOCATIONS TO LONGEST VISITED
#' NIGHT LOCATION

# creating two data frames of same dimension to calculate distances

# sf object of most visited night location
milvus_night_max_sf_dist <- milvus_day %>%
  select(event_id, year_id) %>%
  left_join(milvus_night_max_sf, by = "year_id") %>%
  st_as_sf(crs = 3035)

# sf object of all day locations
milvus_day_sf <- milvus_day %>%
  st_as_sf(coords = c("long_eea", "lat_eea"), crs = 3035)

# calculating the distances
milvus_day_sf$dist_to_max_night <- as.numeric(st_distance(milvus_day_sf,
                                                          milvus_night_max_sf_dist,
                                                          by_element = T))

# summarising median for each brood cycle
milvus_day_median_dist <- milvus_day_sf %>%
  st_drop_geometry() %>%
  group_by(year_id) %>%
  summarise(median_dist_to_max_night = median(dist_to_max_night))

# creating a data frame with all relevant information
milvus_dist_norm <- milvus_max_res_time %>%
  st_drop_geometry() %>%
  left_join(milvus_day_median_dist, by = "year_id") %>%
  mutate(distance_normalised = dist_day_to_night/median_dist_to_max_night)

#




# splitting the data to individuals with nest and without nest
milvus_dist_norm_nest <- milvus_dist_norm %>%
  filter(nest == "nest")
milvus_dist_norm_no_nest <- milvus_dist_norm %>%
  filter(nest == "no nest")



# DISTANCE PLOT OF BIRDS WITH A NEST
# arrange data by distance (increasing)
milvus_dist_norm_nest <- milvus_dist_norm_nest %>%
  arrange(distance_normalised)

# calculating percentiles
n_99 <- round(0.99*nrow(milvus_dist_norm_nest))
dist_99_percent <- milvus_dist_norm_nest %>%
  slice(1:n_99)
max_dist_99_percent <- max(dist_99_percent$distance_normalised)

n_95 <- round(0.95*nrow(milvus_dist_norm_nest))
dist_95_percent <- milvus_dist_norm_nest %>%
  slice(1:n_95)
max_dist_95_percent <- max(dist_95_percent$distance_normalised)

n_50 <- round(0.50*nrow(milvus_dist_norm_nest))
dist_50_percent <- milvus_dist_norm_nest %>%
  slice(1:n_50)
max_dist_50_percent <- max(dist_50_percent$distance_normalised)

# plotting
milvus_dist_norm_nest %>%
  ggplot(aes(x = round(distance_normalised, 2))) +
  geom_bar(width = 0.003, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent + 0.02, y = 13,
           label = paste0("50% Percentile (", round(max_dist_50_percent, 2), ")"),
           angle = 90, color = "orange") +
  geom_vline(xintercept = max_dist_95_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_95_percent + 0.02, y = 13,
           label = paste0("95% Percentile (", round(max_dist_95_percent, 2), ")"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_99_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_99_percent + 0.02, y = 13,
           label = paste0("99% Percentile (", round(max_dist_99_percent, 2), ")"),
           angle = 90, color = "darkred") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) \n normalised by median distance of all day locations to longest visited night location",
       title = paste0("Individuals with a nest (n = ",
                      length(unique(milvus_dist_norm_nest$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,26,2)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
  )

# # saving plot
# ggsave(here("plots/nest/02_day_night_dist_nest_norm.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)



# DISTANCE PLOT OF BIRDS WITHOUT A NEST
# arrange data by distance (increasing)
milvus_dist_norm_no_nest <- milvus_dist_norm_no_nest %>%
  arrange(distance_normalised)

# calculating percentiles
n_99 <- round(0.99*nrow(milvus_dist_norm_no_nest))
dist_99_percent <- milvus_dist_norm_no_nest %>%
  slice(1:n_99)
max_dist_99_percent <- max(dist_99_percent$distance_normalised)

n_95 <- round(0.95*nrow(milvus_dist_norm_no_nest))
dist_95_percent <- milvus_dist_norm_no_nest %>%
  slice(1:n_95)
max_dist_95_percent <- max(dist_95_percent$distance_normalised)

n_50 <- round(0.50*nrow(milvus_dist_norm_no_nest))
dist_50_percent <- milvus_dist_norm_no_nest %>%
  slice(1:n_50)
max_dist_50_percent <- max(dist_50_percent$distance_normalised)

# plotting
milvus_dist_norm_no_nest %>%
  ggplot(aes(x = round(distance_normalised, 5))) +
  geom_bar(width = 0.01, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent - 3, y = 2,
           label = paste0("50% Percentile (", round(max_dist_50_percent, 2), ")"),
           angle = 90, color = "orange") +
  geom_vline(xintercept = max_dist_95_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_95_percent + 3, y = 2,
           label = paste0("95% Percentile (", round(max_dist_95_percent, 2), ")"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_99_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_99_percent + 3, y = 2,
           label = paste0("99% Percentile (", round(max_dist_99_percent, 2), ")"),
           angle = 90, color = "darkred") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) \n normalised by median distance of all day locations to longest visited night location",
       title = paste0("Individuals without a nest (n = ",
                      length(unique(milvus_dist_norm_no_nest$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,6,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
  )

# saving plot
# ggsave(here("plots/nest/02_day_night_dist_no_nest_norm.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)
# 


# DISTANCE PLOT OF BIRDS WITHOUT A NEST (LOWER 50% FOR DETAILLED INSPECTATION)
# arrange data by distance (increasing)
milvus_dist_norm_no_nest_50 <- dist_50_percent %>%
  arrange(distance_normalised)

# calculating percentiles
n_25 <- round(0.50*nrow(milvus_dist_norm_no_nest_50))
dist_25_percent <- milvus_dist_norm_no_nest_50 %>%
  slice(1:n_25)
max_dist_25_percent <- max(dist_25_percent$distance_normalised)

n_10 <- round(0.20*nrow(milvus_dist_norm_no_nest_50))
dist_10_percent <- milvus_dist_norm_no_nest_50 %>%
  slice(1:n_10)
max_dist_10_percent <- max(dist_10_percent$distance_normalised)

# plotting
milvus_dist_norm_no_nest_50 %>%
  ggplot(aes(x = round(distance_normalised, 5))) +
  geom_bar(width = 0.0001, color = "black", fill = "black") +
  geom_vline(xintercept = max_dist_50_percent, linetype = "dashed", 
             color = "darkred", linewidth = 0.5) +
  annotate("text", x = max_dist_50_percent - 0.004, y = 2,
           label = paste0("50% Percentile (", round(max_dist_50_percent, 2), ")"),
           angle = 90, color = "darkred") +
  geom_vline(xintercept = max_dist_25_percent, linetype = "dashed", 
             color = "red", linewidth = 0.5) +
  annotate("text", x = max_dist_25_percent + 0.006, y = 2,
           label = paste0("25% Percentile (", round(max_dist_25_percent, 2), ")"),
           angle = 90, color = "red") +
  geom_vline(xintercept = max_dist_10_percent, linetype = "dashed", 
             color = "orange", linewidth = 0.5) +
  annotate("text", x = max_dist_10_percent - 0.005, y = 2,
           label = paste0("10% Percentile (", round(max_dist_10_percent, 3), ")"),
           angle = 90, color = "orange") +
  labs(y = "Count", x = "Distance of locations with longes residence times (day to night) \n normalised by median distance of all day locations to longest visited night location",
       title = paste0("Lower 50% of individuals without a nest (n = ",
                      length(unique(milvus_dist_norm_no_nest_50$year_id)), ")")) +
  scale_y_continuous(breaks = seq(0,6,1)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray20", size = .05),
        panel.grid.minor = element_line(colour = "gray90"),
        axis.ticks = element_blank()
  )

# saving plot
# ggsave(here("plots/nest/02_day_night_dist_no_nest_norm_50.pdf"),
#        width = 3000, height = 1500, units = "px", limitsize = F)



# CREATING A TABLE WITH ALL RELEVANT INFORMATION FOR A RANDOM FOREST RUN
milvus_dist_summary <- milvus_dist_norm %>%
  select(year_id, sex, revisits_day, residence_time_day, date_day, revisits_night,
         residence_time_night, date_night,
         dist_max_day_to_max_night = dist_day_to_night,
         median_day_dist_to_max_night = median_dist_to_max_night,
         relative_dist_max_day_to_max_night = distance_normalised, nest,
         -geometry)

# # saving file
# write.csv(milvus_dist_summary, here("output/04_nest/03_distance_RF.csv"),
#           row.names = F)
