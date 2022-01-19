#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")
###loading packages
library(tidyverse)
library(tmap)
library(leaflet) 
library(sf) 
library(RColorBrewer)
library(htmltools) 
library(leafsync) 
library(kableExtra) 
library(rnaturalearth)
library(rnaturalearthhires)
library(viridis)
library(mapview)
##loading data
AMR <- read_csv("AMR.csv", na = "empty")

world <- ne_countries(type = 'countries', 
                      scale = 'small', 
                      returnclass = "sf") 


###using map view package to plot world AMR rates
AMR <- AMR %>%
  na.omit

df <- AMR%>%
  group_by(country) %>%
 select(country, percentage_isolatesresistant, x_coordinate, 
        y_coordinate, species)%>%
  set_names(c("country", "percentage_resistance", 
              "long", "lat", "species")) %>%
  na.omit()

df %>%
  count(country)%>%   
  view()
###changing percentage_resistance to numeric class
 df$percentage_resistance <- as.numeric(as.character(df$percentage_resistance))

 ##Summarizing the data
 df1 <- df %>%
  group_by(country) %>%
summarise(percentage_resistance = mean(percentage_resistance, na.rm = TRUE))

world_amr1 <- world %>%
  filter(continent != "Antarctica", continent !="Greenland",
         continent !="North america", continent !="South America",
         continent !="Australia", continent !="Oceania") %>%
  select(country = sovereignt, geometry) %>%
  left_join(df1, by = "country")

##Adding number of studies to the dataset
data1 <- AMR %>% 
  group_by(country, author, pub_date) %>% 
  filter(row_number()==1) %>% 
  dplyr:: select(continent, country) %>%
  distinct() %>% as.data.frame() # drop duplicates.
data1

data2 <- data1 %>% 
  group_by(continent, country) %>% 
  mutate(no_studies = n()) %>%
  dplyr:: select(continent, country, no_studies) %>%
  distinct() %>% as.data.frame() # drop duplicates.
data2

#Joining the data to add number of studies
world_amr2 <- world_amr1 %>%
  left_join(data2, by = "country")

world_amr2 %>%
  mapview(zcol = "percentage_resistance",
          col.regions = viridisLite::plasma) 
 

##using gg_save to save map