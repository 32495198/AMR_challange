#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")
library(tidyverse)
library(ggplot2)
library (gggibbous)
library(tmap)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(viridis)

##loading data
AMR <- read_csv("AMR.csv")

kenya1 <- data.frame(AMR)%>%
  group_by(country) %>%
  filter(country == "Kenya") %>%
  select(country, x_coordinate, y_coordinate, species, farm_type_urbanvsrural,
         percentage_isolatesresistant, author)%>%
  mutate(percentage_isolatesresistant = percentage_isolatesresistant/100)

 kenya2 <- na.omit(kenya1) 
 
 kenya2$percentage_isolatesresistant <- as.numeric(
   as.character(kenya2$percentage_isolatesresistant))
 kenya2 %>%
   count(species)
 kenya3 <- aggregate(percentage_isolatesresistant~author+
                     country+x_coordinate+y_coordinate+
                       farm_type_urbanvsrural, 
                   kenya2, mean
 )
 
 
 
myzipFile <- "https://www.kaggle.com/ambarish/kenya-counties-shapefile/download"
download.file(myzipFile, destfile = "counties.zip")
unzip("counties.zip")
 ##couldn't be able to unzip shape file using the above code. The shape file
 #was therefore added manually to the working directory
 kenyan_counties <- sf::read_sf("County.shp")
 kenyan_counties
 
 
kenyamap <- ggplot(data = kenyan_counties, aes()) + 
   geom_sf(size = 0.5) + 
    geom_sf_label(aes(label = Name)) + 
 theme_void()
kenyamap





##Resistance percentage in kenyan counties map
ken_res <- data.frame(AMR)%>%
  group_by(country) %>%
  filter(country == "Kenya") %>%
  select(country, x_coordinate, y_coordinate, species, farm_type_urbanvsrural,
         percentage_isolatesresistant, author)

#omitting NA's
ken_res1 <- na.omit(ken_res) 

#changing percentage_isolates resistant class to numeric
ken_res1$percentage_isolatesresistant <- as.numeric(
  as.character(ken_res1$percentage_isolatesresistant))


ken_res2 <- aggregate(percentage_isolatesresistant~author+
                      country+x_coordinate+y_coordinate+species+
                      farm_type_urbanvsrural, 
                    ken_res1, mean
)

##Adding county names columns to dataset
Name = c(
  "Kericho", "Isiolo", "Isiolo", "Nairobi", "Nakuru", "Nairobi", 
  "Nairobi", "Nairobi", "Nairobi", "Nairobi", "Kiambu"," Nairobi",
  "Nairobi", "Nairobi"
  
  
)

##New data set
ken_res3 <- cbind(ken_res2, Name)

#using shape file to plot kenyan map
kenyan_counties.sf <- sf::read_sf("County.shp")
kenyan_counties.sf

ken_res4.sf <- kenyan_counties.sf %>%
  left_join(ken_res3, by = "Name")
  

  
  


##AMR kenyan counties map
kenres_plot <- ggplot() +
  geom_sf(fill = "grey98") +
 geom_sf(data = ken_res4.sf, 
          aes(fill = percentage_isolatesresistant), color = "gray75") +
  scale_fill_viridis(option = "plasma", direction = 1, na.value = "grey85") +
  labs(fill = "Percentage resistance") +
  theme(legend.position = "right")+facet_wrap(~species)
##saving the plot
ggsave(filename = "resistanceplot.jpeg")