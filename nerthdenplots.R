#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")
library(tidyverse)
library(plotly)
library(cowplot)
library(patchwork)
library(wesanderson)
##loading data
AMR <- read_csv("AMR.csv", na = "empty")


##filtering data
nerth <- AMR %>%
  filter(country == "Netherlands", 
         sampling_start_date == "2016" | sampling_start_date == "2017" |
           sampling_start_date == "2018" | sampling_start_date == "2019" |
           sampling_start_date == "2020")%>%
  select(country, species, sampling_start_date, pathogen, 
         antibiotic_class, percentage_isolatesresistant)
nerth$percentage_isolatesresistant <- as.numeric(
  as.character(nerth$percentage_isolatesresistant))
  nerth1 <- aggregate(percentage_isolatesresistant~country+
                        sampling_start_date+antibiotic_class+pathogen+
                       species, 
                     nerth, mean
                     )
                                      


nerthplot1 <- ggplot(nerth1, aes(x = species, 
                                y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Netherlands",
     x = "hosts", y = "Percentage resistance", 
     fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


####Adding colorblind friendly scheme####
cbPalette <- c("#000000", "#E69F00", 
               "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7", 
               "#999999", "#330066",
               "#0000FF", "#339900",
               "#00CCCC", "#FF66FF",
               "#FF3333", "#666600",
               "#336699", "#006600",
               "#E7298A", "#A7BBF8")

nerthplot1 + scale_fill_manual(values = cbPalette) 

##Nerthplot
nerthplot2 <- ggplot(nerth1, aes(x = pathogen, 
                                y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Netherlands",
    x = "pathogen", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

nerthplot2 + scale_fill_manual(values = cbPalette) 

##Denmark data 2019 and 2020
Dan <- AMR %>%
  filter(country == "Denmark", 
         sampling_start_date == "2019" | sampling_start_date == "2018" |
        sampling_start_date == "2017" | sampling_start_date == "2016" |
          sampling_start_date == "2015")%>%
  select(country, species, sampling_start_date, pathogen, 
         antibiotic_class, percentage_isolatesresistant)
Dan$percentage_isolatesresistant <- as.numeric(
  as.character(Dan$percentage_isolatesresistant))
Dan1 <- aggregate(percentage_isolatesresistant~country+
                      sampling_start_date+antibiotic_class+pathogen+
                      species, 
                    Dan, mean
)

Danplot1 <- ggplot(Dan1, aes(x = species, 
                                y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Denmark",
    x = "Hosts", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


Danplot1 + scale_fill_manual(values = cbPalette) 
###Danplot2

Danplot2 <- ggplot(Dan1, aes(x = pathogen, 
                             y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Denmark",
    x = "Pathogen", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 



Danplot2 + scale_fill_manual(values = cbPalette) 
##using cow plot to join all the plots
#plot_grid(Danplot1, Danplot2 nerthplot1, nerthplot2, nrow = 3, ncol = 1)

#using patchwork

(Danplot1/Danplot2/nerthplot1/nerthplot2) + plot_layout(guides = "collect") +
  plot_annotation(title = 
                    "AMR rates of Denmark and Netherlands in 2019 and 2020",
                  tag_levels = "A", tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))& 
  scale_fill_manual(values = cbPalette)
