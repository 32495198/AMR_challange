
library(ggplot2)
library(tidyverse)
library(readr)
library("scales")
library(cowplot)
library(maps)
library (mapproj)
library(sf)
library(spData)
library(spDataLarge)
library(terra)
library(dplyr)
library(zoo)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")

#Load data
AMR <- read_csv("AMR.csv", na = "empty")
#Getting the names of the data set
names(AMR) 

head(AMR)

tibble(AMR)

tail(AMR)
####Sub-setting AMR data, getting variables of interest naming it to AMR.1####
AMR.1 <- AMR %>%
  
  select(country,
         species, 
         sampling_start_date, 
         sampling_end_date, 
         pathogen,
         antibiotic_class,
         percentage_isolatesresistant, 
         reference)
AMR.1

###Extracting Muloi, Dishon, et al. 2019 from AMR.1 data set and naming it DATA.1####

DATA.1 <- AMR.1 %>%
  filter(reference == "Muloi, Dishon, et al. 2019")
DATA.1

####Bar plot: Fig. 3 from Dishon Muloi et al.2019####
P1 <- ggplot(DATA.1, aes(x = pathogen, y = percentage_isolatesresistant, 
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli across different species in Nairobi, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "antibiotic_class", caption = "Dishon Muloi et al., 2019") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
P1

####Adding colorblind friendly scheme####
cbPalette <- c("#000000", "#E69F00", 
               "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7", 
               "#999999", "#330066",
               "#0000FF", "#339900",
               "#00CCCC", "#FF66FF",
               "#FF3333", "#666600",
               "#336699", "#006600")

P1 + scale_fill_manual(values = cbPalette) 

####Extracting Shitandi and Sternesjo.2004 from AMR.1 data set and naming it DATA.2####
DATA.2 <- AMR.1 %>%
  filter(reference == "Shitandi and Sternesjo. 2004")
DATA.2

####Bar plot from Shitandi and Sternesjo.2004#### 

P2 <- ggplot(DATA.2, aes(x = pathogen, y = percentage_isolatesresistant,
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of S.aureus in cattle in Nakuru County, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Shitandi and Sternesjo. 2004") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
P2

P2 + scale_fill_manual(values = cbPalette) 

####Extracting Langata, Lydia, et al.2019 from AMR.1 data set and naming it DATA.3####
DATA.3 <- AMR.1 %>%
  filter(reference == "Langata, Lydia, et al.2019")
DATA.3
####Bar plot from Langata, Lydia, et al.2019####

P3 <- ggplot(DATA.3, aes(x = pathogen, y =percentage_isolatesresistant, 
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of E.coli and Salmonella in Chicken in Nairobi, Kenya", x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Langata, Lydia, et al.2019") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P3

P3 + scale_fill_manual(values = cbPalette) 

####Extracting Nato, Samuel, et al.2019 from AMR.1 data set and naming it DATA.4####
DATA.4 <- AMR.1 %>%
  filter(reference == "Nato, Samuel, et al.2019")
DATA.4

####Bar plot from Nato, Samuel, et al.2019####

P4 <- ggplot(DATA.4, aes(x = pathogen, y = percentage_isolatesresistant, 
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of  E.coli in Cattle and Camels in Isiolo and Nairobi",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Nato, Samuel, et al.2019") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
P4

P4 + scale_fill_manual(values = cbPalette) 


####Extracting Nguyen, Tuan, et al.2016 from AMR.1 data set and naming it DATA.5####
DATA.5 <- AMR.1 %>%
  filter(reference == "Nguyen, Tuan, et al.2016")
DATA.5

####Bar plot from Nguyen, Tuan, et al.2016####

P5 <- ggplot(DATA.5, aes(x = pathogen, y = percentage_isolatesresistant,
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of campylobacter in Chicken in Thika, Kiambu county, Kenya", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Nguyen, Tuan, et al.2016") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
P5

P5 + scale_fill_manual(values = cbPalette) 

####Extracting Naliaka, M, et al.2017 from AMR.1 data set and naming it DATA.6####
DATA.6 <- AMR.1 %>%
  filter(reference == "Naliaka, M, et al.2017")
DATA.6

####Bar plot from Naliaka, M, et al.2017####

P6 <- ggplot(DATA.6, aes(x = pathogen, y = percentage_isolatesresistant,
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of E.coli in chicken in Kericho county, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Naliaka, M, et al.2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P6

P6 + scale_fill_manual(values = cbPalette) 

#### Extracting Adelaide, A, et al.2008 from AMR.1 data set and naming it DATA.7####
DATA.7 <- AMR.1 %>%
  filter(reference == "Adelaide, A, et al.2008")
DATA.7


####Bar plot from Adelaide, A, et al.2008 ####
P7 <- ggplot(DATA.7, aes(x = pathogen, y = percentage_isolatesresistant,
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of E.coli in chicken in Limuru, Kiambu county, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Adelaide, A, et al.2008 ") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P7

P7 + scale_fill_manual(values = cbPalette)

#### Extracting Ajak, Tino 2017 from AMR.1 data set and naming it DATA.8####
DATA.8 <- AMR.1 %>%
  filter(reference == "Ajak, Tino 2017")
DATA.8

####Bar plot from Ajak, Tino 2017 ####
P8 <- ggplot(DATA.8, aes(x = pathogen, y = percentage_isolatesresistant, 
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of different pathogens in chicken in Nairobi, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Ajak, Tino 2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P8

P8 + scale_fill_manual(values = cbPalette)
#### Extracting Wesonga et al.2010 from AMR.1 data set and naming it DATA.9#### 
DATA.9 <- AMR.1 %>%
  filter(reference == "Wesonga et al.2010")
DATA.9

####Bar plot from Wesonga et al.2010 ####
P9 <- ggplot(DATA.9, aes(x = pathogen, 
                         y = percentage_isolatesresistant, 
                         fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of E.coli and Salmonella in Chickens in Nairobi, Kenya",
       
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Wesonga et al.2010") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P9

P9 + scale_fill_manual(values = cbPalette)


####Extracting Odwar, Joyce, et al.2014 from AMR.1 data set and naming it DATA.10#### 
DATA.10 <- AMR.1 %>%
  filter(reference == "Odwar, Joyce, et al.2014")
DATA.10

####Bar plot from Odwar, Joyce, et al.2014 ####
P10 <- ggplot(DATA.10, aes(x = pathogen, y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of E.coli in Chickens in Nairobi, Kenya",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Odwar, Joyce, et al.2014") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P10

P10 + scale_fill_manual(values = cbPalette)

####Extracting Jans, Christoph, et al.2017 from AMR.1 data set and naming it DATA.11#### 
DATA.11 <- AMR.1 %>%
  filter(reference == "Jans, Christoph, et al.2017")
DATA.11

####Bar plot from Jans, Christoph, et al.2017 ####
P11 <- ggplot(DATA.11, aes(x = pathogen, y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR percentages of S.aureus in Cattle and sheep in Kenya", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = " (www.resistancebank.org) Jans, Christoph, et al.2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P11

P11 + scale_fill_manual(values = cbPalette)

####Using cowplot to put plots for Kenya together####

library(cowplot)

KENYAPLOT <- plot_grid(P1, P2, P3, nrow = 2, align = "v")
KENYAPLOT
KENYAPLOT + scale_fill_manual(values = cbPalette)
ggsave("KENYAPLOT.png", KENYAPLOT, height = 10, width = 12, dpi = 600)

####Ethiopia AMR data####

###Extracting Alemayehu et al.2003 from AMR.1 data set and naming it DATA.12####
DATA.12 <- AMR.1 %>%
  filter(reference == "Alemayehu et al.2003")
DATA.12

####Bar plot from Alemayehu et al.2003####
P12 <- ggplot(DATA.12, aes(x = pathogen, y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Salmonella in Cattle in Ethiopia",
       x = "Pathogen", 
       y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "www.resistancebank.org Alemayehu et al.2003") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P12

P12 + scale_fill_manual(values = cbPalette)

###Extracting Molla, et al.2006 from AMR.1 data set and naming it DATA.13####
DATA.13 <- AMR.1 %>%
  filter(reference == "Molla, et al.2006")
DATA.13

####Bar plot from Molla, et al.2006####
P13 <- ggplot(DATA.13, aes(x = pathogen, y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Salmonella across different species in Ethiopia", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "www.resistancebank.org Molla, et al.2006") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P13

P13 + scale_fill_manual(values = cbPalette)

###Extracting Tassew, et al.2010 from AMR.1 data set and naming it DATA.14####
DATA.14 <- AMR.1 %>%
  filter(reference == "Tassew, et al.2010")

DATA.14

####Bar plot from Tassew, et al.2010####
P14 <- ggplot(DATA.14, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of Salmonella and S.aureus in cattle in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Tassew, et al.2010") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P14

P14 + scale_fill_manual(values = cbPalette)

###Extracting Abdi, Reta, et al.2017 from AMR.1 data set and naming it DATA.15####
DATA.15 <- AMR.1 %>%
  filter(reference == "Abdi, Reta, et al.2017")
DATA.15

####Bar plot from Abdi, Reta, et al.2017####
P15 <- ggplot(DATA.14, aes(x = pathogen, 
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella and S.aureus in chicken Ethiopia", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Abdi, Reta, et al.2017") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P15

P15 + scale_fill_manual(values = cbPalette)


###Extracting Atnafie et al.2017 from AMR.1 data set and naming it DATA.16####
DATA.16 <- AMR.1 %>%
  filter(reference == "Atnafie et al.2017")
DATA.16

####Bar plot from Atnafie et al.2017####
P16 <- ggplot(DATA.16, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in cattle Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Atnafie et al.2017") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P16

P16 + scale_fill_manual(values = cbPalette)

###Extracting Azage and Kibret 2017 from AMR.1 data set and naming it DATA.17####
DATA.17 <- AMR.1 %>%
  filter(reference == "Azage and Kibret 2017")
DATA.17

####Bar plot from Azage and Kibret 2017####
P17 <- ggplot(DATA.17, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in cattle Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Azage and Kibret 2017") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P17

P17 + scale_fill_manual(values = cbPalette)

###Extracting Dulo et al.2015 from AMR.1 data set and naming it DATA.18####
DATA.18 <- AMR.1 %>%
  filter(reference == "Dulo et al.2015")
DATA.18

####Bar plot from Dulo et al.2015####
P18 <- ggplot(DATA.18, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli in goats in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "www.resistancebank.org Dulo et al.2015") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P18

P18 + scale_fill_manual(values = cbPalette)

###Extracting Seyeoum et al.2018 from AMR.1 data set and naming it DATA.19####
DATA.19 <- AMR.1 %>%
  filter(reference == "Seyeoum et al.2018")
DATA.19

####Bar plot from Seyeoum et al.2018####
P19 <- ggplot(DATA.19, aes(x = pathogen, 
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of S.aureus in cattle in Ethiopia", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Seyeoum et al.2018") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P19

P19 + scale_fill_manual(values = cbPalette)

###Extracting Messele et al.2017 from AMR.1 data set and naming it DATA.20####
DATA.20 <- AMR.1 %>%
  filter(reference == "Messele et al.2017")
DATA.20

####Bar plot from Messele et al.2017####
P20 <- ggplot(DATA.20, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli across different hosts in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Messele et al.2017") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P20

P20 + scale_fill_manual(values = cbPalette)

###Extracting Eguale Tadesse 2018 from AMR.1 data set and naming it DATA.21####
DATA.21 <- AMR.1 %>%
  filter(reference == "Eguale Tadesse 2018")
DATA.21

####Bar plot from Eguale Tadesse 2018####
P21 <- ggplot(DATA.21, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in chickens in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Eguale Tadesse 2018") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P21

P21 + scale_fill_manual(values = cbPalette)


###Extracting Hiko, Adem et al.2008 from AMR.1 data set and naming it DATA.22####
DATA.22 <- AMR.1 %>%
  filter(reference == "Hiko, Adem et al.2008")
DATA.22

####Bar plot from Hiko, Adem et al.2008####
P22 <- ggplot(DATA.22, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli across different species in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Hiko, Adem et al.2008") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P22

P22 + scale_fill_manual(values = cbPalette)




###Extracting Kemal et al.2016 from AMR.1 data set and naming it DATA.23####
DATA.23 <- AMR.1 %>%
  filter(reference == "Kemal et al.2016")
DATA.23

####Bar Kemal et al.2016####
P23 <- ggplot(DATA.23, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in chickens in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Kemal et al.2016") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P23

P23 + scale_fill_manual(values = cbPalette)

###Extracting Wabeto et al.2017 from AMR.1 data set and naming it DATA.24####
DATA.24 <- AMR.1 %>%
  filter(reference == "Wabeto et al.2017")
DATA.24

####Bar Wabeto et al.2017####
P24 <- ggplot(DATA.24, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in cattle in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Wabeto et al.2017") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P24

P24 + scale_fill_manual(values = cbPalette)

###Extracting Zewdu and Cornelius 2009 from AMR.1 data set and naming it DATA.25####
DATA.25 <- AMR.1 %>%
  filter(reference == "Zewdu and Cornelius 2009")
DATA.25

####Bar Zewdu and Cornelius 2009####
P25 <- ggplot(DATA.25, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella in cattle in Ethiopia",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Zewdu and Cornelius 2009") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P25

P25 + scale_fill_manual(values = cbPalette)

####Extracting Nertherlands data from AMR.1 and naming it DATA.NERTH####

DATA.NERTH <- AMR.1 %>%
  group_by(country) %>%
  filter(country == "Netherlands")
select(species, 
       sampling_start_date, 
       sampling_end_date, 
       pathogen, antibiotic_class,
       percentage_isolatesresistant,
       reference)

DATA.NERTH

###Extracting Dorado-Garcia et al.2016 from DATA.NERTH data set and naming it DATA.26####
DATA.26 <- DATA.NERTH %>%
  filter(reference == "Dorado-Garcia et al.2016")
DATA.26

####Bar Dorado-Garcia et al.2016####
P26 <- ggplot(DATA.26, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of E.coli across different hosts in the Netherlands",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Dorado-Garcia et al.2016") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P26

P26 + scale_fill_manual(values = cbPalette)


###Extracting de Greeff, S. C., et al.2021 from DATA.NERTH data set and naming it DATA.27####
DATA.27 <- DATA.NERTH %>%
  filter(reference == "de Greeff, S. C., et al.2021")
DATA.27

####de Greeff, S. C., et al.2021####
P27 <- ggplot(DATA.27, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity",
           position = "dodge") + 
  labs(title = "AMR of salmonella, E.coli, campylobacter and MRSA across different hosts in the Netherlands",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "de Greeff, S. C., et al.2021") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P27

P27 + scale_fill_manual(values = cbPalette)



###Extracting de Greeff, S. C., et al.2020 from DATA.NERTH data set and naming it DATA.28####
DATA.28 <- DATA.NERTH %>%
  filter(reference == "de Greeff, S. C., et al.2020")
DATA.28

####de Greeff, S. C., et al.2020####
P28 <- ggplot(DATA.28, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR of salmonella, E.coli and campylobacter across different hosts in the Netherlands",
       x = "Pathogen",
       y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "de Greeff, S. C., et al.2020") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P28

P28 + scale_fill_manual(values = cbPalette)


###Denmark report
###Extracting Korsgaard, Helle Bisgaard, et al.2020 from AMR.1 data set and naming it DATA.29####
DATA.29 <- AMR.1 %>%
  filter(reference == "Korsgaard, Helle Bisgaard, et al.2020")
DATA.29

####Plot Korsgaard, Helle Bisgaard, et al.2020####
P29 <- ggplot(DATA.29, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Campylobacter spp across different hosts in Denmark",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Korsgaard, Helle Bisgaard, et al.2020") + 
  facet_wrap(~species, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P29

P29 + scale_fill_manual(values = cbPalette)

####India studies
####Extracting Anju, Kottlahouse., et al.2020 from AMR.1 dataset####

DATA.30 <- AMR.1 %>%
  filter(reference == "Anju, Kottlahouse., et al.2020")

DATA.30

####Plot Anju, Kottlahouse., et al.2020####
P30 <- ggplot(DATA.30, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of C.perfrigens across different hosts in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Anju, Kottlahouse., et al.2020") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P30

P30 + scale_fill_manual(values = cbPalette)

####Extracting Naik et al.2015 from AMR.1 dataset####

DATA.31 <- AMR.1 %>%
  filter(reference == "Naik et al.2015")

DATA.31

####Plot Naik et al.2015####
P31 <- ggplot(DATA.31, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of salmonella in Chicken and goats in India", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Naik et al.2015") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P31

P31 + scale_fill_manual(values = cbPalette)

####Extracting Mir, Abdul., et al.2014 from AMR.1 data set##
DATA.32 <- AMR.1 %>%
  filter(reference == "Mir, Abdul., et al.2014")

DATA.32

####Plot Mir, Abdul., et al.2014####
P32 <- ggplot(DATA.32, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of C.perfrigens across different hosts in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", caption = "Mir, Abdul., et al.2014") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P32

P32 + scale_fill_manual(values = cbPalette)

####Extracting Mandakini, R., et al.2020 from AMR.1 dataset####

DATA.33 <- AMR.1 %>%
  filter(reference == "Mandakini, R., et al.2020")

DATA.33

####Mandakini, R., et al.2020####
P33 <- ggplot(DATA.33, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR profiles of E.coli in pigs in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Mandakini, R., et al.2020") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P33

P33 + scale_fill_manual(values = cbPalette)

####Extracting Brower, Charles H., et al.2017 from AMR.1 data set##
DATA.34 <- AMR.1 %>%
  filter(reference == "Brower, Charles H., et al.2017")

DATA.34

####Plot Brower, Charles H., et al.2017####
P34 <- ggplot(DATA.34, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of E.coli in poultry in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Brower, Charles H., et al.2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P34

P34 + scale_fill_manual(values = cbPalette)

####Extracting Hussain Arif et., et al.2017 from AMR.1 dataset####

DATA.35 <- AMR.1 %>%
  filter(reference == "Hussain Arif et., et al.2017")

DATA.35

####Plot Hussain Arif et., et al.2017####
P35 <- ggplot(DATA.35, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of E.coli in poultry in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Hussain Arif et., et al.2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P35

P35 + scale_fill_manual(values = cbPalette)

########Extracting Kumar, Ravinder., et al.2011 from AMR.1 dataset####

DATA.36 <- AMR.1 %>%
  filter(reference == "Kumar, Ravinder., et al.2011")

DATA.36

####Kumar, Ravinder., et al.2011####
P36 <- ggplot(DATA.36, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill =antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR profiles of S.aureus in bovine",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Kumar, Ravinder., et al.2011") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P36

P36 + scale_fill_manual(values = cbPalette)


####Extracting Suman, Kumar., et al.2021 from AMR.1 dataset####

DATA.37 <- AMR.1 %>%
  filter(reference == "Suman, Kumar., et al.2021")

DATA.37

####Plot Suman, Kumar., et al.2021####
P37 <- ggplot(DATA.37, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of campylobacter in poultry in India",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Suman, Kumar., et al.2021") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P37

P37 + scale_fill_manual(values = cbPalette)

####Extracting Rawat, Neelam., et al.2018from AMR.1 dataset####

DATA.38 <- AMR.1 %>%
  filter(reference == "Rawat, Neelam., et al.2018")

DATA.38

####Plot Rawat, Neelam., et al.2018####
P38 <- ggplot(DATA.38, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Campylobacter in Bovine, Pigs and Poultry",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Rawat, Neelam., et al.2018") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P38

P38 + scale_fill_manual(values = cbPalette)


####Extracting Cheng, Jia., et al.2019 from AMR.1 dataset####

DATA.39 <- AMR.1 %>%
  filter(reference == "Cheng, Jia., et al.2019")

DATA.39

####Plot Cheng, Jia., et al.2019####
P39 <- ggplot(DATA.39, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of pathogens in dairy cattle",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Cheng, Jia., et al.2019") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P39

P39 + scale_fill_manual(values = cbPalette)

####Extracting Yu, Xin., et al.2021 from AMR.1 dataset####

DATA.40 <- AMR.1 %>%
  filter(reference == "Yu, Xin., et al.2021")

DATA.40

####Yu, Xin., et al.2021####
P40 <- ggplot(DATA.40, aes(x = pathogen, 
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR profiles of Salmonella in poultry in China", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Yu, Xin., et al.2021") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P40

P40 + scale_fill_manual(values = cbPalette)

####Extracting Yang, Xiaojuan., et al.2020 from AMR.1 dataset####

DATA.41 <- AMR.1 %>%
  filter(reference == "Yang, Xiaojuan., et al.2020")

DATA.41

####Plot Yang, Xiaojuan., et al.2020####
P41 <- ggplot(DATA.41, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Salmonella in poultry in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Yang, Xiaojuan., et al.2020") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P41

P41 + scale_fill_manual(values = cbPalette)

####Extracting Zhu, Yuangtin., et al.2017 from AMR.1 dataset####

DATA.42 <- AMR.1 %>%
  filter(reference == "Zhu, Yuangtin., et al.2017")

DATA.42

####Plot Zhu, Yuangtin., et al.2017####
P42 <- ggplot(DATA.42, aes(x = pathogen, 
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Salmonella in poultry in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Zhu, Yuangtin., et al.2017") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P42

P42 + scale_fill_manual(values = cbPalette)

####Extracting Tian, X. Y., et al.2019 from AMR.1 dataset####

DATA.43 <- AMR.1 %>%
  filter(reference == "Tian, X. Y., et al.2019")

DATA.43

####Plot Tian, X. Y., et al.2019####
P43 <- ggplot(DATA.43, aes(x = pathogen, 
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity",
           position = "dodge") + 
  labs(title = "AMR rates of Streptococcus in cattle in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class", 
       caption = "Tian, X. Y., et al.2019") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P43

P43 + scale_fill_manual(values = cbPalette)

####Extracting Wang, Dengfeng., et al.2015 from AMR.1 dataset####

DATA.44 <- AMR.1 %>%
  filter(reference == "Wang, Dengfeng., et al.2015")

DATA.44

####Plot Wang, Dengfeng., et al.2015####
P44 <- ggplot(DATA.44, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of S.aureus in cattle in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Wang, Dengfeng., et al.2015") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P44

P44 + scale_fill_manual(values = cbPalette)

####Extracting Xu, Yaohui., et al.2020 from AMR.1 dataset####

DATA.45 <- AMR.1 %>%
  filter(reference == "Xu, Yaohui., et al.2020")

DATA.45

####Plot Xu, Yaohui., et al.2020####
P45 <- ggplot(DATA.45, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Salmonella in poultry in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Xu, Yaohui., et al.2020") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P45

P45 + scale_fill_manual(values = cbPalette)

####Extracting Xiuhua, Kuang., et al.2015from AMR.1 dataset####

DATA.46 <- AMR.1 %>%
  filter(reference == "Xiuhua, Kuang., et al.2015")

DATA.46

####Plot Xiuhua, Kuang., et al.2015####
P46 <- ggplot(DATA.46, aes(x = pathogen,
                           y = percentage_isolatesresistant, 
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Salmonella across various hosts in china", 
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Xiuhua, Kuang., et al.2015") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P46

P46 + scale_fill_manual(values = cbPalette)

####Extracting Zhao, Xiaonan., et al.2021 from AMR.1 dataset####

DATA.47 <- AMR.1 %>%
  filter(reference == "Zhao, Xiaonan., et al.2021")

DATA.47

####Plot Zhao, Xiaonan., et al.2021####
P47 <- ggplot(DATA.47, aes(x = pathogen,
                           y = percentage_isolatesresistant,
                           fill = antibiotic_class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "AMR rates of Salmonella in poultry in china",
       x = "Pathogen", y = "Percentage resistance", 
       fill = "Antibiotic class",
       caption = "Zhao, Xiaonan., et al.2021") + 
  facet_wrap(~species, nrow = 1) +
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

P47

P47 + scale_fill_manual(values = cbPalette)

####Data representation####

?map_data
#world map



##increasing memory limit
memory.limit()
memory.limit(size = 12000)




##map data
new <- AMR %>%
  as_tibble()

new <- AMR %>%
  group_by(country)%>%
  select(country, percentage_isolatesresistant)
colnames(new)<- c('region', 'percentage_resistance')

mapdata <- map_data("world")
new <- left_join(mapdata, new, by = "region")
new1 <- new %>%
  filter(!is.na(new$percentage_resistance))

map1 <- ggplot(new1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = percentage_resistance), color = "black")

##plotting the heatmapheatmap <- map1 + scale_fill_gradient(name = "% resistance", low = "yellow",
                                      high = "red", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
    

    axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
heatmap


###plotting the gibbous moon plots


library(gggibbous)


moondata <- AMR %>%
  select(country, x_coordinate, y_coordinate,
         percentage_isolatesresistant, farm_type_urbanvsrural) %>%
  rename(region = country)

moondata1 <- moondata %>%
  group_by(region)%>%
  filter(region == "Kenya")
moondata1 <- moondata1 %>% 
  na.omit()
moondata1
view(moondata1)
colnames(moondata1)<- c("region", "long", "lat",
                       " percentage_isolatesresistant", "farm_type")
##Line graphs Netherlands
nerthdata <- DATA.NERTH %>%
  filter(sampling_start_date == "2019" | sampling_start_date == "2020")%>%
  select(country, sampling_start_date, species, antibiotic_class,
         percentage_isolatesresistant)
nerthdata


nerthdata1 <- aggregate(percentage_isolatesresistant~antibiotic_class+species+
                          sampling_start_date, nerthdata,mean)
view(nerthdata1)


L1 <- ggplot(nerthdata1, aes(x = sampling_start_date, 
                                    y = percentage_isolatesresistant,
                                    fill = antibiotic_class))+
  geom_col(position = position_dodge(), stat = "identitiy", width = 0.5)+
 facet_wrap(~species)+
  theme_ipsum()+
  theme(legend.position = "none")


L1+scale_fill_manual(values = cbPalette)