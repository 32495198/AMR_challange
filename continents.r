#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")
library(tidyverse)
library(patchwork)
library(wesanderson)
library(ggplot2)
##loading data
AMR <- read_csv("AMR.csv", na = "empty")
AMR %>%
  count(sampling_start_date)%>%
  view()
##filtering data
AMR_africa <- AMR %>%
  filter(continent == "Africa",
          sampling_start_date == "2013" |
           sampling_start_date == "2014" | sampling_start_date == "2015" |
           sampling_start_date == "2016" | sampling_start_date == "2017" |
           sampling_start_date == "2018" | sampling_start_date == "2019" |
           sampling_start_date == "2020") %>%
  select(continent, species, sampling_start_date, pathogen, 
         antibiotic_class, percentage_isolatesresistant)

AMR_ %>%
  count(sampling_start_date)%>%
  view()

AMR_africa$percentage_isolatesresistant <- as.numeric(
  as.character(AMR_africa$percentage_isolatesresistant))
AMR_africa1 <- aggregate(percentage_isolatesresistant~continent+
                      sampling_start_date+antibiotic_class+pathogen+
                      species, 
                    AMR_africa, mean
)

##Africa plot

africaplot <- ggplot(AMR_africa1, aes(x = pathogen, 
                                 y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Africa",
    x = "Pathogen", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


####Adding colorblind friendly scheme####
wes_palettes <- c("#FAD510", "#CB2314", "#273046", "#354823", 
  "#EABE94", "#0B775E","#F2300F",
  "#E1BD6D", "#35274A",
 "#899DA4", "#9A8822", "#3B9AB2", "#78B7C5", "#F98400", "#000000", "#C7B19C",
 "#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20", "#F3DF6C", "#CEAB07",
"#D5D5D3", "#24281A", "#798E87", "#C27D38", "#CCC591", "#29211F",
 "#9C964A", "#02401B", "#FD6467", "#5B1A18", "#E6A0C4")
#cbPalette <- c("#000000", "#E69F00", 
              # "#009E73", 
                #"#0072B2", 
               #"#D55E00", "#CC79A7", 
               #"#999999", "#330066",
              # "#0000FF", "#339900",
               #"#FF66FF",
              # "#FF3333", "#666600",
              # "#336699", "#006600",
               #"#E7298A", "#FAD510", "#CB2314", "#273046", "#354823", 
               #"#EABE94", "#0B775E","#F2300F", "#24281A", "#798E87", 
               #"#C27D38", "#CCC591")
africaplot + scale_fill_manual(values = wes_palettes)

##Asia plot
AMR_asia <- AMR %>%
  filter(continent == "Asia",
           sampling_start_date == "2015" |
           sampling_start_date == "2016" | sampling_start_date == "2017" |
           sampling_start_date == "2018" | sampling_start_date == "2019" |
           sampling_start_date == "2020") %>%
  select(continent, species, sampling_start_date, pathogen, 
         antibiotic_class, percentage_isolatesresistant)

AMR_asia$percentage_isolatesresistant <- as.numeric(
  as.character(AMR_asia$percentage_isolatesresistant))
AMR_asia1 <- aggregate(percentage_isolatesresistant~continent+
                           sampling_start_date+antibiotic_class+pathogen+
                           species, 
                         AMR_asia, mean
)

##Asia plot

asiaplot <- ggplot(AMR_asia1, aes(x = pathogen, 
                                      y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Asia",
    x = "Pathogen", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
  facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


####Adding colorblind friendly scheme####
wes_palettes <- c("#FAD510", "#CB2314", "#273046", "#354823", 
                  "#EABE94", "#0B775E","#F2300F",
                  "#E1BD6D", "#35274A",
                  "#899DA4", "#9A8822", "#3B9AB2", "#78B7C5", "#F98400", "#000000", "#C7B19C",
                  "#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20", "#F3DF6C", "#CEAB07",
                  "#D5D5D3", "#24281A", "#798E87", "#C27D38", "#CCC591", "#29211F",
                  "#9C964A", "#02401B", "#FD6467", "#5B1A18", "#E6A0C4")

asiaplot + scale_fill_manual(values = wes_palettes)


##Europe plot
AMR_euro <- AMR %>%
  filter(continent == "Europe",
         sampling_start_date == "2020" |
           sampling_start_date == "2019" | sampling_start_date == "2018" |
           sampling_start_date == "2017" |  sampling_start_date == "2016") %>%
  select(continent, species, sampling_start_date, pathogen, 
         antibiotic_class, percentage_isolatesresistant)

AMR_euro$percentage_isolatesresistant <- as.numeric(
  as.character(AMR_euro$percentage_isolatesresistant))
AMR_euro1 <- aggregate(percentage_isolatesresistant~continent+
                         sampling_start_date+antibiotic_class+pathogen+
                         species, 
                       AMR_euro, mean
)

##Europe plot

europlot <- ggplot(AMR_euro1, aes(x = pathogen, 
                                  y = percentage_isolatesresistant)) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(fill = antibiotic_class)) +
  labs(
    title =
      "Europe",
    x = "Pathogen", y = "Percentage resistance", 
    fill = "antibiotic_class") + 
facet_wrap(~sampling_start_date, nrow = 1) 
theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


####Adding colorblind friendly scheme####
wes_palettes <- c("#FAD510", "#CB2314", "#273046", "#354823", 
                  "#EABE94", "#0B775E","#F2300F",
                  "#E1BD6D", "#35274A",
                  "#899DA4", "#9A8822", "#3B9AB2", "#78B7C5", "#F98400", "#000000", "#C7B19C",
                  "#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20", "#F3DF6C", "#CEAB07",
                  "#D5D5D3", "#24281A", "#798E87", "#C27D38", "#CCC591", "#29211F",
                  "#9C964A", "#02401B", "#FD6467", "#5B1A18", "#E6A0C4")

europlot + scale_fill_manual(values = wes_palettes)


##Margin plots using patchwork
(africaplot / asiaplot / europlot) + plot_layout(guides = "collect") +
  plot_annotation(title = 
                    "AMR rates in Africa, Europe and Asia across different years",
                  tag_levels = "A", tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))& 
  scale_fill_manual(values = wes_palettes)
