library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(stats)
#Setting working directory
setwd("C:/Users/Moses/Desktop/Stopping AMR project/Data")
###Loading the data set


AMR <- read_csv("AMR.csv", na = "empty")
 View(AMR)
 
 colnames(AMR)
 ##Cleaning variable names using the clean names function
 
 AMR_clean <- clean_names(AMR)
 
 View(AMR_clean)
 
##removing empty columns and rows
 AMR_clean <- AMR_clean %>% remove_empty( c("rows", "cols"))
 View(AMR_clean)
 
 ###cleaning the country column
 
 AMR_clean %>% tabyl(country)
 AMR_clean
 
 AMR_clean %>%
   count(country) %>%
  View()
 
 
 
 AMR_clean <- AMR_clean %>% 
   mutate(country = ifelse(country =="RWA", "Rwanda", country))
 
 AMR_clean <- AMR_clean %>% 
   mutate(country = ifelse(country =="KAZ", "Kazakhstan", country))
 AMR_clean <- AMR_clean %>% 
   mutate(country = ifelse(country =="CdIvoire", "Ivory coast", country))
 
 
 AMR_clean <- AMR_clean %>%
   filter(country != "Argentina" ,country != "Bolivia", country != "Brazil",
          country != "Chile", country != "Colombia", country != "Costa Rica",
          country != "Cuba", country != "Ecuador", country != "Grenada",
          country != "Jamaica", country != "Mexico", country != "Nicaragua",
          country != "Peru", country != "Puerto Rico", 
          country != "Saint Lucia", country != "Trinidad and Tobago",
          country != "Uruguay", country != "Venezuela")
   
 K <- AMR_clean %>%
   filter(country == "Kenya")
   count(country == "Kenya") %>%
   View()
 
 ##Cleaning species column
 AMR_clean %>%
   count(species) %>%
   View()
 
 AMR_clean<- AMR_clean %>%
   mutate(species = ifelse(species =="Pigss", "Pigs", species))
 
 AMR_clean <- AMR_clean %>%
   mutate(species = ifelse(species =="Human", "Humans", species))
 
 AMR_clean %>%
   count(species) %>%
   View()
 
 ##Cleaning farm_type_urbanvsrural column
 AMR_clean %>% count(farm_type_urbanvsrural) %>%
   View()
 
 ##Cleaning sampling_start_date column
 AMR_clean %>% count(sampling_start_date) %>%
    View()
    str(AMR_clean)
 
AMR_clean$sampling_start_date %>% 
   as.Date(AMR_clean$sampling_start_date, "%b-%Y")

AMR_clean %>% count(sampling_start_date) %>%
   View()

##Cleaning sampling_end_date column
AMR_clean %>% count(sampling_end_date) %>%
   View()
str(AMR_clean)

AMR_clean$sampling_end_date %>% 
   as.Date(AMR_clean$sampling_end_date, "%b-%Y")

AMR_clean %>% count(sampling_end_date) %>%
   View()

##Cleaning pathogen column
AMR_clean %>% count(pathogen) %>%
   View()
 
AMR_clean<- AMR_clean %>%
   mutate(pathogen = ifelse(pathogen =="E. coli", "E.coli", pathogen))

AMR_clean<- AMR_clean %>%
   mutate(pathogen = ifelse(pathogen =="S. aureus", "S.aureus", pathogen))

AMR_clean %>% count(pathogen) %>%
   View()

###Clean strain column
AMR_clean %>% count(strain) %>%
   View()
##Clean serotype column
AMR_clean %>% count(serotype) %>%
   View()
##clean control_strain column
AMR_clean %>% count(control_strain) %>%
   View()
##clean sample_type column
AMR_clean %>% count(sample_type) %>%
   View()

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Caecal saple", 
                    "Caecal", sample_type))

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Cloaca Swab", 
                    "Cloacal swab", sample_type))
AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Cloacal swabs", 
                    "Cloacal swab", sample_type))

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Feacal", 
                    "Faecal", sample_type))

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Feacal sample", 
                    "Faecal", sample_type))

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Feaces", 
                    "Faecal", sample_type))
AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "Liveanimal", 
                    "Live animal", sample_type))

AMR_clean<- AMR_clean %>%
   mutate(sample_type = 
             ifelse(sample_type == "LivingAnimal", 
                    "Live animal", sample_type))



AMR_clean %>% count(sample_type) %>%
   View()
##Clean no_sample column
AMR_clean %>% count(no_sample) %>%
   View()
##clean no_isolate column
AMR_clean %>% count(no_isolate) %>%
   View()
##Clean antimicrobial column
   
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxicilin", 
                    "Amoxicillin", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxillin", 
                    "Amoxicillin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxycillin", 
                    "Amoxicillin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxicillin-cluvanic acid", 
                    "Amoxicillin-clavulanic acid", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxycillin-clavulanic acid", 
                    "Amoxicillin-clavulanic acid", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Amoxycillin-cluvanic acid", 
                    "Amoxicillin-clavulanic acid", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Cefalotin", 
                    "Cefalothin", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "cefalotin", 
                    "Cefalothin", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Ceflothin", 
                    "Cefalothin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Cefalozin", 
                    "Cefazolin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "ceftiofur", 
                    "Ceftiofur", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Ceftiafur", 
                    "Ceftiofur", antimicrobial))
AMR_clean<- AMR_clean %>%
  mutate(antibiotic_class = 
           ifelse(antibiotic_class == "cefoxitin", 
                  "Cefoxitin", antibiotic_class))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Cefuroxime sodium", 
                    "Cefuroxime", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Ciproflaxin", 
                    "Ciprofloxacin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Sulfamethoxazole-Trimethoprim", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Sulfamethaxole-trimethoprim", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Sulfamethaxazole-trimethoprin", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Sulfamethaxazole-trimethoprim", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Co-trimoxazole", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == 
                       "Co-trimoxazole (sulfamethazole-trimethoprim)", 
                    "Sulfamethaxazole-trimethoprim", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Levafloxacin", 
                    "Levofloxacin", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == 
                       "PenicillinG", 
                    "Penicillin", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "sulfisoxazole", 
                    "Sulfisoxazole", antimicrobial))

AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == 
                       "Sulfamethaxazole", 
                    "Sulfamethoxazole", antimicrobial))
AMR_clean<- AMR_clean %>%
   mutate(antimicrobial = 
             ifelse(antimicrobial == "Sulfamethaxole", 
                    "Sulfamethoxazole", antimicrobial))


AMR_clean %>% count(antimicrobial) %>%
   View()

##clean antimicrobial_compound column
AMR_clean %>% 
   count(antimicrobial_compound) %>%
   View()

##clean antibiotic_class
AMR_clean %>% 
   count(antibiotic_class) %>%
   View()

AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "1st Generation Cephalosporin", 
                    "1st generation Cephalosporin", antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "2nd Generation Cephalosporin", 
                    "2nd generation Cephalosporin", antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "3rd Generation Cephalosporin", 
                    "3rd generation Cephalosporin", antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "4th Generation Cephalosporin", 
                    "4th generation Cephalosporin", antibiotic_class))

AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Amidinopenicillins", 
                    "Aminopenicillins", antibiotic_class))


AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Penicillin", 
                    "Penicillins", antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Steroid antibacterials", 
                    "Steroid antebacterials", antibiotic_class))

AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Steroid antibacterials", 
                    "Steroid Antibacterials", antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Sulfonamides, Trimethoprim and Combinations", 
                    "Sulfonamide, Trimethoprim and Combinations",
                    antibiotic_class))
AMR_clean<- AMR_clean %>%
   mutate(antibiotic_class = 
             ifelse(antibiotic_class == 
                       "Tetracycline", 
                    "Tetracyclines", antibiotic_class))


AMR_clean %>% 
   count(antibiotic_class) %>%
   View()

##Clean ast_method coloumn

AMR_clean %>% 
   count(ast_method) %>%
   View()
##Clean percentage_isolatesresistant

AMR_clean %>% 
   count(percentage_isolatesresistant) %>%
   View()
##clean percentage_prevalenceofinfection

AMR_clean %>% 
   count(percentage_prevelanceofinfection) %>%
   View()
##clean reference
AMR_clean%>%
   count(reference) %>%
View()

##clean tittle
AMR_clean%>%
   count(tittle) %>%
   View()
##clean journal
AMR_clean%>%
   count(journal) %>%
   View()
##clean author
AMR_clean%>%
   count(author) %>%
   View()
##clean pub_date
AMR_clean%>%
   count(pub_date) %>%
   View()

##getting duplicates
AMR_clean <- AMR_clean[duplicated(AMR_clean$pub_date, AMR_clean$author), 
           fromLast = TRUE]
View(AMR_clean)

AMR_clean <- AMR_clean %>%
  mutate_if(is.numeric, round, digits = 2)
###Exporting data to csv file in the stopping AMR project folder
write.table(AMR_clean, file = "AMR1.csv", row.names = FALSE,
            sep = ",")