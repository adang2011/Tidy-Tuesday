################################################################
## Looking at EU Student Mobility ##############################
## Created by: Alex Dang #######################################
## Updated on: 2022-03-08 ######################################
################################################################


## Load libraries ##############################################
library(tidyverse)
library(here)
library(sysfonts)
library(showtextdb)
library(showtext)


## Add fonts ##################
font_add_google("Yanone Kaffeesatz", "yanone")


## Load data ##################################################
Students<-read_csv(here("EU_Students_Mobility", "data", "erasmus.csv"))
view(Students)


## Data analysis ##############################################
 ## Data wrangling ###
Students_Clean<-Students %>% 
  drop_na() %>% 
  filter(participant_gender != "Undefined") %>% 
  select("mobility_duration", "participant_nationality", "participant_age", "participant_gender", 
         "special_needs")
view(Students_Clean)

  ## Data Plotting ###.
ggplot(data = Students_Clean, 
       aes(x = special_needs, y = mobility_duration, 
           color = participant_gender)) +
  geom_violin() +
  theme(plot.title = element_text(size = 26, hjust = 0.5, family = "yanone"),
        axis.title = element_text(size = 18, family = "yanone"), 
        legend.title = element_text(size = 18, family = "yanone")) +
  labs(title = "Student mobility duration based on special needs",
       x = "Special needs (Y/N)", y = "Mobility duration", 
       color = "Gender", 
       caption = "Erasmus student mobility data from Data.Europa.eu")

ggsave(here("EU_Students_Mobility", "output", "Nationality_based_mobility.png"), 
       width = 3, height = 3)
