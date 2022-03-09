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


## Load data ##################################################
Students<-read_csv(here("EU_Students_Mobility", "data", "erasmus.csv"))
view(Students)


## Data anlaysis ##############################################
 ## Data wrangling
Students_Clean<-Students %>% 
  drop_na() %>% 
  filter(participant_gender != "Undefined") %>% 
  select("mobility_duration", "participant_nationality", "participant_age", "participant_gender")

view(Students_Clean)

## NOTES: -keep mobility duration, participant nationality, participant age, participant gender
## facet gender, color  nationality, x = age and y = duration
## try out map plots....... challenge with 3D