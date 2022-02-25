##################################################################
### Today I am going to plot dog breed data #########
### Creator: Alex Dang ###########
### Updated on: 2022-02-10 ###########
##################################################################

### Load libraries ###############################################
library(tidyverse)
library(tidytuesdayR)
library(beyonce)
library(here)

### Data analysis ################################################
# load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# quick look at data
head(chocolate)


  