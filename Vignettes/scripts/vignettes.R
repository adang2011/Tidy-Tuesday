#######################################################
## Looking at CRAN and BIOC vignettes #################
## Created by: Alex Dang ##############################
## Updated on: 2022-04-09 #############################
#######################################################


## Load libraries #####################################
library(tidyverse)
library(here)
library(lubridate)
library(sysfonts)
library(showtextdb)
library(showtext)
library(vapoRwave)

  ## Adding fonts ###
font_add_google("Orbitron", "orb")
font_add_google("Caveat", "cav")


## Load Data ##########################################
bioc <- read_csv(here("Vignettes", "data", "bioc.csv"))
view(bioc)
cran <- read_csv(here("Vignettes", "data", "cran.csv"))
view(cran)


## Data Analysis ######################################
  ## Data Wrangling ###
bioc_clean <- bioc %>% 
  drop_na() %>% 
  select(package, rnw, rmd)
view(bioc_clean)

cran_clean <- cran %>% 
  drop_na() %>% 
  select(package, rnw, rmd)
view(cran_clean)

full_data <- inner_join(bioc_clean, cran_clean)  
view(full_data)

full_data_rnw <- full_data %>% 
  filter(rnw > 0)
view(full_data_rnw)

full_data_rmd <- full_data %>% 
  filter(rmd >0)
view(full_data_rmd)

  ## Data Plotting ###
showtext_auto()

full_data_rnw %>% 
  ggplot() + 
  geom_bar(aes(x = rnw, y = package, fill = package), stat = "identity") + 
  labs(title = "CRAN/BIOC Vignette RNW", 
       x = "RNW", y = "Packages", fill = "Packages", 
       caption = "CRAN/BIOC Vignette Data from Robert Flight GitHub") + 
  new_retro() +
  scale_fill_newRetro() +
  theme(plot.title = element_text(size = 30, family = "orb"),
        legend.title = element_text(size = 18, family = "orb"), 
        legend.text = element_text(size = 12, family = "orb"), 
        axis.title.x = element_text(size = 18, family = "orb"), 
        axis.title.y = element_text(size = 18, family = "orb"), 
        axis.text = element_text(size = 12, family = "orb"))
ggsave(here("Vignettes", "output", "Vignette_RNW.png"), 
       width = 5, height = 3)

full_data_rmd %>% 
  ggplot() + 
  geom_bar(aes(x = rmd, y = package, fill = package), stat = "identity") +
  labs(title = "CRAN/BIOC Vignette RMD", 
       x = "RMD", y = "Packages", fill = "Packages", 
       caption = "CRAN/BIOC Vignette Data from Robert Flight GitHub") +
  floral_shoppe() +
  scale_fill_vapoRwave() +
  theme(plot.title = element_text(size = 35, family = "cav"),
        legend.title = element_text(size = 24, family = "cav"), 
        legend.text = element_text(size = 16, family = "cav"), 
        axis.title.x = element_text(size = 24, family = "cav"), 
        axis.title.y = element_text(size = 24, family = "cav"), 
        axis.text = element_text(size = 16, family = "cav"))
ggsave(here("Vignettes", "output", "Vignette_RMD.png"), 
       width = 5, height = 3)
