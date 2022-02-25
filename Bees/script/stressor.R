#############################################################
## Looking at stressors in bee colonies #####################
## Created by: Alex Dang ####################################
## Updated on: 2022-02-25 ###################################
#############################################################


## Load libraries ###########################################
library(tidyverse)
library(here)
library(showtext)
library(showtextdb)
library(sysfonts)

## Adding fonts ############################################
font_add_google("Yanone Kaffeesatz", "yanone")

## Load data ###############################################
BeeStressor<-read_csv(here("Bees", "data", "stressor.csv"))
view(BeeStressor)


## Data analysis ###########################################
  ## Data wrangling
BeeStressor_WestCoast<-BeeStressor %>% 
  drop_na() %>% 
  filter(state == "California"| state == "Hawaii"| state == "Oregon"| state == "Washington")
  
view(BeeStressor_WestCoast)

BeeStressor_EastCoast<-BeeStressor %>% 
  drop_na() +
  filter(year == 2021)   #year not found???

view(BeeStressor_EastCoast)

  ## Data plotting
showtext_auto()

BeeStressor_WestCoast %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = stressor, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~state) +
  labs(title = "Bee Colonies Stressors Across West Coast",
       x = "Stress level (pct)", y = "Stressor",
       fill = "Months", 
       caption = "Bee Colonies Stressor Data from USDA") +
  theme(plot.title = element_text(size = 26, hjust = 0.5,
                                  family = "yanone"),
        axis.title = element_text(size = 14, 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key = element_rect(fill = "beige"),
        legend.title = element_text(family = "yanone")) 
ggsave(here("Bees", "output", "West_Coast.png"),      
         width = 4, height = 3) 


BeeStressor_EastCoast %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = stressor, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~state) 
