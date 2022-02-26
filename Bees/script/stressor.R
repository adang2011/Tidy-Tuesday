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
      ## West Coast Data
BeeStressor_WestCoast<-BeeStressor %>% 
  drop_na() %>% 
  filter(state == "California"| state == "Hawaii"| state == "Oregon"| state == "Washington")
  
view(BeeStressor_WestCoast)

      ## East Coast Data
BeeStressor_EastCoast<-BeeStressor %>% 
  drop_na() %>% 
  filter(state == "Maine"| state == "Massachusetts"| state == "Connecticut"| state == "New York"| 
           state == "New Jersey"| state == "Pensylvannia"| state == "Maryland"| state == "Virginia"|
           state == "North Carolina"| state == "South Carolina"| state == "Georgia"| state == "Florida")

view(BeeStressor_EastCoast)

      ## Mid West Data
BeeStressor_MidWest<-BeeStressor %>% 
  drop_na() %>% 
  filter(state == "Illinois"| state == "Indiana"| state == "Iowa"| state == "Kansas"| state == "Michigan"|
           state == "Minnesota"| state == "Missouri"| state == "Nebraska"| state == "North Dakota"| 
           state == "South Dakota"| state == "Ohio"| state == "Wisconsin")

view(BeeStressor_MidWest)


      ## Sothern States Data
BeeStressor_Southern<-BeeStressor %>% 
  drop_na() %>% 
  filter(state == "Texas"| state == "Florida"| state == "Geirgia"| state == "North Carolina"| state == "Virginia"|
           state == "Tennessee"| state == "Maryland"| state == "South Carolina"| state == "Alabama"| 
           state == "Louisiana"| state == "Kentucky"| state == "Oklahoma"| state == "Arkansas"|
           state == "Mississippi"| state == "West Virginia")

view(BeeStressor_Southern)

  ## Data plotting
showtext_auto()

      ## West Coast 
BeeStressor_WestCoast %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = state, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~stressor) +
  labs(title = "Bee Colonies Stressors Across West Coast",
       x = "Stress level (pct)", y = "Stressor",
       fill = "Months", 
       caption = "Bee Colonies Stressor Data from USDA") +
  theme(plot.title = element_text(size = 30, hjust = 0.5,
                                  family = "yanone"),
        axis.title = element_text(size = 20, 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key = element_rect(fill = "beige"),
        legend.title = element_text(size = 20,
                                    family = "yanone")) 
ggsave(here("Bees", "output", "West_Coast.png"),      
         width = 6, height = 3) 

      ## East Coast
BeeStressor_EastCoast %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = state, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~stressor) +
  labs(title = "Bee Colonies Stressors Across East Coast",
       x = "Stress level (pct)", y = "Stressor",
       fill = "Months", 
       caption = "Bee Colonies Stressor Data from USDA") +
  theme(plot.title = element_text(size = 30, hjust = 0.5,
                                  family = "yanone"),
        axis.title = element_text(size = 20, 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key = element_rect(fill = "beige"),
        legend.title = element_text(size = 20, 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "East_Coast.png"),      
       width = 6, height = 3) 

       ## Mid West
BeeStressor_MidWest %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = state, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~stressor) +
  labs(title = "Bee Colonies Stressors Across Mid West",
       x = "Stress level (pct)", y = "Stressor",
       fill = "Months", 
       caption = "Bee Colonies Stressor Data from USDA") +
  theme(plot.title = element_text(size = 30, hjust = 0.5,
                                  family = "yanone"),
        axis.title = element_text(size = 20, 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key = element_rect(fill = "beige"),
        legend.title = element_text(size = 20, 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Mid_West.png"),      
       width = 6, height = 3) 

        ## Southern States
BeeStressor_Southern %>% 
  ggplot(stressor, mapping = aes(x = stress_pct, y = state, color = months)) +
  geom_jitter(size = 0.6) +
  facet_grid(~stressor) +
  labs(title = "Bee Colonies Stressors Across Southern States",
       x = "Stress level (pct)", y = "Stressor",
       fill = "Months", 
       caption = "Bee Colonies Stressor Data from USDA") +
  theme(plot.title = element_text(size = 30, hjust = 0.5,
                                  family = "yanone"),
        axis.title = element_text(size = 20, 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),
        panel.border = element_rect(fill = NA, color = "black"),
        legend.key = element_rect(fill = "beige"),
        legend.title = element_text(size = 20, 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Southern_States.png"),      
       width = 6, height = 3) 
