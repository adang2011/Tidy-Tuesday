#############################################################
## Looking at losses in bee colonies #####################
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
BeeColony<-read_csv(here("Bees", "data", "colony.csv"))
view(BeeColony)



## Data analysis ###########################################
  ## Data wrangling
    ## West Coast Data
BeeColony_WestCoast<-BeeColony %>% 
  drop_na() %>%                ## drop NAs
  filter(state == "California"| state == "Hawaii"| state == "Oregon"| state == "Washington")
## filter to west coast states

view(BeeColony_WestCoast)


    ## East Coast Data
BeeColony_EastCoast<-BeeColony %>% 
  drop_na() %>%                ## drop NAs
  filter(state == "Maine"| state == "Massachusetts"| state == "Connecticut"| state == "New York"| 
           state == "New Jersey"| state == "Pensylvannia"| state == "Maryland"| state == "Virginia"|
           state == "North Carolina"| state == "South Carolina"| state == "Georgia"| state == "Florida")
## filter to east coast states

view(BeeColony_EastCoast)


    ## Mid West Data
BeeColony_MidWest<-BeeColony %>% 
  drop_na() %>%                 ## drop NAs
  filter(state == "Illinois"| state == "Indiana"| state == "Iowa"| state == "Kansas"| state == "Michigan"|
           state == "Minnesota"| state == "Missouri"| state == "Nebraska"| state == "North Dakota"| 
           state == "South Dakota"| state == "Ohio"| state == "Wisconsin")
## filter to mid west states

view(BeeColony_MidWest)


    ## Sothern States Data
BeeColony_Southern<-BeeColony %>% 
  drop_na() %>%                 ## drop NAs
  filter(state == "Texas"| state == "Florida"| state == "Geirgia"| state == "North Carolina"| state == "Virginia"|
           state == "Tennessee"| state == "Maryland"| state == "South Carolina"| state == "Alabama"| 
           state == "Louisiana"| state == "Kentucky"| state == "Oklahoma"| state == "Arkansas"|
           state == "Mississippi"| state == "West Virginia")
## filter to southern states

view(BeeColony_Southern)


  ## Data plotting
showtext_auto()    ## automatically use showtext to render text

    ## West Coast 
BeeColony_WestCoast %>% 
  ggplot(colony, mapping = aes(x = colony_max, y = colony_lost, color = state)) +     ## set up plot
  geom_jitter(size = 0.6) +                                                           ## select type of plot
  facet_grid(~months) +                                                               ## facet to stressor values
  labs(title = "Bee Colonies Losses Across West Coast",                               ## set title label
       x = "Max Colonies", y = "Colonies Lost",                                       ## set x and y axes labels
       fill = "States",                                                             
       caption = "Bee Colonies Losses Data from USDA") +                              ## set caption for source
  theme(plot.title = element_text(size = 30, hjust = 0.5,                             ## adjust title size/centering/fonts
                                  family = "yanone"),
        axis.title = element_text(size = 20,                                          ## adjust axes size/fonts
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),                              ## set background color
        panel.border = element_rect(fill = NA, color = "black"),                      ## set border color
        legend.key = element_rect(fill = "beige"),                                    ## set legend background color
        legend.title = element_text(size = 20,                                        ## set legend title size/fonts
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Colony_West_Coast.png"),      
       width = 8, height = 3) 

     ## East Coast
BeeColony_EastCoast %>% 
  ggplot(colony, mapping = aes(x = colony_max, y = colony_lost, color = state)) +     ## set up plot
  geom_jitter(size = 0.6) +                                                           ## select type of plot
  facet_grid(~months) +                                                               ## facet to stressor values
  labs(title = "Bee Colonies Losses Across East Coast",                               ## set title label
       x = "Max Colonies", y = "Colonies Lost",                                       ## set x and y axes labels
       fill = "States", 
       caption = "Bee Colonies Losses Data from USDA") +                              ## set caption for source
  theme(plot.title = element_text(size = 30, hjust = 0.5,                             ## adjust title size/centering/fonts
                                  family = "yanone"),
        axis.title = element_text(size = 20,                                          ## adjust axes size/fonts 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),                              ## set background color
        panel.border = element_rect(fill = NA, color = "black"),                      ## set border color
        legend.key = element_rect(fill = "beige"),                                    ## set legend background color
        legend.title = element_text(size = 20,                                        ## set legend title size/fonts 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Colony_East_Coast.png"),      
       width = 8, height = 3) 

    ## Mid West
BeeColony_MidWest %>% 
  ggplot(colony, mapping = aes(x = colony_max, y = colony_lost, color = state)) +     ## set up plot
  geom_jitter(size = 0.6) +                                                           ## select type of plot
  facet_grid(~months) +                                                               ## facet to stressor values
  labs(title = "Bee Colonies Losses Across Mid West",                                 ## set title label
       x = "Max Colonies", y = "Colonies Lost",                                       ## set x and y axes labels
       fill = "States", 
       caption = "Bee Colonies Losses Data from USDA") +                              ## set caption for source
  theme(plot.title = element_text(size = 30, hjust = 0.5,                             ## adjust title size/centering/fonts
                                  family = "yanone"),
        axis.title = element_text(size = 20,                                          ## adjust axes size/fonts 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),                              ## set background color
        panel.border = element_rect(fill = NA, color = "black"),                      ## set border color
        legend.key = element_rect(fill = "beige"),                                    ## set legend background color
        legend.title = element_text(size = 20,                                        ## set legend title size/fonts 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Colony_Mid_West.png"),      
       width = 8, height = 3) 

    ## Southern States
BeeColony_Southern %>% 
  ggplot(colony, mapping = aes(x = colony_max, y = colony_lost, color = state)) +     ## set up plot
  geom_jitter(size = 0.6) +                                                           ## select type of plot
  facet_grid(~months) +                                                               ## facet to stressor values
  labs(title = "Bee Colonies Losses Across Southern States",                          ## set title label
       x = "Max Colonies", y = "Colonies Lost",                                       ## set x and y axes labels
       fill = "Months", 
       caption = "Bee Colonies Losses Data from USDA") +                              ## set caption for source
  theme(plot.title = element_text(size = 30, hjust = 0.5,                             ## adjust title size/centering/fonts
                                  family = "yanone"),
        axis.title = element_text(size = 20,                                          ## adjust axes size/fonts 
                                  family = "yanone"), 
        panel.background = element_rect(fill = "beige"),                              ## set background color
        panel.border = element_rect(fill = NA, color = "black"),                      ## set border color
        legend.key = element_rect(fill = "beige"),                                    ## set legend background color
        legend.title = element_text(size = 20,                                        ## set legend title size/fonts 
                                    family = "yanone")) 
ggsave(here("Bees", "output", "Colony_Southern_States.png"),      
       width = 8, height = 3) 