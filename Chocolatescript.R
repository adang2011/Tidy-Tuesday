##################################################################
### First Tidy Tuesday #########
### Creator: Alex Dang ###########
### Updated on: 2022-02-24 ###########
##################################################################


### Load libraries ###############################################
library(tidyverse)
library(tidytuesdayR)
library(here)


### Data analysis ################################################
chocolate<-read_csv(here("Chocolate", "data", "chocolate.csv")) %>% 
  drop_na()

view(chocolate)

chocolate %>% 
  ggplot(mapping =  aes(x = rating, y = cocoa_percent)) +               ## setting up data plot
  geom_violin() +                                                       ## selecting type of plot
  labs(title = "Chocolate Rating by Cocoa Percent",                     ## adding plot title
       x = "Rating", y = "Cocoa Percent",                               ## adding axes titles           
       caption = "Flavors of Cacao by Will Canniford on Goldberg")      ## adding caption for source


## Plot output ######################
ggsave(here("chocolate", "output", "Chocolate.png"),      ## saves the plot to output folder
       width = 3, height = 6)   ## in inches