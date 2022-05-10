#######################################################
## Looking at Solar and Wind Price vs Capacity ########
## Created by: Alex Dang ##############################
#######################################################

## Load libraries #####################################
library(tidyverse)
library(here)
library(patchwork)

## Load data ###########################################
solar <- read_csv(here("US_Solar_Wind", "data", "solar.csv"))
view(solar)
wind <- read_csv(here("US_Solar_Wind", "data", "wind.csv"))
view(wind)

## Data analysis #######################################
solar_plot <- solar %>% 
  ggplot(aes(x = solar_capacity, y = solar_mwh, color = solar_capacity)) +
  geom_jitter() +
  scale_color_viridis_c() +
  labs(title = "Solar Power Costs vs Capacity", 
       x = "Solar Capacity", y = "Solar Price ($/MWh)", 
       color = "Solar Capacity", 
       caption = "Solar/Wind Utilities data from Berkeley Lab") +
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 12), 
        panel.background = element_rect(fill = "gray"), 
        axis.text = element_text(size = 10), 
        panel.grid = element_line(color = "beige"))
solar_plot
ggsave(here("US_Solar_Wind", "output", "Solar_power_cost_cap.png"), 
       width = 8, height = 5)

wind_plot <- wind %>% 
  ggplot(aes(x = wind_capacity, y = wind_mwh, color = wind_capacity)) +
  geom_jitter() +
  scale_color_viridis_c() +
  labs(title = "Wind Power Costs vs Capacity", 
       x = "Wind Capacity", y = "Wind Price ($/MWh)", 
       color = "Wind Capacity", 
       caption = "Solar/Wind Utilities data from Berkeley Lab") +
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.title = element_text(size = 16), 
        legend.title = element_text(size = 12), 
        panel.background = element_rect(fill = "gray"), 
        axis.text = element_text(size = 10), 
        panel.grid = element_line(color = "beige"))
wind_plot
ggsave(here("US_Solar_Wind", "output", "Wind_power_cost_cap.png"), 
       width = 8, height = 5)

solar_plot/wind_plot
ggsave(here("US_Solar_Wind", "output", "Solar_vs_Wind.png"), 
       width = 8, height = 12)
