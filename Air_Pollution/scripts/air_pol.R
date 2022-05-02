#######################################################
## Looking at Air Pollution Deaths### #################
## Created by: Alex Dang ##############################
#######################################################


## Load libraries #####################################
library(tidyverse)
library(here)
library(tvthemes)
library(sysfonts)
library(showtextdb)
library(showtext)

## Add fonts ##########################################
font_add_google("Titillium Web", "web")

## Load Data ##########################################
air_pol <- read_csv(here("Air_Pollution", "data", "indoor_pollution.csv"))
view(air_pol)

## Data Analysis ######################################
  ## Data Wrangling ###
air_pol_clean_data <- air_pol %>% 
  drop_na() %>% 
  mutate(Deaths = `Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`) %>% 
  filter(Entity == "United States") %>% 
  summarise(Year, Deaths, Entity)
view(air_pol_clean_data)

  ## Data Plotting ###
showtext_auto()

air_pol_clean_data %>% 
  ggplot(aes(x = Year, y = Deaths, fill = Deaths)) +
  geom_col() +
  labs(title = "Deaths from Indoor Air Pollution in United States", 
       caption = "Indoor Air Pollution Data from OurWorldInData.org") +
  theme_brooklyn99() +
  scale_fill_viridis_c() +
  theme(plot.title = element_text(size = 26, hjust = 0.5, family = "web"),
        axis.title = element_text(size = 18, family = "web"), 
        legend.title = element_text(size = 18, family = "web"))
ggsave(here("Air_Pollution", "output", "pollution_deaths.png"), 
       width = 6, height = 4)
