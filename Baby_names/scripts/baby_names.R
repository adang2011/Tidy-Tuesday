##########################################################
## Looking at baby names popularity ######################
## Created by: Alex Dang #################################
## Updated on: 2022-03-31 ################################
##########################################################


## Load Libraries ########################################
library(tidyverse)
library(here)
library(sysfonts)
library(showtextdb)
library(showtext)
library(patchwork)


## Add fonts ##############################################
font_add_google("Yanone Kaffeesatz", "yanone")


## Load Data ##############################################
babies <- read_csv(here("Baby_names", "data", "babynames.csv"))
view(babies)


## Data Analysis ##########################################
  ## Data Wrangling ###
modern_names <- babies %>% 
  drop_na() %>% 
  filter(year > 1999)
view(modern_names)

girls <- babies %>% 
  drop_na() %>% 
  filter(year > 1999, sex == "F", name == "Olivia"| 
         name == "Sophia"| name == "Isabella"| name == "Emily"| 
         name == "Hannah"| name == "Emma"| name == "Madison"|
         name == "Ava"| name == "Ashley"| name == "Sarah")
view(girls)

boys <- babies %>% 
  drop_na() %>% 
  filter(year > 1999, sex == "M", name =="Joseph"| 
         name == "Joshua"| name == "Matthew"| name == "Ethan"| 
         name == " Michael"| name == "Jacob"| name == "Christopher"| 
         name == "Nicholas"| name == "Andrew"| name == "Daniel")
view(boys)

  ## Data plotting ###
showtext_auto()

girls_plot <- girls %>% 
  ggplot(aes(x = n, y = name, color = year)) + 
  geom_point(size = 0.8) +
  scale_color_viridis_b() +
  labs(title = "Top 10 Names for Girls in the 21st Century", 
       x = "Count", y = "Names", color = "Year", 
       caption = "Baby Names Data from US babynames and nzbabynames") + 
  theme(plot.title = element_text(size = 26, hjust = 0.5, family = "yanone"),
        axis.title = element_text(size = 18, family = "yanone"), 
        legend.title = element_text(size = 18, family = "yanone"), 
        panel.background = element_rect(fill = "lightpink"), 
        axis.text = element_text(size = 12), 
        panel.grid = element_line(color = "beige"))
girls_plot
ggsave(here("Baby_names", "output", "girlsnames.png"), 
       width = 4, height = 3)

boys_plot <- boys %>% 
  ggplot(aes(x = n, y = name, color = year)) +
  geom_point(size = 0.8) +
  scale_color_viridis_b() +
  labs(title = "Top 10 Names for Boys in the 21st Century", 
       x = "Count", y = "Names", color = "Year", 
       caption = "Baby Names Data from US babynames and nzbabynames") + 
  theme(plot.title = element_text(size = 26, hjust = 0.5, family = "yanone"),
        axis.title = element_text(size = 18, family = "yanone"), 
        legend.title = element_text(size = 18, family = "yanone"), 
        panel.background = element_rect(fill = "lightblue"), 
        axis.text = element_text(size = 12), 
        panel.grid = element_line(color = "beige"))
boys_plot
ggsave(here("Baby_names", "output", "boysnames.png"), 
       width = 4, height = 3)

boys_plot+girls_plot +
  plot_layout(guides = 'collect')
ggsave(here("Baby_names", "output", "popularnames.png"), 
       width = 6, height = 3)
