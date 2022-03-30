######################################################################
## Looking at collegiate sports budgets ##############################
## Created by: Alex Dang #############################################
## Update on: 2022-03-29 #############################################
######################################################################


## Load libraries ####################################################
library(tidyverse)
library(here)
library(patchwork)
library(beyonce)
library(sysfonts)
library(showtextdb)
library(showtext)


## Add fonts ########################################################
font_add_google("Yanone Kaffeesatz", "yanone")


## Load data ########################################################
sports <- read_csv(here("Collegiate_sports", "data", "sports.csv"))
view(sports)


## Data analysis ####################################################
  ## Data wrangling ###
## Revenue
women_rev <- sports %>%
  drop_na("rev_women", "total_rev_menwomen") %>%
  mutate(percent_women_rev = c(rev_women/total_rev_menwomen)*100,
         sport_type = sports) %>%  
  filter(percent_women_rev != 100,
         sport_type != "Track and Field, X-Country", sport_type != "Track and Field, Indoor",
         sport_type != "Track and Field, Outdoor", sport_type != "Other Sports", 
         sum_partic_women > 20) %>% 
  select("sum_partic_women", "rev_women", "sport_type", "total_rev_menwomen", "percent_women_rev")
view(women_rev)

men_rev <- sports %>%
  drop_na("rev_men", "total_rev_menwomen") %>%
  mutate(percent_men_rev = c(rev_men/total_rev_menwomen)*100, 
         sport_type = sports) %>% 
  filter(percent_men_rev != 100,
         sport_type != "Track and Field, X-Country", sport_type != "Track and Field, Indoor",
         sport_type != "Track and Field, Outdoor", sport_type != "Other Sports", 
         sum_partic_men > 20) %>% 
  select("sum_partic_men", "rev_men", "total_rev_menwomen", "sport_type", "percent_men_rev")
view(men_rev)

## Expenditures
women_exp <- sports %>%
  drop_na("exp_women", "total_exp_menwomen") %>%
  mutate(percent_women_exp = c(exp_women/total_exp_menwomen)*100, 
         sport_type = sports) %>% 
  filter(percent_women_exp != 100,
         sport_type != "Track and Field, X-Country", sport_type != "Track and Field, Indoor",
         sport_type != "Track and Field, Outdoor", sport_type != "Other Sports", 
         sum_partic_women > 20) %>% 
  select("sum_partic_women", "exp_women", "sport_type", "total_exp_menwomen", "percent_women_exp")
view(women_exp)

men_exp <- sports %>%
  drop_na("exp_men", "total_exp_menwomen") %>%
  mutate(percent_men_exp = c(exp_men/total_exp_menwomen)*100, 
         sport_type = sports) %>% 
  filter(percent_men_exp != 100,
         sport_type != "Track and Field, X-Country", sport_type != "Track and Field, Indoor",
         sport_type != "Track and Field, Outdoor", sport_type != "Other Sports", 
         sum_partic_men > 20) %>% 
  select("sum_partic_men", "exp_men", "total_exp_menwomen", "sport_type", "percent_men_exp")
view(men_exp)


  ## Data plotting ###
showtext_auto()
## Functions
women_plot <- function(data, x, y){
  ggplot(data, aes(x = {{x}}, y = {{y}}, color = sum_partic_women)) +
    geom_point(size = 0.5) +
    scale_color_viridis_c() +
    theme(plot.title = element_text(size = 26, hjust = 0.5, family = "yanone"),
          axis.title = element_text(size = 18, family = "yanone"), 
          legend.title = element_text(size = 18, family = "yanone"))
  }

men_plot <- function(data, x, y){
  ggplot(data, aes(x = {{x}}, y = {{y}}, color = sum_partic_men)) +
    geom_point(size = 0.5) +
    scale_color_viridis_c() +
    theme(plot.title = element_text(size = 26, hjust = 0.5, family = "yanone"),
          axis.title = element_text(size = 18, family = "yanone"), 
          legend.title = element_text(size = 18, family = "yanone"))
}

## Revenue
pwomen_rev <- women_plot(data = women_rev, x = percent_women_rev, y = sport_type) +
  labs(title = "Collegiate Sports Revenue for Women", 
       x = "Percent Revenue", y = "Sports", 
       color = "Participants", 
       caption = "Collegiate Sports Budgets data from NPR")
pwomen_rev

pmen_rev <- men_plot(data = men_rev, x = percent_men_rev, y = sport_type) +
  labs(title = "Collegiate Sports Revenue for Men", 
       x = "Percent Revenue", y = "Sports", 
       color = "Participants", 
       caption = "Collegiate Sports Budgets data from NPR")
pmen_rev

pmen_rev+pwomen_rev 
ggsave(here("Collegiate_sports", "output", "Collegiate_Sports_Revenue.png"), 
       width = 8, height = 4)

## Expenditures
pwomen_exp <- women_plot(data = women_exp, x = percent_women_exp, y = sport_type) +
  labs(title = "Collegiate Sports Expenditures for Women", 
       x = "Percent Expenditures", y = "Sports", 
       color = "Participants", 
       caption = "Collegiate Sports Budgets data from NPR")
pwomen_exp

pmen_exp <- men_plot(data = men_exp, x = percent_men_exp, y = sport_type) +
  labs(title = "Collegiate Sports Expenditures for Men", 
       x = "Percent Expenditures", y = "Sports", 
       color = "Participants", 
       caption = "Collegiate Sports Budgets data from NPR")
pmen_exp

pmen_exp+pwomen_exp
ggsave(here("Collegiate_sports", "output", "Collegiate_Sports_Expenditures.png"), 
       width = 8, height = 4)

