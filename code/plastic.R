library(tidytuesdayR)
library(tidyverse)

# Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

plastics %>% 
    filter(str_detect(country, "United Kingdom")) %>%
    select(year, parent_company, hdpe:pvc) %>%
    pivot_longer(cols = hdpe:pvc, names_to = "plastic_type", values_to = "number_of_items") -> uk_plastics

check <- uk_plastics %>%
    mutate("Unknown" = case_when(
        parent_company %in% c("Unbranded", "null") ~ "Unknown",
        TRUE ~ "Known"
    )) %>%
    filter(year == "2020" & Unknown == "Unknown") %>%
    group_by(plastic_type) %>%
    summarise(n = sum(number_of_items, na.rm = T)) %>%
    arrange(desc(n))

## ggforce be kind to me
library(ggforce)

bottle <- tribble(
    ~id, ~x, ~y,
    #############
    1.1, 1, 1, 
    1.2, 1.5, 1, 
    1.3, 1.5, 2,
    1.4, 1, 2,
)

top <- tribble(
    ~id, ~x, ~y,
    ############
    2.1, 1.15, 2,
    2.2, 1.15, 2.1,
    2.3, 1.35, 2.1,
    2.4, 1.35, 2
)

# My test country, Neverland, will have 75% of its collected plastic waste labelled "unknown"

neverland <- tribble(
    ~id, ~x, ~y,
    #############
    1.1, 1, 1, 
    1.2, 1.5, 1, 
    1.3, 1.5, 2 - 0.25,
    1.4, 1, 2 - 0.25
)

neverland2 <- tribble(
    ~id, ~x, ~y,
    #############
    1.1, 1, 1.3, 
    1.2, 1.5, 1.3, 
    1.3, 1.5, 2 - 0.25,
    1.4, 1, 2 - 0.25,
)

ggplot() +
    geom_shape(data = positions, aes(x = x, y = y), fill = NA, col = "white",
               expand = unit(-2, 'mm'), radius = unit(17.5, 'mm')) +
    geom_shape(data = neverland, aes(x = x, y = y), fill = "#5ACFCF",
               expand = unit(-2, 'mm'), radius = unit(17.5, 'mm')) +
    geom_shape(data = neverland2, aes(x = x, y = y), fill = "#5ACFCF",
               expand = unit(-2, 'mm')) +
    geom_shape() +
    geom_shape(data = top, aes(x = x, y = y), fill = NA, col = "white",
               radius = unit(5, 'mm')) +
    xlim(c(0.1, 2.4)) +
    labs(title = "Neverland") +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white", hjust = 0.5)
    ) 

         