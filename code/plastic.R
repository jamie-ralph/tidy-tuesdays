library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(ggforce)
library(ggtext)
library(extrafont)
library(here)

# Load fonts (Monda font was downloaded from Google fonts)

loadfonts(device = "win")

# Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

# Prepare data

plastics %>% 
    filter(str_detect(country, "United Kingdom|France|Germany|Spain|Ireland|Denmark|Luxembourg|Portugal|Netherlands|Italy|Greece|Romania")) %>%
    mutate(country = case_when(
        str_detect(country, "United Kingdom") ~ "United Kingdom",
        TRUE ~ country
    )) %>%
    select(country, year, parent_company, hdpe:pvc) %>%
    pivot_longer(cols = hdpe:pvc, names_to = "plastic_type", values_to = "number_of_items") %>%
    group_by(country, year, parent_company, plastic_type) %>%
    summarise(number_of_items = sum(number_of_items, na.rm = T)) %>%
    ungroup() -> selected_countries

selected_countries %>%
    filter(year == 2020 & parent_company != "Grand Total") %>%
    mutate(is_known = case_when(
        parent_company %in% c("Unbranded", "null", "NULL", "see excel file") ~ "unknown",
        TRUE ~ "known"
    )) %>%
    ungroup() -> cleaned_selected_countries

cleaned_selected_countries %>%
    select(country, is_known, number_of_items) %>%
    group_by(country, is_known) %>%
    summarise(number_of_items = sum(number_of_items, na.rm = T)) %>%
    pivot_wider(names_from = is_known, values_from = number_of_items) %>%
    mutate(proportion_unknown = unknown / (unknown + known)) -> proportions_unknown_by_country

# Define a function for plotting the "bottle" shape plots

plot_bottle_plot <- function(df, country_name, top_fill_radius) {
    
    # Get the percent value of the country
    
    percent_value <- df %>% filter(country == country_name) %>% pull(proportion_unknown)
    
    # Main body of bottle
    bottle <- tribble(
        ~id, ~x, ~y,
        #############
        1.1, 1, 1, 
        1.2, 1.5, 1, 
        1.3, 1.5, 2,
        1.4, 1, 2,
    )
    
    # Bottle top
    top <- tribble(
        ~id, ~x, ~y,
        ############
        2.1, 1.15, 2,
        2.2, 1.15, 2.1,
        2.3, 1.35, 2.1,
        2.4, 1.35, 2
    )
    
    
    # The main shape for the country "fill"
    country_tab <- tribble(
        ~id, ~x, ~y,
        #############
        1.1, 1, 1, 
        1.2, 1.5, 1, 
        1.3, 1.5, 2 - (1 - percent_value),
        1.4, 1, 2 - (1 - percent_value)
    )
    
    # An added rectangle to square the top of the main shape
    country_tab_extra <- tribble(
        ~id, ~x, ~y,
        #############
        1.1, 1, 1.3, 
        1.2, 1.5, 1.3, 
        1.3, 1.5, 2 - (1 - percent_value),
        1.4, 1, 2 - (1 - percent_value),
    )
    
    
    ggplot() + 
        geom_shape(data = country_tab, aes(x = x, y = y), fill = "#8ecae6",
                   expand = unit(-2, 'mm'), radius = unit(17.5, 'mm')) +
        geom_shape(data = country_tab_extra, aes(x = x, y = y), fill = "#8ecae6",
                   expand = unit(-2, 'mm'), radius = unit(top_fill_radius, 'mm')) +
        geom_shape(data = bottle, aes(x = x, y = y), fill = NA, col = "black",
                   expand = unit(-2, 'mm'), radius = unit(17.5, 'mm'), size = 1.5) +
        geom_shape(data = top, aes(x = x, y = y), fill = NA, col = "black",
                   radius = unit(3, 'mm'), size = 1.5) +
        annotate("text", x = 1.25, y = 1.3, label = paste0(round(percent_value, 2) * 100, "%"), size = 8, col = "black", family = "Monda") +
        xlim(c(0.4, 2.1)) +
        ggtitle(country_name) +
        theme(
            text = element_text(family = "Monda", colour = "#8d99ae"),
            strip.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "#ffc8dd", colour = NA),
            panel.background = element_rect(fill = "#ffc8dd"),
            plot.title = element_text(hjust = 0.5, size = 22, color = "#ef233c")
        ) 
    
    
}

# Create seperate plot for each country (arg 3 = amount of rounding off at top)

france <- plot_bottle_plot(proportions_unknown_by_country, "France", 11)
germany <- plot_bottle_plot(proportions_unknown_by_country, "Germany", 16)
uk <- plot_bottle_plot(proportions_unknown_by_country, "United Kingdom", 6)
ireland <- plot_bottle_plot(proportions_unknown_by_country, "Ireland", 1)
greece <- plot_bottle_plot(proportions_unknown_by_country, "Greece", 1)
italy <- plot_bottle_plot(proportions_unknown_by_country, "Italy", 1)
denmark <- plot_bottle_plot(proportions_unknown_by_country, "Denmark", 1)
netherlands <- plot_bottle_plot(proportions_unknown_by_country, "Netherlands", 5)

# Arrange plots with patchwork

plots_grid <- (germany + france + uk + netherlands + plot_layout(nrow = 1)) /
    (denmark +italy + ireland + greece + plot_layout(nrow = 1)) 


plots_grid +
    plot_annotation(
        title = "BREAK FREE FROM PLASTIC",
        subtitle = "#breakfreefromplastic is a global movement campaigning for an end to plastic pollution.
Their annual Brand Audit uses plastic waste collected by volunteers<br> to identify the
most polluting brands. Around 63% of collected plastic is clearly branded, but this varies by country. 
This graphic explores the <span style = 'color:#00b4d8;'>proportion of <br>plastics with no reported brand</span> in a few select countries in 2020.\n\n",
        caption = "Source: Break Free From Plastic | Data vis by @statsjam for #TidyTuesday",
        theme = theme(
            plot.title = element_text(family = "Monda", colour = "#ef233c", size = 50, face = "bold"),
            plot.subtitle = element_textbox_simple(family = "Monda", colour = "#ef233c", size = 18,
                                                   padding = unit(c(0.5, 0, 1, 0), "cm")),
            plot.caption = element_text(family = "Monda", colour = "#ef233c", size = 15, face = "bold"),
            plot.background = element_rect(fill = "#ffc8dd", color = NA)
        )
    ) 

# Save the image

ggsave(here("images", "plastic_bottles.png"), width = 1920/72, height = 1080/72)
