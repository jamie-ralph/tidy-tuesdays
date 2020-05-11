library(tidyverse)
library(scales)
library(viridis)
library(showtext)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# Summarise eruptions

eruptions_summary <- eruptions %>%
    filter(eruption_category == "Confirmed Eruption") %>%
    group_by(volcano_number) %>%
    summarise(
        number_of_eruptions = n(),
        max_vei = max(vei, na.rm = T)
    ) %>%
    filter(max_vei != "-Inf")

# Join data

df <- volcano %>%
    inner_join(eruptions_summary, by = "volcano_number")

# plot!

ggplot(df, aes(x = number_of_eruptions, y = population_within_30_km)) +
    geom_point(aes(size = max_vei, colour = max_vei, alpha = max_vei)) +
    scale_y_continuous(label = scales::number_format()) +
    labs(
        x = "Number of confirmed eruptions",
        y = "Populations within 30km"
    ) +
    scale_color_viridis(option = "magma") +
    scale_size_continuous(guide = FALSE) +
    scale_alpha_continuous(guide = FALSE) +
    theme(
        text = element_text(colour = "white"),
        plot.background = element_rect(fill = "#574a48"),
        panel.background = element_rect(fill = "#574a48"),
        legend.background = element_rect(fill = "#574a48"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ) +
    annotate("text", x = 120, y = 4400000, label = "Merapi", angle = 20, colour = "white") +
    annotate(
        geom = "curve", x = 118, y = 4400000, xend = 105, yend = 4350000, 
        curvature = .3, arrow = arrow(length = unit(2, "mm")), colour = "white")



#### To dos

# Increase y axis spacing
# Add top right text
# Add google font
# Make legend long?
# Add annotation to the coral coloured one
