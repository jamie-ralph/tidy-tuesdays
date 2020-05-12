library(tidyverse)
library(scales)
library(viridis)
library(showtext)

# Import google font
font_add_google("Ubuntu", "ubuntu")

# Read the data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# Summarise eruptions to get count and maximum VEI
eruptions_summary <- eruptions %>%
    filter(eruption_category == "Confirmed Eruption") %>%
    group_by(volcano_number) %>%
    summarise(
        number_of_eruptions = n(),
        max_vei = max(vei, na.rm = T)
    ) %>%
    filter(!str_detect(max_vei, "Inf"))

# Inner join to get data for all volcanos with VEI data
df <- volcano %>%
    inner_join(eruptions_summary, by = "volcano_number")

# Make the plots
showtext_auto()

p <- ggplot(df, aes(x = number_of_eruptions, y = population_within_30_km)) +
    geom_point(aes(size = max_vei, colour = max_vei, alpha = max_vei)) +
    scale_y_continuous(label = scales::number_format(),
                       breaks = seq(0, 8e6, 1e6)) +
    labs(
        x = "Number of confirmed eruptions",
        y = "Population within 30km",
        colour = "Maximum\nVEI",
        title = "Populations living within 30km of volcanic activity",
        caption = "Tidy Tuesday - 12th May 2020\nVEI = Volcano Explosivity Index"
    ) +
    scale_color_viridis(option = "magma") +
    scale_size_continuous(guide = FALSE) +
    scale_alpha_continuous(guide = FALSE) +
    theme(
        text = element_text(colour = "white", family = "ubuntu"),
        plot.background = element_rect(fill = "#574a48"),
        panel.background = element_rect(fill = "#574a48"),
        legend.background = element_rect(fill = "#574a48"),
        legend.title = element_text(size = 8),
        legend.position = c(0.93, 0.55),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    ) +
    annotate("text", x = 136, y = 4300000, label = "Mount Merapi", angle = -10,
             colour = "white", size = 3.5) +
    annotate(
        geom = "curve", x = 118, y = 4400000, xend = 105, yend = 4350000, 
        curvature = .3, arrow = arrow(length = unit(2, "mm")), colour = "white") +
    annotate("text", x = 156, y = 1150000, label = "Mount Etna", angle = -10,
             colour = "white", size = 3.5) +
    annotate(
        geom = "curve", x = 172, y = 1050000, xend = 192, yend = 1050000, 
        curvature = .3, arrow = arrow(length = unit(2, "mm")), colour = "white") 


# Save image
x11()
print(p)
ggsave("volcano_plot.png", dpi = 96)

showtext_auto(FALSE)
