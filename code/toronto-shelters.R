library(tidyverse)
library(grid)
library(extrafont)
library(patchwork)
library(scales)

loadfonts(device = "win")

# Read in the data 

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# Create monthly summaries

monthly <- shelters %>%
    mutate(
        occupancy_yearmon = format(occupancy_date, "%Y %m")
    ) %>%
    group_by(occupancy_yearmon, sector) %>%
    summarise(
        median_occupancy = median(occupancy, na.rm = T),
        median_capacity  = median(capacity, na.rm = T)
    )

integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}

# Define text grobs
plot_shelter_data <- function(shelter_sector, line_colour) {
    monthly %>% 
        filter(sector == shelter_sector) %>%
        ggplot() +
        geom_point(aes(x = occupancy_yearmon, y = median_capacity),
                   col = line_colour, shape = 4, size = 5, show.legend = F) +
        geom_point(aes(x = occupancy_yearmon, y = median_occupancy),
                   shape = 17, col = line_colour, fill = line_colour, size = 5) +
        geom_segment(
            aes(x = occupancy_yearmon, xend = occupancy_yearmon,
                y = median_occupancy, yend = median_capacity)
        ) +
        scale_x_discrete(breaks = c("2017 01", "2018 01", "2019 01"),
                         labels = c("2017", "2018", "2019")) +
        scale_y_continuous(breaks = integer_breaks()) +
        labs(
            x = shelter_sector,
            y = ""
        ) +
        theme(
            text = element_text(family = "Playfair Display", color = "#02040f"),
            plot.background = element_rect(fill = "#e5dada", color = NA),
            panel.background = element_rect(fill = "#e5dada", color = NA),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.x = element_text(colour = "#02040f", size = 16),
            axis.text.y = element_text(size = 12),
            
        )
}

p1 <- plot_shelter_data("Co-ed", "#335c67")
p2 <- plot_shelter_data("Families", "#840032")
p3 <- plot_shelter_data("Men", "#e59500")
p4 <- plot_shelter_data("Women", "#02040f")
p5 <- plot_shelter_data("Youth", "#5f0f40")

# Create the "text box" with a chart

# Grob

grob_1 <- textGrob("\u2715  Median capacity\n\n\n\n \u25B2  Median occupancy",
                   gp = gpar(fontfamily = "Playfair Display", col = "#02040f", size = 34))


# Patched together
              

p1 + p2 + p3 + p4 + p5 + grob_1 +
    plot_annotation(
        title = "Occupancy and capacity in Toronto shelters",
        subtitle = paste0(
            "These graphs show monthly trends in shelter occupation and capacity in Toronto between January 2017 and December 2019.\n",
            "Shelters were frequently at full capacity or over capacity."
            ),
        caption = "Data source: opendatatoronto\nVisualisation: Jamie Ralph"
    ) &
    theme(plot.background = element_rect(fill = "#e5dada", colour = NA),
          text = element_text(colour = "#02040f", family = "Playfair Display"),
          plot.title = element_text(size = 30, face = "bold"))
 
ggsave(paste0(here::here("images"), "/toronto-shelters.png"), width = 40, height = 20, units = "cm")

