library(tidyverse)
library(showtext)

# Read in the data
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# Get number of total attempts and succesful attempts by peak id
peak_attempts <- expeditions %>%
    mutate(
        successful = str_detect(termination_reason, "Succes")
        ) %>%
    group_by(peak_id) %>%
    summarise(
        total_attempts = n(),
        successful_attempts = sum(successful)
    ) %>%
    ungroup()

peak_attempts <- peak_attempts %>%
    mutate(
        prop_succesful = (successful_attempts / total_attempts) * 100
    )

# Join attempts dataset to original peaks dataset, and get top 10 ranks

full_peaks <- peaks %>%
    inner_join(peak_attempts, by = c("peak_id" = "peak_id")) %>%
    mutate(
        attempts_rank = dense_rank(desc(total_attempts)),
        top_10 = case_when(
            attempts_rank <= 10 ~ "Top 10",
            TRUE ~ "Other"
        )
    )

top_10 <- full_peaks %>%
    filter(top_10 == "Top 10")

other <- full_peaks %>%
    filter(top_10 == "Other")


# Make some plots plots

font_add_google(name = "Poppins", family = "poppins") 

showtext_auto()

himalayans_plot <- ggplot() +
    geom_point(data = top_10, aes(x = total_attempts, y = prop_succesful, fill = top_10), shape = 23, size = 6) +
    geom_point(data = other, aes(x = total_attempts, y = prop_succesful), shape = 23, size = 6, fill = "#458B74") +
    xlim(c(0, 2300)) +
    ylim(c(0, 100)) +
    labs(
        x = "Number of attempts",
        y = "% succesful",
        title = "Himalayan Climbing Expeditions",
        subtitle = "This graph shows the relationship between the number \nof attempts and proportion of attempts that were succesful \nfor each peak in the Himalayan Climbing Expeditions dataset.",
        fill = "",
        caption = "Source: The Himalayan Database"
    ) +
    scale_fill_manual(
        values = "#EDA604",
        breaks = "Top 10", 
        labels = "10 most attempted \npeaks"
        ) +
    theme_bw() +
    theme(
        text = element_text(color = "white", family = "poppins"),
        rect = element_rect(fill = "#101924ff", color = NA),
        panel.background = element_rect(fill = "#101924ff", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#6D6C6C"),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(color = "white"),
        axis.text.x = element_text(color = "grey"),
        axis.title.y = element_text(color = "white"),
        axis.text.y = element_text(color = "grey"),
        plot.title.position = "panel",
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.size = unit(10, "pt"),
        legend.key.width = unit(40, "pt"),
        legend.position = c(0.85, 1),
        legend.text = element_text(size = 12)
    ) +
    annotate(
        geom = "text", x = 2140, y = 58,
        label = "Everest", color = "white"
        
    ) +
    annotate(
        geom = "text", x = 320, y = 30,
        label = "Annapurna 1", color = "white"
    ) +
    guides(
        fill = guide_legend(override.aes = list(size = 5))
    )

ggsave(plot = himalayans_plot, "../images/himalayans_plot.png", width = 9, height = 7, dpi = 96)

showtext_auto(FALSE)
