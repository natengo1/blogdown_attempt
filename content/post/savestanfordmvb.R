library(tidyverse)

data0 <- read_csv("mnthistoricalroster.csv")

player <- data0 %>%
  group_by(name,city,state,college,year) %>%
  summarise(.groups = "drop")

college <- player %>%
  mutate(n_years = length(unique(year))) %>%
  group_by(college) %>%
  mutate(avg = n()/n_years) %>%
  group_by(college,year,avg) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  mutate(col = ifelse(college == "Stanford","Stanford","other"))

ggplot(college,
       aes(y = fct_reorder(college,avg),
           x = factor(year),
           size = n)) +
  geom_point(aes(color = col)) +
  theme_bw() +
  scale_color_manual(values = c("other" = "#b6b1a9",
                                "Stanford" = "#8c1515")) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16)) +
  guides(color = FALSE) +
  labs(size = "Number of Men's National Team Athletes",
       title = "USA Volleyball Men's Indoor National Team Roster by College",
       subtitle = "Which colleges are the USA Men's National Team athletes coming from?",
       caption = "Created by Nate Ngo @natengo1")

