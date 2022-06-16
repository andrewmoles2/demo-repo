library(tidyverse)
# https://github.com/jimjam-slam/ggflags
#remotes::install_github("https://github.com/jimjam-slam/ggflags")
library(ggflags)
library(ggimage)
library(MetBrewer)
library(ggthemes)

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
eurovision_votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

# reviewing data ---
glimpse(eurovision)
summary(eurovision)

# looking at countries with the best win ratio ---
# make winner into int for aggregation
eurovision$winner_int <- as.integer(eurovision$winner)
# now filter for final and grand-final, agg by country
unique(eurovision$section)
# calculate avg points and rank, and total wins and year
country_agg <- eurovision %>%
  filter(section == "final" | section == "grand-final") %>%
  group_by(artist_country) %>%
  summarise(avg_points = mean(total_points, na.rm = TRUE),
            avg_rank = mean(rank, na.rm = TRUE),
            tot_wins = sum(winner_int, na.rm = TRUE),
            tot_years = n()) %>%
  arrange(desc(tot_wins)) %>%
  # calc win ratio (tot_wins/tot_years)
  mutate(win_ratio = (tot_wins/tot_years)*100)

country_agg

# plot only winners of 3 eurovisions or more
top_countries <- country_agg %>%
  filter(tot_wins >= 3) %>%
  ggplot(aes(x = avg_points, y = win_ratio, colour = artist_country)) +
  geom_point() +
  scale_colour_manual(values = met.brewer("Archambault", 11)) +
  labs(title = "Top Eurovision countries!",
       subtitle = "Only winners with 3 or more wins included",
       x = "Average points per competition",
       y = "Win ratio (wins/number years competing)",
       color = "Country") +
  annotate("text", x = 175, y = 14.5, hjust = 'right',
           label = "Ukraine are Eurovision\n superstars with\n 3 wins in 17 years") +
annotate('segment', x = 175, y = 15, xend = 197, yend = 17.5,
         arrow = arrow(length = unit(2, "mm"))) +
  ggthemes::theme_clean()

top_countries

ggsave(filename = "top_countries.png", top_countries,
       width = 3508, height = 3508, units = "px", dpi = 500)

# make visualisation with flags and winners ----
# extract two digit flag code to use with ggflags
flags <- eurovision$country_emoji
str_remove(flags, ":flag_") |> str_remove(":") -> flag_emoji
eurovision$flag_emoji <- flag_emoji

# testing out ggflags
eurovision %>%
  filter(year >= 2018) %>%
  filter(year != 2020) %>%
  filter(section == "grand-final") %>%
  ggplot(aes(x = rank, y = total_points, country = flag_emoji)) +
  geom_flag(show.legend = FALSE) +
  facet_wrap(vars(year))

# Look at winners by year, with points on the y. 
# bit of cleaning required
# filter for just finals, winners, and year over 1970
winners <- eurovision %>%
  filter(section == "final" | section == "grand-final") %>%
  filter(winner == TRUE) %>%
  filter(year >= 1970) %>%
  filter(nchar(flag_emoji) <= 2) %>% # deal with any with no flags
  # make text for plot
  mutate(winner_text = paste0(artist, " (", total_points, " Points)"))

# plot with flags, and text with artist and points
ggplot(winners, aes(year, total_points, country = flag_emoji)) +
  geom_bar(stat = "identity", width = 0.05) +
  geom_flag() +
  geom_text(aes(label = winner_text),
            hjust = -0.1, vjust = 0.1, angle = 90,
            family = "Avenir") +
  scale_y_continuous(limits = c(0, 1200)) +
  labs(x = "Year", y = "Points",
       title = "Eurovision winners: 1970 - 2022") +
  theme_minimal(base_family = "Avenir")

# same but flipped
png(filename = "../MISDI/eurovision_winners.png", 
    width = 3508, height = 3508, res = 320)
ggplot(winners, aes(year, total_points, country = flag_emoji)) +
  geom_bar(stat = "identity", width = 0.05) +
  geom_flag() +
  coord_flip() +
  geom_text(aes(label = winner_text),
            hjust = -0.1, vjust = 0.15, angle = 0,
            family = "Avenir", size = 3) +
  scale_y_continuous(limits = c(0, 1200)) +
  labs(x = "Year", y = "Points",
       title = "Eurovision winners: 1970 - 2022") +
  theme_minimal(base_family = "Avenir") +
  # add annotation information
  annotate(geom = "text", x = 1989, y = 1000, size = 3,
           label = "1989 winners Yugoslavia\n removed due to no flag",
           family = "Avenir") +
  annotate(geom = "curve", x = 1989, y = 925, xend = 1989, yend = 300, 
           curvature = -0.2, colour = "forestgreen") +
  annotate(geom = "text", x = 2020, y = 1000, size = 3,
           label = "No competition in 2020\n due to COVID-19",
           family = "Avenir") +
  annotate(geom = "curve", x = 2020, y = 925, xend = 2020, yend = 500, 
           curvature = 0.05, colour = "forestgreen")
dev.off()


# which winners were also the hosts? ----
library(gghighlight)
winners %>%
  ggplot(aes(x = year, y = total_points, colour = host_country)) +
  geom_point() +
  gghighlight(host_country == artist_country)

