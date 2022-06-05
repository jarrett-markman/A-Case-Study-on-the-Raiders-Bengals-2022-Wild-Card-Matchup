# let's look at the data for the Raiders - Bengals playoff game in which the Raiders spiked the ball on first down with about 30 seconds left
library(nflfastR)
nflreadr::.clear_cache()
library(tidyverse)
library(ggrepel)
library(ggimage)
library(gt)
library(ggpubr)
data <- load_pbp(2021) %>%
  filter(season_type == "POST") %>%
  filter(posteam == "LV") %>%
  filter(defteam == "CIN") %>%
  filter(qtr == 4)
data <- data %>%
  separate(time, c('minute', 'second'), ':') %>%
  mutate(minute = as.numeric(minute),
         second = as.numeric(second))
data <- data %>%
  mutate(seconds_in_minutes = minute * 60)
data <- data %>%
  mutate(seconds_remaining = seconds_in_minutes + second)
data <- data %>%
  filter(seconds_remaining < 60)
names(data)
data %>%
  filter(seconds_remaining < 47 & seconds_remaining > 26) %>%
  select(posteam, defteam, score_differential, seconds_remaining, down, yrdln, ep, epa, wp, wpa, td_prob, no_score_prob) %>%
  arrange(-seconds_remaining) %>%
  head()
data <- data %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
data %>%
  filter(seconds_remaining < 47 & seconds_remaining > 26) %>%
  ggplot(aes(x=-seconds_remaining, y=wp)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Win Probability",
       title = "Time and Win Probability on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(seconds_remaining < 47 & seconds_remaining > 26) %>%
  ggplot(aes(x=-seconds_remaining, y=ep)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Expected Points",
       title = "Time and Expected Points on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(seconds_remaining < 47 & seconds_remaining > 26) %>%
  ggplot(aes(x=-seconds_remaining, y=no_score_prob)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Probability the Raiders do not Score",
       title = "No-Score Probability and the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")
data %>%
  filter(seconds_remaining < 47 & seconds_remaining > 26) %>%
  ggplot(aes(x=-seconds_remaining, y=td_prob)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_line() +
  labs(x = "Seconds Remaining",
       y = "Touchdown Probability",
       title = "Touchdown Probability and Time on the Three Plays",
       caption = "Jarrett Markman | Sports Analytics")