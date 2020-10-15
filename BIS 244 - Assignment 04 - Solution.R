# Solution file for BIS 244 Assignment 4 Fall 2020

library(tidyverse)
library(scales)
Twitch <- read_csv("Most Popular Twitch Streamers.csv")
str(Twitch)
Twitch$Date <- as.Date(Twitch$Date, "%m/%d/%Y")
str(Twitch)


p <- ggplot(data = Twitch,
            mapping = aes(x = Date,
                          y = Viewers,
                          color = Streamer))
p + geom_line(mapping = aes(group = Streamer)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Date",
       y = "Viewers",
       title = "Most Popular Twitch Streamers")      


p + geom_line(mapping = aes(group = Streamer)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Date",
       y = "Viewers",
       title = "Most Popular Twitch Streamers") + 
  facet_wrap(~Streamer)
