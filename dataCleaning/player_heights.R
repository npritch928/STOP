
# player heights are stored as characters
# this script will convert them to inches

data_dir = "nfl-big-data-bowl-2024/"
players = read.csv(file = paste0(data_dir, "players.csv"))

library(dplyr)
library(stringr)

heights = str_split_fixed(players$height, "\\-", 2) %>% 
  as.data.frame() %>%
  mutate(
    V1 = as.integer(V1),
    V2 = as.integer(V2),
    height_in = (V1 * 12 + V2),
    height_yards = round(height_in / 36, 2)
  ) %>%
  select(height_in, height_yards)

player_heights = bind_cols(players, heights)

write.csv(player_heights, file = paste0(data_dir, "player_heights.csv"))
