
# script to identify the frames where a defender comes within 
# a specified epsilon of the ball carrier.
# 
# we will specifically be interested in an epsilon proportional
# to the defender's height.
#
# the output will be used to build the P(contact) model and the
# P(tackle|contact) model.

library(dplyr)
library(purrr)
library(readr)

data_dir = "nfl-big-data-bowl-2024/"
players = read.csv(file = paste0(data_dir, "player_heights.csv"))
heights = players %>%
  select(nflId, height_yards)

#Assuming heights datasset has the nflId and the height in yards
get_contact_frames <- function(week, heights){
  distances = read.csv(file = paste0(data_dir, "dist_week_",week,".csv"))
  # assume Nathaniel has computed the distance data
  return(left_join(distances, heights, by = join_by(nflId == nflId)) %>%
    filter(dist_f <= height_yards, nflId != CarrierId) %>%
    select(gameId, playId, frameId, nflId, dist_f) %>%
    rename(contactDist = dist_f) %>%
      rename(potContactFrame = frameId) 
  )
}

contact_frames = map_df(1:9, get_contact_frames, heights)
write_csv(contact_frames, paste0(data_dir, "contact.csv"))
