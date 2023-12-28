
# build the design matrix for a model to predict if a player will make a tackle 
# given that they are within tackling distance of the ball carrier

library(dplyr)

contact = read.csv(file = "data/contact_data/contact.csv")

# read in frames where the defender was within contact distance of the ball carrier
get_contact_data = function(filename){
  df = read.csv(file = filename) %>%
    filter(contact == 1)
  return(df)
}

# get frames where the defender came within contact distance of the ball carrier
contact_files = paste0("data/contact_data/contact_track_week_", 1:9, ".csv")
contact_data_list = lapply(X = contact_files, FUN = get_contact_data)
contact_data = bind_rows(contact_data_list)
# write.csv(contact_data, file = "data/contact_data/contact_frames.csv")

# combine frames of plays from the same defender into one dataset
# let's assume for now that instances where a player misses a tackle and then subsequently makes the tackle are negligible
# but this is an edge case we could revisit later
summarized_contact_data = contact_data %>%
  group_by(gameId, playId, nflId, displayName, jerseyNumber, club, CarrierId) %>%
  summarize(
    tackle = max(tackle),
    assist = max(assist),
    forcedFumble = max(forcedFumble),
    missed_tackle = max(pff_missedTackle),
    total_dis = sum(dis)
    ) %>%
  mutate(
    gold_stars = tackle + assist + forcedFumble,
    made_play = ifelse(gold_stars > 0, 1, 0)
  )

write.csv(summarized_contact_data, file = "data/tackle_design_mat.csv")
