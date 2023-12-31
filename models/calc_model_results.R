
# Model results
# script to calculate SCOPE and STOP from probability models
# NOTE: update data_dir with file path to data
#       outputs from this script are saved in data_dir

# packages
library(dplyr)

data_dir = "../data/"
model_dir = "../data/"

# load models and data
load(paste0(model_dir, "pass_contact_model.RData"))
load(paste0(model_dir, "run_contact_model.RData"))
load(paste0(data_dir, "pass_tackle_pred.RData"))
load(paste0(data_dir, "run_tackle_pred.RData"))
pass_contact_info = read.csv(paste0(data_dir, "contact_info.csv")) %>% select(gameId, playId, nflId, contact)
run_contact_info = read.csv(paste0(data_dir, "contact_info_run.csv")) %>% select(gameId, playId, nflId, contact)
players = read.csv(paste0(data_dir, "players.csv")) %>% select(nflId, position, displayName,)
plays = read.csv(paste0(data_dir, "plays.csv")) %>% select(gameId, playId, defensiveTeam)
tackles = read.csv(paste0(data_dir, "tackles.csv")) %>% select(gameId, playId, nflId, tackle)

# extract contact probabilities
pass_contact_prob = pass_contact_model$prob.train.mean
run_contact_prob = run_contact_model$prob.train.mean

# combine data
pass_contact_info$contact_prob = pass_contact_prob[,1]
pass_contact_info$tackle_prob = pass_tackle_pred
pass_contact_info$type = "pass"

run_contact_info$contact_prob = run_contact_prob[,1]
run_contact_info$tackle_prob = run_tackle_pred
run_contact_info$type = "run"

all_plays = bind_rows(pass_contact_info, run_contact_info) %>%
  left_join(tackles, by = join_by(gameId, playId, nflId)) %>%
  replace(is.na(.), 0) %>%
  mutate(xtackles = contact_prob * tackle_prob)

# save play results
write.csv(all_plays, file = paste0(data_dir, "all_plays.csv"))

# summarize results by player
# we are only interested in passing plays
pass_player_summary = all_plays %>%
  filter(type == "pass") %>%
  group_by(nflId) %>%
  summarize(
    n_plays = n(),
    tot_tackles = sum(tackle),
    tot_contact = sum(contact),
    tot_xcontacts = sum(contact_prob),
    tot_xtackles = sum(xtackles)
  ) %>%
  left_join(players, by = join_by("nflId")) %>%
  mutate(
    STOP = tot_tackles / tot_xtackles,
    SCOPE = tot_contact / tot_xcontacts,
    contacts_above_expected = tot_contact - tot_xcontacts,
    tackles_above_expected = tot_tackles - tot_xtackles,
    pos_group = case_when(
      position %in% c("CB", "FS", "SS", "DB") ~ "DB",
      position %in% c("MLB", "OLB", "ILB") ~ "LB",
      position %in% c("NT", "DT", "DE") ~ "DL",
      TRUE ~ "error"
    )
  ) %>%
  arrange(desc(STOP))

write.csv(pass_player_summary, file = paste0(data_dir, "pass_player_summary.csv"))

# summarize results by team
pass_team_summary = all_plays %>%
  filter(type == "pass") %>%
  left_join(plays, by = join_by(gameId, playId)) %>%
  group_by(defensiveTeam) %>%
  summarize(
    n_plays = n(),
    tot_tackles = sum(tackle),
    tot_contact = sum(contact),
    tot_xcontacts = sum(contact_prob),
    tot_xtackles = sum(xtackles)
  ) %>%
  mutate(
    STOP = tot_tackles / tot_xtackles,
    SCOPE = tot_contact / tot_xcontacts,
    contacts_above_expected = tot_contact - tot_xcontacts,
    tackles_above_expected = tot_tackles - tot_xtackles
  ) %>%
  arrange(desc(STOP))

write.csv(pass_team_summary, file = paste0(data_dir, "pass_team_summary.csv"))
