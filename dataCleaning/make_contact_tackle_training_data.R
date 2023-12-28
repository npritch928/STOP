library(dplyr)
library(readr)
library(furrr)
library(stringr)
library(purrr)
library(lubridate)
source("./dataCleaning/helpers.R")
# Need to be sure to have run "label_tracking.R" first or have contact_track_week_*.csv available
directory <- "./nfl-big-data-bowl-2024/" #This is the directory where the data stored
#There are a lot of players with missing birthdates
Players <- read_csv("nfl-big-data-bowl-2024/players.csv",
  col_select = c("nflId", "weight", "position"))

Sides <- read_csv("nfl-big-data-bowl-2024/games.csv",
                  col_select = c("gameId", "homeTeamAbbr"))


Speeds <- map_df(1:9, ~read_csv(paste0(directory, "tracking_week_", .,".csv"), 
                                col_select = c("nflId", "s"))) %>%
  group_by(nflId) %>%
  summarise(max_speed = max(s))

# This function filters to all the players that make contact and computes the 
# desired information for the tackling model
tackleData <- function(week, direct = directory, players = Players, speeds = Speeds, ct = T, run = F){
  return(
     test = read_csv(paste0(data_dir, "contact_track_week_",ifelse(run,"run_",""), week,".csv"), 
       col_select = c("gameId", "playId", "nflId", "frameId", "time", "timeRemaining",
                      "defendersInTheBox", "offenseFormation", "down", "rel_direction",
                      "CarrierId", "dist_f", "x", "y", "s", "a", "dir", "contactDist", 
                      "contact", "contactFrame", "tackle", "assist", "forcedFumble", 
                      "pff_missedTackle", "yardsGained", "yardsRemaining",
                      "airYards", "Dist2side", "Dist2endZ", "Dist2LoS", "Dist2FD",str_c("bc_",1:11))) %>% 
       # Remove non contact frames
       filter(contactFrame == 1 | contactFrame == ifelse(ct, 1, 0)) %>% 
       group_by(gameId, playId, frameId, contactFrame) %>%
       # Record players close enough for contact and note closest player
       mutate(TeamInContact = sum(!is.na(contactDist)),
              ClosestDist = custom_min(contactDist, na.rm = T),
              ClosestDist = if_else(is.infinite(ClosestDist), NA, ClosestDist)
              ) %>%
       ungroup() %>%
       left_join(Players, by = "nflId") %>%
       left_join(Players %>% rename(Bc_weight=weight), by = c("CarrierId" = "nflId")) %>%
       left_join(speeds, by = "nflId") %>%
       mutate(pMaxSpeed = s/max_speed) %>%
       #rename(YardsToGo = yardsToGo.x) %>%
       group_by(gameId, playId) %>% 
       mutate(mf = min(frameId)) %>%
       ungroup() %>%
       filter(frameId == mf) %>%
       select(-mf) 
  )
}

yardsGainedData <- function(week, direct = directory, players = Players, speeds = Speeds, run = F){
  yGained = read_csv(paste0(data_dir, "contact_track_week_",ifelse(run,"run_",""), week,".csv"),
                     col_select = c("gameId", "playId", "nflId", "CarrierId", "yardsGained", "rel_direction",
                     "airYards")) %>%
                    filter(nflId != CarrierId) %>%
                    select(gameId, playId, yardsGained, airYards) %>%
    unique()
  return(
    test = read_csv(paste0(data_dir, "contact_track_week_",ifelse(run,"run_",""), week,".csv"), 
                    col_select = c("gameId", "playId", "nflId", "frameId", "time", "timeRemaining",
                                   "defendersInTheBox", "offenseFormation", "down", "yardsRemaining",
                                   "CarrierId", "x", "y", "s", "a", "dir", "contactDist", "rel_direction",
                                   "contact", "contactFrame", "Dist2side", "Dist2endZ", "Dist2LoS", "Dist2FD",str_c("bc_",1:11))) %>% 
      # Remove frames where no contact occurs
      filter(nflId == CarrierId) %>% 
      ungroup() %>%
      left_join(yGained) %>%
      left_join(Players, by = "nflId") %>%
      left_join(Players %>% rename(Bc_weight=weight), by = c("CarrierId" = "nflId")) %>%
      left_join(speeds, by = "nflId") %>%
      mutate(pMaxSpeed = s/max_speed) %>%
      #rename(YardsToGo = yardsToGo.x) %>%
      group_by(gameId, playId) %>% 
      mutate(mf = min(frameId)) %>%
      ungroup() %>%
      filter(frameId == mf) %>%
      select(-mf) 
  )
}

# This is a function that returns the relevant contact data
contactData <- function(week, direct = directory, players = Players, speeds = Speeds, run = F){
  return(
    read_csv(paste0(data_dir, "contact_track_week_",ifelse(run,"run_",""), week,".csv"), 
      col_select = c("gameId", "playId", "nflId", "frameId", "time", "CarrierId", "timeRemaining",
                     "defendersInTheBox", "offenseFormation", "down",
                     "dist_f", "x", "y", "s", "a","dir", "contactDist", "contact",
                     "contactFrame", "yardsGained", "yardsRemaining", "rel_direction",
                     "airYards", "Dist2side", "Dist2endZ", "Dist2LoS", "Dist2FD", str_c("bc_",1:11))) %>% 
      group_by(gameId, playId, frameId, contactFrame) %>%
      # Record players close enough for contact and note closest player
      mutate(TeamInContact = sum(!is.na(contactDist)),
             ClosestDist = custom_min(contactDist, na.rm = T),
             ClosestDist = if_else(is.infinite(ClosestDist), NA, ClosestDist)
             ) %>%
      ungroup() %>%
      left_join(Players, by = "nflId") %>%
      left_join(Players %>% rename(Bc_weight=weight), by = c("CarrierId" = "nflId")) %>%
      left_join(speeds, by = "nflId") %>%
      mutate(pMaxSpeed = s/max_speed) %>%
      #rename(yardsToGo = yardsToGo.x) %>%
      group_by(gameId, playId) %>% 
      mutate(mf = min(frameId)) %>%
      ungroup() %>%
      filter(frameId == mf) %>%
      select(-mf) 
  )
}

#plan(multisession(workers = 9))
future_map_dfr(1:9, tackleData) %>% write_csv("nfl-big-data-bowl-2024/xYards_info.csv")
future_map_dfr(1:9, contactData) %>% write_csv("nfl-big-data-bowl-2024/contact_info.csv")
future_map_dfr(1:9, tackleData, ct=F) %>% write_csv("nfl-big-data-bowl-2024/xYards_info_full.csv")
future_map_dfr(1:9, yardsGainedData)%>% write_csv("nfl-big-data-bowl-2024/YardsGained.csv")
#Get Running versions of same 
future_map_dfr(1:9, tackleData, run = T) %>% write_csv("nfl-big-data-bowl-2024/xYards_info_run.csv")
future_map_dfr(1:9, contactData, run = T) %>% write_csv("nfl-big-data-bowl-2024/contact_info_run.csv")
future_map_dfr(1:9, tackleData, ct=F, run = T) %>% write_csv("nfl-big-data-bowl-2024/xYards_info_full_run.csv")
future_map_dfr(1:9, yardsGainedData, run = T)%>% write_csv("nfl-big-data-bowl-2024/YardsGained_run.csv")
