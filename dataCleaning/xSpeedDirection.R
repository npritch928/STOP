library(tidyverse)
library(purrr)
data_dir <- "./nfl-big-data-bowl-2024/"
playsTib <- read_csv(file = paste0(data_dir,"plays.csv")) %>% select(gameId, playId, ballCarrierId)

XspeedDir <- function(dir, speed){
  #Convert the direction into radians
  RadDir <- 2 * pi * dir / 360 
  #Speed in the x direction is speed time cos(dir)
  return(cos(RadDir) * speed)
}

AddXspeedDir <- function(week, data_directory = data_dir, plays = playsTib){
  return(
    read_csv(paste0(data_directory, "tracking_week_",week,".csv"), col_select = 
             c("gameId", "playId", "frameId", "nflId", "s", "dir")) %>%
      inner_join(playsTib, by=c("gameId", "playId", c("nflId" = "ballCarrierId"))) %>%
      mutate(BC_xSpeed = XspeedDir(dir, s)) %>%
      select(-s, -dir, -nflId)
  )
}

#Get all the ball carrier ids
ball_carrier_speeds <- map_df(1:9, AddXspeedDir)

read_csv(paste0(data_dir,  "xYards_info_full.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "xYards_info_full.csv"))

read_csv(paste0(data_dir,  "xYards_info.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "xYards_info.csv"))

read_csv(paste0(data_dir,  "YardsGained.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "YardsGained.csv"))

read_csv(paste0(data_dir,  "contact_info.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>%
  left_join(ball_carrier_speeds) %>% 
  write_csv(paste0(data_dir,  "contact_info.csv"))

#Do same for run data
#Add the speed columns to contact data
read_csv(paste0(data_dir,  "contact_info_run.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>%
  left_join(ball_carrier_speeds) %>% 
  write_csv(paste0(data_dir,  "contact_info_run.csv"))

#Add the speed columns to tackle data
read_csv(paste0(data_dir,  "xYards_info_run.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "xYards_info_run.csv"))

read_csv(paste0(data_dir,  "xYards_info_full_run.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "xYards_info_full_run.csv"))

read_csv(paste0(data_dir,  "YardsGained_run.csv")) %>%
  mutate(xSpeed = XspeedDir(dir, s)) %>% 
  left_join(ball_carrier_speeds, by = c("gameId", "playId", "frameId")) %>% 
  write_csv(paste0(data_dir,  "YardsGained_run.csv"))

