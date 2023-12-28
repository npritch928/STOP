# This is the set of functions that will create labels on the tracking data.
# It requires that the contact, tackle, and distance datasets have already been made
library(readr)
library(dplyr)
library(furrr)
library(tidyr)
source("dataCleaning/helpers.R")
#Function that converts the data time into useable time data
getTime <- function(time){
  l = length(time)
  nums = str_split(time, ":",simplify = T)
  if(l > 1){ 
    return(as.numeric(nums[,1]) + as.numeric(nums[,2]) / 60)
  }else{
    return(as.numeric(nums[1]) + as.numeric(nums[2]) / 60)
  }
}
#Assume that the contact_week _*.csv files are in the nfl-big-data-bowl-2024 dataset
data_direct = "./nfl-big-data-bowl-2024/" #"../../data/raw/"   

tacklesTib <- read_csv(paste0(data_direct, "tackles.csv"))
contactTib <- read_csv(file = paste0(data_direct, "contact.csv"))
gamesTib <- read_csv(file = paste0(data_direct, "games.csv"), col_select = c(gameId, homeTeamAbbr))
playsTib <- read_csv(file = paste0(data_direct,"plays.csv")) %>% 
  #Remove screen passes
  filter(!is.na(passLength)) %>%
  select(gameId, playId, ballCarrierId) 


RunPlaysTib <- read_csv(file = paste0(data_direct,"plays.csv")) %>% 
  #Remove screen passes
  filter(is.na(passLength)) %>%
  select(gameId, playId, ballCarrierId) 

penaltyTib <- read_csv(file = paste0(data_direct,"plays.csv")) %>% 
  #penalties that do not affect tackle are not removed
  filter(!is.na(foulName1), 
         !(foulName1 %in% c("Taunting", "Unsportsmanlike Conduct", "Unnecessary Roughness", "Roughing the Passer"))
        ) %>%
  select(gameId, playId)

directions <- read_csv(file = paste0(data_dir, "tracking_week_",week,".csv")) %>%
  semi_join(gamesTib, by=c("gameId", c("club" = "homeTeamAbbr"))) %>% 
  group_by(gameId, playId) %>%
  filter(frameId == 1 | frameId == max(frameId) ) %>%
  group_by(gameId, playId,frameId) %>%
  summarise(mx = median(x))

#This block of code will get the line of scrimmage and first down line for each play
Lines <- read_csv("nfl-big-data-bowl-2024/plays.csv", 
                  col_select = c("prePenaltyPlayResult","gameId", "playId", "yardsToGo", "yardlineNumber", "absoluteYardlineNumber",
                                 "yardlineSide", "possessionTeam", "quarter", "down", "offenseFormation", "defendersInTheBox", "passLength", "playResult", "gameClock"))%>% 
  left_join(gamesTib) %>%
  #Assuming here that you score in the opposing endzone
   mutate(lineOfScrim = absoluteYardlineNumber,
          #Compute the time remaining in the game
          timeRemaining = 60 - ((quarter - 1) * (15 - getTime(gameClock)))
  ) %>%
  select(prePenaltyPlayResult,gameId, playId, lineOfScrim, yardsToGo, quarter, down, offenseFormation, defendersInTheBox, yardlineNumber, passLength, playResult, timeRemaining)
  

label_tackles <- function(week, frames2contact=10, 
                          tackles = tacklesTib, 
                          contact = contactTib, 
                          plays = playsTib,
                          penalty = penaltyTib,
                          games = gamesTib,
                          data_dir = data_direct,
                          lines = Lines,
                          run = F
                          ){
read_csv(file = paste0(data_dir, "tracking_week_",week,".csv")) %>%
    #Remove all the plays where a penalty occurs
    anti_join(penalty) %>% 
    #Remove all non - relevant plays (in run case non run plays)
    right_join(plays, by = c("gameId","playId")) %>%
    #Add team information
    left_join(games) %>%
    group_by(gameId, playId) %>%
    mutate(
           #figure out start of play by looking at when first of these events occur
           effective_start = custom_max(frameId[event %in% ifelse(run, c("handoff", "lateral","run","snap_direct"),
                                                                  c("pass_arrived","pass_outcome_caught"))], na.rm = T),
           #This may causse a warning for max of empty but it does not appear to matter
           startEvent = custom_min(event[frameId == effective_start]),
           #figure out end of play looking when last frame where these events occur
           effective_end = custom_max(frameId[event %in% c("qb_slide", 
                                                    "fumble_defense_recovered", 
                                                    "fumble_offense_recovered",
                                                    "out_of_bounds",
                                                    "tackle",
                                                    "qb_sack",
                                                    "touchdown",
                                                    "fumble"
                                                    )], na.rm = T),
           endEvent = custom_min(event[frameId == effective_end])
           ) %>%
    # cut frames to be between catch and end of play
    filter(frameId >= effective_start & frameId <= effective_end) %>%
    group_by(gameId, playId) %>%
    mutate(
           xStart = max(x[frameId == effective_start & nflId == ballCarrierId], na.rm = T),
           xEnd = max(x[frameId == effective_end & nflId == ballCarrierId], na.rm = T)
           ) %>% 
    group_by(gameId, playId, frameId) %>%
    #Get the x for each ball carrier
    mutate(xBc = max(x[nflId == ballCarrierId], na.rm = T),
           DirBc = max(dir[nflId == ballCarrierId], na.rm = T)) %>%
    #label the offensive team
    mutate(oTeam = max(club[nflId == ballCarrierId], na.rm = T)) %>% 
    filter((club != oTeam | nflId == ballCarrierId) & !is.na(nflId)) %>%
    select(-oTeam) %>%
    #add the field lines coordinates 
    left_join(lines, by = c("gameId","playId")) %>%
    group_by(gameId, playId, frameId, nflId) %>%
    #Distance to line will be projection onto nearest line
    mutate(Dist2side = Vmin(53.3 - y, y),
           Dist2endZ = Vmin(110 - x, x),
           #Determine where the first down line should be placed based on play direction
           firstDown = lineOfScrim + if_else(playDirection == "right", 1, -1) * yardsToGo,
           Dist2LoS = (x - lineOfScrim) * sign(x - lineOfScrim),
           Dist2FD = (x - firstDown) * sign(x - firstDown)) %>%
    #compute air yards as where the ball is caught mine ths location of the line of scrimmage
    #Yards remaining is how many yards gained after a chosen frame
    mutate(yardsRemaining = (xEnd - xBc) * if_else(playDirection == "right", 1, -1),
           #Air yards should have same sign as official pass length
           airYards = if_else(run, 0, (xStart - lineOfScrim) * if_else(playDirection == "right", 1, -1)),
           #If the direction is the same as air yards use the same sign otherwise flip the sign
           yardsGained = (xEnd - xStart) * if_else(playDirection == "right", 1, -1)) %>%
    select(-firstDown, -lineOfScrim) %>% 
    select(-xEnd, -xStart) %>%
    ungroup() %>%
    left_join(contact, by = c("gameId", "playId", "nflId", c("frameId" = "potContactFrame"))) %>%
    #Need to expand contact information to every row corresponding to a player in a game
    group_by(gameId, playId, nflId) %>%
    mutate(
      #Label frame where player is within epsilon distance
      contactFrame = case_when(
        !is.na(contactDist) ~ 1,
        TRUE ~ 0), 
      #Label if player will make contact
      contact = max(contactFrame),
      rel_direction = min(abs((180 - (DirBc - dir)) / 180), abs((DirBc - dir) / 180))
    ) %>%
    group_by(gameId, playId, nflId, contact) %>%
    mutate(
      #Label Frame where first contact occurs
      first_contact = case_when(contact == 1 ~ custom_min(frameId[contactFrame], na.rm = T), TRUE ~ NA),
      last_contact = case_when(contact == 1 ~ custom_max(frameId[contactFrame], na.rm = T), TRUE ~ NA)
    )%>%
    ungroup() %>%
    group_by(gameId, playId, nflId) %>%
    left_join(tackles, by = c("gameId","playId","nflId")) %>% 
    mutate(
          #replace NAs with zeros
          tackle = replace_na(tackle, 0), 
          assist = replace_na(assist, 0), 
          forcedFumble= replace_na(forcedFumble, 0), 
          pff_missedTackle = replace_na(pff_missedTackle, 0)
          ) %>%
    select(-effective_start, -effective_end, -first_contact, -last_contact) %>%
    #Join with the individual player distances
    left_join(read_csv(paste0(data_dir, "dist_week_",week,".csv"))) %>% 
    #Join with the ordered distances of all defenders on the field
    left_join(read_csv(paste0(data_dir, "dist_rankings_week_",week,".csv"))) %>%
    #Remove Ball Carriers
    filter(nflId != ballCarrierId) %>%
    write_csv(paste0(data_dir, "contact_track_week_",ifelse(run,"run_",""), week,".csv"))
}

#Run in parallel using 9 cores
plan(multisession(workers = 7))
future_map(1:9, label_tackles)
future_map(1:9, label_tackles,run = T, plays = RunPlaysTib) 

