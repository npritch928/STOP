library(progressr)

plays <- read_csv("./nfl-big-data-bowl-2024/plays.csv")

#compute distances from ball carrier
plan(multisession, workers = 16)
for(i in 1:9){
  print(i)
  in_filename <- str_c("./nfl-big-data-bowl-2024/tracking_week_",i,".csv")
  trk <- read_csv(in_filename)
  out_filename <- str_c("./nfl-big-data-bowl-2024/dist_week",i,".csv")
  write_csv(DistFullDataSet(trk, plays), out_filename, progress = TRUE) 
}

#Compute distances between all defensive players
plan(multisession, workers = 16)
for(i in 1:9){
  print(i)
  in_filename <- str_c("./nfl-big-data-bowl-2024/tracking_week_",i,".csv")
  trk <- read_csv(in_filename)
  out_filename <- str_c("./nfl-big-data-bowl-2024/dist_week_All_D",i,".csv")
  write_csv(DistFullDataSetAllDPlayers(trk, plays), out_filename, progress = TRUE) 
}

#Need to pivot wider for distances to ball carrier
#For distances need to first extract plays that are used

pivotDistWiderBC <- function(week){
  in_filename <- str_c("./nfl-big-data-bowl-2024/dist_week_",week,".csv")
  out_filename <- str_c("./nfl-big-data-bowl-2024/dist_rankings_week_",week,".csv")
  
  Usable_entries <- read_csv(str_c("./nfl-big-data-bowl-2024/tracking_week_",week,".csv")) %>% 
    left_join(read_csv(in_filename))%>% 
    group_by(gameId, playId) %>%
    #label the offensive team
    mutate(oTeam = max(club[nflId == CarrierId], na.rm = T)) %>% 
    filter(club != oTeam & !is.na(nflId)) %>%
    select(-oTeam) %>%
    ungroup() %>%
    select(gameId, playId, nflId, frameId) 
  
  read_csv(in_filename) %>%
    semi_join(Usable_entries, by = c("gameId", "playId", "nflId")) %>%
    group_by(gameId, playId, frameId, CarrierId) %>%
    mutate(rank = rank(dist_f, ties.method = "first"), rank = str_c("bc_", rank)) %>%
    select(-nflId, - CarrierId) %>%
    unique() %>%
    ungroup() %>%
    pivot_wider(id_cols = c("gameId", "playId", "frameId"), 
                names_from = rank, values_from = dist_f) %>%
    write_csv(out_filename, progress = T)
}
future_map(1:9, pivotDistWiderBC)

#Need to pivot wider for distances to defenders
pivotDistWider <- function(week){
  in_filename <- str_c("./nfl-big-data-bowl-2024/dist_week_All_D",week,".csv")

   read_csv(in_filename) %>%
      group_by(gameId, playId, frameId, CarrierId) %>%
      mutate(rank = rank(dist_f, ties.method = "first"), rank = str_c("c_", rank)) %>%
      select(-nflId) %>%
      unique() %>%
      pivot_wider(id_cols = c("gameId", "playId", "frameId", "CarrierId"), 
                  names_from = rank, values_from = dist_f) %>%
      rename(nflId = CarrierId) %>%
      write_csv(in_filename, progress = T)
}
future_map(1:9, pivotDistWider)
