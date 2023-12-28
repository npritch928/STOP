library(furrr)
library(tidyverse)
# Function to compute the standard euclidean distance returns the distance in yards
Distancefrom <- function(xOther, yOther, 
                         xCarrier, yCarrier, 
                         aOther = 0, vOther = 0,
                         aCarrier = 0, vCarrier = 0){
  dist <- sqrt((xOther - xCarrier)^2 + (yOther - yCarrier)^2)
  return(dist)
}
# Function to compute distance of one frame and allows for the input of any
# distance function that has the same inputs as Distancefrom function
GetDistFrom <- function(Data, CarrierId, GameId, FrameId, Play, distance = Distancefrom){
  frame_data <- Data %>%
    filter( gameId == GameId, playId == Play, frameId == FrameId)%>%
    select(gameId, playId, frameId, nflId, x, y, a, s) %>%
    drop_na() #Remove ball location
  Carrier_location <- frame_data %>% 
    filter(nflId == CarrierId)
  xCarrier <- Carrier_location[[5]]
  yCarrier <- Carrier_location[[6]]
  aCarrier <- Carrier_location[[7]]
  vCarrier <- Carrier_location[[8]]
  return(
    frame_data %>%
      mutate(CarrierId = CarrierId, dist_f = do.call(distance, list(x, y, 
                                             xCarrier, 
                                             yCarrier,
                                             a, 
                                             s,
                                             aCarrier,
                                             vCarrier))) %>%
      select(gameId, playId, frameId, nflId, CarrierId, dist_f)
  )
}
# Computes the distance for all frames in a play for a single game
# baseInfo contains the playId, maxframe, and CarrierId
DistAllframes <- function(data_track, GameId, baseInfo, distance = Distancefrom){
  CarrierId <- baseInfo$ballCarrierId
  PlayId <- baseInfo$playId
  FrameIds <- 1:baseInfo$maxframe
  map_df(FrameIds, 
         ~GetDistFrom(data_track, 
                      CarrierId, 
                      GameId, 
                      ., 
                      PlayId, 
                      distance=distance)
         )
}
# Computes the distance for all frames in a single game
# Needs the input of the tracking data and the play data as well as the gameId
DistAllplays <- function(data_track, data_play, GameId, distance = Distancefrom){
  # Find the ball carrier for each 
  ballCarrier <- data_play %>%
    filter(gameId == GameId) %>%
    select(playId, ballCarrierId)
  return( 
    data_track %>%
      filter(gameId == GameId) %>%
      group_by(playId) %>%
      summarise(maxframe = max(frameId, na.rm = F)) %>%
      left_join(ballCarrier) %>% 
      rowwise() %>% #Makes each row be treated as a group
      group_map(~DistAllframes(data_track, 
                               GameId, 
                               ., 
                               distance=distance
                               )
                ) %>%
      bind_rows()
  )
}
#Function that finds the distances for an entire dataset
#NEEDS INPUT OF TRACKING DATA AND PLAY DATA
DistFullDataSet <- function(data_track, data_play, distance = Distancefrom){
  games <- data_track %>% pull(gameId) %>% unique()
  return(
    future_map_dfr(games, 
           ~DistAllplays(data_track, 
                         data_play, 
                         ., 
                         distance=distance
                         ),
           progress = T
    )
  )
}

# Computes the distance for all frames in a play for a single game
# baseInfo contains the playId, maxframe, and CarrierId
DistAllDPlayersFrames <- function(data_track, GameId, baseInfo, distance = Distancefrom){
  CarrierId <- baseInfo$ballCarrierId
  PlayId <- baseInfo$playId
  FrameIds <- 1:baseInfo$maxframe
  map_df(FrameIds, 
         ~GetDistFrom(data_track, 
                      CarrierId, 
                      GameId, 
                      ., 
                      PlayId, 
                      distance=distance)
  )
}
# Computes the distance for all frames in a single game for all defensive players
# Needs the input of the tracking data and the play data as well as the gameId
DistAllDplayers <- function(data_track, data_play, GameId, distance = Distancefrom){
  # Find the ball carrier for each 
  ballCarrier <- data_play %>%
    filter(gameId == GameId) %>%
    select(gameId, playId, defensiveTeam) %>%
    left_join(data_track %>% 
                select(gameId, playId, nflId, club) %>%
                unique(), 
              by = c("gameId", "playId", c("defensiveTeam" = "club"))) %>%
    rename(ballCarrierId = nflId)
  
  data_track <- data_track %>%
    filter(gameId == GameId) %>%
    semi_join(ballCarrier, by = c("gameId","playId", c("club" = "defensiveTeam")))
  
  return( 
  te = data_track %>%
      group_by(playId) %>%
      summarise(maxframe = max(frameId)) %>%
      right_join(ballCarrier, by = "playId") %>%
      group_by(playId, ballCarrierId) %>%
      drop_na() %>%
      rowwise() %>%  #Makes each row be treated as a group
      group_map(~DistAllDPlayersFrames(data_track, 
                               GameId, 
                               ., 
                               distance=distance
      )
      ) %>%
      bind_rows()
  )
}
#Function that finds the distances for an entire dataset
#NEEDS INPUT OF TRACKING DATA AND PLAY DATA
DistFullDataSetAllDPlayers <- function(data_track, data_play, distance = Distancefrom){
  games <- data_track %>% pull(gameId) %>% unique()
  return(
    future_map_dfr(games, 
                   ~DistAllDplayers(data_track, 
                                 data_play, 
                                 ., 
                                 distance=distance
                   ),
                   progress = T
    )
  )
}
