library(tidyverse)
library(purrr)
library(gganimate)
playsTib <- read_csv(str_c("./nfl-big-data-bowl-2024/","plays.csv"))
gamesTib <- read_csv(str_c("./nfl-big-data-bowl-2024/","games.csv"))
TeamColor<-function(team){
  return(case_when(
    team == "ARI" ~ rgb(151,25,63, maxColorValue = 255),
    team == "ATL" ~ rgb(167,25,48, maxColorValue = 255),
    team == "BLT" ~ rgb(25,25,95, maxColorValue = 255),
    team == "BUF" ~ rgb(0,51,141, maxColorValue = 255),
    team == "CAR" ~ rgb(0,133,202, maxColorValue = 255),
    team == "CHI" ~ rgb(11,22,42, maxColorValue = 255),
    team == "CLV" ~ rgb(49,29,0, maxColorValue = 255),
    team == "DAL" ~ rgb(134,147,151, maxColorValue = 255),
    team == "DEN" ~ rgb(251,79,20, maxColorValue = 255),
    team == "DET" ~ rgb(0,48,40, maxColorValue = 255),
    team == "GB" ~ rgb(255,184,28, maxColorValue = 255),
    team == "HST" ~ rgb(3,32,47, maxColorValue = 255),
    team == "IND" ~ rgb(0,44,95, maxColorValue = 255),
    team == "JAX" ~ rgb(0,103,120, maxColorValue = 255),
    team == "KC" ~ rgb(227,24,55, maxColorValue = 255),
    team == "LV" ~ rgb(0,0,0, maxColorValue = 255),
    team == "LAC" ~ rgb(0,128,198, maxColorValue = 255),
    team == "LA" ~ rgb(255,209,0, maxColorValue = 255),
    team == "MIA" ~ rgb(0,142,151, maxColorValue = 255),
    team == "MIN" ~ rgb(79,38,131, maxColorValue = 255),
    team == "NE" ~ rgb(151,25,63, maxColorValue = 255),
    team == "NO" ~ rgb(211,188,141, maxColorValue = 255),
    team == "NYG" ~ rgb(1,35,82, maxColorValue = 255),
    team == "NYJ" ~ rgb(18,87,64, maxColorValue = 255),
    team == "PHI" ~ rgb(0,76,84, maxColorValue = 255),
    team == "PIT" ~ rgb(255,182,18, maxColorValue = 255),
    team == "SF" ~ rgb(170,0,0, maxColorValue = 255),
    team == "SEA" ~ rgb(105,190,40, maxColorValue = 255),
    team == "TB" ~ rgb(213,10,10, maxColorValue = 255),
    team == "TEN" ~ rgb(75,146,219, maxColorValue = 255),
    team == "WAS" ~ rgb(90,20,20, maxColorValue = 255),
  ))
}

PlotPlay <- function(data, playid, gameid){
  playgame <- data %>% filter(playId == playid, gameId == gameid)
  teams <- unique(playgame$club)
  homec <- TeamColor(teams[1])
  awayc <- "ivory2"
  colors <- c(homec, awayc, "brown")[order(teams)]
  title <- str_c(teams[1], " vs. ", teams[2], " play ",playid)
  p<-ggplot() + 
    geom_rect(aes(xmin = 10, xmax = 110, ymin = 0, ymax = 53.5), 
              fill = "forestgreen",color = "white",linewidth=1)+
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 53.5), 
              fill = homec,color = "white", alpha=.6)+
    geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.5), 
              fill = "lightblue", color = "white", alpha=.6)+
    geom_segment(aes(x=c(10,20,30,40,50,60,70,80,90,100,110),
                     xend=c(10,20,30,40,50,60,70,80,90,100,110), 
                     y=0, 
                     yend=53.5),
                 color = "white")+
    ggtitle(title)+
    theme_bw()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_blank())
  return(
    p+
      geom_point(data = playgame, aes(x=x, y=y, color=club))+
      scale_color_manual(values = colors)+
      transition_time(frameId) +
      ease_aes("linear")
  )
}


PlotPred <- function(data, playid, gameid){
  playgame <- data %>% filter(playId == playid, gameId == gameid)
  #Assume there is only one prediction column
  predictionCol <- colnames(playgame)[str_detect(colnames(playgame),"(?i).*prediction.*")]
  playgame <- playgame %>%
    rename(Prediction = predictionCol)
  teams <- unique(playgame$club)
  homec <- TeamColor(teams[1])
  awayc <- "ivory2"
  title <- str_c(teams[1], " vs. ", teams[2], " play ",playid)
  #Create teh field
  p<-ggplot() + 
    geom_rect(aes(xmin = 10, xmax = 110, ymin = 0, ymax = 53.5), 
              fill = "forestgreen",color = "white",linewidth=1)+
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 53.5), 
              fill = homec,color = "white", alpha=.6)+
    geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.5), 
              fill = "red", color = "white", alpha=.6)+
    geom_segment(aes(x=c(10,20,30,40,50,60,70,80,90,100,110),
                     xend=c(10,20,30,40,50,60,70,80,90,100,110), 
                     y=0, 
                     yend=53.5),
                 color = "white")+
    ggtitle(title)+
    theme_bw()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_blank())
  return(
    p+
      geom_point(data=subset(playgame, is.na(Prediction) & club != "football"), aes(x=x, y=y), col="red", size = 3) +
      geom_point(data=subset(playgame, is.na(Prediction) & club == "football"), aes(x=x, y=y), col="brown", size = 3, shape = 18) +
      geom_point(data = subset(playgame, !is.na(Prediction)), aes(x=x, y=y, color=Prediction), size = 3)+
      transition_time(frameId) +
      ease_aes("linear")
  )
}

#function to combine prediction with tracking data to be easily plotted
TrackPredictions <- function(prediction, tracking, playid, gameid) {
  predict <- prediction %>%
    select(gameId, playId, frameId, nflId, contains("prediction"))
  framesUsed <- predict %>% pull(frameId) %>% unique()
  track <- tracking %>%
    select(gameId, playId, frameId, nflId, club, x, y) %>%
    filter(playId == playid, gameId == gameid, frameId %in% framesUsed)
  if(nrow(tracking) == 0) stop("Make sure you have the tracking dataset from the correct week")
  return(
    left_join(track, predict)
  )
}

#Function that plots the predictions for a given play
PlotPredictions <- function(prediction, tracking, playid, gameid) {
  trackPred <- TrackPredictions(prediction, tracking, playid, gameid)
  return(PlotPred(trackPred, playid, gameid))
}
# Function to plot the gif for a particular play that highlights the tackler and ball carrier
PlotPlayers <- function(data, playid, gameid, playerid, bcid, plays = playsTib, games = gamesTib){
  play <- plays %>%
    filter(playId == playid, gameId == gameid)
  
  game <- games %>%
    filter(gameId == gameid)
  
  hometeam <- game$homeTeamAbbr[1]
  
  playgame <- data %>% filter(playId == playid, gameId == gameid) %>% 
    mutate(orders = case_when(club == "football" ~ 1,
                              club == hometeam ~ 2,
                              TRUE ~ 3
                              )
           ) %>%
    mutate(club = if_else(club == "football", "Football", club), 
           club = fct_reorder(club, orders))
  
  teams <- levels(playgame$club)
  # Get the players that will be highlighted
  playergame <- data %>% 
    filter(nflId %in% c(bcid,playerid), playId == playid, gameId == gameid) %>%
    mutate(orders = case_when(club == teams[2] ~ 1,
                              club == teams[3] ~ 2)) %>% 
      mutate(displayName = fct_reorder(displayName, orders))
  # Get the colors for the teams
  homec <- TeamColor(teams[2])
  awayc <- TeamColor(teams[3])
  colors <- c("#d97224", homec, awayc)
  title <- str_c(teams[3], " at ", teams[2], ": ", case_when(play$down == 1 ~ "1st", play$down == 2 ~ "2nd", play$down == 3 ~ "3rd", play$down == 4 ~ "4th"), 
                  " and ", play$yardsToGo, " in quarter ", play$quarter[1])
  p<-ggplot() + 
    #Build the field
    geom_rect(aes(xmin = 10, xmax = 110, ymin = 0, ymax = 53.5), 
              fill = "forestgreen",color = "white",linewidth=1, alpha = .9)+
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 53.5), 
              fill = homec,color = "white")+
    geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.5), 
              fill = awayc, color = "white")+
    geom_segment(aes(x=seq(10,110,5),
                     xend=seq(10,110,5), 
                     y=0, 
                     yend=53.5),
                 color = "white")+
    geom_segment(aes(x=seq(10,109,1),
                     xend=seq(10,109,1), 
                     y=0, 
                     yend=.5),
                 color = "white")+
    geom_segment(aes(x=seq(10,109,1),
                     xend=seq(10,109,1), 
                     y=53.5, 
                     yend=53),
                 color = "white")+
    geom_segment(aes(x=seq(10,109,1),
                     xend=seq(10,109,1), 
                     y=23.41667, 
                     yend=23.91667),
                 color = "white")+
    geom_segment(aes(x=seq(10,109,1),
                     xend=seq(10,109,1), 
                     y=30.5833, 
                     yend=30.0833),
                 color = "white")+
    annotate(geom="text", x = 60, y = c(2.5,51), label = "50", color = "white", size = 5)+
    annotate(geom="text", x = c(50,70,50,70), y = c(2.5,2.5,51,51), label = "40", color = "white", size = 5)+
    annotate(geom="text", x = c(40,80,40,80), y = c(2.5,2.5,51,51), label = "30", color = "white", size = 5)+
    annotate(geom="text", x = c(30,90,30,90), y = c(2.5,2.5,51,51), label = "20", color = "white", size = 5)+
    annotate(geom="text", x = c(20,100,20,100), y = c(2.5,2.5,51,51), label = "10", color = "white", size = 5)+
    annotate(geom="text", x = 5, y = 26.75, label = teams[2], color = "white", size = 10, angle = 90,  alpha = .8)+
    annotate(geom="text", x = 115, y = 26.75, label = teams[3], color = "white", size = 10, angle = 270, alpha = .8)+
    ggtitle(title, subtitle = play$playDescription[1])+
    theme_bw()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text=element_blank()) +
    #Plot the players
    geom_point(data = playgame, aes(x=x, y=y, color=club, group = nflId))+
    #Plot the tracking lines
    geom_line(data = playergame, aes(x=x, y=y, color=club))+
    #Plot the highlight points
    geom_point(data = playergame, aes(x=x, y=y, fill=displayName, group = nflId), size = 3, shape = 21)+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = colors[2:3])+
    transition_time(frameId) +
    transition_reveal(frameId)+ 
    ease_aes("linear")+
    labs(color = "Team:", fill = "Highlighted Player:")+ 
    theme(legend.position="bottom",
          legend.title=element_text(size=11,face="bold"))
  return(
    animate(p, height = 4.5,
            width = 7, units = "in", res = 150)
  )
}

#Make the zaven collins gif
PlotPlayers(dat,572, 2022092510,53445,43399)
