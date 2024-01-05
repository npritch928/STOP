library(tidyverse)
library(htmltools)
library(reactable)
library(reactablefmtr)
library(webshot)
library(htmlwidgets)
pos <- "LB"
min_tot_tackle <- 5
min_tot_plays <- 5
mkTable <- function(pos, min_tot_tack = min_tot_tackle, min_tot_play = min_tot_plays){
  df <- read_csv("nfl-big-data-bowl-2024/pass_player_summary.csv") %>%
    rename(SCOP = SCOPE) %>%
    filter(pos_group == pos,
         tot_xtackles >= min_tot_tack,
         n_plays >= min_tot_play)
    #Get color range for STOP
    STOP_normalized <- (df$STOP - min(df$STOP)) / (max(df$STOP) - min(df$STOP))
    STOP_colors <-  rgb(colorRamp(c("#FAFAFA", "#CC4C02"))(STOP_normalized), maxColorValue = 255)
    #Get color range for SCOP
    SCOP_normalized <- (df$SCOP - min(df$SCOP)) / (max(df$SCOP) - min(df$SCOP))
    SCOP_colors <-  rgb(colorRamp(c("#F2F8FF", "#762A83"))(SCOP_normalized), maxColorValue = 255)
    #Create Table
    df  %>%
      select(displayName, position, tot_tackles, STOP, SCOP) %>%
      rename(Name = displayName,
           Position = position,
           Tackles = tot_tackles) %>%
      mutate(STOP = round(STOP,2),
           SCOP = round(SCOP,2)) %>%
      arrange(desc(STOP)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, everything()) %>%
      #head(10) %>%
      reactable(
        columns = list(
        STOP = colDef(
          width = 60,
          align = "center",
          style = JS("function(rowInfo, column, state) {
          const { showColors, stopColors, stopNorm } = state.meta
          if (showColors) {
            if (stopNorm[rowInfo.index] > .75){
              return { color: '#FFFFFF', backgroundColor: stopColors[rowInfo.index] }
            } else {
              return { backgroundColor: stopColors[rowInfo.index] }
            }
          }
        }")
        ),
        SCOP = colDef(
          width = 65,
          name = "SCOPE",
          align = "center",
          style = JS("function(rowInfo, column, state) {
          const { showColors, scopColors, scopNorm } = state.meta
          if (showColors) {
            if (scopNorm[rowInfo.index] > .6){
              return { color: '#FFFFFF', backgroundColor: scopColors[rowInfo.index] }
            } else {
              return { backgroundColor: scopColors[rowInfo.index] }
            }
          }
        }"),
          cell = function(value, index) {
            div(
              div(style = list(fontWeight = 200), value)
            )
          }
        ),
        Name = colDef(width = 170,
                    align = "left",
                    cell = function(value, index) {
                      div(
                        div(style = list(fontWeight = 200), value)
                      )
                    }),
        Tackles = colDef(width = 70,
                       align = "center",
                    cell = function(value, index) {
                      div(
                        div(style = list(fontWeight = 200), value)
                      )
                    }),
        Position = colDef(align = "left")
      ),
      meta = list(
        stopColors = STOP_colors,
        stopNorm = STOP_normalized,
        scopColors = SCOP_colors,
        scopNorm = SCOP_normalized,
        showColors = TRUE
      ),
      elementId = "Stop",
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "4px 4px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      ),
    defaultColDef = colDef(minWidth = 7, maxWidth = 70)
  ) %>%
  add_title(title = html(str_c("<p style=\"font-family:-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif\">", "Top 10 Scores for ", pos, "s",
                               "<p>")),
            align = "center",
            font_size = 30,
            margin = margin(0, 0, -25, 0)) %>%
  add_subtitle(subtitle = html(str_c("<h style=\"font-family:-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif\">", "(Minimum ",min_tot_tack, " expected tackles)",
                                     "<h>")),
               font_size = 18,
               align = "center",
               margin = margin(0, 0, 15, 0),
               font_weight = "normal")
}
lb <- mkTable("LB", min_tot_tack = 5)
html <- "lbSTOP.html"
htmlwidgets::saveWidget(lb, html)
webshot(html, "lbSTOP.png")

db <- mkTable("DB", min_tot_tack = 5)
html <- "dbSTOP.html"
htmlwidgets::saveWidget(db, html)
webshot(html, "dbSTOP.png")

dl <- mkTable("DL", min_tot_tack = 2)
html <- "dlSTOP.html"
htmlwidgets::saveWidget(dl, html)
webshot(html, "dlSTOP.png")