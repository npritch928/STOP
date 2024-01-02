
# result visualization
# script to make figures visualizing key findings
# NOTE: update data_dir with file path to results data

# packages
library(dplyr)
library(scales)
library(nflplotR)

# file paths
data_dir = "../data/"
fig_dir = "../figures/"

# data
pass_player_summary = read.csv(paste0(data_dir, "pass_player_summary.csv"))
pass_team_summary = read.csv(paste0(data_dir, "pass_team_summary.csv"))
filtered_summary = pass_player_summary %>% filter(tot_xtackles >= 5)
filtered_summary$color = rescale(filtered_summary$STOP, to = c(0, 1), from = c(min(filtered_summary$STOP), max(filtered_summary$STOP)))

png(file = paste0(fig_dir, "archetype_plot.png"), width = 6, height = 6, units = "in", res = 300)

col_list = rev(RColorBrewer::brewer.pal(n = 11, name = "RdYlBu"))
par(mar = c(4.1, 4.1, 3.1, 4.1))  # bottom, left, top, right
avg_cae = mean(filtered_summary$contacts_above_expected)
avg_tae = mean(filtered_summary$tackles_above_expected)
plot(1, type = "n", xlim = c(avg_tae - 18, avg_tae + 18), ylim = c(avg_cae - 18, avg_cae + 18), main = "Tackling archetypes by STOP", xlab = "Tackles above expected", ylab = "Contacts above expected", xpd = TRUE)
abline(h = avg_cae, v = avg_tae)
for (i in 1:nrow(filtered_summary)) {
  points(
    filtered_summary$tackles_above_expected[i],
    filtered_summary$contacts_above_expected[i],
    pch = 20,
    col = rgb(colorRamp(col_list, bias = 1)(filtered_summary$color[i])/255)
  )
}

# draw legend
plim = par("usr")
res = 200
w = (plim[4] - plim[3] - 10) / (res + 1)
l = plim[2] + 40 * 0.025
r = plim[2] + 40 * 0.055
for (i in 0:res) {
  p = plim[3] + 5 + (w * i) + (w / 2)
  rect(l, p - (w / 2), r, p + (w / 2), border = NA, col = rgb(colorRamp(col_list, bias = 1)(i/res)/255), xpd = TRUE)
}
rect(r, plim[3] + 5, l, plim[4] - 5, xpd = TRUE)

# legend labels
nlab = 5
y_tic = (plim[4] - plim[3] - 10) / nlab
x_tic = plim[2] + (plim[2] - plim[1]) * 0.125
text(x = rep(x_tic, times = nlab + 1), y = seq(plim[3] + 5, plim[4] - 5, by = y_tic), labels = round(seq(min(filtered_summary$STOP), max(filtered_summary$STOP), length.out = nlab + 1), 2), cex = 0.75, xpd = TRUE)
text(x = 22.5, y = 17, labels = substitute(bold("STOP")), xpd = TRUE)

# superlatives
top_stop = filtered_summary %>% slice_max(STOP, n = 1)
text(top_stop$tackles_above_expected, top_stop$contacts_above_expected, labels = top_stop$displayName, pos = 4, cex = 0.5)

low_stop = filtered_summary %>% slice_min(STOP, n = 1)
text(low_stop$tackles_above_expected, low_stop$contacts_above_expected, labels = low_stop$displayName, pos = 2, cex = 0.5)

top_cae = filtered_summary %>% slice_max(contacts_above_expected, n = 1)
text(top_cae$tackles_above_expected, top_cae$contacts_above_expected, labels = top_cae$displayName, pos = 4, cex = 0.5)

low_cae = filtered_summary %>% slice_min(contacts_above_expected, n = 2)
text(low_cae$tackles_above_expected, low_cae$contacts_above_expected, labels = low_cae$displayName, pos = 2, cex = 0.5)

low_tae = filtered_summary %>% slice_min(tackles_above_expected, n = 5)
text(low_tae$tackles_above_expected, low_tae$contacts_above_expected, labels = low_tae$displayName, pos = 2, cex = 0.5)

# label by quadrant
q1 = filtered_summary %>% filter(contacts_above_expected < 6, contacts_above_expected > 0, tackles_above_expected > 4.8, displayName != "T.J. Edwards")
text(q1$tackles_above_expected, q1$contacts_above_expected, labels = q1$displayName, pos = 4, cex = 0.5)

q2 = filtered_summary %>% filter(contacts_above_expected > 5, tackles_above_expected < -3)
text(q2$tackles_above_expected, q2$contacts_above_expected, labels = q2$displayName, pos = 2, cex = 0.5)

q4 = filtered_summary %>% filter(contacts_above_expected < 0, tackles_above_expected > 5)
text(q4$tackles_above_expected, q4$contacts_above_expected, labels = q4$displayName, pos = 4, cex = 0.5)

# notable players
not1 = filtered_summary %>% filter(displayName %in% c("T.J. Edwards"))
text(not1$tackles_above_expected, not1$contacts_above_expected, labels = not1$displayName, adj = c(-0.15, 1.5), cex = 0.5)

not2 = filtered_summary %>% filter(displayName %in% c("Eli Apple"))
text(not2$tackles_above_expected, not2$contacts_above_expected, labels = not2$displayName, pos = 2, cex = 0.5)

not4 = filtered_summary %>% filter(displayName %in% c("Devin McCourty", "Bobby Wagner", "Zaire Franklin"))
text(not4$tackles_above_expected, not4$contacts_above_expected, labels = not4$displayName, pos = 4, cex = 0.5)

# quadrant labels
text(avg_tae + 10, avg_cae + 18, substitute(bold("Complete")), cex = 0.9)
text(avg_tae - 10, avg_cae + 18, substitute(bold("Positional")), cex = 0.9)
text(avg_tae - 10, avg_cae - 18, substitute(bold("Flawed")), cex = 0.9)
text(avg_tae + 10, avg_cae - 18, substitute(bold("Finishers")), cex = 0.9)

dev.off()

# team results
league_avg_stop = mean(pass_team_summary$STOP)
league_avg_scope = mean(pass_team_summary$SCOPE)
team_plot = pass_team_summary %>%
  ggplot(aes(STOP, SCOPE, label = defensiveTeam)) +
  geom_hline(yintercept = league_ave_scope, col = "black", linewidth = 0.4) +
  geom_vline(xintercept = league_avg_stop, col = "black", linewidth = 0.4) +
  geom_nfl_logos(aes(team_abbr = defensiveTeam), width = 0.075) +
  xlim(league_avg_stop - 0.15, league_avg_stop + 0.15) +
  ylim(league_avg_scope - .12, league_avg_scope + .12) +
  ggtitle("SCOPE and STOP scores by team") +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.75),
    axis.line = element_line(colour = "black", linewidth = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
  )

ggsave(
  file = paste0(fig_dir, "team_plot.png"),
  plot = team_plot,
  width = 600,
  height = 500,
  units = "px",
  scale = 3,
  bg = "white"
)

