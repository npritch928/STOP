
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
filtered_summary$color = rescale(filtered_summary$tot_tackles, to = c(0, 1), from = c(min(filtered_summary$tot_tackles), max(filtered_summary$tot_tackles)))

png(file = paste0(fig_dir, "archetype_plot.png"), width = 6, height = 6, units = "in", res = 300)

col_list  = colorRampPalette(colors = c('#762A83', '#9970AB', '#C2A5CF', '#FEE391',  '#FB9A29', '#CC4C02'), interpolate = "spline")(12)
par(mar = c(4.1, 4.1, 3.1, 4.1))  # bottom, left, top, right
avg_SCOPE = mean(filtered_summary$SCOPE)
avg_STOP = mean(filtered_summary$STOP)
plot(1, type = "n", xlim = c(-0.1, avg_STOP + 1.5), ylim = c(avg_SCOPE - .7, avg_SCOPE + .7), main = "Tackling Archetypes", xlab = "STOP", ylab = "SCOPE", xpd = TRUE)
abline(h = avg_SCOPE, v = avg_STOP)
for (i in 1:nrow(filtered_summary)) {
  points(
    filtered_summary$STOP[i],
    filtered_summary$SCOPE[i],
    pch = 20,
    col = rgb(colorRamp(col_list, bias = 1)(filtered_summary$color[i])/255)
  )
}

# draw legend
plim = par("usr") # xmin, xmax, ymin, ymax
l = plim[2] + (2.4) * 0.025
r = plim[2] + (2.4) * 0.055
leg_len = 0.75
sign = 1
flip_cols = FALSE
len_adj = (plim[4] - plim[3]) * (1 - leg_len)
w = (plim[4] - plim[3] - len_adj) / (res + 1)
for (i in 0:res) {
  if (flip_cols) {
    p = plim[4] + ((len_adj / 2) + (w * i) + (w / 2)) * sign
  } else {
    p = plim[3] + ((len_adj / 2) + (w * i) + (w / 2)) * sign
  }
  rect(l, p - (w / 2) * sign, r, p + (w / 2) * sign, border = NA, col = rgb(colorRamp(col_list, bias = 1)(i/res)/255), xpd = TRUE)
}
rect(r, plim[3] + (len_adj / 2), l, plim[4] - (len_adj / 2), xpd = TRUE)

# legend labels
nlab = 4
y_tic = (plim[4] - plim[3] - len_adj) / nlab
x_tic = plim[2] + (plim[2] - plim[1]) * 0.125
text(x = rep(x_tic, times = nlab + 1), y = seq(plim[3] + (len_adj / 2), plim[4] - (len_adj / 2), by = y_tic), labels = round(seq(min(filtered_summary$tot_tackles), max(filtered_summary$tot_tackles), length.out = nlab + 1), 2), cex = 0.75, xpd = TRUE)
text(x = 2.8, y = 1.725, labels = substitute(bold("Observed")), cex = 0.75, xpd = TRUE)
text(x = 2.8, y = 1.675, labels = substitute(bold("Tackles")), cex = 0.75, xpd = TRUE)

# superlatives
top_stop = filtered_summary %>% slice_max(STOP, n = 4)
text(top_stop$STOP, top_stop$SCOPE, labels = top_stop$displayName, pos = 4, cex = 0.5)

top_scope = filtered_summary %>% slice_max(SCOPE, n = 2) %>% filter(displayName != "Anthony Brown")
text(top_scope$STOP, top_scope$SCOPE, labels = top_scope$displayName, pos = 4, cex = 0.5)

low_stop = filtered_summary %>% slice_min(STOP, n = 2) 
text(low_stop$STOP, low_stop$SCOPE, labels = low_stop$displayName, pos = 2, cex = 0.5)

low_scope = filtered_summary %>% slice_min(SCOPE, n = 2) %>% filter(displayName != "Lorenzo Carter")
text(low_scope$STOP, low_scope$SCOPE, labels = low_scope$displayName, adj = c(1.1, 1.4), cex = 0.5)

# notable players
not1 = filtered_summary %>% filter(displayName %in% c("T.J. Edwards"))
text(not1$STOP, not1$SCOPE, labels = not1$displayName, adj = c(-0.15, 1.5), cex = 0.5)

not2 = filtered_summary %>% filter(displayName %in% c("Eli Apple"))
text(not2$STOP, not2$SCOPE, labels = not2$displayName, adj = c(1.2, 0), cex = 0.5)

not4 = filtered_summary %>% filter(displayName %in% c("Bobby Wagner"))
text(not4$STOP, not4$SCOPE, labels = not4$displayName, adj = c(-0.15, 1), cex = 0.5)

# Outliers
q1 = filtered_summary %>% filter(SCOPE < 1.05, STOP > 1.3) %>% slice_max(STOP, n = 1)
text(q1$STOP, q1$SCOPE, labels = q1$displayName, pos = 4, cex = 0.5)

q2 = filtered_summary %>% filter(SCOPE > 1.2, STOP < .9) %>% slice_max(SCOPE, n = 1)
text(q2$STOP, q2$SCOPE, labels = q2$displayName, pos = 2, cex = 0.5)

q4 = filtered_summary %>% filter(SCOPE < 0.8, STOP > 1) %>% slice_max(STOP, n = 2)
text(q4$STOP, q4$SCOPE, labels = q4$displayName, pos = 4, cex = 0.5)

# quadrant labels
text(avg_STOP + (plim[2] - avg_STOP) / 2, plim[4] * .95, substitute(bold("Complete")), cex = 0.9)
text(avg_STOP + (plim[1] - avg_STOP) / 2, plim[4] * .95, substitute(bold("Positional")), cex = 0.9)
text(avg_STOP + (plim[1] - avg_STOP) / 2, plim[3] * 1.3, substitute(bold("Flawed")), cex = 0.9)
text(avg_STOP + (plim[2] - avg_STOP) / 2, plim[3] * 1.3, substitute(bold("Finishers")), cex = 0.9)

dev.off()

