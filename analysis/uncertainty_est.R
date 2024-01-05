
# uncertainty estimates
# script to calculate and visualize uncertainty estimates for SCOPE and STOP
# NOTE: update data_dir with file path to data
#       update model_dir with file path to models

library(dplyr)

# file paths
data_dir = "../../data/"
fig_dir = "../figures/"

# load data
pass_contact_info = read.csv(paste0(data_dir, "contact_info.csv")) %>% select(gameId, playId, nflId, contact)
tackles = read.csv(paste0(data_dir, "tackles.csv")) %>% select(gameId, playId, nflId, tackle)
pass_player_summary = read.csv(paste0(data_dir, "pass_player_summary.csv"))
load(paste0(data_dir, "pass_contact_samples.RData"))
load(paste0(data_dir, "pass_tackle_samples.RData"))

# contact uncertainties
pass_plays_contact = bind_cols(pass_contact_info, t(pass_contact_samples))
pass_player_contact = pass_plays_contact %>%
  select(-c(gameId, playId, contact)) %>%
  group_by(nflId) %>%
  summarise(across(everything(), list(sum))) %>%
  select(-c(nflId)) %>%
  apply(MARGIN = 1, FUN = quantile, p = c(.05, .95)) %>%
  t() %>%
  data.frame() %>%
  rename(
    contact_05 = X5.,
    contact_95 = X95.
  )

# tackle uncertainties
pass_plays_tackle = bind_cols(pass_contact_info, t(pass_tackle_samples * pass_contact_samples))
pass_player_tackle = pass_plays_tackle %>%
  select(-c(gameId, playId, contact)) %>%
  group_by(nflId) %>%
  summarise(across(everything(), list(sum))) %>%
  select(-c(nflId)) %>%
  apply(MARGIN = 1, FUN = quantile, p = c(.05, .95)) %>%
  t() %>%
  data.frame() %>%
  rename(
    tackle_05 = X5.,
    tackle_95 = X95.
  )

# combine results
pass_player_uncertainty = pass_contact_info %>%
  left_join(tackles, by = join_by(gameId, playId, nflId)) %>%
  replace(is.na(.), 0) %>%
  group_by(nflId) %>%
  summarize(
    n_plays = n(),
    tot_contact = sum(contact),
    tot_tackles = sum(tackle)
  ) %>%
  bind_cols(pass_player_contact, pass_player_tackle) %>%
  mutate(
    SCOPE_5 = tot_contact / contact_95,
    SCOPE_95 = tot_contact / contact_05,
    STOP_5 = tot_tackles / tackle_95,
    STOP_95 = tot_tackles / tackle_05
  )

pass_player_all = pass_player_summary %>%
  left_join(pass_player_uncertainty, by = join_by(nflId, n_plays, tot_contact, tot_tackles))

write.csv(pass_player_all, file = paste0(data_dir, "pass_player_all.csv"))

# visualizations

pass_player_all = read.csv(paste0(data_dir, "pass_player_all.csv"))
filtered_summary = pass_player_all %>% filter(tot_xtackles >= 5)
filtered_summary$color = scales::rescale(filtered_summary$tot_xtackles, to = c(0, 1), from = c(min(filtered_summary$tot_xtackles), max(filtered_summary$tot_xtackles)))

top_stop_lb = filtered_summary %>% 
  filter(tot_xtackles >= 5, pos_group == "LB") %>%
  # slice_max(STOP, n = 10) %>%
  arrange(desc(STOP))

png(file = paste0(fig_dir, "lb_uncertainty_plot.png"), width = 1000, height = 500, units = "px", pointsize = 16)

col_list  = colorRampPalette(colors = c('#762A83', '#9970AB', '#C2A5CF', '#FEE391',  '#FB9A29', '#CC4C02'), interpolate = "linear")(12)

# every LB
par(mar = c(2.1, 4.1, 3.1, 5.1))  # bottom, left, top, right
plot(x = 1:80, type = "n", ylim = c(0.2, 2.5), main = "90% Credible Interval of STOP scores for LBs (min. 5 expected tackles)", ylab = "STOP", xlab = "", xaxt = 'n')
# plot(1:80, top_stop_lb$STOP, ylim = c(0.2, 2.5), main = "Top 10 Scores for LBs (90% Credible Interval)", ylab = "STOP", xlab = "", xaxt = 'n', pch = 16)
abline(v = 23.5, lty = 2)
abline(v = 42.5, lty = 2)
abline(h = 1)
for(i in 1:80){
  points(x = i, y = top_stop_lb$STOP[i], pch = 16, col = rgb(colorRamp(col_list, bias = 1)(top_stop_lb$color[i])/255))
  arrows(i, top_stop_lb$STOP_5[i], i, top_stop_lb$STOP_95[i], code = 3, angle = 90, lwd = 2, length = 0.025, col = rgb(colorRamp(col_list, bias = 1)(top_stop_lb$color[i])/255))
}

# draw legend
plim = par("usr")
res = 200
w = (plim[4] - plim[3] - 0.6) / (res + 1)
l = plim[2] + 1.5
r = plim[2] + 3
for (i in 0:res) {
  p = plim[3] + 0.3 + (w * i) + (w / 2)
  rect(l, p + (w / 2), r, p - (w / 2), border = NA, col = rgb(colorRamp(col_list, bias = 1)(i/res)/255), xpd = TRUE)
}
rect(r, plim[3] + 0.3, l, plim[4] - 0.3, xpd = TRUE)

# legend labels
nlab = 5
y_tic = (plim[4] - plim[3] - 0.6) / nlab
x_tic = plim[2] - 4 + (plim[2] - plim[1]) * 0.125
text(x = rep(x_tic, times = nlab + 1), y = seq(plim[3] + 0.3, plim[4] - 0.3, by = y_tic), labels = round(seq(min(filtered_summary$tot_xtackles), max(filtered_summary$tot_xtackles), length.out = nlab + 1), 2), cex = 0.75, xpd = TRUE)
text(x = 88, y = 2.55, labels = substitute(bold("Expected")), xpd = TRUE)
text(x = 88, y = 2.45, labels = substitute(bold("Tackles")), xpd = TRUE)
text(x = 11, y = 0.3, "Performed Above Expected")
text(x = 63, y = 0.3, "Performed Below Expected")

dev.off()

top_stop_db = filtered_summary %>% 
  filter(tot_xtackles >= 5, pos_group == "DB") %>%
  # slice_max(STOP, n = 10) %>%
  arrange(desc(STOP))

png(file = paste0(fig_dir, "db_uncertainty_plot.png"), width = 1000, height = 500, units = "px", pointsize = 16)

# every DB
par(mar = c(2.1, 4.1, 3.1, 5.1))  # bottom, left, top, right
plot(x = 1:210, type = "n", ylim = c(0.2, 3.5), main = "90% Credible Interval of STOP scores for DBs (min. 5 expected tackles)", ylab = "STOP", xlab = "", xaxt = 'n')
abline(v = 65.5, lty = 2)
abline(v = 115.5, lty = 2)
abline(h = 1)
for(i in 1:210){
  points(x = i, y = top_stop_db$STOP[i], pch = 16, col = rgb(colorRamp(col_list, bias = 1)(top_stop_db$color[i])/255))
  arrows(i, top_stop_db$STOP_5[i], i, top_stop_db$STOP_95[i], code = 3, angle = 90, lwd = 2, length = 0.025, col = rgb(colorRamp(col_list, bias = 1)(top_stop_db$color[i])/255))
}

# draw legend
plim = par("usr")
res = 200
w = (plim[4] - plim[3] - 0.9) / (res + 1)
l = plim[2] + 4
r = plim[2] + 8
for (i in 0:res) {
  p = plim[3] + 0.45 + (w * i) + (w / 2)
  rect(l, p + (w / 2), r, p - (w / 2), border = NA, col = rgb(colorRamp(col_list, bias = 1)(i/res)/255), xpd = TRUE)
}
rect(r, plim[3] + 0.45, l, plim[4] - 0.45, xpd = TRUE)

# legend labels
nlab = 5
y_tic = (plim[4] - plim[3] - 0.9) / nlab
x_tic = plim[2] - 10 + (plim[2] - plim[1]) * 0.125
text(x = rep(x_tic, times = nlab + 1), y = seq(plim[3] + 0.45, plim[4] - 0.45, by = y_tic), labels = round(seq(min(filtered_summary$tot_xtackles), max(filtered_summary$tot_xtackles), length.out = nlab + 1), 2), cex = 0.75, xpd = TRUE)
text(x = 231, y = 3.55, labels = substitute(bold("Expected")), xpd = TRUE)
text(x = 231, y = 3.4, labels = substitute(bold("Tackles")), xpd = TRUE)
text(x = 30, y = 0.35, "Performed Above Expected")
text(x = 160, y = 0.35, "Performed Below Expected")

dev.off()
