
# model validation
# script to create figures used to analyze and validate probability models
# NOTE: update model_dir with file path to models

# file paths
data_dir = "../data/"
model_dir = "../data/"
fig_dir = "../figures/"

# load result data
pass_player_summary = read.csv(paste0(data_dir, "pass_player_summary.csv"))

# calibration plots

# helper functions
get_calibration_df = function(actual, predicted, width = 0.05, step = 0.01){
  if(length(actual) != length(predicted)){
    return("error")
  } else {
    points = seq(0, (1 - width), by = step)
    emp = rep(NA, times = length(points))
    pred = rep(NA, times = length(points))
    for (i in 1:length(points)){
      ub = points[i] + width
      lb = points[i]
      emp[i] = mean(actual[which(predicted < ub & predicted >= lb)])
      pred[i] = mean(predicted[which(predicted < ub & predicted >= lb)])
    }
    return(data.frame(points = points, empirical = emp, predicted = pred))
  }
}

plot_calibration_df = function(calibration_df, main, width = 0.05, step = 0.01){
  par(mar = c(2.1, 2.1, 2.1, 2.1))
  plot(calibration_df$points + width / 2, calibration_df$empirical, type = "p", xlim = c(0, 1), ylim = c(0, 1), main = main, xlab = "", ylab = "", cex = 0.75)
  lines(calibration_df$points + width / 2, calibration_df$predicted, col = "red")
}

png(file = paste0(fig_dir, "calibration_plot.png"), width = 6, height = 6, units = "in", res = 300)

# hyperparameters
w = 0.05
s = 0.01
par(mfrow = c(2, 2), oma=c(2.5,2.5,0,0))

# contact model - pass
load(paste0(model_dir, "pass_contact_model.RData"))
pass_contact_calibration_df = get_calibration_df(actual = Y_train_pass, predicted = pass_contact_model$prob.train.mean, width = w, step = s)
plot_calibration_df(pass_contact_calibration_df, main = "Contact Model (Pass Plays)", width = w, step = s)

# contact model - run
load(paste0(model_dir, "run_contact_model.RData"))
run_contact_calibration_df = get_calibration_df(actual = Y_train_run, predicted = run_contact_model$prob.train.mean, width = w, step = s)
plot_calibration_df(run_contact_calibration_df, main = "Contact Model (Run Plays)", width = w, step = s)

# tackle model - pass
load(paste0(model_dir, "pass_tackle_model.RData"))
pass_tackle_calibration_df = get_calibration_df(actual = Y_train_pass, predicted = pass_tackle_model$prob.train.mean, width = w, step = s)
plot_calibration_df(pass_tackle_calibration_df, main = "Tackle Model (Pass Plays)", width = w, step = s)

# tackle model - run
load(paste0(model_dir, "run_tackle_model.RData"))
run_tackle_calibration_df = get_calibration_df(actual = Y_train_run, predicted = run_tackle_model$prob.train.mean, width = w, step = s)
plot_calibration_df(run_tackle_calibration_df, main = "Tackle Model (Run Plays)", width = w, step = s)

mtext("Observed Probability", side = 1, outer = TRUE, padj = 1)
mtext("Predicted Probability", side = 2, outer = TRUE, padj = -1)

dev.off()

# observed vs expected quantities

png(file = paste0(fig_dir, "x_plot.png"), width = 6, height = 3, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(2.1, 2.1, 2.1, 2.1), oma=c(2.5,2.5,0,0))

plot(pass_player_summary$tot_contact, pass_player_summary$tot_xcontacts, main = "Contacts", xlab = "", ylab = "", cex = 0.75)
abline(a = 0, b = 1, col = "blue")

plot(pass_player_summary$tot_tackles, pass_player_summary$tot_xtackles, main = "Tackles", xlab = "", ylab = "", cex = 0.75)
abline(a = 0, b = 1, col = "blue")

mtext("Observed", side = 1, outer = TRUE, padj = 1)
mtext("Expected", side = 2, outer = TRUE, padj = -1)

dev.off()

# variable importance

contact_var_counts = apply(X = pass_contact_model$varcounts, MARGIN = 2, FUN = sum)
contact_var_names = c(colnames(X_cont_pass), colnames(X_cat_pass))
contact_var_data = data.frame(var = contact_var_names, counts = contact_var_counts)
contact_var_data = contact_var_data[order(contact_var_data$counts, decreasing = FALSE),]
par(mfrow = c(1, 1), mar = c(4.1, 6.1, 2.1, 3.1))
barplot(contact_var_data$counts, names.arg = contact_var_data$var, horiz = TRUE, las = 1, main = "Contact Model (pass plays)", xlab = "Variable Importance Score")

tackle_var_counts = apply(X = pass_tackle_model$varcounts, MARGIN = 2, FUN = sum)
tackle_var_names = c(colnames(X_cont_pass), colnames(X_cat_pass))
tackle_var_data = data.frame(var = tackle_var_names, counts = tackle_var_counts)
tackle_var_data = tackle_var_data[order(tackle_var_data$counts, decreasing = FALSE),]
par(mfrow = c(1, 1), mar = c(4.1, 6.1, 2.1, 3.1))
barplot(tackle_var_data$counts, names.arg = tackle_var_data$var, horiz = TRUE, las = 1, main = "Tackle Model (pass plays)", xlab = "Variable Importance Score")

