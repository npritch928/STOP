
# script to make calibration plots for probability models

# data directory
data_dir = "../../data/"

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
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  plot(calibration_df$points + width / 2, calibration_df$empirical, type = "p", main = main, xlab = "Predicted Probability", ylab = "Observed Probability")
  lines(calibration_df$points + width / 2, calibration_df$predicted, col = "red")
}

# hyperparameters
w = 0.05
s = 0.01

# contact model - pass
load(paste0(data_dir, "pass_contact_model.RData"))
pass_contact_calibration_df = get_calibration_df(actual = Y_train_pass, predicted = pass_contact_model$prob.train.mean, width = w, step = s)
plot_calibration_df(pass_contact_calibration_df, main = "Contact Model (Pass Plays)", width = w, step = s)

# contact model - run
load(paste0(data_dir, "run_contact_model.RData"))
run_contact_calibration_df = get_calibration_df(actual = Y_train_run, predicted = run_contact_model$prob.train.mean, width = w, step = s)
plot_calibration_df(run_contact_calibration_df, main = "Contact Model (Run Plays)", width = w, step = s)

# tackle model - pass
load(paste0(data_dir, "pass_tackle_model.RData"))
pass_tackle_calibration_df = get_calibration_df(actual = Y_train_pass, predicted = pass_tackle_model$prob.train.mean, width = w, step = s)
plot_calibration_df(pass_tackle_calibration_df, main = "Tackle Model (Pass Plays)", width = w, step = s)

# tackle model - run
load(paste0(data_dir, "run_tackle_model.RData"))
run_tackle_calibration_df = get_calibration_df(actual = Y_train_run, predicted = run_tackle_model$prob.train.mean, width = w, step = s)
plot_calibration_df(run_tackle_calibration_df, main = "Tackle Model (Run Plays)", width = w, step = s)
