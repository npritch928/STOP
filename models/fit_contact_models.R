
# BART contact model
# script to fit both run and contact probability models
# NOTE: update data_dir with file path to data
#       update model_dir with file path to save models

# packages
library(dplyr)
library(scales)
library(flexBART)

# file paths
data_dir = "../data/"
model_dir = "../data/"

# data
pass_contact_info = read.csv(paste0(data_dir, "contact_info.csv"))
run_contact_info = read.csv(paste0(data_dir, "contact_info_run.csv"))

# create inputs for flexBART

# function to get continuous inputs
get_x_cont = function(data, type = "pass"){
  if (type == "pass"){
    X_cont = data %>% select(
      timeRemaining, defendersInTheBox, down,
      dist_f, s, a, yardsRemaining, airYards, Dist2side, Dist2endZ, Dist2LoS, Dist2FD, 
      rel_direction, bc_1, bc_2, bc_3, bc_4, bc_5, bc_6, bc_7, bc_8, bc_9, bc_10, bc_11,
      TeamInContact, weight, max_speed, pMaxSpeed, xSpeed, BC_xSpeed, Bc_weight
    )
  } else {
    X_cont = data %>% select(
      timeRemaining, defendersInTheBox, down,
      dist_f, s, a, yardsRemaining, Dist2side, Dist2endZ, Dist2LoS, Dist2FD, 
      rel_direction, bc_1, bc_2, bc_3, bc_4, bc_5, bc_6, bc_7, bc_8, bc_9, bc_10, bc_11,
      TeamInContact, weight, max_speed, pMaxSpeed, xSpeed, BC_xSpeed, Bc_weight
    )
  }
  X_cont$timeRemaining = scales::rescale(X_cont$timeRemaining, to = c(-1, 1), from = c(min(data$timeRemaining), max(data$timeRemaining)))
  # defendersInTheBox will not have unif_cutpoint
  # down will not have unif_cutpoint
  X_cont$dist_f = scales::rescale(X_cont$dist_f, to = c(-1, 1), from = c(min(data$dist_f, na.rm = TRUE), max(data$dist_f, na.rm = TRUE)))
  X_cont$s = scales::rescale(X_cont$s, to = c(-1, 1), from = c(min(data$s), max(data$s)))
  X_cont$a = scales::rescale(X_cont$a, to = c(-1, 1), from = c(min(data$a), max(data$a)))
  X_cont$yardsRemaining = scales::rescale(X_cont$yardsRemaining, to = c(-1, 1), from = c(min(data$yardsRemaining), max(data$yardsRemaining)))
  if (type == "pass"){X_cont$airYards = scales::rescale(X_cont$airYards, to = c(-1, 1), from = c(min(data$airYards), max(data$airYards)))}
  X_cont$Dist2side = scales::rescale(X_cont$Dist2side, to = c(-1, 1), from = c(min(data$Dist2side), max(data$Dist2side)))
  X_cont$Dist2endZ = scales::rescale(X_cont$Dist2endZ, to = c(-1, 1), from = c(min(data$Dist2endZ), max(data$Dist2endZ)))
  X_cont$Dist2LoS = scales::rescale(X_cont$Dist2LoS, to = c(-1, 1), from = c(min(data$Dist2LoS), max(data$Dist2LoS)))
  X_cont$Dist2FD = scales::rescale(X_cont$Dist2FD, to = c(-1, 1), from = c(min(data$Dist2FD), max(data$Dist2FD)))
  X_cont$rel_direction = scales::rescale(X_cont$rel_direction, to = c(-1, 1), from = c(min(data$rel_direction), max(data$rel_direction)))
  X_cont$bc_1 = scales::rescale(X_cont$bc_1, to = c(-1, 1), from = c(min(data$bc_1), max(data$bc_1)))
  X_cont$bc_2 = scales::rescale(X_cont$bc_2, to = c(-1, 1), from = c(min(data$bc_2), max(data$bc_2)))
  X_cont$bc_3 = scales::rescale(X_cont$bc_3, to = c(-1, 1), from = c(min(data$bc_3), max(data$bc_3)))
  X_cont$bc_4 = scales::rescale(X_cont$bc_4, to = c(-1, 1), from = c(min(data$bc_4), max(data$bc_4)))
  X_cont$bc_5 = scales::rescale(X_cont$bc_5, to = c(-1, 1), from = c(min(data$bc_5), max(data$bc_5)))
  X_cont$bc_6 = scales::rescale(X_cont$bc_6, to = c(-1, 1), from = c(min(data$bc_6), max(data$bc_6)))
  X_cont$bc_7 = scales::rescale(X_cont$bc_7, to = c(-1, 1), from = c(min(data$bc_7), max(data$bc_7)))
  X_cont$bc_8 = scales::rescale(X_cont$bc_8, to = c(-1, 1), from = c(min(data$bc_8), max(data$bc_8)))
  X_cont$bc_9 = scales::rescale(X_cont$bc_9, to = c(-1, 1), from = c(min(data$bc_9), max(data$bc_9)))
  X_cont$bc_10 = scales::rescale(X_cont$bc_10, to = c(-1, 1), from = c(min(data$bc_10), max(data$bc_10)))
  X_cont$bc_11 = scales::rescale(X_cont$bc_11, to = c(-1, 1), from = c(min(data$bc_11), max(data$bc_11)))
  # TeamInContact will not have unif_cutpoint
  X_cont$weight = scales::rescale(X_cont$weight, to = c(-1, 1), from = c(min(data$weight, na.rm = TRUE), max(data$weight, na.rm = TRUE)))
  X_cont$max_speed = scales::rescale(X_cont$max_speed, to = c(-1, 1), from = c(min(data$max_speed), max(data$max_speed)))
  X_cont$pmax_speed = scales::rescale(X_cont$pmax_speed, to = c(-1, 1), from = c(min(data$pmax_speed), max(data$pmax_speed)))
  X_cont$xSpeed = scales::rescale(X_cont$xSpeed, to = c(-1, 1), from = c(min(data$xSpeed), max(data$xSpeed)))
  X_cont$BC_xSpeed = scales::rescale(X_cont$BC_xSpeed, to = c(-1, 1), from = c(min(data$BC_xSpeed), max(data$BC_xSpeed)))
  X_cont$Bc_weight = scales::rescale(X_cont$Bc_weight, to = c(-1, 1), from = c(min(data$Bc_weight, na.rm = TRUE), max(data$Bc_weight, na.rm = TRUE)))
  return(X_cont)
}

# pass model

# inputs for continuous features
X_cont_pass = get_x_cont(pass_contact_info, type = "pass")
unif_cuts_pass = rep(TRUE, 31)
unif_cuts_pass[2] = FALSE # defendersInTheBox
unif_cuts_pass[3] = FALSE # down
unif_cuts_pass[25] = FALSE # TeamInContact
cutpoints_list_pass = list(c(0), seq(0, 11), seq(1, 4), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), seq(0, 11), c(0), c(0), c(0), c(0), c(0), c(0))

# categorical indices
pass_bcId_idx = sort(unique(pass_contact_info$CarrierId))
pass_pos_idx = sort(unique(pass_contact_info$position.x))

# function to get categorical inputs
get_x_cat = function(data, bc_lvls, pos_lvls){
  X_cat = data %>% select(CarrierId, position.x)
  X_cat$CarrierId = as.integer(factor(X_cat$CarrierId, levels = bc_lvls)) - 1
  X_cat$position.x = as.integer(factor(X_cat$position.x, levels = pos_lvls)) - 1
  return(as.matrix(X_cat))
}

# inputs for categorical features
X_cat_pass = get_x_cat(pass_contact_info, bc_lvls = pass_bcId_idx, pos_lvls = pass_pos_idx)
cat_levels_list_pass = list(
  seq(0, length(pass_bcId_idx) - 1),
  seq(0, length(pass_pos_idx) - 1)
)

# response
Y_train_pass = as.integer(pass_contact_info$contact)

pass_contact_model = probit_flexBART(
  Y_train = Y_train_pass,
  X_cont_train = X_cont_pass,
  X_cat_train = X_cat_pass,
  unif_cuts = unif_cuts_pass,
  cutpoints_list = cutpoints_list_pass,
  cat_levels_list = cat_levels_list_pass,
  save_samples = FALSE,
  save_trees = TRUE
)

# save model and inputs
save(pass_contact_model, Y_train_pass, X_cont_pass, X_cat_pass, unif_cuts_pass, cutpoints_list_pass, cat_levels_list_pass, file = paste0(model_dir, "pass_contact_model.RData"))

# run model

# inputs for continuous features
X_cont_run = get_x_cont(run_contact_info, type = "run")
unif_cuts_run = rep(TRUE, 30)
unif_cuts_run[2] = FALSE # defendersInTheBox
unif_cuts_run[3] = FALSE # down
unif_cuts_run[24] = FALSE # TeamInContact
cutpoints_list_run = list(c(0), seq(0, 11), seq(1, 4), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), seq(0, 11), c(0), c(0), c(0), c(0), c(0), c(0))

# categorical indices
run_bcId_idx = sort(unique(run_contact_info$CarrierId))
run_pos_idx = sort(unique(run_contact_info$position.x))

# inputs for categorical features
X_cat_run = get_x_cat(run_contact_info, bc_lvls = run_bcId_idx, pos_lvls = run_pos_idx)
cat_levels_list_run = list(
  seq(0, length(run_bcId_idx) - 1),
  seq(0, length(run_pos_idx) - 1)
)

# response
Y_train_run = as.integer(run_contact_info$contact)

run_contact_model = probit_flexBART(
  Y_train = Y_train_run,
  X_cont_train = X_cont_run,
  X_cat_train = X_cat_run,
  unif_cuts = unif_cuts_run,
  cutpoints_list = cutpoints_list_run,
  cat_levels_list = cat_levels_list_run,
  save_samples = FALSE,
  save_trees = TRUE
)

# save model and inputs
save(run_contact_model, Y_train_run, X_cont_run, X_cat_run, unif_cuts_run, cutpoints_list_run, cat_levels_list_run, file = paste0(model_dir, "run_contact_model.RData"))

# predict samples for pass model
X_cont_test_pass = get_x_cont(pass_contact_info, type = "pass")
X_cat_test_pass = get_x_cat(pass_contact_info, bc_lvls = pass_bcId_idx, pos_lvls = pass_pos_idx)
pass_contact_samples = predict_flexBART(
  fit = pass_contact_model,
  X_cont = X_cont_test_pass,
  X_cat = X_cat_test_pass,
  verbose = TRUE
)
save(pass_contact_samples, file = paste0(data_dir, "pass_contact_samples.RData"))
