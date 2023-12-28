# Compute the distances between players
source("./dataCleaning/compute_distances.R")
# Compute player's heights in meters
source("./dataCleaning/player_heights.R")
# Determine the frames where contact occurs
source("./dataCleaning/get_contact_frames.R")
# Add distance and other labels to the tracking data
source("./dataCleaning/label_tracking.R")
#Select the columns and rows needed for modeling
source("./dataCleaning/make_contact_tackle_training_data.R")
# Add players speeds in the x direction
source("./dataCleaning/xSpeedDirection.R")
