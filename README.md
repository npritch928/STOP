# STOP 
A public repository for the 2024 NFL Big Data Bowl

# Required R Libraries
- tidyverse
- purrr
- furrr
- progressr
- lubridate
- gganimate
- scales
- git@github.com:skdeshpande91/flexBART.git

# Directory set up
Within the Tackling repository one should download the data from the NFL Big Data Bowl 2024 and place it within the repository in a repository labeled `data`.

# Build the datasets
To build the datasets from the main directory, you should run the command 
```
Rscript buildData.R 
``` 
this script requires the computer to have access to at least 8 cores. If you have fewer than this number of cores available, you should replace the `plan(multisession(workers = n))` lines in the following files:
- compute_distances.R
- label_tracking.R
- make_contact_tackle_training_data.R 

# Fit models
To fit the contact and tackle probability models, run `fit_contact_model.R` and `fit_tackle_model.R` located in the `models` directory.
Running both scripts will produce the following outputs:
- pass_contact_model.RData
- run_contact_model.RData
- pass_tackle_model.RData
- run_tackle_model.RData
- pass_tackle_pred.RData
- run_tackle_pred.RData
 
# Compute metrics
To compute SCOPE and STOP, run `calc_model_results` in the `models` directory.
Running this script will produce `all_plays.csv`, `pass_player_summary.csv` and `pass_team_summary.csv`.
`all_plays.csv` contains the contact probability, tackle probability, and expected tackles for every player in every play in our dataset.
`pass_player_summary.csv` contains the expected contacts, expected tackles, observed contacts, and observed tackles on passing plays for each playing in our dataset.
`pass_team_summary.csv` containts the same metrics as the player summary file but is instead grouped by team.

# Analysis
The scripts in the `analysis` directory contain code to make the figures in the write-up.
