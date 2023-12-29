# STOP 
A public repository for the 2024 NFL Big Data Bowl

# Required R Libraries
- tidyverse
- purrr
- furrr
- progressr
- lubridate
- gganimate
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
To fit the contact and tackle probability models, run both scripts in the `models` directory.
Running both scripts will produce the following outputs:
- pass_contact_model.RData
- run_contact_model.RData
- pass_tackle_model.RData
- run_tackle_model.RData
- pass_tackle_pred.RData
- run_tackle_pred.RData
 
