# profile over M and h for widow rockfish model
library(r4ss)

# could point to files on github instead
#dir_mod <- "C:/SS/Widow/Widow2025/widow_rockfish_2025/models/2025 base model"
dir_mod <- "https://raw.githubusercontent.com/mcgoodman/widow_rockfish_2025/refs/heads/main/models/2025%20base%20model"
mod_files <- SS_read(dir_mod, ss_new = FALSE, )

# write files to a local directory
dir_prof <- "widow_profile_2D"
SS_write(
  mod_files,
  dir = dir_prof,
  overwrite = TRUE
)

# create table of h and M values
par_table <- expand.grid(
  hvec = seq(0.4, 0.9, 0.05),
  Mvec = seq(0.10, 0.17, 0.01)
)

# run model once to create control.ss_new with
# good starting parameter values
# exe is assumed to be in PATH, add "exe" argument if needed
run(dir_prof, extras = "-nohess")

# run profile using ss_new file as parameter source and
# overwriting original control file with new values
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "control.ss_new",
  newctlfile = "control.ss",
  string = c("SR_BH_steep", "NatM_uniform_Fem_GP_1"),
  profilevec = par_table,
  extras = "-nohess"
)

# get model output
profilemodels <- SSgetoutput(
  dirvec = dir_prof,
  keyvec = 1:nrow(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- SSsummarize(profilemodels)

# add total likelihood (row 1) to table created above
par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])

# reshape data frame into a matrix for use with contour
like_matrix <- reshape2::acast(
  data = par_table,
  formula = hvec ~ Mvec,
  value.var = "like"
)

# look at change relative to the minimum
# (shows small change when female and male M are equal,
# big change when they are different)
like_matrix - min(like_matrix)
