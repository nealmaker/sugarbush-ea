source("R/setup.R")
source("R/objective.R")
source("R/optimizer.R")
source("R/postop.R")

mods <- treemodeler::initialize_models("5.3", c("slim", "base"),
                                       c("taper", "growth"),
                                       cr_mode = "derived")

out <- opt_mngmt(dat, params, mods)
sim <- postop(out$pct_cut[[1]]$pct_cut, dat$trees, params, mods)
