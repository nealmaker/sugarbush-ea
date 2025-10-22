source("R/setup.R")
source("R/objective.R")
source("R/optimizer.R")
source("R/postop.R")

mods <- treemodeler::initialize_models("5.3", c("slim", "base"),
                                       c("taper", "growth"),
                                       cr_mode = "derived")

# Optimization
out <- opt_mngmt(dat, params, mods)

pct_cut <- out$pct_cut[[1]]
pct_cut$pct_cut[pct_cut$pct_cut > .95] <- 1
pct_cut$pct_cut[pct_cut$pct_cut < .05] <- 0

sim <- postop(pct_cut$pct_cut, dat$trees, params, mods)

if (any(pct_cut$pct_cut == 1)) {
  pct_cut <- pct_cut[1:which(pct_cut$pct_cut == 1), ]
}
sim$pct_cut <- pct_cut

openxlsx::write.xlsx(sim, file = "output/ea_sugarbush_vt_nooverstocking.xlsx")


# Run longer simulation without logging to flesh out rotation logic ############
pct_cut_nocut <- rep(0, 1 + (params$endyr / params$steplength / params$timesteps_per_cuttingcycle))
sim_nocut <- postop(pct_cut_nocut, dat$trees, params, mods)
sim_nocut$pct_cut <-
  data.frame(stand_age = seq(params$start_age, params$start_age + params$endyr,
                             by = params$steplength * params$timesteps_per_cuttingcycle),
             pct_cut = pct_cut_nocut)
openxlsx::write.xlsx(sim_nocut, file = "output/ea_sugarbush_vt_nooverstocking_nocut.xlsx")


# Try one thinning to b-line ###################################################
pct_cut_one_thin <- rep(0, 1 + (params$endyr / params$steplength / params$timesteps_per_cuttingcycle))
pct_cut_one_thin[2] <- .4 # removes 40% when it's at 107 sq ft
sim_onethin <- postop(pct_cut_one_thin, dat$trees, params, mods)
sim_onethin$pct_cut <-
  data.frame(stand_age = seq(params$start_age, params$start_age + params$endyr,
                             by = params$steplength * params$timesteps_per_cuttingcycle),
             pct_cut = pct_cut_one_thin)
openxlsx::write.xlsx(sim_onethin, file = "output/ea_sugarbush_vt_nooverstocking_onethin.xlsx")
