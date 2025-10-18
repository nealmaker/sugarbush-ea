source("R/setup.R")
source("R/objective.R")
source("R/optimizer.R")
source("R/postop.R")

mods <- treemodeler::initialize_models("5.3", c("slim", "base"),
                                       c("taper", "growth"),
                                       cr_mode = "derived")

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
