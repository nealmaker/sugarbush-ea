dat <- readRDS("../forestmaker/modules/forestgrower/tests/testthat/fixtures/example_simcruise.rds")
dat$trees <- dat$trees[1, ]
dat$plots <- dat$plots[1, ]

################################################################################
# Decision Variables

# one tree as idealized "treelist" (stand is tpa identical trees)
dat$trees$spp <- "SM"
dat$trees$dbh <- 8
dat$trees$cr <- 30
dat$trees$height <- 50
dat$trees$logs <- "2225555555"
dat$trees$tpa <- 250

# rich northern hardwood site on deep, loamy Salmon-Adamant complex soils
# along the Winooski River in Moretown VT, deposited in Glacial Lake Winooski.
dat$trees$lat <- 44.316
dat$trees$lon <- -72.7423
dat$trees$elev <- 700
dat$trees$site_class <- 4

# set parameters to guide simulations
params <- forestgrower::params_default
params$start_age <- 50
params$steplength <- 5
params$timesteps_per_cuttingcycle <- 3
# 15 cutting cycles after start age
params$endyr <- 15 * params$steplength * params$timesteps_per_cuttingcycle

################################################################################

dat$trees$ba_ac <- .005454 * dat$trees$dbh ^ 2
dat$trees$ba <- dat$trees$ba_ac * dat$trees$tpa
dat$trees$bal <- (dat$trees$ba - dat$trees$ba_ac) / 2
