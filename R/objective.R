#' Maple value from cutting schedule for optimizer
#'
#' Calculates average value per year of a single plot from a given harvest
#' schedule
#'
#' @param schedule a numeric vector of % trees cut at each cutting entry.
#' @param trees information about sampled trees in a single plot and their
#'   growing conditions. From a \code{simready} object. Trees must be length 1
#'   for this objective function, since a single tree represents the stand.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param model_cache ForestMakerModels model cache used to estimate growth and
#'   change. \code{diameter_change}, \code{height_change},
#'   \code{crownratio_change}, and \code{survival} models must be loaded for
#'   those respective uses. See documentation for \code{treemodeler} package.
#'
#' @return returns the average, per acre per year long run average value, based
#'   on the given management regime.
#' @export
obj <- function(schedule, trees, params, model_cache) {
  # if schedule[x] == 1, any(schedule[>x] == 0)
  # enforce constraint by altering schedule:
  all_cut <- F
  for (x in 1:length(schedule)) {
    if (all_cut) schedule[x] <- 0
    if (schedule[x] > .95) all_cut <- T
  }

  # schedule > .95 means cut all
  # schedule < .05 means cut none
  schedule[schedule > .95] <- 1
  schedule[schedule < .05] <- 0

  # translate cutting schedule to steps (typically 5 yr steps, 15 yr cutting
  # cycle, but defined in params)
  steps <- params$endyr / params$steplength
  cut_per_step <- rep(0, steps + 1)
  idx <- 1 + seq(0, steps, by = params$timesteps_per_cuttingcycle)
  cut_per_step[idx] <- schedule

  trees$cumsurv <- 1 # cumulative survival rate starts at 100%
  trees$sustainable <- TRUE # to track if growth rates are sustainable

  # rotation length forshortened if all trees cut early
  if (any(cut_per_step > .95)) {
    true_endyr <- (which(cut_per_step > .95) - 1) * params$steplength
  } else {
    true_endyr <- params$endyr
  }
  rotation_length <- true_endyr + params$start_age

  # running tally of net maple revenue
  mv <- 0
  # running tally of harvested timber value
  tv <- 0

  t <- 0
  # for each step record terminal values of harvest trees, update ba and bal,
  # and grow one step
  ##################### CANDIDATE FOR C++ LOOP? #######################################
  for (i in 1:steps) {
    if (cut_per_step[i] > 0) {
      revenue <-
        (timbertally::stumpage(trees, params$stump, params,
                               params$truckcost, model_cache) * # ignoring sapling precommercial cost, assuming trees start commercial
           trees$tpa * (cut_per_step[i]) * trees$cumsurv)
      tv <- tv + revenue # No present values because we'll simplify to a long-term average
    }

    if (cut_per_step[i] > .999) break

    # reduce tpa by pct cut
    trees$tpa <- trees$tpa * (1 - cut_per_step[i])
    # stocking modified by survival rate to account for mortality
    trees$ba <- sum(.005454 * (trees$dbh ^ 2) * trees$tpa * trees$cumsurv)
    # BAL is (on average) half of BA minus BA of a single tree
    trees$bal <- (trees$ba - (.005454 * (trees$dbh ^ 2))) / 2

    # will dbh growth be sustainable for maple production in this timestep?
    trees$sustainable <-
      treemodeler::dbh_change(trees, 1, model_cache) >
      ((2 * 1.5) / (pi * (trees$dbh - 2 * 1.5) / 1))

    # wreck grade of but log for tapped trees
    substr(trees$logs[trees$sustainable], 1, 1) <-
      as.character(
        pmax(as.numeric(substr(trees$logs[trees$sustainable], 1, 1)), 3)
      )

    # net annual revenue from maple for this timestep
    live_stems <- trees$tpa[trees$sustainable] *
      trees$cumsurv[trees$sustainable]
    syrup_yr <-
      # only include trees with sustainable growth rates this timestep
      sugarer::syrup_yield(trees$dbh[trees$sustainable],
                           trees$spp[trees$sustainable], min_dbh = 7.9,
                           model_cache = model_cache) *
      live_stems

    gross_annual <- sum(sugarer::gross_maple_revenue(syrup_yr))
    # for now assume one tap per tree
    costs_annual <- sugarer::sap_collection_costs(sum(live_stems))

    net_annual <- gross_annual - costs_annual

    for (i in 1:params$steplength) {
      mv <- mv + net_annual
    }

    trees <- data.frame(forestgrower::grow(trees, params, model_cache = model_cache))

    t <- t + params$steplength
  }

  cat("*")
  # returns average annual net revenue over entire rotation.
  return((mv + tv) / rotation_length)
}
