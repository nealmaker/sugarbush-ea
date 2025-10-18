postop <- function(pct_cut, trees, params, model_cache) {
  # encode variables for use with models
  if (!is.null(model_cache$canonical_feature_spec$categories$spp)) {
    if (!is.integer(trees$spp)) {
      trees <-
        treemodeler::encode_simulation_inputs(trees, model_cache$region,
                                              model_cache$canonical_feature_spec)
    }
    if (!is.integer(params$prices$spp)) {
      params$prices <-
        treemodeler::encode_simulation_inputs(params$prices, model_cache$region,
                                              model_cache$canonical_feature_spec)
    }
  }

  # translate cutting schedule to steps (typically 5 yr steps, 15 yr cutting
  # cycle, but defined in params)
  steps <- params$endyr / params$steplength
  cut_per_step <- rep(0, steps + 1)
  idx <- 1 + seq(0, steps, by = params$timesteps_per_cuttingcycle)
  cut_per_step[idx] <- pct_cut

  trees$cumsurv <- 1 # cumulative survival rate starts at 100%
  trees$sustainable <- TRUE # to track if growth rates are sustainable
  trees$stand_age <- params$start_age
  trees_out <- trees

  # rotation length forshortened if all trees cut early
  if (any(cut_per_step > .999)) {
    true_endyr <- (which(cut_per_step > .999) - 1) * params$steplength
  } else {
    true_endyr <- params$endyr
  }
  rotation_length <- true_endyr + params$start_age

  # table of revenues and costs
  yield <- data.frame(stand_age = seq(params$start_age, rotation_length,
                                      by = params$steplength),
                      sap_yield_per_ac = 0,
                      sugar_content = 0,
                      syrup_yield_per_ac = 0,
                      gross_maple_revenue_per_ac = 0,
                      collection_costs_per_ac = 0,
                      net_maple_revenue_per_ac = 0,
                      timber_income_per_ac = 0)
  tv <- 0 # running tally of timber income
  mv <- 0 # running tally of net maple income

  t <- params$start_age
  # for each step record terminal values of harvest trees, update ba and bal,
  # and grow one step
  ##################### CANDIDATE FOR C++ LOOP? #######################################
  for (i in 1:steps) {
    if (cut_per_step[i] > .999) cut_per_step[i] <- 1
    if (cut_per_step[i] < .05) cut_per_step[i] <- 0

    if (cut_per_step[i] > 0) {
      yield$timber_income_per_ac[i] <-
        (timbertally::stumpage(trees, params$stump, params,
                               params$truckcost, model_cache) * # ignoring sapling precommercial cost, assuming trees start commercial
           trees$tpa * (cut_per_step[i]) * trees$cumsurv)
      tv <- tv + yield$timber_income_per_ac[i] # No present values because we'll simplify to a long-term average
    }

    if (cut_per_step[i] > .999) break

    # reduce tpa by pct cut
    trees$tpa <- trees$tpa * (1 - cut_per_step[i])
    # stocking modified by survival rate to account for mortality
    trees$ba <- sum(.005454 * (trees$dbh ^ 2) * trees$tpa * trees$cumsurv)
    # BAL is (on average) half of BA minus BA of a single tree
    trees$bal <- (trees$ba - trees$ba_ac) / 2

    # will dbh growth be sustainable for maple production in this timestep?
    trees$sustainable <-
      treemodeler::dbh_change(trees, 1, model_cache) >
      ((2 * 1.5) / (pi * (trees$dbh - 2 * 1.5) / 1))

    # wreck grade of but log for tapped trees
    substr(trees$logs[trees$sustainable], 1, 1) <-
      as.character(
        pmax(as.numeric(substr(trees$logs[trees$sustainable], 1, 1)), 3)
      )

    if (i == 1) {
      trees_out <- trees
    } else {
      trees$stand_age <- trees$stand_age + params$steplength
      trees_out <- rbind(trees_out, trees)
    }

    # net annual revenue from maple for this timestep
    live_stems <- trees$tpa[trees$sustainable] *
      trees$cumsurv[trees$sustainable]
    sap_yr <-
      sugarer::sap_yield(trees$dbh[trees$sustainable],
                         trees$spp[trees$sustainable], min_dbh = 7.9,
                         model_cache = model_cache) *
      live_stems

    sug_cont <-
      sugarer::sugar_content(trees$dbh[trees$sustainable],
                             trees$spp[trees$sustainable],
                             model_cache = model_cache)

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

    yield$sap_yield_per_ac[i] <- sap_yr * params$steplength
    yield$sugar_content[i] <- sug_cont
    yield$syrup_yield_per_ac[i] <- syrup_yr * params$steplength
    yield$gross_maple_revenue_per_ac[i] <- gross_annual * params$steplength
    yield$collection_costs_per_ac[i] <- costs_annual * params$steplength
    yield$net_maple_revenue_per_ac[i] <- net_annual * params$steplength

    for (i in 1:params$steplength) {
      mv <- mv + net_annual
    }

    trees <- data.frame(forestgrower::grow(trees, params, model_cache = model_cache))

    t <- t + params$steplength
  }

  summary_tab <-
    data.frame(rotation_length = rotation_length,
               avg_maple_income_peryr_over_rotation = mv / rotation_length,
               avg_timber_income_peryr_over_rotation = tv / rotation_length)

  # returns list of tables
  return(list(summary = summary_tab,
              yields = yield,
              trees = trees_out))
}
