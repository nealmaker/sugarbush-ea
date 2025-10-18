#' optimize management
#'
#' find optimal management regime for even-aged sugarbush
#'
#' @param data a \code{simcruise} object containing information about sample
#'   plots, sample trees, and their growing conditions.
#' @param params a \code{sim_params} object containing parameters to guide the
#'   simulation.
#' @param model_cache ForestMakerModels model cache used to estimate growth and
#'   change. \code{diameter_change}, \code{height_change},
#'   \code{crownratio_change}, and \code{survival} models must be loaded for
#'   those respective uses. See documentation for \code{treemodeler} package.
#'
#' @return list of outputs, including a data frame of trees (\code{trees}) with
#'   their harvest years (\code{cutyr}), a data frame of plots (\code{plots}),
#'   the \code{objective} used for the optimization, and a list of parameters used
#'   to guide the optimization (\code{params}).
#' @export
opt_mngmt <- function(data, params, model_cache){
  timesteps_per_cuttingcycle <- params$timesteps_per_cuttingcycle
  trees <- data$trees

  # encode variables for use with models
  if (!is.null(model_cache$canonical_feature_spec$categories$spp)) {
    if (!is.integer(data$trees$spp)) {
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

  treesgo <- trees

  out <- lapply(unique(treesgo$plot), function(i) {
    treesg <- dplyr::filter(treesgo, plot == i)
    treelength <- nrow(treesg)

    # Decision space is what pct trees to cut at each entry
    # nvars is number of entries (once per cutting cycle)
    nvars <- params$endyr / params$steplength / timesteps_per_cuttingcycle + 1
    domains <- matrix(c(rep(0, nvars),
                        rep(1, nvars)),
                      ncol = 2)

    pct_cut <- rgenoud::genoud(obj,
                               nvars = nvars,
                               max = TRUE,
                               pop.size = 50,
                               max.generations = 100,
                               wait.generations = 30,
                               hard.generation.limit = TRUE,
                               Domains = domains,
                               solution.tolerance = 2,
                               boundary.enforcement = 2,
                               gradient.check = FALSE, # faster
                               BFGS = FALSE, # faster but not as honed in with smooth solution spaces
                               data.type.int = FALSE,
                               print.level = 2,
                               trees = treesg,
                               params = params,
                               model_cache = model_cache)$par

    return(data.frame(stand_age = seq(params$start_age,
                                      params$endyr + params$start_age,
                                      by = params$steplength *
                                        params$timesteps_per_cuttingcycle),
                      pct_cut = pct_cut))
  })

  return(list(pct_cut = out, params = params))
}
