#' @title
#' Run simu
#'
#' @description
#' Wrapper function to simulate data, fit tree, and evaluate outcomes
#'
#' @param gps_spec: gps matching setting
#' @param num_exposure_cats: the number of categories to bin the exposure
#' level into for stratification
#' @param sample_size: sample size for data generation
#' @param em_spec: effect modifier generation setting
#' @param heterogenous_intercept: adding heterogenous intercepts for particular settings
#' @param beta: various levels of beta to test
#' @param outcome_sd: specify outcome generation
#' @param correct_splits: correct splits of particular setting
#' @param true_trt_effect_func: function passed in to get true effect
#' @param noise.var: noise variables
#' @param n_trials: number of trials for simulation
#' @param lambdas: various levels of lambda to test
#' @param stopping.rule: stopping rule
#' @param true_modifiers: the true modifiers for particular setting
#' @param regenerate_covs: whether to regenerate covariates each time we generate (UNUSED)
#'
#' @returns dataframe with one row for each tree in sequence and lambda value,
#' representing the ability of the tree to explain the effect heterogeneity
run_simu <- function(gps_spec = 1,
                     num_exposure_cats,
                     sample_size = 20000,
                     em_spec = 1,
                     heterogenous_intercept = FALSE,
                     beta = NULL,
                     outcome_sd = 1,
                     correct_splits = NULL,
                     true_trt_effect_func = NULL,
                     noise.var,
                     n_trials,
                     lambdas,
                     stopping.rule,
                     exploration.sample_covs = NULL,
                     inference.sample_covs = NULL,
                     val.sample_covs = NULL,
                     matched.exploration.sample = NULL,
                     matched.validation.sample = NULL,
                     matched.inference.sample = NULL,
                     true_modifiers = c(),
                     regenerate_covs = 0) {
  # If the matched covariate dataset (everything but the outcome) is not passed in, generate one
  if (is.null(matched.exploration.sample) | is.null(matched.validation.sample) | is.null(matched.inference.sample) |
    is.null(exploration.sample_covs) | is.null(val.sample_covs) | is.null(inference.sample_covs)) {
    # generate covariate data
    synth_data_covs <- generate_syn_data_covs(sample_size = sample_size, gps_spec = gps_spec)

    # Discretize treatment values for gps matching
    a.vals <- seq(min(synth_data_covs$treat), max(synth_data_covs$treat), length.out = num_exposure_cats)
    delta_n <- a.vals[2] - a.vals[1]

    synth_data_covs <-
      synth_data_covs %>%
      mutate(treat_level = cut(treat, breaks = a.vals)) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)

    # Split data into subsamples, stratifying on exposure level bins
    synth_data_covs <-
      split_dataset(data = synth_data_covs, num_exposure_cats = num_exposure_cats)

    exploration.sample_covs <-
      synth_data_covs %>%
      filter(subsample == "exploration") %>%
      tibble::rowid_to_column("orig_id")

    val.sample_covs <-
      synth_data_covs %>%
      filter(subsample == "validation") %>%
      tibble::rowid_to_column("orig_id")

    inference.sample_covs <-
      synth_data_covs %>%
      filter(subsample == "inference") %>%
      tibble::rowid_to_column("orig_id")

    # GPS matching within subsamples and effect modifier groups
    matched.exploration.sample <-
      stratified_GPS_matching(exploration.sample_covs, delta_n,
        exposure_name = "treat",
        confounders_names = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
        names_of_strata_vars = c("em1", "em2", "em3", "em4"),
        outcome_name = NA
      ) %>%
      mutate(id = row_number()) %>%
      # rename(w = treat) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)

    matched.validation.sample <-
      stratified_GPS_matching(val.sample_covs, delta_n,
        exposure_name = "treat",
        confounders_names = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
        names_of_strata_vars = c("em1", "em2", "em3", "em4"),
        outcome_name = NA
      ) %>%
      mutate(id = row_number()) %>%
      # rename(w = treat) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)

    matched.inference.sample <-
      stratified_GPS_matching(inference.sample_covs, delta_n,
        exposure_name = "treat",
        confounders_names = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
        names_of_strata_vars = c("em1", "em2", "em3", "em4"),
        outcome_name = NA
      ) %>%
      mutate(id = row_number()) %>%
      # rename(w = treat) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- data.frame()
  if (n_trials > 0) {
    for (i in 1:n_trials) {
      # generate outcome data
      exploration.sample_outcome <- generate_syn_data_outcome(
        cf = select(exploration.sample_covs, c(cf1, cf2, cf3, cf4, cf5, cf6)),
        em = select(exploration.sample_covs, c(em1, em2, em3, em4)),
        treat = exploration.sample_covs["treat"],
        outcome_sd = outcome_sd,
        em_spec = em_spec,
        heterogenous_intercept = heterogenous_intercept,
        beta = beta
      ) %>% tibble::rowid_to_column()


      validation.sample_outcome <- generate_syn_data_outcome(
        cf = select(val.sample_covs, c(cf1, cf2, cf3, cf4, cf5, cf6)),
        em = select(val.sample_covs, c(em1, em2, em3, em4)),
        treat = val.sample_covs["treat"],
        outcome_sd = outcome_sd,
        em_spec = em_spec,
        heterogenous_intercept = heterogenous_intercept,
        beta = beta
      ) %>% tibble::rowid_to_column()

      inference.sample_outcome <- generate_syn_data_outcome(
        cf = select(inference.sample_covs, c(cf1, cf2, cf3, cf4, cf5, cf6)),
        em = select(inference.sample_covs, c(em1, em2, em3, em4)),
        treat = inference.sample_covs["treat"],
        outcome_sd = outcome_sd,
        em_spec = em_spec,
        heterogenous_intercept = heterogenous_intercept,
        beta = beta
      ) %>% tibble::rowid_to_column()

      # add outcome data into matched samples
      matched.exploration.sample.outcomes <-
        matched.exploration.sample %>%
        left_join(exploration.sample_outcome, by = c(
          "orig_id" = "rowid", "cf1", "cf2", "cf3", "cf4", "cf5", "cf6", "em1", "em2", "em3", "em4",
          "treat"
        ))

      matched.validation.sample.outcomes <-
        matched.validation.sample %>%
        left_join(validation.sample_outcome, by = c(
          "orig_id" = "rowid", "cf1", "cf2", "cf3", "cf4", "cf5", "cf6", "em1", "em2", "em3", "em4",
          "treat"
        ))
      matched.inference.sample.outcomes <-
        matched.inference.sample %>%
        left_join(inference.sample_outcome, by = c(
          "orig_id" = "rowid", "cf1", "cf2", "cf3", "cf4", "cf5", "cf6", "em1", "em2", "em3", "em4",
          "treat"
        ))

      CCIT_results <- CCIT(
        matched.exploration.sample.outcomes, matched.validation.sample.outcomes, matched.inference.sample.outcomes, lambdas, stopping.rule
      )

      est.treatment.effects <- CCIT_results$est.treatment.effects

      selected.trees <- CCIT_results$selected.trees

      tree.list <- CCIT_results$tree.list

      selected.tree.size <- CCIT_results$selected.tree.size

      true_trt_effects <- true_trt_effect_func(est.treatment.effects[[1]])

      mse <- sapply(est.treatment.effects, function(est.treatment.effect) {
        mean((est.treatment.effect$est - true_trt_effects$eff)^2)
      })

      bias <- sapply(est.treatment.effects, function(est.treatment.effect) {
        mean((est.treatment.effect$est - true_trt_effects$eff))
      })

      variance <- sapply(est.treatment.effects, function(est.treatment.effect) {
        mean((est.treatment.effect$est - true_trt_effects$eff)^2) - mean((est.treatment.effect$est - true_trt_effects$eff))^2
      })

      # Whether selected trees have the correct splits
      selected.correct.splits <- sapply(selected.trees, function(t) {
        list(row.names(t$splits)) %in% correct_splits
      })

      # Whether one of the trees in the generated sequence is correct
      correct.tree.in.sequence <- any(sapply(tree.list, function(t) {
        list(row.names(t$splits)) %in% correct_splits
      }))

      # Number of noise variables selected for all trees
      numb.noise <- sapply(selected.trees, function(t) {
        sum(t$frame$var %in% noise.var)
      })

      # Effect modifier true positives
      em.true.positives <- sapply(selected.trees, function(t) {
        length(intersect(list(row.names(t$splits))[[1]], correct_splits[[1]]))
      })

      # Effect modifier false positives
      em.false.positives <- sapply(selected.trees, function(t) {
        length(setdiff(list(row.names(t$splits))[[1]], correct_splits[[1]]))
      })

      # Effect modifier false negatives
      em.false.negatives <- sapply(selected.trees, function(t) {
        length(setdiff(correct_splits[[1]], list(row.names(t$splits))[[1]]))
      })

      # Find number of true positives
      # Inputs: parsed leaves and true modifiers
      tp <- function(indicators, trues) {
        sum <- 0
        if (length(indicators) == 0) {
          return(0)
        }
        for (i in 1:length(indicators)) {
          for (j in 1:length(trues)) {
            if (setequal(indicators[[i]], trues[[j]])) {
              sum <- sum + 1
              trues[[j]] <- NULL
              break
            }
          }
        }
        return(sum)
      }

      # Find number of false positives
      # Inputs: parsed leaves and true modifiers
      fpdiff <- function(indicators, trues) {
        sum <- length(indicators)
        if (length(indicators) == 0) {
          return(0)
        }
        for (i in 1:length(indicators)) {
          for (j in 1:length(trues)) {
            if (setequal(indicators[[i]], trues[[j]])) {
              sum <- sum - 1
              trues[[j]] <- NULL
              break
            }
          }
        }
        return(sum)
      }

      # Find number of false negatives
      # Inputs: parsed leaves and true modifiers
      fndiff <- function(indicators, trues) {
        sum <- length(trues)
        if (length(indicators) == 0) {
          return(0)
        }

        for (j in 1:length(trues)) {
          for (i in 1:length(indicators)) {
            if (setequal(indicators[[i]], trues[[j]])) {
              sum <- sum - 1
              indicators[[i]] <- NULL
              break
            }
          }
        }
        return(sum)
      }

      # For parsing tree leaves
      # Inputs: current index of rpart frame var list
      parse_leaves <- function(index) {
        if (index > length(ll)) {
        } else {
          highest <<- max(highest, index)
          if (ll[index] == "<leaf>") {
            leaves[[length(leaves) + 1]] <<- stack
          } else {
            stack <<- c(stack, ll[index])
            tt <- stack
            parse_leaves(index + 1)
            parse_leaves(highest + 1)
            stack <<- stack[1:length(stack) - 1]
          }
        }
      }

      # Initialize tree parsing and return true positives
      tp_helper <- function(frame, true_modifiers) {
        ll <<- c(frame)
        leaves <<- list()
        highest <<- 1
        stack <<- c()
        parse_leaves(1)
        return(tp(leaves, true_modifiers))
      }

      dr.true.positives <- sapply(selected.trees, function(t) {
        tp_helper(t$frame$var, true_modifiers)
      })

      dr.false.positives <- sapply(selected.trees, function(t) {
        fpdiff(t$frame$var, true_modifiers)
      })

      dr.false.negatives <- sapply(selected.trees, function(t) {
        fndiff(t$frame$var, true_modifiers)
      })

      iter_results <-
        selected.tree.size %>%
        mutate(
          selected.trees = selected.trees,
          selected.tree.size = selected.tree.size$tree.size,
          selected.correct.splits = selected.correct.splits,
          correct.tree.in.sequence = correct.tree.in.sequence,
          mse = mse,
          bias = bias,
          variance = variance,
          numb.noise = numb.noise,
          em.true.positives = em.true.positives,
          em.false.positives = em.false.positives,
          em.false.negatives = em.false.negatives,
          dr.true.positives = dr.true.positives,
          dr.false.positives = dr.false.positives,
          dr.false.negatives = dr.false.negatives,
          iter = i
        )

      results <- rbind(results, iter_results)
    }
  }
  return(list(
    results = results, matched.exploration.sample = matched.exploration.sample,
    matched.inference.sample = matched.inference.sample,
    matched.validation.sample = matched.validation.sample,
    inference.sample_covs = inference.sample_covs,
    exploration.sample_covs = exploration.sample_covs,
    val.sample_covs = val.sample_covs
  ))
}
