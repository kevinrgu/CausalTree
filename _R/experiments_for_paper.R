<<<<<<< HEAD
# Load in relevant scripts ——————————————————————
source_dir <- "~/Desktop/CCIT"
dir_out <- "~/Desktop/CCIT/simulated_data_09122022/"
source(paste0(source_dir, "/R/evaluate.sequence.R"))
source(paste0(source_dir, "/_R/run_simu.R"))
source(paste0(source_dir, "/R/create.sequence.R"))
source(paste0(source_dir, "/R/rpart_funcs.R"))
source(paste0(source_dir, "/R/stratified_GPS_matching.R"))
source(paste0(source_dir, "/R/split_dataset.R"))
source(paste0(source_dir, "/R/generate_synthetic_data_covs.R"))
source(paste0(source_dir, "/R/generate_synthetic_data_outcome.R"))
source(paste0(source_dir, "/R/CCIT.R"))

library("devtools")
install_github("fasrc/CausalGPS", ref = "master")
=======
# Load in relevant scripts
# all R scripts listed below must be lcoated in source_dir
source_dir <- '/Users/ninakatz-christy/CCIT_Final_refactored/CCIT'
dir_out <- '/Users/ninakatz-christy/Documents/CCIT/Code/CCIT/simulated_data_09122022/'
source(paste0(source_dir, '/_R/evaluate.sequence.R'))
source(paste0(source_dir, '/_R/run_simu.R'))
source(paste0(source_dir, '/_R/create.sequence.R'))
source(paste0(source_dir, '/_R/rpart_funcs.R'))
source(paste0(source_dir, '/_R/stratified_GPS_matching.R'))
source(paste0(source_dir, '/_R/split_dataset.R'))
source(paste0(source_dir, '/_R/generate_synthetic_data_covs.R'))
source(paste0(source_dir, '/_R/generate_synthetic_data_outcome.R'))
source(paste0(source_dir, '/_R/CCIT.R'))
library("devtools")
install_github("fasrc/CausalGPS", ref="master")
>>>>>>> na/main
library(CausalGPS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)
library(truncnorm)
library(caret)
library(rpart)
library(rpart.plot)
library(scales)
library(data.table)
<<<<<<< HEAD
library(caret)
library(gridExtra)
library(xtable)

# Run simulation without generating outcome ——————————————————————

# Data generation: split into subsamples, perform gps matching
# No confounding
matched <- run_simu(
  num_exposure_cats = 20,
  gps_spec = 1,
  sample_size = 20000, n_trials = 0
)
save(matched, file = paste0(source_dir, "/simulated_data_09122022/matched.RData"))
load(paste0(source_dir, "/simulated_data_09122022/matched.RData"))

# Confounding
matched.c <- run_simu(
  num_exposure_cats = 20,
  gps_spec = 2, sample_size = 20000, n_trials = 0
)
save(matched.c, file = paste0(source_dir, "/simulated_data_09122022/matched.c.RData"))
load(paste0(source_dir, "/simulated_data_09122022/matched.c.RData"))

# Effect modifier confounding in treatment
matchedEM.c <- run_simu(
  num_exposure_cats = 20,
  gps_spec = 3, sample_size = 20000, n_trials = 0
)
save(matchedEM.c, file = paste0(source_dir, "/simulated_data_09122022/matchedEM.c.RData"))
load(paste0(source_dir, "/simulated_data_09122022/matchedEM.c.RData"))

# Plot overall covariate balance ——————————————————————
unmatched.no.c.explor <- matched$exploration.sample_covs %>%
  as.data.table()
matched.no.c.explor <- matched$matched.exploration.sample %>%
  as.data.table()
unmatched.c.explor <- matched.c$exploration.sample_covs %>%
  as.data.table()
matched.c.explor <- matched.c$matched.exploration.sample %>%
  as.data.table()
unmatched_cor.no.c <-
  absolute_corr_fun(unmatched.no.c.explor[, "treat"], unmatched.no.c.explor[, 1:6])
matched_cor.no.c <-
  absolute_corr_fun(matched.no.c.explor[, "treat"], matched.no.c.explor[, 6:11])
unmatched_cor.c <-
  absolute_corr_fun(unmatched.c.explor[, "treat"], unmatched.c.explor[, 1:6])
matched_cor.c <-
  absolute_corr_fun(matched.c.explor[, "treat"], matched.c.explor[, 6:11])
abs_cor <-
  rbind(
    data.frame(
      cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
      cor = unmatched_cor.no.c$absolute_corr, matched = FALSE, confounding = "No Confounding"
    ),
    data.frame(
      cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
      cor = matched_cor.no.c$absolute_corr, matched = TRUE, confounding = "No Confounding"
    ),
    data.frame(
      cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
      cor = unmatched_cor.c$absolute_corr, matched = FALSE, confounding = "Confounding"
    ),
    data.frame(
      cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
      cor = matched_cor.c$absolute_corr, matched = TRUE, confounding = "Confounding"
    )
  )
ggplot(abs_cor, aes(x = cov, y = cor, color = matched, group = matched)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(confounding)) +
  theme(axis.text.x = element_text(angle = 45))
ggsave(paste0(source_dir, "/cov_balance_overall.png"), width = 10)

# Plot covariate balance within effect modifier strata ——————————————————————
abs_cor <- data.frame()
for (i in c(0, 1)) {
  for (j in c(0, 1)) {
    for (k in c(0, 1)) {
      for (l in c(0, 1)) {
        unmatched.no.c.explor <- matched$exploration.sample_covs %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table()
        matched.no.c.explor <- matched$matched.exploration.sample %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table()
        unmatched.c.explor <- matched.c$exploration.sample_covs %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table()
        matched.c.explor <- matched.c$matched.exploration.sample %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table()

        unmatched_cor.no.c <-
          absolute_corr_fun(unmatched.no.c.explor[, "treat"], unmatched.no.c.explor[, 1:6])
        matched_cor.no.c <-
          absolute_corr_fun(matched.no.c.explor[, "treat"], matched.no.c.explor[, 6:11])
        unmatched_cor.c <-
          absolute_corr_fun(unmatched.c.explor[, "treat"], unmatched.c.explor[, 1:6])
        matched_cor.c <-
          absolute_corr_fun(matched.c.explor[, "treat"], matched.c.explor[, 6:11])

        abs_cor <-
          rbind(
            abs_cor,
            data.frame(
              cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
              cor = unmatched_cor.no.c$absolute_corr, matched = FALSE, confounding = "No Confounding", em1 = i, em2 = j, em3 = k, em4 = l
            ),
            data.frame(
              cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
              cor = matched_cor.no.c$absolute_corr, matched = TRUE, confounding = "No Confounding", em1 = i, em2 = j, em3 = k, em4 = l
            ),
            data.frame(
              cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
              cor = unmatched_cor.c$absolute_corr, matched = FALSE, confounding = "Confounding", em1 = i, em2 = j, em3 = k, em4 = l
            ),
            data.frame(
              cov = c("cf1", "cf2", "cf3", "cf4", "cf5", "cf6"),
              cor = matched_cor.c$absolute_corr, matched = TRUE, confounding = "Confounding", em1 = i, em2 = j, em3 = k, em4 = l
            )
          )
      }
    }
  }
}
ggplot(abs_cor, aes(x = cov, y = cor, color = matched, group = matched)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(confounding), cols = vars(em1, em2, em3, em4)) +
  theme(axis.text.x = element_text(angle = 45))
ggsave(paste0(source_dir, "/cov_balance_by_strata.png"), width = 10)

# Debugging settings ——————————————————————
options(dplyr.width = Inf)

# Setting 1: 4 groups, same intercept ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"))
noise.var <- c("em3", "em4")
beta <- c(1)
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 1 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {
      as.numeric(as.character(e))
    }) %>%
    mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
  gps_spec = 2, em_spec = 1,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matched.c$inference.sample_covs,
  matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting1.RData"))

# Setting 2: 3 groups, same intercept ——————————————————————
correct_splits <- list(c("em2", "em1"))
true_modifiers <- list(c("em2", "em1"))
noise.var <- c("em3", "em4")
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 2 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {
      as.numeric(as.character(e))
    }) %>%
    mutate(eff = 10 * (1 + 0.5 * em1 * em2 + 0.2 * em2)) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
  gps_spec = 2, em_spec = 2,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matched.c$inference.sample_covs,
  matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting2.RData"))

# Setting 3: 4 groups, different intercept ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"))
noise.var <- c("em3", "em4")
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 3 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {
      as.numeric(as.character(e))
    }) %>%
    mutate(eff = 10 * (1 + 0.5 * em1 - 0.8 * em2)) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
  gps_spec = 2, em_spec = 1,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, heterogenous_intercept = TRUE, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matched.c$inference.sample_covs,
  matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting3.RData"))

# Setting 4: 3 groups, different intercept (DELETED FROM PAPER) ——————————————————————
# correct_splits <- list(c("em2", "em1"))
# noise.var <- c("em3", "em4")
# lambdas <- seq(0, 20, by = 1)

# true_trt_effect_func <- function(df) {
#   df %>%
#     mutate_at(vars(em1, em2, em3, em4), function(e) {
#       as.numeric(as.character(e))
#     }) %>%
#     mutate(eff = 10 * (1 + 0.5 * em1 * em2 + 0.2 * em2)) %>%
#     mutate_at(vars(em1, em2, em3, em4), as.factor)
# }

# results <- run_simu(
#   lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var, gps_spec = 2, em_spec = 2,
#   true_trt_effect_func = true_trt_effect_func, beta = beta,
#   sample_size = 20000, heterogenous_intercept = TRUE, stopping.rule = TRUE, n_trials = 200,
#   exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
#   inference.sample_covs = matched.c$inference.sample_covs,
#   matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
#   matched.inference.sample = matched.c$matched.inference.sample
# )

# tree.data <- results$results

# save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting4.RData"))

# Setting 5: no effect modification ——————————————————————
correct_splits <- list(c())
noise.var <- c("em1", "em2", "em3", "em4")
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 5 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate(eff = 10)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var, gps_spec = 2, em_spec = 0,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, heterogenous_intercept = FALSE, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matched.c$inference.sample_covs,
  matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = correct_splits
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting5.RData"))

# Setting 6: confounding with effect modifier ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"))
noise.var <- c("em3", "em4")
beta <- c(1)
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 6 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {
      as.numeric(as.character(e))
    }) %>%
    mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
  gps_spec = 3, em_spec = 1,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matchedEM.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matchedEM.c$inference.sample_covs,
  matched.exploration.sample = matchedEM.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matchedEM.c$matched.inference.sample, true_modifiers = true_modifiers
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting6.RData"))

# Setting 7: non-linearity ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"))
noise.var <- c("em3", "em4")
beta <- c(1)
lambdas <- seq(0, 20, by = 1)

# Reports the true effect eff for setting 7 given the effect modifiers after passing in the dataframe
true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {
      as.numeric(as.character(e))
    }) %>%
    mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(
  lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
  gps_spec = 2, em_spec = 3,
  true_trt_effect_func = true_trt_effect_func, beta = beta,
  sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
  exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
  inference.sample_covs = matched.c$inference.sample_covs,
  matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
  matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
)

tree.data <- results$results

save(tree.data, file = paste0(dir_out, "selected.tree.data.with.stopping.setting7.RData"))

# Plotting results for 1 and 6 (TESTING) ——————————————————————
# selected.tree.data.combined <- data.frame()

# for (i in c(1, 6)) {
#   load(file = paste0(dir_out, "selected.tree.data.with.stopping.setting", i, ".RData"))
#   selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(setting = i))
# }

# setting.labs <- c("Setting 1", "Setting 6")
# names(setting.labs) <- c(1, 6)

# selected.tree.data.combined %>%
#   filter(lambda == 1) %>%
#   tidyr::gather("metric", "value", c(bias, mse)) %>%
#   ggplot(aes(y = value, group = lambda)) +
#   geom_boxplot() +
#   facet_grid(rows = vars(metric), cols = vars(setting), labeller = labeller(setting = setting.labs), scales = "free_y") +
#   labs(y = NULL) +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# ggsave(file = paste0(dir_out, "mse.biase.with.stopping.no.c.1_6.png"))

# selected.tree.data.combined %>%
#   group_by(beta) %>%
#   summarise_at(vars(selected.correct.splits), mean)

# Plotting setting 7 (for testing) ——————————————————————
# selected.tree.data.combined <- data.frame()

# for (i in c(7)) {
#   load(file = paste0(dir_out, "selected.tree.data.with.stopping.setting", i, ".RData"))
#   selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(setting = i))
# }

# setting.labs <- c("Setting 7")
# names(setting.labs) <- c(7)

# selected.tree.data.combined %>%
#   filter(lambda == 1) %>%
#   tidyr::gather("metric", "value", c(bias, mse)) %>%
#   ggplot(aes(y = value, group = lambda)) +
#   geom_boxplot() +
#   facet_grid(rows = vars(metric), cols = vars(setting), labeller = labeller(setting = setting.labs), scales = "free_y") +
#   labs(y = NULL) +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# ggsave(file = paste0(dir_out, "mse.biase.with.stopping.no.c.7.png"))

# selected.tree.data.combined %>%
#   group_by(beta) %>%
#   summarise_at(vars(selected.correct.splits), mean)

# selected.tree.data.combined %>%
#   group_by(beta, lambda) %>%
#   summarise_at(vars(selected.correct.splits, bias, mse, selected.tree.size), mean)

# Combining results for various settings ——————————————————————
selected.tree.data.combined <- data.frame()

paper <- c(5, 1, 2, 3, 7)
supp <- c(5, 1, 2, 3, 7, 6)

use <- supplementary
for (i in 1:length(use)) {
  print(i)
  load(file = paste0(dir_out, "selected.tree.data.with.stopping.setting", use[i], ".RData"))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(setting = i))
}

# Table ———————————
# Results summaries after selecting minimum MSE for each iteration
splicer <- function(df) {
  return(df %>%
    group_by(setting, iter) %>%
    slice(which.min(mse)))
}

result_summaries <- splicer(selected.tree.data.combined) %>%
  group_by(setting) %>%
  summarise(
    "bias-Q50" = quantile(bias, 0.50),
    "variance-Q50" = quantile(variance, 0.50),
    "mse-Q50" = quantile(mse, 0.50)
  )

xtable(result_summaries)

# Graph ———————————
setting.labs <- c("Setting 0", "Setting 1", "Setting 2", "Setting 3", "Setting 4", "Setting 5")
names(setting.labs) <- c(1, 2, 3, 4, 5, 6)
splicer(selected.tree.data.combined) %>%
  tidyr::gather("metric", "value", c(bias, mse)) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_grid(rows = vars(metric), cols = vars(setting), labeller = labeller(setting = setting.labs), scales = "free_y") +
  labs(y = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file = paste0(dir_out, "mse.biase.with.stopping.no.c.png"))

# Data for lambda graphs ———————————
# Effect modifiers
lambda_em_metrics <- data.frame(matrix(nrow = 21))
lambda_em_metrics["lambdas"] <- lambdas

for (i in c(1, 2, 3, 5, 6, 7)) {
  lambda_recalls <- c()
  lambda_precisions <- c()
  lambda_f1s <- c()
  for (l in lambdas) {
    selected_data <- selected.tree.data.combined %>% filter(lambda == l, setting == i)
    recall <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.negatives) + sum(selected_data$em.true.positives))
    precision <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.positives) + sum(selected_data$em.true.positives))
    f1 <- 2 * recall * precision / (recall + precision)

    lambda_recalls <- c(lambda_recalls, recall)
    lambda_precisions <- c(lambda_precisions, precision)
    lambda_f1s <- c(lambda_f1s, f1)
  }
  lambda_em_metrics[paste("r", as.character(i))] <- lambda_recalls
  lambda_em_metrics[paste("p", as.character(i))] <- lambda_precisions
  lambda_em_metrics[paste("f", as.character(i))] <- lambda_f1s
}

write.csv(lambda_em_metrics, file = "all_settings_lambda_em.csv", row.names = FALSE)

# Decision rules
lambda_dr_metrics <- data.frame(matrix(nrow = 21))
lambda_dr_metrics["lambdas"] <- lambdas

for (i in c(1, 2, 3, 5, 6, 7)) {
  lambda_recalls <- c()
  lambda_precisions <- c()
  lambda_f1s <- c()
  for (l in lambdas) {
    selected_data <- selected.tree.data.combined %>% filter(lambda == l, setting == i)
    recall <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.negatives) + sum(selected_data$dr.true.positives))
    precision <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.positives) + sum(selected_data$dr.true.positives))
    f1 <- 2 * recall * precision / (recall + precision)

    lambda_recalls <- c(lambda_recalls, recall)
    lambda_precisions <- c(lambda_precisions, precision)
    lambda_f1s <- c(lambda_f1s, f1)
  }
  lambda_dr_metrics[paste("r", as.character(i))] <- lambda_recalls
  lambda_dr_metrics[paste("p", as.character(i))] <- lambda_precisions
  lambda_dr_metrics[paste("f", as.character(i))] <- lambda_f1s
}

write.csv(lambda_dr_metrics, file = "all_settings_lambda_dr.csv", row.names = FALSE)

# Bias, variance, MSE, tree size
res <- selected.tree.data.combined %>%
  group_by(setting, lambda) %>%
  summarise(selected.tree.size = mean(selected.tree.size), bias = mean(bias), mse = mean(mse), variance = mean(variance))

write.csv(res, file = "lambda_metrics.csv", row.names = FALSE)

# SETTING 1 EM/DR GRAPH DATA ————————————————————————————————————————————————————————————————————————————————————————
# Setting 1 with different beta value and lambda ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"), c("em2", "em1"), c("em2", "em1"), c("em2", "em1"))
noise.var <- c("em3", "em4")

lambdas <- seq(1, 16, by = 1)
betas <- seq(0.05, 1, by = 0.05)
b2 <- seq(1.5, 5, by = 0.5)
betas <- c(betas, b2)

for (beta in betas) {
  # Reports the true effect eff for setting 1 given the effect modifiers after passing in the dataframe
  true_trt_effect_func <- function(df) {
    df %>%
      mutate_at(vars(em1, em2, em3, em4), function(e) {
        as.numeric(as.character(e))
      }) %>%
      mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- run_simu(
    lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var, gps_spec = 2, em_spec = 1,
    true_trt_effect_func = true_trt_effect_func, beta = beta,
    sample_size = 20000, heterogenous_intercept = FALSE, stopping.rule = FALSE, n_trials = 200,
    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
    inference.sample_covs = matched.c$inference.sample_covs,
    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
    matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers, regenerate_covs = 0
  )
  tree.data <- results$results
  save(tree.data, file = paste0(dir_out, "selected.tree.data.no.stopping.setting1.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
}

# Results for setting 1 with different beta and lambda ——————————————————————
selected.tree.data.combined <- data.frame()

for (beta in betas[21:28]) {
  print(beta)
  load(file = paste0(dir_out, "selected.tree.data.no.stopping.setting1.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = FALSE))
}

# Preprocessing functions for effect modifiers ———————————
get_em_recalls <- function(x, output) {
  if (x$em.true.positives + x$em.false.negatives == 0) {
    return(0)
  } else {
    return(x$em.true.positives / (x$em.true.positives + x$em.false.negatives))
  }
}

get_em_precisions <- function(x, output) {
  if (x$em.true.positives + x$em.false.positives == 0) {
    return(0)
  } else {
    return(x$em.true.positives / (x$em.true.positives + x$em.false.positives))
  }
}

get_em_f1s <- function(x, output) {
  if (x$em_recalls + x$em_precisions == 0) {
    return(0)
  } else {
    return(2 * x$em_recalls * x$em_precisions / (x$em_recalls + x$em_precisions))
  }
}

cv_em <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(em_f1s)))
}

selected.tree.data.combined$em_recalls <- apply(selected.tree.data.combined, 1, get_em_recalls)
selected.tree.data.combined$em_precisions <- apply(selected.tree.data.combined, 1, get_em_precisions)
selected.tree.data.combined$em_f1s <- apply(selected.tree.data.combined, 1, get_em_f1s)

# Preprocessing functions for decision rules ———————————
get_dr_recalls <- function(x, output) {
  if (x$dr.true.positives + x$dr.false.negatives == 0) {
    return(0)
  }
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.negatives))
}

get_dr_precisions <- function(x, output) {
  if (x$dr.true.positives + x$dr.false.positives == 0) {
    return(0)
  }
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.positives))
}

get_dr_f1s <- function(x, output) {
  if (x$dr_recalls + x$dr_precisions == 0) {
    return(0)
  } else {
    return(2 * x$dr_recalls * x$dr_precisions / (x$dr_recalls + x$dr_precisions))
  }
}

cv_dr <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(dr_f1s)))
}

selected.tree.data.combined$dr_recalls <- apply(selected.tree.data.combined, 1, get_dr_recalls)
selected.tree.data.combined$dr_precisions <- apply(selected.tree.data.combined, 1, get_dr_precisions)
selected.tree.data.combined$dr_f1s <- apply(selected.tree.data.combined, 1, get_dr_f1s)

# Effect modifier metrics ———————————
beta_em_recalls <- c()
beta_em_precisions <- c()
beta_em_f1s <- c()

for (beta in betas) {
  selected_data <- cv_em(selected.tree.data.combined, beta)
  recall <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.negatives) + sum(selected_data$em.true.positives))
  precision <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.positives) + sum(selected_data$em.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_em_recalls <- c(beta_em_recalls, recall)
  beta_em_precisions <- c(beta_em_precisions, precision)
  beta_em_f1s <- c(beta_em_f1s, f1)
}

# Decision rule metrics ———————————
beta_dr_recalls <- c()
beta_dr_precisions <- c()
beta_dr_f1s <- c()

for (beta in betas) {
  selected_data <- cv_dr(selected.tree.data.combined, beta)
  recall <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.negatives) + sum(selected_data$dr.true.positives))
  precision <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.positives) + sum(selected_data$dr.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_dr_recalls <- c(beta_dr_recalls, recall)
  beta_dr_precisions <- c(beta_dr_precisions, precision)
  beta_dr_f1s <- c(beta_dr_f1s, f1)
}

beta_metrics <- data.frame(beta = betas, em_recall = beta_em_recalls, em_precision = beta_em_precisions, em_f1 = beta_em_f1s, dr_recall = beta_dr_recalls, dr_precision = beta_dr_precisions, dr_f1 = beta_dr_f1s)
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

beta_metrics[is.nan(beta_metrics)] <- 0
write.csv(beta_metrics, file = "graph_data1_1.csv", row.names = FALSE)

# Generate table ———————————
res <- selected.tree.data.combined %>%
  group_by(beta, lambda) %>%
  summarise(percent.correct.tree.sequence = mean(correct.tree.in.sequence), percent.correct.tree.selected = mean(selected.correct.splits), selected.tree.size = mean(selected.tree.size), bias = mean(bias), mse = mean(mse), variance = mean(variance))
res %>% print(n = 100)

df <- data.frame(res)
png("res_df.png", height = 50 * nrow(df), width = 200 * ncol(df))
grid.table(df)
dev.off()

res <- selected.tree.data.combined %>%
  group_by(beta, lambda)
res

# Generate sample tree images ———————————
# @TODO: sample tree images
for (b in betas) {
  for (l in lambdas) {
    x <- filter(selected.tree.data.combined, lambda == l & beta == b)$selected.trees[[1]]
    png(paste0("testing/beta_", b, "lambda_", l, ".png"))
    prp(x)
    dev.off()
  }
}

# SETTING 2 EM/DR GRAPH DATA ————————————————————————————————————————————————————————————————————————————————————————
# Setting 2 with different beta value and lambda ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"), c("em2", "em1"), c("em2"))
noise.var <- c("em3", "em4")

lambdas <- seq(1, 16, by = 1)
betas <- seq(0.05, 1, by = 0.05)
# b2 <- seq(2, 5, by = 0.5)
betas <- c(betas, b2)

for (beta in betas) {
  true_trt_effect_func <- function(df) {
    df %>%
      mutate_at(vars(em1, em2, em3, em4), function(e) {
        as.numeric(as.character(e))
      }) %>%
      mutate(eff = 10 * (1 + beta * (0.5 * em1 * em2 + 0.2 * em2))) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- run_simu(
    lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
    gps_spec = 2, em_spec = 2,
    true_trt_effect_func = true_trt_effect_func, beta = beta,
    sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
    inference.sample_covs = matched.c$inference.sample_covs,
    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
    matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
  )
  tree.data <- results$results
  save(tree.data, file = paste0(dir_out, "selected.tree.data.no.stopping.setting2.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
}

# Results for setting 2 with different beta and lambda ——————————————————————
selected.tree.data.combined <- data.frame()

for (beta in betas[21:28]) {
  print(beta)
  load(file = paste0(dir_out, "selected.tree.data.no.stopping.setting2.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = FALSE))
}

# Preprocessing functions for effect modifiers ———————————
get_em_recalls <- function(x, output) {
  return(x$em.true.positives / (x$em.true.positives + x$em.false.negatives))
}

get_em_precisions <- function(x, output) {
  return(x$em.true.positives / (x$em.true.positives + x$em.false.positives))
}

get_em_f1s <- function(x, output) {
  return(2 * x$em_recalls * x$em_precisions / (x$em_recalls + x$em_precisions))
}

cv_em <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(em_f1s)))
}

selected.tree.data.combined$em_recalls <- apply(selected.tree.data.combined, 1, get_em_recalls)
selected.tree.data.combined$em_precisions <- apply(selected.tree.data.combined, 1, get_em_precisions)
selected.tree.data.combined$em_f1s <- apply(selected.tree.data.combined, 1, get_em_f1s)

# Preprocessing functions for decision rules ———————————
get_dr_recalls <- function(x, output) {
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.negatives))
}

get_dr_precisions <- function(x, output) {
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.positives))
}

get_dr_f1s <- function(x, output) {
  return(2 * x$dr_recalls * x$dr_precisions / (x$dr_recalls + x$dr_precisions))
}

cv_dr <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(dr_f1s)))
}

selected.tree.data.combined$dr_recalls <- apply(selected.tree.data.combined, 1, get_dr_recalls)
selected.tree.data.combined$dr_precisions <- apply(selected.tree.data.combined, 1, get_dr_precisions)
selected.tree.data.combined$dr_f1s <- apply(selected.tree.data.combined, 1, get_dr_f1s)

# Effect modifier metrics ———————————
beta_em_recalls <- c()
beta_em_precisions <- c()
beta_em_f1s <- c()

for (beta in betas) {
  selected_data <- cv_em(selected.tree.data.combined, beta)
  recall <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.negatives) + sum(selected_data$em.true.positives))
  precision <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.positives) + sum(selected_data$em.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_em_recalls <- c(beta_em_recalls, recall)
  beta_em_precisions <- c(beta_em_precisions, precision)
  beta_em_f1s <- c(beta_em_f1s, f1)
}

# Decision rule metrics ———————————
beta_dr_recalls <- c()
beta_dr_precisions <- c()
beta_dr_f1s <- c()

for (beta in betas) {
  selected_data <- cv_dr(selected.tree.data.combined, beta)
  recall <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.negatives) + sum(selected_data$dr.true.positives))
  precision <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.positives) + sum(selected_data$dr.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_dr_recalls <- c(beta_dr_recalls, recall)
  beta_dr_precisions <- c(beta_dr_precisions, precision)
  beta_dr_f1s <- c(beta_dr_f1s, f1)
}

beta_metrics <- data.frame(beta = betas, em_recall = beta_em_recalls, em_precision = beta_em_precisions, em_f1 = beta_em_f1s, dr_recall = beta_dr_recalls, dr_precision = beta_dr_precisions, dr_f1 = beta_dr_f1s)
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

beta_metrics[is.nan(beta_metrics)] <- 0
write.csv(beta_metrics, file = "graph_data2_1.csv", row.names = FALSE)

# SETTING 3 EM/DR GRAPH DATA ————————————————————————————————————————————————————————————————————————————————————————
# Setting 3 with different beta value and lambda ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"), c("em2", "em1"), c("em2", "em1"), c("em2", "em1"))
noise.var <- c("em3", "em4")

lambdas <- seq(1, 16, by = 1)
betas <- seq(0.05, 1, by = 0.05)
b2 <- seq(1.5, 5, by = 0.5)
betas <- c(betas, b2)

for (beta in betas) {
  true_trt_effect_func <- function(df) {
    df %>%
      mutate_at(vars(em1, em2, em3, em4), function(e) {
        as.numeric(as.character(e))
      }) %>%
      mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- run_simu(
    lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
    gps_spec = 2, em_spec = 1,
    true_trt_effect_func = true_trt_effect_func, beta = beta,
    sample_size = 20000, heterogenous_intercept = TRUE, stopping.rule = TRUE, n_trials = 200,
    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
    inference.sample_covs = matched.c$inference.sample_covs,
    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
    matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
  )
  tree.data <- results$results
  save(tree.data, file = paste0(dir_out, "selected.tree.data.no.stopping.setting3.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
}

# Results for setting 3 with different beta and lambda ——————————————————————
selected.tree.data.combined <- data.frame()

for (beta in betas[21:28]) {
  print(beta)
  load(file = paste0(dir_out, "selected.tree.data.no.stopping.setting3.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = FALSE))
}

# Preprocessing functions for effect modifiers ———————————
get_em_recalls <- function(x, output) {
  return(x$em.true.positives / (x$em.true.positives + x$em.false.negatives))
}

get_em_precisions <- function(x, output) {
  return(x$em.true.positives / (x$em.true.positives + x$em.false.positives))
}

get_em_f1s <- function(x, output) {
  return(2 * x$em_recalls * x$em_precisions / (x$em_recalls + x$em_precisions))
}

cv_em <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(em_f1s)))
}

selected.tree.data.combined$em_recalls <- apply(selected.tree.data.combined, 1, get_em_recalls)
selected.tree.data.combined$em_precisions <- apply(selected.tree.data.combined, 1, get_em_precisions)
selected.tree.data.combined$em_f1s <- apply(selected.tree.data.combined, 1, get_em_f1s)

# Preprocessing functions for decision rules ———————————
get_dr_recalls <- function(x, output) {
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.negatives))
}

get_dr_precisions <- function(x, output) {
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.positives))
}

get_dr_f1s <- function(x, output) {
  return(2 * x$dr_recalls * x$dr_precisions / (x$dr_recalls + x$dr_precisions))
}

cv_dr <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(dr_f1s)))
}

selected.tree.data.combined$dr_recalls <- apply(selected.tree.data.combined, 1, get_dr_recalls)
selected.tree.data.combined$dr_precisions <- apply(selected.tree.data.combined, 1, get_dr_precisions)
selected.tree.data.combined$dr_f1s <- apply(selected.tree.data.combined, 1, get_dr_f1s)

# Effect modifier metrics ———————————
beta_em_recalls <- c()
beta_em_precisions <- c()
beta_em_f1s <- c()

for (beta in betas) {
  selected_data <- cv_em(selected.tree.data.combined, beta)
  recall <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.negatives) + sum(selected_data$em.true.positives))
  precision <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.positives) + sum(selected_data$em.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_em_recalls <- c(beta_em_recalls, recall)
  beta_em_precisions <- c(beta_em_precisions, precision)
  beta_em_f1s <- c(beta_em_f1s, f1)
}

# Decision rule metrics ———————————
beta_dr_recalls <- c()
beta_dr_precisions <- c()
beta_dr_f1s <- c()

for (beta in betas) {
  selected_data <- cv_dr(selected.tree.data.combined, beta)
  recall <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.negatives) + sum(selected_data$dr.true.positives))
  precision <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.positives) + sum(selected_data$dr.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_dr_recalls <- c(beta_dr_recalls, recall)
  beta_dr_precisions <- c(beta_dr_precisions, precision)
  beta_dr_f1s <- c(beta_dr_f1s, f1)
}

beta_metrics <- data.frame(beta = betas, em_recall = beta_em_recalls, em_precision = beta_em_precisions, em_f1 = beta_em_f1s, dr_recall = beta_dr_recalls, dr_precision = beta_dr_precisions, dr_f1 = beta_dr_f1s)
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

beta_metrics[is.nan(beta_metrics)] <- 0
write.csv(beta_metrics, file = "graph_data3_1.csv", row.names = FALSE)

# SETTING 7 EM/DR GRAPH DATA ————————————————————————————————————————————————————————————————————————————————————————
# Setting 7 with different beta value and lambda ——————————————————————
correct_splits <- list(c("em2", "em1", "em1"), c("em1", "em2", "em2"))
true_modifiers <- list(c("em2", "em1"), c("em2", "em1"), c("em2", "em1"), c("em2", "em1"))
noise.var <- c("em3", "em4")

lambdas <- seq(1, 16, by = 1)
betas <- seq(0.05, 1, by = 0.05)
b2 <- seq(1.5, 5, by = 0.5)
betas <- c(betas, b2)

for (beta in betas) {
  # Reports the true effect eff for setting 7 given the effect modifiers after passing in the dataframe
  true_trt_effect_func <- function(df) {
    df %>%
      mutate_at(vars(em1, em2, em3, em4), function(e) {
        as.numeric(as.character(e))
      }) %>%
      mutate(eff = 10 * (1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- run_simu(
    lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 20, noise.var = noise.var,
    gps_spec = 2, em_spec = 3,
    true_trt_effect_func = true_trt_effect_func, beta = beta,
    sample_size = 20000, stopping.rule = TRUE, n_trials = 200,
    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
    inference.sample_covs = matched.c$inference.sample_covs,
    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
    matched.inference.sample = matched.c$matched.inference.sample, true_modifiers = true_modifiers
  )
  tree.data <- results$results
  save(tree.data, file = paste0(dir_out, "selected.tree.data.no.stopping.setting7.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
}

# Results for setting 7 with different beta and lambda ——————————————————————
selected.tree.data.combined <- data.frame()

for (beta in betas[21:28]) {
  print(beta)
  load(file = paste0(dir_out, "selected.tree.data.no.stopping.setting7.lambda3000-5000.100trials.resampled.outcome.beta", beta, ".c.RData"))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = FALSE))
}

# Preprocessing functions for effect modifiers ———————————
get_em_recalls <- function(x, output) {
  if (x$em.true.positives + x$em.false.negatives == 0) {
    return(0)
  } else {
    return(x$em.true.positives / (x$em.true.positives + x$em.false.negatives))
  }
}

get_em_precisions <- function(x, output) {
  if (x$em.true.positives + x$em.false.positives == 0) {
    return(0)
  } else {
    return(x$em.true.positives / (x$em.true.positives + x$em.false.positives))
  }
}

get_em_f1s <- function(x, output) {
  if (x$em_recalls + x$em_precisions == 0) {
    return(0)
  } else {
    return(2 * x$em_recalls * x$em_precisions / (x$em_recalls + x$em_precisions))
  }
}

cv_em <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(em_f1s)))
}

selected.tree.data.combined$em_recalls <- apply(selected.tree.data.combined, 1, get_em_recalls)
selected.tree.data.combined$em_precisions <- apply(selected.tree.data.combined, 1, get_em_precisions)
selected.tree.data.combined$em_f1s <- apply(selected.tree.data.combined, 1, get_em_f1s)

# Preprocessing functions for decision rules ———————————
get_dr_recalls <- function(x, output) {
  if (x$dr.true.positives + x$dr.false.negatives == 0) {
    return(0)
  }
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.negatives))
}

get_dr_precisions <- function(x, output) {
  if (x$dr.true.positives + x$dr.false.positives == 0) {
    return(0)
  }
  return(x$dr.true.positives / (x$dr.true.positives + x$dr.false.positives))
}

get_dr_f1s <- function(x, output) {
  if (x$dr_recalls + x$dr_precisions == 0) {
    return(0)
  } else {
    return(2 * x$dr_recalls * x$dr_precisions / (x$dr_recalls + x$dr_precisions))
  }
}

cv_dr <- function(df, b) {
  return(df %>%
    filter(beta == b) %>%
    group_by(iter) %>%
    slice(which.max(dr_f1s)))
}

selected.tree.data.combined$dr_recalls <- apply(selected.tree.data.combined, 1, get_dr_recalls)
selected.tree.data.combined$dr_precisions <- apply(selected.tree.data.combined, 1, get_dr_precisions)
selected.tree.data.combined$dr_f1s <- apply(selected.tree.data.combined, 1, get_dr_f1s)

# Effect modifier metrics ———————————
beta_em_recalls <- c()
beta_em_precisions <- c()
beta_em_f1s <- c()

for (beta in betas) {
  selected_data <- cv_em(selected.tree.data.combined, beta)
  recall <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.negatives) + sum(selected_data$em.true.positives))
  precision <- sum(selected_data$em.true.positives) / (sum(selected_data$em.false.positives) + sum(selected_data$em.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_em_recalls <- c(beta_em_recalls, recall)
  beta_em_precisions <- c(beta_em_precisions, precision)
  beta_em_f1s <- c(beta_em_f1s, f1)
}

# Decision rule metrics ———————————
beta_dr_recalls <- c()
beta_dr_precisions <- c()
beta_dr_f1s <- c()

for (beta in betas) {
  selected_data <- cv_dr(selected.tree.data.combined, beta)
  recall <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.negatives) + sum(selected_data$dr.true.positives))
  precision <- sum(selected_data$dr.true.positives) / (sum(selected_data$dr.false.positives) + sum(selected_data$dr.true.positives))
  f1 <- 2 * recall * precision / (recall + precision)

  beta_dr_recalls <- c(beta_dr_recalls, recall)
  beta_dr_precisions <- c(beta_dr_precisions, precision)
  beta_dr_f1s <- c(beta_dr_f1s, f1)
}

beta_metrics <- data.frame(beta = betas, em_recall = beta_em_recalls, em_precision = beta_em_precisions, em_f1 = beta_em_f1s, dr_recall = beta_dr_recalls, dr_precision = beta_dr_precisions, dr_f1 = beta_dr_f1s)
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

beta_metrics[is.nan(beta_metrics)] <- 0
write.csv(beta_metrics, file = "graph_data7_1.csv", row.names = FALSE)

prp(head(selected.tree.data.combined)$selected.trees[[1]])
head(selected.tree.data.combined)$em_f1s
head(selected.tree.data.combined)$em_precisions
=======
library(truncnorm)
library(caret)

######################## Run Simulation without generating outcome

# list of different regularization parameters to use. This will only impact how much the full tree is pruned
lambdas <- c(1, 2, 3)

# generate covariates with no confounding, split into subsamples, and perform gps matching
matched <- run_simu(num_exposure_cats = 20,
                    gps_spec = 1,
                    sample_size = 20000, n_trials = 0)
save(matched, file=paste0(source_dir, '/simulated_data_09122022/matched.RData'))
load(paste0(source_dir, '/simulated_data/matched.RData'))

# generate covariates with confounding
matched.c <- run_simu(num_exposure_cats = 20,
                      gps_spec = 2, sample_size = 20000, n_trials = 0)
save(matched.c, file=paste0(source_dir, '/simulated_data_09122022/matched.c.RData'))
load(paste0(source_dir, '/simulated_data/matched.c.RData'))

# plot overall covariate balance
unmatched.no.c.explor <- matched$exploration.sample_covs %>%
  as.data.table
matched.no.c.explor <- matched$matched.exploration.sample %>%
  as.data.table
unmatched.c.explor <- matched.c$exploration.sample_covs %>%
  as.data.table
matched.c.explor <- matched.c$matched.exploration.sample %>%
  as.data.table

unmatched.no.c.explor <- matched$exploration.sample_covs %>%
  as.data.table
matched.no.c.explor <- matched$matched.exploration.sample %>%
  as.data.table
unmatched.c.explor <- matched.c$exploration.sample_covs %>%
  as.data.table
matched.c.explor <- matched.c$matched.exploration.sample %>%
  as.data.table

unmatched_cor.no.c <-
  absolute_corr_fun(unmatched.no.c.explor[,'treat'], unmatched.no.c.explor[,1:6])
matched_cor.no.c <-
  absolute_corr_fun(matched.no.c.explor[,'w'], matched.no.c.explor[,6:11])
unmatched_cor.c <-
  absolute_corr_fun(unmatched.c.explor[,'treat'], unmatched.c.explor[,1:6])
matched_cor.c <-
  absolute_corr_fun(matched.c.explor[,'w'], matched.c.explor[,6:11])

abs_cor <-
  rbind(data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                   #names(synth_data)[3:length(names(synth_data))],
                   cor = unmatched_cor.no.c$absolute_corr, matched = FALSE, confounding = 'No Confounding'),
        data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                   #names(synth_data)[3:length(names(synth_data))],
                   cor = matched_cor.no.c$absolute_corr, matched = TRUE, confounding = 'No Confounding'),
        data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                   #names(synth_data)[3:length(names(synth_data))],
                   cor = unmatched_cor.c$absolute_corr, matched = FALSE, confounding = 'Confounding'),
        data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                   #names(synth_data)[3:length(names(synth_data))],
                   cor = matched_cor.c$absolute_corr, matched = TRUE, confounding = 'Confounding'))
ggplot(abs_cor, aes(x = cov, y = cor, color = matched, group = matched)) + geom_point() + geom_line() +
  facet_grid(rows = vars(confounding)) +
  theme(axis.text.x = element_text(angle = 45))
ggsave(paste0(source_dir, '/cov_balance_overall.png'), width = 10)

# plot covariate balance within effect modifier strata
abs_cor <- data.frame()
for(i in c(0,1)) {
  for (j in c(0,1)) {
    for (k in c(0,1)) {
      for (l in c(0,1)) {
        unmatched.no.c.explor <- matched$exploration.sample_covs %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table
        matched.no.c.explor <- matched$matched.exploration.sample %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table
        unmatched.c.explor <- matched.c$exploration.sample_covs %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table
        matched.c.explor <- matched.c$matched.exploration.sample %>%
          filter(em1 == i, em2 == j, em3 == k, em4 == l) %>%
          as.data.table

        unmatched_cor.no.c <-
          absolute_corr_fun(unmatched.no.c.explor[,'treat'], unmatched.no.c.explor[,1:6])
        matched_cor.no.c <-
          absolute_corr_fun(matched.no.c.explor[,'w'], matched.no.c.explor[,6:11])
        unmatched_cor.c <-
          absolute_corr_fun(unmatched.c.explor[,'treat'], unmatched.c.explor[,1:6])
        matched_cor.c <-
          absolute_corr_fun(matched.c.explor[,'w'], matched.c.explor[,6:11])

        abs_cor <-
          rbind(abs_cor,
                data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                           #names(synth_data)[3:length(names(synth_data))],
                           cor = unmatched_cor.no.c$absolute_corr, matched = FALSE, confounding = 'No Confounding', em1 = i, em2 = j, em3 = k, em4 = l),
                data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                           #names(synth_data)[3:length(names(synth_data))],
                           cor = matched_cor.no.c$absolute_corr, matched = TRUE, confounding = 'No Confounding', em1 = i, em2 = j, em3 = k, em4 = l),
                data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                           #names(synth_data)[3:length(names(synth_data))],
                           cor = unmatched_cor.c$absolute_corr, matched = FALSE, confounding = 'Confounding', em1 = i, em2 = j, em3 = k, em4 = l),
                data.frame(cov = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6'),
                           #names(synth_data)[3:length(names(synth_data))],
                           cor = matched_cor.c$absolute_corr, matched = TRUE, confounding = 'Confounding', em1 = i, em2 = j, em3 = k, em4 = l))

      }}}}
ggplot(abs_cor, aes(x = cov, y = cor, color = matched, group = matched)) + geom_point() + geom_line() +
  facet_grid(rows = vars(confounding), cols = vars(em1, em2, em3, em4)) +
  theme(axis.text.x = element_text(angle = 45))
ggsave(paste0(source_dir, '/cov_balance_by_strata.png'), width = 10)

######################## Setting 1: 4 groups, same intercept
correct_splits <- list(c('em2', 'em1', 'em1'), c('em1', 'em2', 'em2'))
noise.var <- c('em3', 'em4')
beta <- 1

true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {as.numeric(as.character(e))}) %>%
    mutate(eff = 10*(1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)
}

results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var,
                    gps_spec = 2, em_spec = 1,
                    true_trt_effect_func = true_trt_effect_func, beta = beta,
                    sample_size = 2000, stopping.rule = TRUE, n_trials = 100,
                    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                    inference.sample_covs = matched.c$inference.sample_covs,
                    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                    matched.inference.sample = matched.c$matched.inference.sample)

tree.data <- results$results

save(tree.data, file = paste0(dir_out,'selected.tree.data.with.stopping.setting1.RData'))

################## Setting 2: 3 groups, same intercept
correct_splits <- list(c('em2', 'em1'))
noise.var <- c('em3', 'em4')

true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {as.numeric(as.character(e))}) %>%
    mutate(eff = 10*(1 + 0.5 * em1 * em2 + 0.2 * em2)) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)

}

results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var,
                    gps_spec = 2, em_spec = 2,
                    true_trt_effect_func = true_trt_effect_func, beta = beta,
                    sample_size = 2000, stopping.rule = TRUE, n_trials = 100,
                    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                    inference.sample_covs = matched.c$inference.sample_covs,
                    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                    matched.inference.sample = matched.c$matched.inference.sample)

tree.data <- results$results

save(tree.data, file = paste0(dir_out,'selected.tree.data.with.stopping.setting2.RData'))

######################## Setting 3: 4 groups, different intercept
correct_splits <- list(c('em2', 'em1', 'em1'), c('em1', 'em2', 'em2'))
noise.var <- c('em3', 'em4')

true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {as.numeric(as.character(e))}) %>%
    mutate(eff = 10*(1 + 0.5 * em1 - 0.8 * em2)) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)

}

results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var,
                    gps_spec = 1, em_spec = 1,
                    true_trt_effect_func = true_trt_effect_func, beta = beta,
                    sample_size = 2000, heterogenous_intercept = TRUE, stopping.rule = TRUE, n_trials = 100,
                    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                    inference.sample_covs = matched.c$inference.sample_covs,
                    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                    matched.inference.sample = matched.c$matched.inference.sample)

tree.data <- results$results

save(tree.data, file = paste0(dir_out,'selected.tree.data.with.stopping.setting3.RData'))

################## Setting 4: 3 groups, different intercept
correct_splits <- list(c('em2', 'em1'))
noise.var <- c('em3', 'em4')

true_trt_effect_func <- function(df) {
  df %>%
    mutate_at(vars(em1, em2, em3, em4), function(e) {as.numeric(as.character(e))}) %>%
    mutate(eff = 10*(1 + 0.5 * em1 * em2 + 0.2 * em2)) %>%
    mutate_at(vars(em1, em2, em3, em4), as.factor)

}

results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var, gps_spec = 1, em_spec = 2,
                    true_trt_effect_func = true_trt_effect_func, beta = beta,
                    sample_size = 2000, heterogenous_intercept = TRUE, stopping.rule = TRUE, n_trials = 100,
                    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                    inference.sample_covs = matched.c$inference.sample_covs,
                    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                    matched.inference.sample = matched.c$matched.inference.sample)

tree.data <- results$results

save(tree.data, file = paste0(dir_out,'selected.tree.data.with.stopping.setting4.RData'))


########################## Setting 5: no effect modification
correct_splits <- list(c())
noise.var <- c('em1', 'em2', 'em3', 'em4')
true_trt_effect_func <- function(df) {
  df %>%
    mutate(eff = 10)
}

results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var, gps_spec = 1, em_spec = 0,
                    true_trt_effect_func = true_trt_effect_func, beta = beta,
                    sample_size = 2000, heterogenous_intercept = FALSE, stopping.rule = TRUE, n_trials = 100,
                    exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                    inference.sample_covs = matched.c$inference.sample_covs,
                    matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                    matched.inference.sample = matched.c$matched.inference.sample)

tree.data <- results$results

save(tree.data, file = paste0(dir_out,'selected.tree.data.with.stopping.setting5.RData'))

###################### Combining results for different settings
selected.tree.data.combined <- data.frame()
#load(file = paste0(dir_out,'selected.tree.data.with.stopping.setting', 1, '.lambda3000-5000.100trials.resampled.outcome2.RData'))
#selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(setting = 1))

for (i in 1:5){
  load(file = paste0(dir_out,'selected.tree.data.with.stopping.setting', i, '.RData'))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(setting = i))
}

setting.labs <- c('Setting 1', 'Setting 2', 'Setting 3', 'Setting 4', 'Setting 5')
names(setting.labs) <- c(1,2,3,4,5)

selected.tree.data.combined %>%
  filter(lambda == 1) %>%
  tidyr::gather("metric", "value", c(bias, mse)) %>%
  ggplot(aes(y = value, group = lambda)) + geom_boxplot() +
  facet_grid(rows = vars(metric), cols = vars(setting), labeller = labeller(setting = setting.labs), scales = 'free_y') +
  labs(y=NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file = paste0(dir_out, 'mse.biase.with.stopping.no.c.png'))

selected.tree.data.combined %>%
  group_by(beta) %>%
  summarise_at(vars(selected.correct.splits), mean)

############################ Setting 1 with Different Beta Values
correct_splits <- list(c('em2', 'em1', 'em1'), c('em1', 'em2', 'em2'))
noise.var <- c('em3', 'em4')

betas <- c(0.1, 0.15,0.2,0.3,1)
for (beta in betas) {
  true_trt_effect_func <- function(df) {
    df %>%
      mutate_at(vars(em1, em2, em3, em4), function(e) {as.numeric(as.character(e))}) %>%
      mutate(eff = 10*(1 + beta * (0.5 * em1 - 0.8 * em2))) %>%
      mutate_at(vars(em1, em2, em3, em4), as.factor)
  }

  results <- run_simu(lambdas = lambdas, correct_splits = correct_splits, num_exposure_cats = 10, noise.var = noise.var, gps_spec = 1, em_spec = 1,
                      true_trt_effect_func = true_trt_effect_func, beta = beta,
                      sample_size = 2000, heterogenous_intercept = FALSE, stopping.rule = FALSE, n_trials = 50,
                      exploration.sample_covs = matched.c$exploration.sample_covs, val.sample_covs = matched.c$val.sample_covs,
                      inference.sample_covs = matched.c$inference.sample_covs,
                      matched.exploration.sample = matched.c$matched.exploration.sample, matched.validation.sample = matched.c$matched.validation.sample,
                      matched.inference.sample = matched.c$matched.inference.sample)
  tree.data <- results$results
  save(tree.data, file = paste0(dir_out,'selected.tree.data.no.stopping.setting1.lambda3000-5000.100trials.resampled.outcome.beta', beta, '.c.RData'))
}


## results for setting 1 with different beta
selected.tree.data.combined <- data.frame()
for (beta in betas){
  load(file = paste0(dir_out,'selected.tree.data.with.stopping.setting1.lambda3000-5000.100trials.resampled.outcome.beta', beta, '.no.c.RData'))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = TRUE))
}
for (beta in betas){
  load(file = paste0(dir_out,'selected.tree.data.no.stopping.setting1.lambda3000-5000.100trials.resampled.outcome.beta', beta, '.c.RData'))
  selected.tree.data.combined <- selected.tree.data.combined %>% rbind(tree.data %>% cbind(beta = beta, stopping = FALSE))
}

beta.labs <- sapply(betas, function(x) {paste0('Beta = ', x)})
names(beta.labs) <- as.character(betas)

selected.tree.data.combined %>%
  filter(lambda == 1) %>%
  tidyr::gather("metric", "value", c(bias, mse)) %>%
  ggplot(aes(y = value, group = lambda)) + geom_boxplot() +
  facet_grid(rows = vars(metric), cols = vars(beta), labeller = labeller(beta = beta.labs), scales = 'free_y') +
  labs(y=NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file = paste0(dir_out, 'mse.bias.no.stopping.c.beta.png'))

selected.tree.data.combined %>%
  filter(lambda == 3000) %>%
  group_by(beta, stopping) %>%
  summarise(percent.correct.tree = mean(selected.correct.splits), n = n())

>>>>>>> na/main
