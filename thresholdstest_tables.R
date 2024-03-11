## This script runs the commented-out code chunk from the PhonologicalNetworks-Tables-SI.R script separately, as this takes a long time to knit when
## embedded into the main script.

globalthresholds_AOP_thresholdstest <- feather::read_feather("Data/globalthresholds_AOP_thresholdstest.feather")
regression_data_lyon_thresholdstest <- feather::read_feather("ignore/large_files/regression_data_lyon_thresholdstest.feather")
regression_data_providence_thresholdstest <- feather::read_feather("ignore/large_files/regression_data_providence_thresholdstest.feather")
regression_data_thresholdstest <- rbind(regression_data_lyon_thresholdstest, regression_data_providence_thresholdstest) %>%
  mutate(threshold = as.numeric(threshold)) %>% ungroup()
full_thresholds <- feather::read_feather("Data/full_thresholds.feather")

reg_dat_thresholdstest <- regression_data_thresholdstest[which(complete.cases(regression_data_thresholdstest
                                                                              [,c('EXT_scaled_target',
                                                                                  'INT_scaled',
                                                                                  'length_scaled',
                                                                                  'freq_scaled',
                                                                                  'aoa_scaled',
                                                                                  'age_scaled',
                                                                                  'tokens_scaled',
                                                                                  'corpus',
                                                                                  'AOP',
                                                                                  'category',
                                                                                  'threshold')])),]

reg_dat_thresholdstest_A <- reg_dat_thresholdstest %>% filter(data_type == "actual")
reg_dat_thresholdstest_T <- reg_dat_thresholdstest %>% filter(data_type == "target")

network_diff_15_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="actual"))
network_diff_18_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="actual"))
network_diff_19_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="actual"))
network_diff_21_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="actual"))
network_diff_24_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="actual"))
network_diff_34_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .34 & data_type=="actual"))
network_diff_50_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="actual"))

unconnected_words_A <- rbind(network_diff_15_A,
                           network_diff_18_A,
                           network_diff_19_A,
                           network_diff_21_A,
                           network_diff_24_A,
                           network_diff_34_A,
                           network_diff_50_A)

unconnected_words_A <- as.data.frame(unconnected_words_A)

threshold <- c(.15, .18, .19, .21, .24, .34, .5)
threshold_rep <- c(.15, .15, .18, .18, .19, .19, .21, .21, .24, .24, .34, .34, .5, .5)

network_diff_tab_actual <- data.frame(threshold) %>%
  mutate(`unconnected words` = unconnected_words_A$V1)

table.aop.deg.corr.speaker_thresholdstest <- globalthresholds_AOP_thresholdstest %>%
  group_by(threshold, data_type) %>%
  summarize(rho = stats::cor.test(AOP, degree, method = "sp")$estimate,
            pval = stats::cor.test(AOP, degree, method = "sp")$p.value
  ) %>%
  ungroup() %>%
  rename("p" = "pval") %>%
  mutate(p = scales::pvalue(p),
         significant = ifelse(p <.05, T, F),
         threshold = as.numeric(threshold))

thresholdstest_corr <- ggplot(table.aop.deg.corr.speaker_thresholdstest, 
                              aes(x = threshold, y = as.numeric(rho), colour = Corpus, shape = significant)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 16)) +
  ylab("Spearman's rho") +
  xlab("Threshold") +
  guides(color=guide_legend(""), shape = "none") +
  theme_bw() +
  geom_vline(xintercept = 0.25, linetype = "twodash", colour = "darkgrey") +
  facet_wrap(~data_type, ncol=2)

corrs_thresholdtest_actual <- table.aop.deg.corr.speaker_thresholdstest %>%
  filter(data_type == "actual") %>%
  left_join(network_diff_tab_actual)

threshold_GLMER_results_actual <- (reg_dat_thresholdstest_A
                                   %>% split(reg_dat_thresholdstest_A$threshold)
                                   %>% purrr::map(~glmer(learned_next~INT_scaled*age_scaled +
                                                           EXT_scaled_target*age_scaled +
                                                           length_scaled*age_scaled +
                                                           freq_scaled*age_scaled +
                                                           aoa_scaled*age_scaled +
                                                           corpus +
                                                           category +
                                                           (1+age_scaled|Speaker), 
                                                         family=binomial("logit"),
                                                         data = .))
                                   %>% purrr::map_dfr(~tidy(., effects = "fixed",conf.int=TRUE, .id = "threshold"))
)

threshold_GLMER_results_actual_coefs <- threshold_GLMER_results_actual %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2),
         threshold = threshold_rep) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",") %>%
  #mutate(row = row_number()) %>%
  pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))

threshold_GLMER_results_actual_coefs$`95% CI_INT_scaled` <- paste0("[", threshold_GLMER_results_actual_coefs$`95% CI_INT_scaled`, "]")
threshold_GLMER_results_actual_coefs$`95% CI_EXT_scaled_target` <- paste0("[", threshold_GLMER_results_actual_coefs$`95% CI_EXT_scaled_target`, "]")

actual.thresholdstest <- corrs_thresholdtest_actual %>%
  left_join(threshold_GLMER_results_actual_coefs) %>%
  #rename("unconnected words" = `unconnected_words`) %>%
  dplyr::select(threshold, `unconnected words`, rho, 
                estimate_INT_scaled, p.value_INT_scaled, `95% CI_INT_scaled`, 
                estimate_EXT_scaled_target, p.value_EXT_scaled_target, `95% CI_EXT_scaled_target`)

write_csv(actual.thresholdstest, "Data/actual_thresholdtest.csv")

network_diff_15_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="target"))
network_diff_18_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="target"))
network_diff_19_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="target"))
network_diff_21_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="target"))
network_diff_24_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="target"))
network_diff_34_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .34 & data_type=="target"))
network_diff_50_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="target"))

unconnected_words_T <- rbind(network_diff_15_T,
                           network_diff_18_T,
                           network_diff_19_T,
                           network_diff_21_T,
                           network_diff_24_T,
                           network_diff_34_T,
                           network_diff_50_T)

unconnected_words_T <- as.data.frame(unconnected_words_T)

network_diff_tab_target <- data.frame(threshold) %>%
  mutate(`unconnected words` = unconnected_words_T$V1)

corrs_thresholdtest_target <- table.aop.deg.corr.speaker_thresholdstest %>%
  filter(data_type == "target") %>%
  left_join(network_diff_tab_target)

threshold_GLMER_results_target <- (reg_dat_thresholdstest_T
                                   %>% split(reg_dat_thresholdstest_T$threshold)
                                   %>% purrr::map(~glmer(learned_next~INT_scaled*age_scaled +
                                                           EXT_scaled_target*age_scaled +
                                                           length_scaled*age_scaled +
                                                           freq_scaled*age_scaled +
                                                           aoa_scaled*age_scaled +
                                                           corpus +
                                                           category +
                                                           (1+age_scaled|Speaker), 
                                                         family=binomial("logit"),
                                                         data = .))
                                   %>% purrr::map_dfr(~tidy(., effects = "fixed",conf.int=TRUE, .id = "threshold"))
)

threshold_GLMER_results_target_coefs <- threshold_GLMER_results_target %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2),
         threshold = threshold_rep) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",") %>%
  #mutate(row = row_number()) %>%
  pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))

threshold_GLMER_results_target_coefs$`95% CI_INT_scaled` <- paste0("[", threshold_GLMER_results_target_coefs$`95% CI_INT_scaled`, "]")
threshold_GLMER_results_target_coefs$`95% CI_EXT_scaled_target` <- paste0("[", threshold_GLMER_results_target_coefs$`95% CI_EXT_scaled_target`, "]")

target.thresholdstest <- corrs_thresholdtest_target %>%
  left_join(threshold_GLMER_results_target_coefs) %>%
  dplyr::select(threshold, `unconnected words`, rho, 
                estimate_INT_scaled, p.value_INT_scaled, `95% CI_INT_scaled`, 
                estimate_EXT_scaled_target, p.value_EXT_scaled_target, `95% CI_EXT_scaled_target`)

write_csv(target.thresholdstest, "Data/target_thresholdtest.csv")
