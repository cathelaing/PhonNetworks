## Tables and figure for SI, Phonological Networks and Systematicity in Early Lexical Acquisition

## NOTE: the first code chunk is commented out and run in a separate script (thresholdtest_tables.R) because it takes too long to knit otherwise.

## S2: Explanation of how the connectivity threshold of 0.25 was established for the analysis

# reg_dat_thresholdstest <- regression_data_thresholdstest[which(complete.cases(regression_data_thresholdstest
#                                                                               [,c('EXT_scaled_target',
#                                                                                   'INT_scaled',
#                                                                                   'length_scaled',
#                                                                                   'freq_scaled',
#                                                                                   'aoa_scaled',
#                                                                                   'age_scaled',
#                                                                                   'tokens_scaled',
#                                                                                   'corpus',
#                                                                                   'AOP',
#                                                                                   'category',
#                                                                                   'threshold')])),]
# 
# reg_dat_thresholdstest_A <- reg_dat_thresholdstest %>% filter(data_type == "actual")
# reg_dat_thresholdstest_T <- reg_dat_thresholdstest %>% filter(data_type == "target")
# 
# network_diff_15_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="actual"))
# network_diff_18_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="actual"))
# network_diff_19_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="actual"))
# network_diff_21_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="actual"))
# network_diff_24_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="actual"))
# network_diff_33_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .33 & data_type=="actual"))
# network_diff_50_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="actual"))
# 
# threshold <- c(.15, .18, .19, .21, .24, .33, .5)
# threshold_rep <- c(.15, .15, .18, .18, .19, .19, .21, .21, .24, .24, .34, .34, .5, .5)
# 
# network_diff_tab_actual <- data.frame(threshold) %>%
#   mutate(unconnected_words = rbind(network_diff_15_A,
#                                    network_diff_18_A,
#                                    network_diff_19_A,
#                                    network_diff_21_A,
#                                    network_diff_24_A,
#                                    network_diff_33_A,
#                                    network_diff_50_A))
# 
# table.aop.deg.corr.speaker_thresholdstest <- globalthresholds_AOP_thresholdstest %>%
#   group_by(threshold, data_type) %>%
#   summarize(rho = stats::cor.test(AOP, degree, method = "sp")$estimate,
#             pval = stats::cor.test(AOP, degree, method = "sp")$p.value
#   ) %>%
#   ungroup() %>%
#   rename("p" = "pval") %>%
#   mutate(p = scales::pvalue(p),
#          significant = ifelse(p <.05, T, F),
#          threshold = as.numeric(threshold))
# 
# thresholdstest_corr <- ggplot(table.aop.deg.corr.speaker_thresholdstest, 
#                               aes(x = threshold, y = as.numeric(rho), colour = Corpus, shape = significant)) +
#   geom_point(size = 3) +
#   scale_shape_manual(values = c(1, 16)) +
#   ylab("Spearman's rho") +
#   xlab("Threshold") +
#   guides(color=guide_legend(""), shape = "none") +
#   theme_bw() +
#   geom_vline(xintercept = 0.25, linetype = "twodash", colour = "darkgrey") +
#   facet_wrap(~data_type, ncol=2)
# 
# corrs_thresholdtest_actual <- table.aop.deg.corr.speaker_thresholdstest %>%
#   filter(data_type == "actual") %>%
#   left_join(network_diff_tab_actual)
# 
# threshold_GLMER_results_actual <- (reg_dat_thresholdstest_A
#                                    %>% split(reg_dat_thresholdstest_A$threshold)
#                                    %>% purrr::map(~glmer(learned_next~INT_scaled*age_scaled +
#                                                            EXT_scaled_target*age_scaled +
#                                                            length_scaled*age_scaled +
#                                                            freq_scaled*age_scaled +
#                                                            aoa_scaled*age_scaled +
#                                                            corpus +
#                                                            category +
#                                                            (1+age_scaled|Speaker), 
#                                                          family=binomial("logit"),
#                                                          data = .))
#                                    %>% purrr::map_dfr(~tidy(., effects = "fixed",conf.int=TRUE, .id = "threshold"))
# )
# 
# threshold_GLMER_results_actual_coefs <- threshold_GLMER_results_actual %>%
#   filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
#   select(term, estimate, conf.low, conf.high, p.value) %>%
#   mutate(p.value=scales::pvalue(p.value),
#          across(where(is.numeric), round, 2),
#          threshold = threshold_rep) %>%
#   unite(`95% CI`, c(conf.low, conf.high), sep = ",") %>%
#   #mutate(row = row_number()) %>%
#   pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))
# 
# threshold_GLMER_results_actual_coefs$`95% CI_INT_scaled` <- paste0("[", threshold_GLMER_results_actual_coefs$`95% CI_INT_scaled`, "]")
# threshold_GLMER_results_actual_coefs$`95% CI_EXT_scaled_target` <- paste0("[", threshold_GLMER_results_actual_coefs$`95% CI_EXT_scaled_target`, "]")
# 
# actual.thresholdstest <- corrs_thresholdtest_actual %>%
#   left_join(threshold_GLMER_results_actual_coefs) %>%
#   rename("unconnected words" = `unconnected_words`) %>%
#   dplyr::select(threshold, `unconnected words`, rho, 
#                 estimate_INT_scaled, p.value_INT_scaled, `95% CI_INT_scaled`, 
#                 estimate_EXT_scaled_target, p.value_EXT_scaled_target, `95% CI_EXT_scaled_target`)
# 
# network_diff_15_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="target"))
# network_diff_18_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="target"))
# network_diff_19_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="target"))
# network_diff_21_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="target"))
# network_diff_24_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="target"))
# network_diff_33_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .33 & data_type=="target"))
# network_diff_50_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="target"))
# 
# network_diff_tab_target <- data.frame(threshold) %>%
#   mutate(unconnected_words = rbind(network_diff_15_T,
#                                    network_diff_18_T,
#                                    network_diff_19_T,
#                                    network_diff_21_T,
#                                    network_diff_24_T,
#                                    network_diff_33_T,
#                                    network_diff_50_T))
# 
# corrs_thresholdtest_target <- table.aop.deg.corr.speaker_thresholdstest %>%
#   filter(data_type == "target") %>%
#   left_join(network_diff_tab_target)
# 
# threshold_GLMER_results_target <- (reg_dat_thresholdstest_T
#                                    %>% split(reg_dat_thresholdstest_T$threshold)
#                                    %>% purrr::map(~glmer(learned_next~INT_scaled*age_scaled +
#                                                            EXT_scaled_target*age_scaled +
#                                                            length_scaled*age_scaled +
#                                                            freq_scaled*age_scaled +
#                                                            aoa_scaled*age_scaled +
#                                                            corpus +
#                                                            category +
#                                                            (1+age_scaled|Speaker), 
#                                                          family=binomial("logit"),
#                                                          data = .))
#                                    %>% purrr::map_dfr(~tidy(., effects = "fixed",conf.int=TRUE, .id = "threshold"))
# )
# 
# threshold_GLMER_results_target_coefs <- threshold_GLMER_results_target %>%
#   filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
#   select(term, estimate, conf.low, conf.high, p.value) %>%
#   mutate(p.value=scales::pvalue(p.value),
#          across(where(is.numeric), round, 2),
#          threshold = threshold_rep) %>%
#   unite(`95% CI`, c(conf.low, conf.high), sep = ",") %>%
#   #mutate(row = row_number()) %>%
#   pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))
# 
# threshold_GLMER_results_target_coefs$`95% CI_INT_scaled` <- paste0("[", threshold_GLMER_results_target_coefs$`95% CI_INT_scaled`, "]")
# threshold_GLMER_results_target_coefs$`95% CI_EXT_scaled_target` <- paste0("[", threshold_GLMER_results_target_coefs$`95% CI_EXT_scaled_target`, "]")
# 
# target.thresholdstest <- corrs_thresholdtest_target %>%
#   left_join(threshold_GLMER_results_target_coefs) %>%
#   rename("unconnected words" = `unconnected_words`) %>%
#   dplyr::select(threshold, `unconnected words`, rho, 
#                 estimate_INT_scaled, p.value_INT_scaled, `95% CI_INT_scaled`, 
#                 estimate_EXT_scaled_target, p.value_EXT_scaled_target, `95% CI_EXT_scaled_target`)

actual.thresholdstest <- read_csv("Data/actual_thresholdtest.csv")  ## read in the tables that would be generated above but are actually generated elsewhere!
target.thresholdstest <- read_csv("Data/target_thresholdtest.csv")

### S2: Overview of how the connectivity threshold of 0.25 was established for the analysis

thresholds.corr <- ggplot(subset(globalthresholds_corr, !is.na(estimate)), 
                          aes(x = threshold, y = as.numeric(estimate), colour = corpus, shape = significant)) +
  geom_point() +
  scale_shape_manual(values = c(1, 16)) +
  ylab("Spearman's rho") +
  xlab("Threshold") +
  guides(color=guide_legend(""), shape = "none") +
  theme_bw() +
  geom_vline(xintercept = 0.25, linetype = "twodash", colour = "darkgrey") +
  facet_wrap(~data_type, ncol=2)

### S3: AoP ~ Degree connectivity across infants in the dataset

table.aop.deg.corr.speaker <- globalthresholds_AOP %>%
  group_by(Speaker, corpus, data_type) %>%
  summarize(rho = stats::cor.test(AOP, degree, method = "sp")$estimate,
            pval = stats::cor.test(AOP, degree, method = "sp")$p.value
  ) %>%
  ungroup() %>%
  arrange(corpus) %>%
  rename("Corpus" = "corpus",
         "p" = "pval") %>%
  mutate(p = scales::pvalue(p)) %>%
  pivot_wider(names_from=data_type, values_from=c("rho", "p")) %>%
  dplyr::select(Speaker, Corpus, rho_actual, p_actual, rho_target, p_target)

cor.deg.AOP.fig <- ggplot(globalthresholds_AOP,
                          aes(x = AOP, y = degree, colour = data_type)) +
  geom_rect(data = subset(globalthresholds_AOP ,corpus == 'French'), aes(fill = corpus),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.08, fill = "gray88") +
  geom_point(shape = 21, size = 1, alpha = 0.5, position = position_jitter(.02)) +
  scale_x_continuous(breaks = seq(from = 10, to = 30, by = 10)) +
  geom_smooth(method=lm,
              se=FALSE, size = 2) +
  ylab("degree (z-score)") +
  theme_bw(base_size = 12) +
  theme(axis.text=element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~Speaker, ncol=3)



### S5: By-speaker comparisons of the data

data.type.plot.bysubj <- ggplot(data = subset(regression_data, age == (AOP-1)), aes(x = age, y = INT_scaled, colour = data_type)) +
  geom_point(shape = 21, position = position_jitter(.1)) +
  geom_smooth(aes(fill = data_type)) +
  ylab("INT value (normalised)") +
  xlab("Age (months)") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~Speaker, ncol=3)
