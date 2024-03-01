

speaker.data.sessions <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  rename("n Sessions" = "n")

speaker.data.tokens <- regression_data %>%
  group_by(corpus, Speaker) %>%
  tally()  %>% 
  ungroup() %>%
  rename("n Tokens" = "n")

speaker.data.sessions.mean <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(mean_sessions = mean(n))

speaker.data.sessions.sd <- regression_data %>%  
  group_by(Speaker, corpus) %>%
  distinct(Speaker, age) %>%
  tally() %>%
  ungroup() %>%
  group_by(corpus) %>%
    #rename("n Sessions" = "n") %>%
  summarise(sd_sessions = sd(n))

speaker.data.overview <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(`Min. age` = min(age)) %>%
  left_join(speaker.data.sessions) %>%
  left_join(speaker.data.tokens) %>%
  arrange(corpus)

speaker.data.overview.mean <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(min_age = min(age)) %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(mean_age = mean(min_age))

speaker.data.overview.sd <- regression_data %>%
  group_by(Speaker, corpus) %>%
  summarise(min_age = min(age)) %>%
  ungroup() %>%
  group_by(corpus) %>%
  #rename("n Sessions" = "n") %>%
  summarise(sd_age = sd(min_age))

table.data.summary.mean <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  #rename("Types" = "n") %>%
  #spread(corpus, Types) %>%
  summarise(Types = mean(n, na.rm=T))

table.data.summary.sd <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  #rename("Types" = "n") %>%
  #spread(corpus, Types) %>%
  summarise(Types = sd(n, na.rm=T))

table.data.summary.tokens.mean <- regression_data %>%
  group_by(corpus, Speaker) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  summarise(mean_tokens = mean(n, na.rm=T))

table.data.summary.tokens.sd <- regression_data %>%
  group_by(corpus, Speaker) %>%
  tally()  %>% 
  ungroup() %>%
  group_by(corpus) %>%
  summarise(sd_tokens = sd(n, na.rm=T))

overview.sds <- speaker.data.overview.sd %>%
  left_join(speaker.data.sessions.sd) %>%
  left_join(table.data.summary.sd) %>%
  left_join(table.data.summary.tokens.sd) %>%
  mutate("Speaker" = "SD") %>%
  rename("Min. age" = "sd_age",
         "n Sessions" = "sd_sessions",
         "n Tokens" = "sd_tokens")

overview.means <- speaker.data.overview.mean %>%
  left_join(speaker.data.sessions.mean) %>%
  left_join(table.data.summary.mean) %>%
  left_join(table.data.summary.tokens.mean) %>%
    mutate("Speaker" = "Mean") %>%
  rename("Min. age" = "mean_age",
         "n Sessions" = "mean_sessions",
         "n Tokens" = "mean_tokens") %>%
  bind_rows(overview.sds)

### now create means and sds for full corpus

full.data.summary.mean.typ <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(Types = mean(n, na.rm=T)) %>%
  mutate("Speaker" = "Mean")

full.data.summary.sd.typ <- regression_data %>%
  group_by(corpus, Speaker) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(Types = sd(n, na.rm=T)) %>%
  mutate("Speaker" = "SD")

full.data.summary.tokens.mean <- regression_data %>%
  group_by(corpus, Speaker) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(`n Tokens` = mean(n, na.rm=T)) %>%
  mutate("Speaker" = "Mean")

full.data.summary.tokens.sd <- regression_data %>%
  group_by(corpus, Speaker) %>%
  tally()  %>% 
  ungroup() %>%
  summarise(`n Tokens` = sd(n, na.rm=T)) %>%
  mutate("Speaker" = "SD")

full.data.summary.mean <- speaker.data.overview %>%
  ungroup() %>%
  summarise(`Min. age` = mean(`Min. age`),
            `n Sessions` = mean(`n Sessions`)) %>%
  mutate("Speaker" = "Mean") %>%
  left_join(full.data.summary.mean.typ) %>%
  left_join(full.data.summary.tokens.mean)

full.data.summary.sd <- speaker.data.overview %>%
  ungroup() %>%
  summarise(`Min. age` = sd(`Min. age`),
            `n Sessions` = sd(`n Sessions`)) %>%
  mutate("Speaker" = "SD") %>%
  left_join(full.data.summary.sd.typ) %>%
  left_join(full.data.summary.tokens.sd)

full.data.summary <- full.data.summary.mean %>%
  bind_rows(full.data.summary.sd) %>%
  mutate(Corpus = "All")

## bind all dataframes together

table.data.overview <- regression_data %>%
  group_by(Speaker, corpus) %>%
  distinct(gloss1, .keep_all = T) %>%
  tally() %>% 
  rename("Types" = "n") %>%
  left_join(speaker.data.overview) %>%
  dplyr::select(Speaker, corpus, `Min. age`, `n Sessions`, Types, `n Tokens`) %>%
  bind_rows(overview.means) %>%
  rename("Corpus" = "corpus") %>%
  arrange(Corpus) %>%
  bind_rows(full.data.summary)
  

table.aop.deg.corr.speaker <- globalthresholds_AOP %>%
  group_by(Speaker, corpus, data_type) %>%
  summarize(rho = stats::cor.test(AOP, degree, method = "sp")$estimate,
            pval = stats::cor.test(AOP, degree, method = "sp")$p.value
  ) %>%
  ungroup() %>%
  arrange(corpus) %>%
  rename("Corpus" = "corpus",
         "p" = "pval") %>%
  mutate(p = scales::pvalue(p))

all_distances <- read_csv("Data/all_distances.csv") # table showing average scaled phonological distances across each corpus


## thresholds testing for SI (tidy this up later as it can surely be done more efficiently)

globalthresholds_AOP_thresholdstest <- feather::read_feather("Data/globalthresholds_AOP_thresholdstest.feather")
regression_data_lyon_thresholdstest <- feather::read_feather("ignore/large_files/regression_data_lyon_thresholdstest.feather")
regression_data_providence_thresholdstest <- feather::read_feather("ignore/large_files/regression_data_providence_thresholdstest.feather")
regression_data_thresholdstest <- rbind(regression_data_lyon_thresholdstest, regression_data_providence_thresholdstest) %>%
  mutate(threshold = as.numeric(threshold)) %>% ungroup()

network_diff_15_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="actual"))
network_diff_18_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="actual"))
network_diff_19_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="actual"))
network_diff_21_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="actual"))
network_diff_24_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="actual"))
network_diff_33_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .33 & data_type=="actual"))
network_diff_50_A <- nrow(subset(full_thresholds, data_type=="actual")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="actual"))

threshold <- c(.15, .18, .19, .21, .24, .33, .5)

network_diff_tab_actual <- data.frame(threshold) %>%
  mutate(unconnected_words = rbind(network_diff_15_A,
                                   network_diff_18_A,
                                   network_diff_19_A,
                                   network_diff_21_A,
                                   network_diff_24_A,
                                   network_diff_33_A,
                                   network_diff_50_A))

network_diff_15_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .15 & data_type=="target"))
network_diff_18_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .18 & data_type=="target"))
network_diff_19_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .19 & data_type=="target"))
network_diff_21_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .21 & data_type=="target"))
network_diff_24_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .24 & data_type=="target"))
network_diff_33_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .33 & data_type=="target"))
network_diff_50_T <- nrow(subset(full_thresholds, data_type=="target")) - nrow(subset(globalthresholds_AOP_thresholdstest, threshold == .5 & data_type=="target"))

network_diff_tab_target <- data.frame(threshold) %>%
  mutate(unconnected_words = rbind(network_diff_15_T,
                                   network_diff_18_T,
                                   network_diff_19_T,
                                   network_diff_21_T,
                                   network_diff_24_T,
                                   network_diff_33_T,
                                   network_diff_50_T))


table.aop.deg.corr.speaker_thresholdstest <- globalthresholds_AOP_thresholdstest %>%
  group_by(data_type, threshold) %>%
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

full_A_15 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .15))

full_A_18 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .18))

full_A_19 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .19))


full_A_21 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .21))

full_A_24 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .24))

full_A_33 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .33))

full_A_5 <- glmer(learned_next ~
                    INT_scaled*age_scaled +
                    EXT_scaled_target*age_scaled +
                    length_scaled*age_scaled +
                    freq_scaled*age_scaled +
                    aoa_scaled*age_scaled +
                    corpus +
                    category +
                    (1+age_scaled|Speaker),
                  family=binomial("logit"),
                  control=glmerControl(calc.derivs=FALSE,
                                       optimizer="bobyqa",
                                       # specifiying optimizer to support convergence 
                                       #(does not converge without this)
                                       optCtrl=list(maxfun=2e5)),
                  data=subset(reg_dat_thresholdstest, data_type == "actual" & threshold == .5))


corrs_thresholdtest <- table.aop.deg.corr.speaker_thresholdstest %>%
  mutate(threshold = as.numeric(threshold)) %>%
  left_join(network_diff_tab)

full_A_15_coefs <- tidy(full_A_15,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.15)

full_A_18_coefs <- tidy(full_A_18,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.18)

full_A_19_coefs <- tidy(full_A_19,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.19)

full_A_21_coefs <- tidy(full_A_21,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.21)

full_A_24_coefs <- tidy(full_A_24,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.24)

full_A_33_coefs <- tidy(full_A_33,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.33)

full_A_5_coefs <- tidy(full_A_5,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.5)

full_modeloutputs_tab_actual <- rbind(full_A_15_coefs,
                                      full_A_18_coefs,
                                      full_A_19_coefs,
                                      full_A_21_coefs,
                                      full_A_24_coefs,
                                      full_A_33_coefs,
                                      full_A_5_coefs) %>%
  pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))

full_T_15 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .15))

full_T_18 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .18))

full_T_19 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .19))


full_T_21 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .21))

full_T_24 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .24))

full_T_33 <- glmer(learned_next ~
                     INT_scaled*age_scaled +
                     EXT_scaled_target*age_scaled +
                     length_scaled*age_scaled +
                     freq_scaled*age_scaled +
                     aoa_scaled*age_scaled +
                     corpus +
                     category +
                     (1+age_scaled|Speaker),
                   family=binomial("logit"),
                   control=glmerControl(calc.derivs=FALSE,
                                        optimizer="bobyqa",
                                        # specifiying optimizer to support convergence 
                                        #(does not converge without this)
                                        optCtrl=list(maxfun=2e5)),
                   data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .33))

full_T_5 <- glmer(learned_next ~
                    INT_scaled*age_scaled +
                    EXT_scaled_target*age_scaled +
                    length_scaled*age_scaled +
                    freq_scaled*age_scaled +
                    aoa_scaled*age_scaled +
                    corpus +
                    category +
                    (1+age_scaled|Speaker),
                  family=binomial("logit"),
                  control=glmerControl(calc.derivs=FALSE,
                                       optimizer="bobyqa",
                                       # specifiying optimizer to support convergence 
                                       #(does not converge without this)
                                       optCtrl=list(maxfun=2e5)),
                  data=subset(reg_dat_thresholdstest, data_type == "target" & threshold == .5))


full_T_15_coefs <- tidy(full_T_15,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.15)

full_T_18_coefs <- tidy(full_T_18,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.18)

full_T_19_coefs <- tidy(full_T_19,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.19)

full_T_21_coefs <- tidy(full_T_21,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.21)

full_T_24_coefs <- tidy(full_T_24,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.24)

full_T_33_coefs <- tidy(full_T_33,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.33)

full_T_5_coefs <- tidy(full_T_5,effects="fixed",conf.int=TRUE) %>%
  filter(term %in% c("INT_scaled", "EXT_scaled_target")) %>%
  select(term, estimate, conf.low, conf.high ,p.value) %>%
  mutate(p.value=scales::pvalue(p.value),
         across(where(is.numeric), round, 2)) %>%
  unite(`95% CI`, c(conf.low, conf.high), sep = ",",) %>%
  mutate(threshold = 0.5)

full_modeloutputs_tab_target <- rbind(full_T_15_coefs,
                                      full_T_18_coefs,
                                      full_T_19_coefs,
                                      full_T_21_coefs,
                                      full_T_24_coefs,
                                      full_T_33_coefs,
                                      full_T_5_coefs) %>%
  pivot_wider(names_from = "term", values_from = c("estimate", "p.value", `95% CI`))

thresholdstest_actual <- table.aop.deg.corr.speaker_thresholdstest %>% filter(data_type == "actual") %>%
  select(-p, -significant) %>%
  left_join(network_diff_tab_actual) %>%
  left_join(full_modeloutputs_tab_actual)

thresholdstest_target <- table.aop.deg.corr.speaker_thresholdstest %>% filter(data_type == "target") %>%
  select(-p, -significant) %>%
  left_join(network_diff_tab_target) %>%
  left_join(full_modeloutputs_tab_target)

