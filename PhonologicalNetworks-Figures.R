# Figures for Phonological Networks in Development paper

# cor.test(subset(reg_data, age == AOP-1)$INT_scaled, 
#          subset(reg_data, age == AOP-1)$EXT_scaled_target)
# 
# ggplot(subset(reg_data, age == AOP-1), aes(x=INT_scaled, y = EXT_scaled_target)) + 
#   geom_point() +
#   geom_smooth(method = lm)



# Data type comparisons

data_type_plot <- ggplot(data = subset(regression_data, age == (AOP-1)), aes(x = age, y = INT_scaled, colour = data_type)) +
  geom_point(shape = 21, position = position_jitter(.1)) +
  geom_smooth(aes(fill = data_type)) +
  ylab("INT value (normalised)") +
  xlab("Age (months)") +
  #ggtitle("Figure 4") +
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank())


