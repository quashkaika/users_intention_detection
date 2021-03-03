library(readr)
library(dplyr)
library(tidyverse)

str(fix_trials_2_3)
fix_trials_2_3 <- as.data.frame(fix_trials_2_3)
str(fix_trials_2_3)
fix_trials_2_3$X8 <- as.numeric(fix_trials_2_3$X8)
fix_trials_2_3$X2 <- as.factor(fix_trials_2_3$X2)
fix_trials_2_3 <- fix_trials_2_3 %>% rename(Trials = X2)
str(fix_trials_2_3)
nrow(fix_trials_2_3)
fix_trials_2_3 <- na.omit(fix_trials_2_3)
nrow(fix_trials_2_3)
str(fix_trials_2_3)
hist_p_value <- ggplot(fix_trials_2_3, aes(x=X1, y=X8), color=Trials)+
  geom_point(aes(color=Trials))+
  geom_hline(yintercept=0.05, col="darkcyan")+
  facet_grid(Trials ~ .)+
  xlab("VP")+
  ylab("p-value")+
  theme_light()
my_plot <- hist_p_value+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
my_plot

str(sac_trials_2_3)
sac_trials_2_3 <- as.data.frame(sac_trials_2_3)
sac_trials_2_3$X8 <- as.numeric(sac_trials_2_3$X8)
sac_trials_2_3$X2 <- as.factor(sac_trials_2_3$X2)
sac_trials_2_3 <- sac_trials_2_3 %>% rename(Trials = X2)
str(sac_trials_2_3)
sac_trials_2_3 <- na.omit(sac_trials_2_3)
hist_p_value2 <- ggplot(sac_trials_2_3, aes(x=X1, y=X8), color=Trials)+
  geom_point(aes(color=Trials))+
  geom_hline(yintercept=0.05, col="mediumvioletred")+
  facet_grid(Trials ~ .)+
  xlab("VP")+
  ylab("p-value")+
  theme_light()
my_plot2 <- hist_p_value2+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
my_plot2


