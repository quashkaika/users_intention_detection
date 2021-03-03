library(readr)
library(dplyr)
library(tidyverse)

str(fix_steps_1_2)
fix_steps_1_2 <- as.data.frame(fix_steps_1_2)
str(fix_steps_1_2)
fix_steps_1_2$X8 <- as.numeric(fix_steps_1_2$X8)
fix_steps_1_2$X2 <- as.factor(fix_steps_1_2$X2)
fix_steps_1_2$X3 <- as.factor(fix_steps_1_2$X3)
fix_steps_1_2 <- fix_steps_1_2 %>% rename(Trials = X2, Steps = X3)
str(fix_steps_1_2)
nrow(fix_steps_1_2)
fix_steps_1_2 <- na.omit(fix_steps_1_2)
nrow(fix_steps_1_2)
str(fix_steps_1_2)
nrow(fix_steps_1_2)
#fix_steps_1_22<- filter(fix_steps_1_2, X8 < 0.05)
#nrow(fix_steps_1_22)
hist_p_value <- ggplot(fix_steps_2_3, aes(x=X1, y=X8), color=Trials)+
  geom_point(aes(color=Trials))+
  geom_hline(yintercept=0.05, col="darkcyan")+
  facet_grid(Trials ~ Steps)+
  xlab("VP")+
  ylab("p-value")+
  theme_light()
my_plot <- hist_p_value+theme(axis.text.x = element_blank())  
my_plot

str(sac_steps_1_2)
sac_steps_1_2 <- as.data.frame(sac_steps_1_2)
str(sac_steps_1_2)
sac_steps_1_2$X8 <- as.numeric(sac_steps_1_2$X8)
sac_steps_1_2$X2 <- as.factor(sac_steps_1_2$X2)
sac_steps_1_2$X3 <- as.factor(sac_steps_1_2$X3)
sac_steps_1_2 <- sac_steps_1_2 %>% rename(Trials = X2, Steps = X3)
str(sac_steps_1_2)
nrow(sac_steps_1_2)
sac_steps_1_2 <- na.omit(sac_steps_1_2)
nrow(sac_steps_1_2)
str(sac_steps_1_2)
nrow(sac_steps_1_2)
#sac_steps_1_22<- filter(sac_steps_1_2, X8 < 0.05)
#nrow(sac_steps_1_22)
hist_p_value2 <- ggplot(sac_steps_1_3, aes(x=X1, y=X8), color=Trials)+
  geom_point(aes(color=Trials))+
  geom_hline(yintercept=0.05, col="mediumvioletred")+
  facet_grid(Trials ~ Steps)+
  xlab("VP")+
  ylab("p-value")+
  theme_light()
my_plot2 <- hist_p_value2+theme(axis.text.x = element_blank())  
my_plot2




