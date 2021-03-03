Conditions <- c("1", "2", "3")
Classification <- c("by reaction","by reaction", "by reaction", "by reaction type (positive vs. negative)","by reaction type (positive vs. negative)","by reaction type (positive vs. negative)")
Accuracy <- c("0.72","0.79", "0.78","0.63", "0.53","0.54")
ma4_dataframe <- cbind(Conditions,Classification,Accuracy )
ma4_dataframe <- as.data.frame(ma4_dataframe)
str(ma4_dataframe)
ma4_dataframe$Classification <- as.factor(ma4_dataframe$Classification)
ma4_dataframe$Conditions <- as.factor(ma4_dataframe$Conditions)
ma4_dataframe$Accuracy <- as.numeric(ma4_dataframe$Accuracy)
str(ma4_dataframe)
l <- ma4_dataframe %>%
  mutate(Conditions = fct_relevel(Conditions, 
                                  "1","2", "3"))
ma4_plot <- ggplot(l, aes(x=Conditions, y=Accuracy, col=Classification))+
  geom_point(aes(group=Classification),size =4, shape=18)+
  xlab("Conditions")+
  ylab("Performance accuracy in %")+
  theme_classic()
q <- ma4_plot + theme(legend.position = "bottom")
a <- q + lims(y = c(0, 1))
a
