##########################################################
# Figure 4
# Created in R 3.5.0
# By: Kristin Eccles
##########################################################


# Break Point Plots
breaks=cbind(1838,1883,1938)
p6=ggplot(data=oc_adj_count, aes(Year, hg_mean_adj))+
  geom_rect(data=oc_adj_count, mapping=aes(xmin=1833, xmax=1853, ymin=1, ymax=4), 
            fill = "grey50", alpha=0.6) +
  geom_rect(data=oc_adj_count, mapping=aes(xmin=1873, xmax=1888, ymin=1, ymax=4), 
            fill = "grey50", alpha=0.6) +
  geom_rect(data=oc_adj_count, mapping=aes(xmin=1918, xmax=1948, ymin=1, ymax=4), 
            fill = "grey50", alpha=0.6) +
  geom_smooth(data=oc_adj_count, aes(Year, hg_mean_adj),method = "lm", 
              formula=y ~ ns(x, 15), color="red")+ 
  geom_segment(aes(y = 1, x = 1838, yend = 4, xend = 1838), colour = "black", linetype="dashed")+
  geom_segment(aes(y = 1, x = 1883, yend = 4, xend = 1883), colour = "black", linetype="dashed")+
  geom_segment(aes(y = 1, x = 1938, yend = 4, xend = 1938), colour = "black", linetype="dashed")+
  geom_line(data=oc_adj_count, aes(Year, hg_mean_adj), color="black", size=1)+
  geom_smooth(data=oc_adj_count, aes(Year, hg_mean_adj),method = "lm", 
              formula=y ~ ns(x, 15), color="red")+
  ylab("Adjusted Total Hg (ng/g)")+
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw.expanded$Year), by = 50),1))+
  scale_y_continuous(expand=c(0,0))+
  theme_minimal(base_size=16)
p6 

################################################################
# Change outlier, color, shape and size
p7=ggplot(stack_avg_break, aes(x=X2, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  theme_minimal(base_size=16)+
  labs(y="Adjusted Total Hg (ng/g)", x="Group")

#############################################################
#Plot Raw and Adjusted figures together
p8=ggarrange(p6,p7,
             labels = c("A","B"),
             vjust = 1,
             hjust = -0.5,
             ncol = 1, nrow = 2,
             common.legend = FALSE,
             legend = "right",
             font.label = list(size = 16))
p8
#Plot figures with dpi=300
save_plot("Figure4.tif", p8, width = 20, height = 20, dpi = 300)
