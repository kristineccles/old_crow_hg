##########################################################
# Tree ring Plots Old Crow
# Created in R 3.5.0
# By: Kristin Eccles

# run file from complete_dataset.r first
##########################################################
# Load libraries
library(utils)
library(ggplot2)
library(reshape)
library(ggpubr)
library(sjPlot)
library(splines) 

# Load data

###############################################################
#### RAW ####
# Stack dataset raw
oc_raw_stack=melt(oc_raw.expanded[,1:13], id.vars=1)
colnames(oc_raw_stack)=c("Year","Site", "Average")

# Summarize number of trees by Year 
oc_raw.expanded$count = apply(oc_raw.expanded[,2:14], 1, function(x) table(x>0))

# Only do statistics on Years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_raw_count=subset(oc_raw.expanded, count>=3)
# Subset data

#### ADJUSTED DATASET ####
oc_adj_stack=melt(oc_adj.expanded[,1:13], id.vars=14)
colnames(oc_adj_stack)=c("Year","Site", "Average")

# Summarize number of trees by Year 
oc_adj.expanded$count = apply(oc_adj.expanded[,2:13], 1, function(x) table(x>0))

# Only do statistics on Years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_adj_count=subset(oc_adj.expanded, count>=3)
# Subset data

## Add average to dataset
oc_adj_count$hg_mean_adj=apply(oc_adj_count[,1:12], 1, function(x) TukeyBiweight(x, na.rm = TRUE))

###################################################################
#### FIGURE 3 ####
#Plot number of trees 
p1 = ggplot(data = old_crow_adj, aes(x= Year, y=count)) +
  theme_minimal(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45))+
  geom_line()+
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  scale_y_continuous(expand=c(0,0), limits=c(0, 12), breaks = 
                       round(seq(min(0), max(12), by = 5),1))+
  labs(y="# of Trees")
#geom_bar(stat="identity")+
p1


# Time series plot RAW
p2=ggplot() + 
  theme_minimal(base_size = 16)+
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(2005), by = 50),1))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank())+
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray"), guide=FALSE)+
  geom_line(data=oc_raw_count, aes(Year, Hg_mean_raw), color="black", size=1)+
  geom_smooth(data=oc_raw_count, aes(Year, Hg_mean_raw),method = "lm", 
              formula=y ~ ns(x, 10), color="red")+
  ylab("Raw Total Hg (ng/g)")
p2

# Time series plot ADJUSTED
p3=ggplot()+
  theme_minimal(base_size = 16)+ 
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(2005), by = 50),1))+
  scale_y_continuous(expand=c(0,0))+
  geom_line(data=oc_adj_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray"), guide=FALSE) +
  geom_line(data=oc_adj_count, aes(Year, hg_mean_adj), color="black", size=1)+
  geom_smooth(data=oc_adj_count, aes(Year, hg_mean_adj),method = "lm", 
              formula=y ~ ns(x, 10), color="red")+  
  ylab("Adjusted Total Hg (ng/g)")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank())
p3

#############################################################################
# Plot Raw and Adjusted figures together
figure3=ggarrange(p2,p3,p1,
             labels = c("A","B","C"),
             vjust = 1,
             hjust = -0.5,
             ncol = 1, nrow = 3,
             common.legend = FALSE,
             legend = "right",
             font.label = list(size = 16),
             heights=c(1,1,0.5))
figure3
#Plot figures with dpi=300
save_plot("compare_means.tif", figure3, width = 20, height = 30, dpi = 300)
