##########################################################
# Compare locations
# Created in R 3.5.0
# By: Kristin Eccles

# Must run complete_dataset first
##########################################################
# Load libraries
library(ggplot2)
library(reshape)
library(ggpubr)
library(sjPlot)
library(splines)
library(ggpubr)
library(DescTools)

# Load data
mac_adj=read.csv("mackenzie_delta.csv", header=TRUE)
mac_adj=read.csv("mackenzie_delta_5_year.csv", header=TRUE)

#########################################################
# Modify data
# Subset data based on count OC
oc_adj.expanded$count = apply(oc_adj.expanded[,2:13], 1, function(x) table(x>0))
oc_subset=subset(oc_adj.expanded, count>=3)
oc_subset$hg_mean_adj=apply(oc_subset[,1:12], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
# Stack dataset
oc_adj_stack=melt(oc_subset[,1:12], id.vars=1)

# Subset data based on count MAC 25
#mac_adj$count = apply(mac_adj[,2:23], 1, function(x) table(x>0))
#mac_subset=subset(mac_adj, count>=3)
#mac_subset$hg_mean_adj=apply(mac_subset[,2:23], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
#Mackenize Delta 5 year
#mac_adj_stack=melt(mac_subset[,1:23], id.vars=1)

# Subset data based on count MAC 5
mac_adj$count = apply(mac_adj[,2:12], 1, function(x) table(x>0))
mac_subset=subset(mac_adj, count>=3)
mac_subset$hg_mean_adj=apply(mac_subset[,1:12], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
#Mackenize Delta 5 year
mac_adj_stack=melt(mac_subset[,1:12], id.vars=1)

# Subset data based on count SCREE
scree.expanded$count = apply(scree.expanded[,2:21], 1, function(x) table(x>0))
scree_subset=subset(scree.expanded, count>=3)
scree_subset$hg_mean_adj=apply(scree_subset[,1:21], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
#Scree Hill
scree.expanded_stack=melt(scree_subset[,1:21], id.vars=1)

###################################################################
# Time series plot 
# Old Crow
oc=ggplot() + 
  theme_minimal(base_size=18)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank())+
  #geom_line(data=oc_adj_stack, aes(x = Year, y = value), size = 0.5) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2020), breaks = 
                       round(seq(min(1700), max(2020), by = 50),1))+
  scale_y_continuous(expand=c(0,0))+
  geom_line(data=oc_subset, aes(Year, hg_mean_adj), color="black", size=1)+
  geom_smooth(data=oc_subset, aes(Year, hg_mean_adj), method = "lm", 
              formula=y ~ ns(x, 10), color="red")+
  ylab("Adjusted Total Hg (ng/g)")
oc

# Scree Hill
scree=ggplot() + 
  #geom_line(data=scree.expanded_stack, aes(x = Year, y = value), size = 0.5) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2020), breaks = 
                       round(seq(min(1700), max(2020), by = 50),1))+
  scale_y_continuous(expand=c(0,0))+
  geom_line(data=scree_subset, aes(Year, hg_mean_adj), color="black", size=1)+
  geom_smooth(data=scree_subset, aes(Year, hg_mean_adj),method = "lm", 
              formula=y ~ ns(x, 10), color="red")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank())+
  ylab("Adjusted Total Hg (ng/g)")+
  theme_minimal(base_size=18)
scree

mac=ggplot() + 
  theme_minimal(base_size=18)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_blank())+
  #geom_line(data=mac_adj_stack, aes(x = year, y = value), size = 0.5) +
   scale_x_continuous(expand=c(0,0), limits=c(1690, 2020), breaks = 
                       round(seq(min(1700), max(2020), by = 50),1))+
  scale_y_continuous(expand=c(0,0.5))+
  geom_line(data=mac_subset, aes(year, hg_mean_adj), color="black", size=1)+
  geom_smooth(data=mac_subset, aes(year, hg_mean_adj),method = "lm", 
              formula=y ~ ns(x, df=10), color="red")+
  ylab("Adjusted Total Hg (ng/g)")
mac
############################################################################
# Plot  figures together
plot1=ggarrange(mac, oc, scree,
             labels = cbind("(A)", "(B)", "(C)"),
             hjust = -2.5,
             vjust = 1,
             ncol = 1,
             nrow = 3,
             align=("hv"),
             font.label = list(size = 16))
plot1
#Plot figures with dpi=300
save_plot("compare_hg_plots_5year.tif", plot1, width = 20, height = 25, dpi = 300)

