##########################################################
# Figure 2
# Created in R 3.5.0
# By: Kristin Eccles

##########################################################
# Load Libraries
library(DescTools) # tbwm
library(ggplot2)
library(splines)
library(ggpubr)
library(sjPlot)
library(reshape)

# Load data
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)
oc_raw=old_crow_raw[,2:13]

##########################################################
#### BIAS ADJUST THE DATA ####
# Average Hg for each year (row) minus i using Tukey's robust biweighted mean
tbw1=apply(oc_raw[-1], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw2=apply(oc_raw[-2], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw3=apply(oc_raw[-3], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw4=apply(oc_raw[-4], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw5=apply(oc_raw[-5], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw6=apply(oc_raw[-6], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw7=apply(oc_raw[-7], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw8=apply(oc_raw[-8], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw9=apply(oc_raw[-9], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw10=apply(oc_raw[-10], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw11=apply(oc_raw[-11], 1, function(x) TukeyBiweight(x, na.rm = TRUE))
tbw12=apply(oc_raw[-12], 1, function(x) TukeyBiweight(x, na.rm = TRUE))

# Combine average dataset
individual_tbwa=as.data.frame(cbind(tbw1,tbw2,tbw3,tbw4,tbw5,tbw6,tbw7,tbw8,tbw9,tbw10,tbw11,tbw12))
colnames(individual_tbwa)=colnames(oc_raw)

# Create new dataset of the differences between raw and mean
differences_oc=as.data.frame(oc_raw-individual_tbwa)

# mean of the differences
colmean = colMeans(differences_oc, na.rm = TRUE, dims = 1)
colmean

# subtract raw value by mean difference of each tree
old_crow_adj <- as.data.frame(t(apply(oc_raw, FUN=function(x) x - colmean, MARGIN = 1)))

# Add year data
old_crow_adj$Year=old_crow_raw$Year

##########################################################
#### COMPLETE THE DATASET  ####

#### RAW ####
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)
# New column with time interval - 5 years
old_crow_raw$freq = 5

#replicate the rows
oc_raw.expanded = old_crow_raw[rep(row.names(old_crow_raw), old_crow_raw$freq), 1:15]

# first number is 2 years preceeding start year, and 2 years after ending year
oc_raw.expanded$Year=array(1696:2005)

#### EXPAND TUKEYS BIWEIGHTED MEAN DATA ####
# Add year
individual_tbwa$Year=old_crow_raw$Year
# Combine average dataset
# New column with time interval - 5 years
individual_tbwa$freq = 5

#replicate the rows
oc_tbwm_raw.expanded = individual_tbwa[rep(row.names(individual_tbwa), individual_tbwa$freq), 1:14]

# first number is 2 years preceeding start year, and 2 years after ending year
oc_tbwm_raw.expanded$Year=array(1696:2005)

#### ADJUSTED #####
# New column with time interval - 5 years
old_crow_adj$freq = 5

#replicate the rows
oc_adj.expanded = old_crow_adj[rep(row.names(old_crow_adj), 
                                       old_crow_adj$freq), 1:14]

# first number is 2 years preceeding start year, and 2 years after ending year
oc_adj.expanded$Year=array(1696:2005)


###################################################################
#### STACK THE DATASETS ####
# Raw
oc_raw_stack=melt(oc_raw.expanded[,1:13], id.vars=1)
colnames(oc_raw_stack)=c("Year","Site", "Average")

# Tukets biweight mean
oc_tbwm_stack=melt(oc_tbwm_raw.expanded[,1:13], id.vars=13)
colnames(oc_tbwm_stack)=c("Year","Site", "Average")

# Adjusted
oc_adj_stack=melt(oc_adj.expanded[,1:13], id.vars=1)
colnames(oc_adj_stack)=c("Year","Site", "Average")

###################################################################
#### MAKE DATA FOR TREND LINES >3 ####
#### RAW ####
# Summarize number of trees by Year 
oc_raw.expanded$count = apply(oc_raw.expanded[,2:13], 1, function(x) table(x>0))

# Only do statistics on Years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_raw_count=subset(oc_raw.expanded, count>=3)
# Subset data

#### ADJUSTED####
# Summarize number of trees by Year 
oc_adj.expanded$count = apply(oc_adj.expanded[,2:13], 1, function(x) table(x>0))

# Only do statistics on Years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_adj_count=subset(oc_adj.expanded, count>=3)
# Subset data


################################################
#### MAKE PLOT ####

# Use oc_raw.expanded, oc_adj.expanded, and oc_adj_count
# Time series plot RAW
### 1 DP.08.30.40a 0.34973961  ####
DP.08.30.40a=ggplot() + 
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
 # scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  theme_minimal(base_size=20)+
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, DP.08.30.40a ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, DP.08.30.40a), color="red", size=1)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.35", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="DP.08.30.40a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), 
  panel.background = element_rect(colour = "black", size=1))
DP.08.30.40a

#### 2 ####
DP.08.30.41b=ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  theme_minimal(base_size=20)+
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
 # scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, DP.08.30.41b ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, DP.08.30.41b), color="red", size=1)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.10", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="DP.08.30.41b", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
DP.08.30.41b

#### 3 ####
DP.08.30.43a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
 # scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, DP.08.30.43a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, DP.08.30.43a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+  
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.21", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="DP.08.30.43a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
DP.08.30.43a 

#### 4 DP.08.30.42a ####
DP.08.30.42a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
 # scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, DP.08.30.42a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, DP.08.30.42a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.33", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="DP.08.30.42a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
DP.08.30.42a 

#### 5 TM2.08.40a  ####
TM2.08.40a  =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = round(seq(min(oc_raw_stack$Year), 
                                                                             max(oc_tbwm_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, TM2.40a), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, TM2.40a), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.20", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="TM2.08.40a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
TM2.08.40a  

### 6 TM2.08.42b ####
TM2.08.42b  =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
 # scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, TM2.42b   ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, TM2.42b  ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.17", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="TM2.08.42b", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
TM2.08.42b  

#### 7  TM2.08.41a ####

TM2.08.41a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, TM2.41a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, TM2.41a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.77", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="TM2.08.41a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
TM2.08.41a

#### 8  TM2.08.43 ####

TM2.08.43a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, TM2.43a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, TM2.43a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.59", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="TM2.08.43a", color="black", size=6)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
TM2.08.43a

#### 9 OC.08.51.40a ####

OC.08.51.40a  =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, OC.08.51.40a   ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, OC.08.51.40a  ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.61", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="OC.08.51.40a", color="black", size=6)+
  theme (panel.background = element_rect(colour = "black", size=1))
OC.08.51.40a 

#### 10 OC.08.51.43a ####

OC.08.51.43a  =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_raw.expanded, aes(Year, OC.08.51.43a  ), color="red", size=1)+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, OC.08.51.43a  ), color="black", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=0.35", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="OC.08.51.43a", color="black", size=6)+
  theme(axis.ticks = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), 
  panel.background = element_rect(colour = "black", size=1))
OC.08.51.43a 

#### 11 OC.08.51.42a ####

OC.08.51.42a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, OC.08.51.42a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, OC.08.51.42a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.43", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="OC.08.51.42a", color="black", size=6)+
  theme(axis.ticks = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
OC.08.51.42a

#### 12 OC.08.51.42a ####

OC.08.51.41a =ggplot() + 
  geom_line(data=oc_raw_stack, aes(x = Year, y = Average, color = Site), size = 0.5) +
  scale_color_manual(values = c("darkgray", "darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray",
                                "darkgray","darkgray","darkgray", "darkgray"), guide=FALSE) +
  scale_x_continuous(expand=c(0,0), limits=c(1690, 2005), breaks = 
                       round(seq(min(1700), max(oc_raw_stack$Year), by = 50),1))+
  #scale_y_continuous(expand=c(0,0), limits=c(0, 6))+
  geom_line(data=oc_tbwm_raw.expanded, aes(Year, OC.08.51.41a  ), color="black", size=1)+
  geom_line(data=oc_raw.expanded, aes(Year, OC.08.51.41a ), color="red", size=1)+
  theme_minimal(base_size=20)+
  ylab("Total Hg (ng/g)")+
  annotate(geom="text", x=1965, y=1.75, label="diff=-0.49", color="black", size=6)+
  annotate(geom="text", x=1750, y=5.75, label="OC.08.51.41a", color="black", size=6)+
  theme(axis.ticks = element_blank(),axis.title.y=element_blank(), axis.text.y = element_blank(), 
        panel.background = element_rect(colour = "black", size=1))
OC.08.51.41a


#################################################################################
#
fig2=ggarrange(DP.08.30.40a, DP.08.30.41b, DP.08.30.43a, DP.08.30.42a, TM2.08.40a,TM2.08.42b, TM2.08.41a, TM2.08.43a, OC.08.51.40a, OC.08.51.43a, OC.08.51.42a, OC.08.51.41a, 
             #labels = c("DP.08.30.40a", "DP.08.30.41b", "DP.08.30.43a", "DP.08.30.42a","TM2.08.40a","TM2.08.42b", "TM2.08.41a", "TM2.08.43a", "OC.08.51.40a", "OC.08.51.43a", "OC.08.51.42a", "OC.08.51.41a"),
             #vjust = 1,
             #hjust = -0.5,
             #heights = c(1,1,1,1,1,1,1,1,1,1,1,1),
             #widths = c(1,1,1,1,1,1,1,1,1,1,1,1),
             align=("hv"),
             ncol = 4, nrow = 3)
fig2
#Plot figures with dpi=300
save_plot("figure2_kristin2.tif", fig2, width = 50, height = 30, dpi = 300)
