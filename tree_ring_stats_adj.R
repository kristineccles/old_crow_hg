##########################################################
# Tree ring analysis Old Crow
# Created in R 3.5.0
# By: Kristin Eccles

# Must run bias adjustment first to get old_crow_adj
##########################################################
# Load libraries
library(strucchange) # time series analysis
library(lmtest)
library(psych)

# load data
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)

##########################################################
# Create average for each year
old_crow_adj$hg_mean_adj=apply(old_crow_adj[,1:12], 1, function(x) TukeyBiweight(x, na.rm = TRUE))

# Create count of no. trees per year
oc_summarized=aggregate(old_crow_adj[,1:12], list(old_crow_adj$Year), sum)
old_crow_adj$count = apply(old_crow_adj[,1:12], 1, function(x) table(x>0))

# Only do statistics on years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_subset=subset(old_crow_adj, count>3)

#####################################################################
# EDA of dataset with >3
summary(oc_subset)
sd(oc_subset$hg_mean_adj)
# variance
sapply(old_crow_adj, var, na.rm = TRUE)


#####################################################################
# Create a time series dataset
# freuqnecy =1 means year, 4 is quarterly, 12 is monthly, 365 is day
# 0.2 = 1 year/5

oc_ts=ts(oc_subset$hg_mean_adj, start=(1748), end=(2003), frequency=0.2)
plot(oc_ts)

#####################################################################
# Check for normality of Hg concentrations
hist(oc_ts)
shapiro.test(oc_ts) #not normal

#####################################################################
# Break Point Analysis
bp.oc <- breakpoints(oc_ts ~ 1)
summary(bp.oc)

## the BIC also chooses one breakpoint
plot(bp.oc)
breakpoints(bp.oc)

## confidence interval
ci.oc <- confint(bp.oc)
ci.oc
lines(ci.oc)

lm1=lm(hg_mean_adj~ Year, data=old_crow_adj)
summary(lm1)

#########################################################################
# Calculate the enrichement factor

break1= subset(old_crow_adj, old_crow_adj$Year>=1748 & old_crow_adj$Year<=1837)
mean(break1$hg_mean_adj)
sd(break1$hg_mean_adj)
lm1= lm(hg_mean_adj~Year, data=break1)
summary(lm1)
#2.544922

# Break 1 maximum
break1.1= subset(old_crow_adj, old_crow_adj$Year>=1748 & old_crow_adj$Year<=1853)
mean(break1.1$hg_mean_adj)
sd(break1.1$hg_mean_adj)
lm1= lm(hg_mean_adj~Year, data=break1.1)
summary(lm1)
#2.58

break2= subset(old_crow_adj, old_crow_adj$Year>=1838 & old_crow_adj$Year<=1882)
mean(break2$hg_mean_adj)
sd(break2$hg_mean_adj)
#2.816686

break2.2= subset(old_crow_adj, old_crow_adj$Year>=1833 & old_crow_adj$Year<=1888)
mean(break2.2$hg_mean_adj)
lm2= lm(hg_mean_adj~Year, data=break2.2)
summary(lm2)
#2.816686

break3= subset(old_crow_adj, old_crow_adj$Year>=1883& old_crow_adj$Year<=1937)
mean(break3$hg_mean_adj)
sd(break3$hg_mean_adj)
#3.144141

break4= subset(old_crow_adj, old_crow_adj$Year>=1938 & old_crow_adj$Year<=2003)
mean(break4$hg_mean_adj)
SD(break4$hg_mean_adj)
#3.521089

####
# difference amon groups
avg_breaks=cbind(break1$hg_mean_adj, break2$hg_mean_adj,break3$hg_mean_adj, break4$hg_mean_adj)
colnames(avg_breaks)=cbind("Break 1","Break 2","Break 3","Break 4")
stack_avg_break=melt(avg_breaks)

# Change outlier, color, shape and size
p1=ggplot(stack_avg_break, aes(x=X2, y=value)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  theme_minimal(base_size=20)+
  labs(y="Adj. Total Hg (ng/g)", x="Group")
  

aov1=aov(value~X2, data=stack_avg_break)
summary(aov1)
post_hoc=TukeyHSD(aov1)
post_hoc
plot(post_hoc)
t.test(break1$hg_mean_adj, break4$hg_mean_adj)
hist(break1$hg_mean_adj, break4$hg_mean_adj)

####
# Calculate enrichment factor
mean(break1$hg_mean_adj)
#2.544922

max(old_crow_adj$hg_mean_adj)
EF=(max(old_crow_adj$hg_mean_adj))/(mean(break1$hg_mean_adj))
EF


########################################################################
# Over all Linear model 
lm1= lm(log10(hg_mean_adj)~ log10(Year), data=oc_subset)
summary(lm1)

lm1= lm(hg_mean_adj~ Year, data=oc_subset)
summary(lm1)

