##########################################################
# Tree ring analysis Old Crow
# Created in R 3.5.0
# By: Kristin Eccles

# Must run bias rawustment first to get old_crow_raw
##########################################################
# Load libraries
library(psych)

# load data
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)

##########################################################
# Create average for each year
old_crow_raw$hg_mean_raw=apply(old_crow_raw[,2:13], 1, function(x) TukeyBiweight(x, na.rm = TRUE))

# Create count of no. trees per year
oc_summarized=aggregate(old_crow_raw[,2:13], list(old_crow_raw$Year), sum)
old_crow_raw$count = apply(old_crow_raw[,2:13], 1, function(x) table(x>0))

# Only do statistics on years with 3 trees or more
# Subset data with >3 trees at teach time period
oc_raw_subset=subset(old_crow_raw, count>3)

#####################################################################
# EDA of dataset all trees
summary(old_crow_raw)
sd(old_crow_raw$hg_mean_raw)
# variance
sapply(old_crow_raw, var, na.rm = TRUE)


# EDA of dataset with >3
summary(oc_raw_subset)
sd(oc_raw_subset$hg_mean_raw)
# variance
sapply(old_crow_raw, var, na.rm = TRUE)


