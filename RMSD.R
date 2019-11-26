##########################################################
# RMSD
# Created in R 3.5.0
# By: Kristin Eccles

# Must run bias adjustment first
##########################################################
# Load libraries
library(plyr)

##########################################################
#Raw Data
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)

# Get only rows with >=3 Trees
old_crow_raw$count = apply(old_crow_raw[,2:13], 1, function(x) table(x>0))
old_crow_raw_subset=subset(old_crow_raw, count>=3)
old_crow_raw_subset$mean_hg=apply(old_crow_raw_subset[,2:13], 1, function(x) mean(x, na.rm = TRUE))

# difference between mean and individual
raw_differences=sqrt((old_crow_raw_subset[,2:12]-old_crow_raw_subset$mean_hg)^2)

#Average by tree
avg_diff_tree_raw=apply(raw_differences, 2, function(x) mean(x, na.rm = TRUE))
avg_diff_tree_raw
mean(avg_diff_tree_raw)

############
# Adjusted data

# Get only rows with >=3 Trees
old_crow_adj$count = apply(old_crow_adj[,1:12], 1, function(x) table(x>0))
old_crow_adj_subset=subset(old_crow_adj, count>=3)
old_crow_adj_subset$mean_hg=apply(old_crow_adj_subset[,1:12], 1, function(x) mean(x, na.rm = TRUE))

# difference between mean and individual
adj_differences=sqrt((old_crow_adj_subset[,1:12]-old_crow_adj_subset$mean_hg)^2)

#Average by tree
avg_diff_tree_adj=(apply(adj_differences, 2, function(x) mean(x, na.rm = TRUE)))
mean(avg_diff_tree_adj)
avg_diff_tree_adj
####################################################################
# substract the two time serices
mean_difference= old_crow_adj_subset$hg_mean_adj-old_crow_raw_subset$Hg_mean_raw
mean_difference=cbind(old_crow_adj_subset$Year, mean_difference, abs(mean_difference))
mean_difference
