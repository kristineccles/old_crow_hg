##########################################################
# Adjust data for tree specific bias
# Created in R 3.5.0
# By: Kristin Eccles

##########################################################
# Load Libraries
library(DescTools) # tbwm

# Load data
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)
oc_raw=old_crow_raw[,2:13]

##########################################################
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
old_crow_adj = as.data.frame(t(apply(oc_raw, FUN=function(x) x - colmean, MARGIN = 1)))

# Add year data
old_crow_adj$Year=old_crow_raw$Year
