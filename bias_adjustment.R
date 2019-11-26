##########################################################
# Adjust data for tree specific bias

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
oc_adjusted_data <- as.data.frame(t(apply(oc_raw, FUN=function(x) x - colmean, MARGIN = 1)))

# Add year data
oc_adjusted_data$Year=old_crow_raw$Year
cbind(oc_adjusted_data)

############################################
# Site average -i
# Average Hg for each year (row) minus i using Tukey's robust biweighted mean

average1=apply(individual_tbwa[-1], 1, mean)
average2=apply(individual_tbwa[-2], 1, mean)
average3=apply(individual_tbwa[-3], 1, mean)
average4=apply(individual_tbwa[-4], 1, mean)
average5=apply(individual_tbwa[-5], 1, mean)
average6=apply(individual_tbwa[-6], 1, mean)
average7=apply(individual_tbwa[-7], 1, mean)
average8=apply(individual_tbwa[-8], 1, mean)
average9=apply(individual_tbwa[-9], 1, mean)
average10=apply(individual_tbwa[-10], 1, mean)
average11=apply(individual_tbwa[-11], 1, mean)
average12=apply(individual_tbwa[-12], 1, mean)

# Combine average dataset
site_average_tbwa=as.data.frame(cbind(average1,average2, average3, average4, average5, average6, average7, average8, average9, average10, average11, average12))
colnames(individual_tbwa)=colnames(individual_tbwa)