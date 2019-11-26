##########################################################
# Convert 5-year datasets into yearly data
# Created in R 3.5.0
# By: Kristin Eccles
##########################################################

#### Old Crow Adj ####
old_crow_adj=read.csv("old_crow_adjusted.csv", header=TRUE)
# New column with time interval - 5 years
old_crow_adj$freq = 5

#replicate the rows
oc_adj.expanded = old_crow_adj[rep(row.names(old_crow_adj), old_crow_adj$freq), 1:16]

# first number is 2 years preceeding start year, and 2 years after ending year
oc_adj.expanded$Year=array(1696:2005)

#### Old Crow raw ####
old_crow_raw=read.csv("old_crow_raw.csv", header=TRUE)
# New column with time interval - 5 years
old_crow_adj$freq = 5

#replicate the rows
oc_raw.expanded = old_crow_adj[rep(row.names(old_crow_adj), old_crow_adj$freq), 1:16]

# first number is 2 years preceeding start year, and 2 years after ending year
oc_raw.expanded$Year=array(1696:2005)

#### Scree Hill ####
scree_adj=read.csv("scree_hill.csv", header=TRUE)
# New column with time interval - 5 years
scree_adj$freq = 5

#replicate the rows
scree.expanded = scree_adj[rep(row.names(scree_adj), scree_adj$freq), 1:23]

# first number is 2 years preceeding start year, and 2 years after ending year
scree.expanded$Year=array(1606:2015)


