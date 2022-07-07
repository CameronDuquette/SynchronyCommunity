
###Reading in veg quadrat data
library(tidyverse)
quads=read.csv('F:/NMSU/Spatiotemporal Synchrony/JQPNoannuals15_79.csv')

str(quads)

jrn_quads_latest=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%ungroup()

