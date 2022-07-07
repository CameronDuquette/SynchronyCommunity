
###Reading in veg quadrat data
library(tidyverse)
quads=read.csv('F:/NMSU/Spatiotemporal Synchrony/JQPNoannuals15_79.csv')

str(quads)

jrn_quads_summed_total=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%filter(project_year<=1979)%>%
  mutate(spsum=sum(area, na.rm = TRUE),absum=sum(Abundance, na.rm=TRUE))%>%slice_head()%>%ungroup()


###total cover vs total abundance over time
ggplot(jrn_quads_summed_total, aes(x=project_year))+geom_smooth(aes(y=scale(spsum)), fill='blue')+
  geom_smooth(aes(y=scale(absum)), fill='red')+theme_classic()


###boer cover vs abundance over time
jrn_quads_summed_BOER=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%filter(project_year<=1979&species_code=='BOER4')%>%
  mutate(spsum=sum(area, na.rm = TRUE),absum=sum(Abundance, na.rm=TRUE))%>%slice_head()%>%ungroup()


###total cover vs total abundance over time
ggplot(jrn_quads_summed_BOER, aes(x=project_year))+geom_smooth(aes(y=scale(spsum)), fill='blue')+
  geom_smooth(aes(y=scale(absum)), fill='red')+theme_classic()
