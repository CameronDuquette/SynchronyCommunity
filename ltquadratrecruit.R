
library(wsyn)
library(ecodist)
library(tidyverse)
library(matrixStats)
library(sf)
# -----------------------------------------------------------------------
# Data import, formatting, and cleaning

jrn_quadrats=read.csv("F:/NMSU/Spatiotemporal Synchrony/JQP.csv")
#remove duplicate sampling events from beginning of time series
##If quadratxproject year not unique, choose later sampling date
jrn_quads_latest=jrn_quadrats%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%ungroup()

jrnQuads80=jrn_quads_latest[jrn_quads_latest$quadrat=="I2"|jrn_quads_latest$quadrat=="I7"|jrn_quads_latest$quadrat=="I1"|
                              jrn_quads_latest$quadrat=="I4"|jrn_quads_latest$quadrat=="I3"|jrn_quads_latest$quadrat=="I5"|
                              jrn_quads_latest$quadrat=="I6"|jrn_quads_latest$quadrat=="J1"|jrn_quads_latest$quadrat=="J8"|
                              jrn_quads_latest$quadrat=="H1"|jrn_quads_latest$quadrat=="H2"|jrn_quads_latest$quadrat=="B1"|
                              jrn_quads_latest$quadrat=="B2"|jrn_quads_latest$quadrat=="B3"|jrn_quads_latest$quadrat=="N5"|
                              jrn_quads_latest$quadrat=="H3"|jrn_quads_latest$quadrat=="J12"|jrn_quads_latest$quadrat=="N4"|
                              jrn_quads_latest$quadrat=="R2"|jrn_quads_latest$quadrat=="R3"|jrn_quads_latest$quadrat=="J9",]


JRN_PreDrought<-jrnQuads80[jrnQuads80$project_year<1951,]
JRN_PostDrought<-jrnQuads80[jrnQuads80$project_year>=1957 & jrnQuads80$project_year<=1979,]

plotsandsoil=read.csv("F:/NMSU/Spatiotemporal Synchrony/QuadratLocations.csv")

precip.drought.ra<-read.csv("F:/NMSU/Spatiotemporal Synchrony/Raind_Drought.csv")
Slow_Vars<-read.csv("F:/NMSU/Spatiotemporal Synchrony/PDO_ENSO.csv")
precip.drought.raw=precip.drought.ra%>%inner_join(Slow_Vars)


#need to center and scale
plot.ids<-unique(jrn_quadrats[,1])

years_pre<-1915:1950
years_post<-1957:1979

###now lets format total cover

################################################################################
################################################################################
################################################################################
###############################################################################
jrn_Quads_total_cover_pre=JRN_PreDrought%>%group_by(QYear)%>%mutate(spsum=sum(area, na.rm = TRUE))%>%slice_head()%>%ungroup()
jrn_Quads_total_cover_post=JRN_PostDrought%>%group_by(QYear)%>%mutate(spsum=sum(area, na.rm = TRUE))%>%slice_head()%>%ungroup()

###now do boer, forb, grass
jrn_Quads_boer_cover_pre=JRN_PreDrought%>%filter(species_code=="BOER4")%>%group_by(QYear)%>%mutate(boersum=sum(area))%>%slice_head()%>%ungroup()
jrn_Quads_boer_cover_post=JRN_PostDrought%>%filter(species_code=="BOER4")%>%group_by(QYear)%>%mutate(boersum=sum(area))%>%slice_head()%>%ungroup()

jrn_Quads_forb_cover_pre=JRN_PreDrought%>%filter(species_code=="ACNA2"|species_code=='ACOUR'|species_code=='ALCH'|
                                                   species_code=='ALIN'|species_code=='AMAL'|species_code=='AMRE'|
                                                   species_code=='AMAC2'|species_code=='AMCH3'|species_code=='APRA'|
                                                   species_code=='ASSU2'|species_code=='ASTE8'|species_code=='ASAL6'|
                                                   species_code=='ASPA14'|species_code=='ASNU4'|species_code=='ASMOB'|
                                                   species_code=='BAAB'|species_code=='BABI3'|species_code=='BAMU'|
                                                   species_code=='BELY'|species_code=='BOSP'|species_code=='CHER2'|
                                                   species_code=='CHCO'|species_code=='CHCO2'|species_code=='CHSO'|
                                                   species_code=='CHGL13'|species_code=='CHSE7'|species_code=='CHMI7'|
                                                   species_code=='CHSE6'|species_code=='CHAL11'|species_code=='CHIN2'|
                                                   species_code=='CHLE4'|species_code=='CIOC2'|species_code=='COER'|
                                                   species_code=='COEQ'|species_code=='CODR2'|species_code=='COAM8'|
                                                   species_code=='CRPO5'|species_code=='CRTE4'|species_code=='CRMI'|
                                                   species_code=='CRCR3'|species_code=='DACY'|species_code=='DAPO'|
                                                   species_code=='DANA'|species_code=='DALA3'|species_code=='DEPI'|
                                                   species_code=='DECO2'|species_code=='DIWI2'|species_code=='DRPA3'|
                                                   species_code=='ERBE2'|species_code=='ERAB2'|species_code=='ERAN4'|
                                                   species_code=='ERHI3'|species_code=='ERJA'|species_code=='ERTR8'|
                                                   species_code=='ERRO2'|species_code=='ERUM'|species_code=='EUDA5'|
                                                   species_code=='EVNU'|species_code=='FUCYC'|species_code=='GAPI'|
                                                   species_code=='GLBI2'|species_code=='GUSP'|species_code=='HEAN3'|
                                                   species_code=='HEPE'|species_code=='HECO5'|species_code=='HOGL2'|
                                                   species_code=='HODR'|species_code=='HOHU'|species_code=='HYVE'|
                                                   species_code=='HYFL'|species_code=='HYOD'|species_code=='IBTE2'|
                                                   species_code=='IPCO2'|species_code=='IPCR'|species_code=='IPLO2'|
                                                   species_code=='KAHI'|species_code=='KAPA'|species_code=='LACO13'|
                                                   species_code=='LEDE'|species_code=='LEAL4'|species_code=='LELA'|
                                                   species_code=='LEFE'|species_code=='LEGO'|species_code=='LEDE23'|
                                                   species_code=='LIVE2'|species_code=='LIAU4'|species_code=='MAPI'|
                                                   species_code=='MELE2'|species_code=='MEMU3'|species_code=='MEAL6'|
                                                   species_code=='MIRU5'|species_code=='MOCE'|species_code=='NAHI'|
                                                   species_code=='OEPR'|species_code=='OEPA'|species_code=='OEAL'|
                                                   species_code=='PASP'|species_code=='PEAN'|species_code=='PEPA2'|
                                                   species_code=='PEAM'|species_code=='PHIN'|species_code=='PHPO'|
                                                   species_code=='PHAU13'|species_code=='PHHE4'|species_code=='POJA5'|
                                                   species_code=='POPI3'|species_code=='POOL'|species_code=='POHA5'|
                                                   species_code=='PSTA'|species_code=='REAR'|species_code=='SATR12'|
                                                   species_code=='SAAB'|species_code=='SEFL3'|species_code=='SEFLF'|
                                                   species_code=='SEBA3'|species_code=='SOEL'|species_code=='SPAN3'|
                                                   species_code=='SPFE'|species_code=='SPIN2'|species_code=='SPCO'|
                                                   species_code=='SPHA'|species_code=='TECO'|species_code=='THPE4'|
                                                   species_code=='TILA2'|species_code=='TRNE'|species_code=='TRTE'|
                                                   species_code=='VEBR'|species_code=='VEEN'|species_code=='UNKAF'|
                                                   species_code=='UNKAF1'|species_code=='UNKAF2'|species_code=='UNKAF3'|
                                                   species_code=='UNKAF4'|species_code=='UNKF'|species_code=='UNKPF'|
                                                   species_code=='UNKPF1'|species_code=='UNKPF2'|species_code=='UNKPF3')%>%mutate(fcount=1)%>%group_by(QYear)%>%mutate(forbsum=sum(fcount, na.rm = TRUE))%>%slice_head()%>%ungroup()
jrn_Quads_forb_cover_post<-JRN_PostDrought%>%filter(species_code=="ACNA2"|species_code=='ACOUR'|species_code=='ALCH'|
                                                      species_code=='ALIN'|species_code=='AMAL'|species_code=='AMRE'|
                                                      species_code=='AMAC2'|species_code=='AMCH3'|species_code=='APRA'|
                                                      species_code=='ASSU2'|species_code=='ASTE8'|species_code=='ASAL6'|
                                                      species_code=='ASPA14'|species_code=='ASNU4'|species_code=='ASMOB'|
                                                      species_code=='BAAB'|species_code=='BABI3'|species_code=='BAMU'|
                                                      species_code=='BELY'|species_code=='BOSP'|species_code=='CHER2'|
                                                      species_code=='CHCO'|species_code=='CHCO2'|species_code=='CHSO'|
                                                      species_code=='CHGL13'|species_code=='CHSE7'|species_code=='CHMI7'|
                                                      species_code=='CHSE6'|species_code=='CHAL11'|species_code=='CHIN2'|
                                                      species_code=='CHLE4'|species_code=='CIOC2'|species_code=='COER'|
                                                      species_code=='COEQ'|species_code=='CODR2'|species_code=='COAM8'|
                                                      species_code=='CRPO5'|species_code=='CRTE4'|species_code=='CRMI'|
                                                      species_code=='CRCR3'|species_code=='DACY'|species_code=='DAPO'|
                                                      species_code=='DANA'|species_code=='DALA3'|species_code=='DEPI'|
                                                      species_code=='DECO2'|species_code=='DIWI2'|species_code=='DRPA3'|
                                                      species_code=='ERBE2'|species_code=='ERAB2'|species_code=='ERAN4'|
                                                      species_code=='ERHI3'|species_code=='ERJA'|species_code=='ERTR8'|
                                                      species_code=='ERRO2'|species_code=='ERUM'|species_code=='EUDA5'|
                                                      species_code=='EVNU'|species_code=='FUCYC'|species_code=='GAPI'|
                                                      species_code=='GLBI2'|species_code=='GUSP'|species_code=='HEAN3'|
                                                      species_code=='HEPE'|species_code=='HECO5'|species_code=='HOGL2'|
                                                      species_code=='HODR'|species_code=='HOHU'|species_code=='HYVE'|
                                                      species_code=='HYFL'|species_code=='HYOD'|species_code=='IBTE2'|
                                                      species_code=='IPCO2'|species_code=='IPCR'|species_code=='IPLO2'|
                                                      species_code=='KAHI'|species_code=='KAPA'|species_code=='LACO13'|
                                                      species_code=='LEDE'|species_code=='LEAL4'|species_code=='LELA'|
                                                      species_code=='LEFE'|species_code=='LEGO'|species_code=='LEDE23'|
                                                      species_code=='LIVE2'|species_code=='LIAU4'|species_code=='MAPI'|
                                                      species_code=='MELE2'|species_code=='MEMU3'|species_code=='MEAL6'|
                                                      species_code=='MIRU5'|species_code=='MOCE'|species_code=='NAHI'|
                                                      species_code=='OEPR'|species_code=='OEPA'|species_code=='OEAL'|
                                                      species_code=='PASP'|species_code=='PEAN'|species_code=='PEPA2'|
                                                      species_code=='PEAM'|species_code=='PHIN'|species_code=='PHPO'|
                                                      species_code=='PHAU13'|species_code=='PHHE4'|species_code=='POJA5'|
                                                      species_code=='POPI3'|species_code=='POOL'|species_code=='POHA5'|
                                                      species_code=='PSTA'|species_code=='REAR'|species_code=='SATR12'|
                                                      species_code=='SAAB'|species_code=='SEFL3'|species_code=='SEFLF'|
                                                      species_code=='SEBA3'|species_code=='SOEL'|species_code=='SPAN3'|
                                                      species_code=='SPFE'|species_code=='SPIN2'|species_code=='SPCO'|
                                                      species_code=='SPHA'|species_code=='TECO'|species_code=='THPE4'|
                                                      species_code=='TILA2'|species_code=='TRNE'|species_code=='TRTE'|
                                                      species_code=='VEBR'|species_code=='VEEN'|species_code=='UNKAF'|
                                                      species_code=='UNKAF1'|species_code=='UNKAF2'|species_code=='UNKAF3'|
                                                      species_code=='UNKAF4'|species_code=='UNKF'|species_code=='UNKPF'|
                                                      species_code=='UNKPF1'|species_code=='UNKPF2'|species_code=='UNKPF3')%>%mutate(fcount=1)%>%group_by(QYear)%>%mutate(forbsum=sum(fcount, na.rm = TRUE))%>%slice_head()%>%ungroup()


jrn_Quads_grass_cover_pre<-JRN_PreDrought%>%filter(species_code=='ARPUL'|species_code=='ARPU9'|
                                                     species_code=='ARAD'|species_code=='ARTE3'|
                                                     species_code=='ARIST'|species_code=='ARPA9'|
                                                     species_code=='BOER4'|species_code=='BOGR2'|
                                                     species_code=='BOHI2'|species_code=='BOAR'|
                                                     species_code=='BOCU'|species_code=='BOBA2'|
                                                     species_code=='BOUTE'|species_code=='CESP4'|
                                                     species_code=='CHVI4'|species_code=='CYRE14'|
                                                     species_code=='CYPER'|species_code=='DAPU7'|
                                                     species_code=='DICA8'|species_code=='ECCO2'|
                                                     species_code=='ENDE'|species_code=='ERLE'|
                                                     species_code=='ERCI'|species_code=='ERPE'|
                                                     species_code=='ERAGR'|species_code=='LYSE3'|
                                                     species_code=='LYPH'|species_code=='MUPO2'|
                                                     species_code=='MUAR'|species_code=='MUAR2'|
                                                     species_code=='MUHLE'|species_code=='MUSQ3'|
                                                     species_code=='PAHA'|species_code=='PAHI5'|
                                                     species_code=='PANIC'|species_code=='PAOB'|
                                                     species_code=='PACA6'|species_code=='PLMU3'|
                                                     species_code=='SCAR'|species_code=='SCBR2'|
                                                     species_code=='SELE6'|species_code=='SPAI'|
                                                     species_code=='SPNE'|species_code=='SPFL2'|
                                                     species_code=='SPCR'|species_code=='SPCO4'|
                                                     species_code=='SPORO'|species_code=='TRBE'|
                                                     species_code=='TRMU'|species_code=='UNKAG'|
                                                     species_code=='UNKG'|species_code=='UNKPG')%>%group_by(QYear)%>%mutate(grasssum=sum(area, na.rm = TRUE))%>%slice_head()%>%ungroup()


jrn_Quads_grass_cover_post<-JRN_PostDrought%>%filter(species_code=='ARPUL'|species_code=='ARPU9'|
                                                       species_code=='ARAD'|species_code=='ARTE3'|
                                                       species_code=='ARIST'|species_code=='ARPA9'|
                                                       species_code=='BOER4'|species_code=='BOGR2'|
                                                       species_code=='BOHI2'|species_code=='BOAR'|
                                                       species_code=='BOCU'|species_code=='BOBA2'|
                                                       species_code=='BOUTE'|species_code=='CESP4'|
                                                       species_code=='CHVI4'|species_code=='CYRE14'|
                                                       species_code=='CYPER'|species_code=='DAPU7'|
                                                       species_code=='DICA8'|species_code=='ECCO2'|
                                                       species_code=='ENDE'|species_code=='ERLE'|
                                                       species_code=='ERCI'|species_code=='ERPE'|
                                                       species_code=='ERAGR'|species_code=='LYSE3'|
                                                       species_code=='LYPH'|species_code=='MUPO2'|
                                                       species_code=='MUAR'|species_code=='MUAR2'|
                                                       species_code=='MUHLE'|species_code=='MUSQ3'|
                                                       species_code=='PAHA'|species_code=='PAHI5'|
                                                       species_code=='PANIC'|species_code=='PAOB'|
                                                       species_code=='PACA6'|species_code=='PLMU3'|
                                                       species_code=='SCAR'|species_code=='SCBR2'|
                                                       species_code=='SELE6'|species_code=='SPAI'|
                                                       species_code=='SPNE'|species_code=='SPFL2'|
                                                       species_code=='SPCR'|species_code=='SPCO4'|
                                                       species_code=='SPORO'|species_code=='TRBE'|
                                                       species_code=='TRMU'|species_code=='UNKAG'|
                                                       species_code=='UNKG'|species_code=='UNKPG')%>%group_by(QYear)%>%mutate(grasssum=sum(area, na.rm = TRUE))%>%slice_head()%>%ungroup()
###now lets get things into a transect by year matrix
###totalcover
jrn_Q_sm_pre=jrn_Quads_total_cover_pre[,c(1,2,12)]
jrn_Q_sm_post=jrn_Quads_total_cover_post[,c(1,2,12)]
jrn_Q_wide_pre=spread(jrn_Q_sm_pre, key='project_year', value='spsum')
jrn_Q_wide_post=spread(jrn_Q_sm_post, key='project_year', value='spsum')
##BOER
jrn_Q_BOER_pre=jrn_Quads_boer_cover_pre[,c(1,2,12)]
jrn_Q_BOER_post=jrn_Quads_boer_cover_post[,c(1,2,12)]
jrn_Q_wide_BOER_pre=spread(jrn_Q_BOER_pre, key='project_year', value='boersum')
jrn_Q_wide_BOER_post=spread(jrn_Q_BOER_post, key='project_year', value='boersum')
##Forbs
jrn_Q_FORBS_pre=jrn_Quads_forb_cover_pre[,c(1,2,13)]
jrn_Q_FORBS_post=jrn_Quads_forb_cover_post[,c(1,2,13)]
jrn_Q_wide_FORBS_pre=spread(jrn_Q_FORBS_pre, key='project_year', value='forbsum')
jrn_Q_wide_FORBS_post=spread(jrn_Q_FORBS_post, key='project_year', value='forbsum')

##Grass
jrn_Q_GRASS_pre=jrn_Quads_grass_cover_pre[,c(1,2,12)]
jrn_Q_GRASS_post=jrn_Quads_grass_cover_post[,c(1,2,12)]
jrn_Q_wide_GRASS_pre=spread(jrn_Q_GRASS_pre, key='project_year', value='grasssum')
jrn_Q_wide_GRASS_post=spread(jrn_Q_GRASS_post, key='project_year', value='grasssum')






###fill in missing years with cell medians
years_of_quad_pre=c("quadrat",1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,
                    1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,
                    1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,
                    1949,1950)
years_of_quad_post=c("quadrat",1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,
                     1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979)
###total
Missing_Pre <- setdiff(years_of_quad_pre, names(jrn_Q_wide_pre))  # Find names of missing columns
Missing_Post <- setdiff(years_of_quad_post, names(jrn_Q_wide_post))  # Find names of missing columns
##BOER
Missing_BOER_Pre <- setdiff(years_of_quad_pre, names(jrn_Q_wide_BOER_pre))  # Find names of missing columns
Missing_BOER_Post <- setdiff(years_of_quad_post, names(jrn_Q_wide_BOER_post))  # Find names of missing columns
###FORBS
Missing_FORB_Pre <- setdiff(years_of_quad_pre, names(jrn_Q_wide_FORBS_pre))  # Find names of missing columns
Missing_FORB_Post <- setdiff(years_of_quad_post, names(jrn_Q_wide_FORBS_post))  # Find names of missing columns
###GRASS
Missing_GRASS_Pre <- setdiff(years_of_quad_pre, names(jrn_Q_wide_GRASS_pre))  # Find names of missing columns
Missing_GRASS_Post <- setdiff(years_of_quad_post, names(jrn_Q_wide_GRASS_post))  # Find names of missing columns

##total
jrn_Q_wide_pre[Missing_Pre] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_post[Missing_Post] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_pre <- jrn_Q_wide_pre[years_of_quad_pre]
jrn_Q_wide_post <- jrn_Q_wide_post[years_of_quad_post]
colna_pre= colnames(jrn_Q_wide_pre[, colSums(is.na(jrn_Q_wide_pre)) == nrow(jrn_Q_wide_pre)])
rowmedians_pre=rowMedians(as.matrix(jrn_Q_wide_pre[,2:ncol(jrn_Q_wide_pre)]), na.rm=TRUE)
colna_post= colnames(jrn_Q_wide_post[, colSums(is.na(jrn_Q_wide_post)) == nrow(jrn_Q_wide_post)])
rowmedians_post=rowMedians(as.matrix(jrn_Q_wide_post[,2:ncol(jrn_Q_wide_post)]), na.rm=TRUE)
##BOER
jrn_Q_wide_BOER_pre[Missing_BOER_Pre] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_BOER_post[Missing_BOER_Post] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_BOER_pre <- jrn_Q_wide_BOER_pre[years_of_quad_pre]
jrn_Q_wide_BOER_post <- jrn_Q_wide_BOER_post[years_of_quad_post]
colna_BOER_pre= colnames(jrn_Q_wide_BOER_pre[, colSums(is.na(jrn_Q_wide_BOER_pre)) == nrow(jrn_Q_wide_BOER_pre)])
rowmedians_BOER_pre=rowMedians(as.matrix(jrn_Q_wide_BOER_pre[,2:ncol(jrn_Q_wide_BOER_pre)]), na.rm=TRUE)
colna_BOER_post= colnames(jrn_Q_wide_BOER_post[, colSums(is.na(jrn_Q_wide_BOER_post)) == nrow(jrn_Q_wide_BOER_post)])
rowmedians_BOER_post=rowMedians(as.matrix(jrn_Q_wide_BOER_post[,2:ncol(jrn_Q_wide_BOER_post)]), na.rm=TRUE)
##FORBS
jrn_Q_wide_FORBS_pre[Missing_FORB_Pre] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_FORBS_post[Missing_FORB_Post] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_FORBS_pre <- jrn_Q_wide_FORBS_pre[years_of_quad_pre]
jrn_Q_wide_FORBS_post <- jrn_Q_wide_FORBS_post[years_of_quad_post]
colna_FORBS_pre= colnames(jrn_Q_wide_FORBS_pre[, colSums(is.na(jrn_Q_wide_FORBS_pre)) == nrow(jrn_Q_wide_FORBS_pre)])
rowmedians_FORBS_pre=rowMedians(as.matrix(jrn_Q_wide_FORBS_pre[,2:ncol(jrn_Q_wide_FORBS_pre)]), na.rm=TRUE)
colna_FORBS_post= colnames(jrn_Q_wide_FORBS_post[, colSums(is.na(jrn_Q_wide_FORBS_post)) == nrow(jrn_Q_wide_FORBS_post)])
rowmedians_FORBS_post=rowMedians(as.matrix(jrn_Q_wide_FORBS_post[,2:ncol(jrn_Q_wide_FORBS_post)]), na.rm=TRUE)

##GRASS
jrn_Q_wide_GRASS_pre[Missing_GRASS_Pre] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_GRASS_post[Missing_GRASS_Post] <- NA                    # Add them, filled with 'NA's
jrn_Q_wide_GRASS_pre <- jrn_Q_wide_GRASS_pre[years_of_quad_pre]
jrn_Q_wide_GRASS_post <- jrn_Q_wide_GRASS_post[years_of_quad_post]
colna_GRASS_pre= colnames(jrn_Q_wide_GRASS_pre[, colSums(is.na(jrn_Q_wide_GRASS_pre)) == nrow(jrn_Q_wide_GRASS_pre)])
rowmedians_GRASS_pre=rowMedians(as.matrix(jrn_Q_wide_GRASS_pre[,2:ncol(jrn_Q_wide_GRASS_pre)]), na.rm=TRUE)
colna_GRASS_post= colnames(jrn_Q_wide_GRASS_post[, colSums(is.na(jrn_Q_wide_GRASS_post)) == nrow(jrn_Q_wide_GRASS_post)])
rowmedians_GRASS_post=rowMedians(as.matrix(jrn_Q_wide_GRASS_post[,2:ncol(jrn_Q_wide_GRASS_post)]), na.rm=TRUE)

###total
jrn_Q_wide_pre[,colna_pre]=rowMedians(as.matrix(jrn_Q_wide_pre[,2:ncol(jrn_Q_wide_pre)]), na.rm=TRUE)
jrn_Q_wide_post[,colna_post]=rowMedians(as.matrix(jrn_Q_wide_post[,2:ncol(jrn_Q_wide_post)]), na.rm=TRUE)
###now fill in remaining nas with rowmedians
for (i in 1:nrow(jrn_Q_wide_pre)){
  jrn_Q_wide_pre[i,which(is.na(jrn_Q_wide_pre[i,]))]=rowmedians_pre[i]
}

for (i in 1:nrow(jrn_Q_wide_post)){
  jrn_Q_wide_post[i,which(is.na(jrn_Q_wide_post[i,]))]=rowmedians_post[i]
}

jrn_Q_wide_pre<-as.data.frame(jrn_Q_wide_pre)
jrn_Q_wide_post<-as.data.frame(jrn_Q_wide_post)

rownames(jrn_Q_wide_pre)=jrn_Q_wide_pre[,1]
rownames(jrn_Q_wide_post)=jrn_Q_wide_post[,1]

jrn_Q_wide_pre=jrn_Q_wide_pre[,c(2:ncol(jrn_Q_wide_pre))]
jrn_Q_wide_post=jrn_Q_wide_post[,c(2:ncol(jrn_Q_wide_post))]


jrn_Q_wide_mat_pre=as.matrix(jrn_Q_wide_pre)
jrn_Q_wide_mat_post=as.matrix(jrn_Q_wide_post)


plot.ids_pre<-rownames(jrn_Q_wide_mat_pre)
plot.ids_post<-rownames(jrn_Q_wide_mat_post)


total_pre.cln<-cleandat(jrn_Q_wide_mat_pre,times=years_pre,clev=5)$cdat
total_post.cln<-cleandat(jrn_Q_wide_mat_post,times=years_post,clev=5)$cdat

###########BOER
jrn_Q_wide_BOER_pre[,colna_BOER_pre]=rowMedians(as.matrix(jrn_Q_wide_BOER_pre[,2:ncol(jrn_Q_wide_BOER_pre)]), na.rm=TRUE)
jrn_Q_wide_BOER_post[,colna_BOER_post]=rowMedians(as.matrix(jrn_Q_wide_BOER_post[,2:ncol(jrn_Q_wide_BOER_post)]), na.rm=TRUE)
###now fill in remaining nas with rowmedians
for (i in 1:nrow(jrn_Q_wide_BOER_pre)){
  jrn_Q_wide_BOER_pre[i,which(is.na(jrn_Q_wide_BOER_pre[i,]))]=rowmedians_BOER_pre[i]
}

for (i in 1:nrow(jrn_Q_wide_BOER_post)){
  jrn_Q_wide_BOER_post[i,which(is.na(jrn_Q_wide_BOER_post[i,]))]=rowmedians_BOER_post[i]
}

jrn_Q_wide_BOER_pre<-as.data.frame(jrn_Q_wide_BOER_pre)
jrn_Q_wide_BOER_post<-as.data.frame(jrn_Q_wide_BOER_post)

rownames(jrn_Q_wide_BOER_pre)=jrn_Q_wide_BOER_pre[,1]
rownames(jrn_Q_wide_BOER_post)=jrn_Q_wide_BOER_post[,1]

jrn_Q_wide_BOER_pre=jrn_Q_wide_BOER_pre[,c(2:ncol(jrn_Q_wide_BOER_pre))]
jrn_Q_wide_BOER_post=jrn_Q_wide_BOER_post[,c(2:ncol(jrn_Q_wide_BOER_post))]


jrn_Q_wide_mat_BOER_pre=as.matrix(jrn_Q_wide_BOER_pre)
jrn_Q_wide_mat_BOER_post=as.matrix(jrn_Q_wide_BOER_post)


plot.ids_BOER_pre<-rownames(jrn_Q_wide_mat_BOER_pre)
plot.ids_BOER_post<-rownames(jrn_Q_wide_mat_BOER_post)


total_pre.cln_BOER<-cleandat(jrn_Q_wide_mat_BOER_pre,times=years_pre,clev=5)$cdat

####remove transects with constant trend B2, J8, N5, R2
jrn_Q_wide_mat_BOER_post=jrn_Q_wide_mat_BOER_post[c(1,3,4,5,6,7,8,12),]

total_post.cln_BOER<-cleandat(jrn_Q_wide_mat_BOER_post,times=years_post,clev=5)$cdat

#########GRASS
jrn_Q_wide_GRASS_pre[,colna_GRASS_pre]=rowMedians(as.matrix(jrn_Q_wide_GRASS_pre[,2:ncol(jrn_Q_wide_GRASS_pre)]), na.rm=TRUE)
jrn_Q_wide_GRASS_post[,colna_GRASS_post]=rowMedians(as.matrix(jrn_Q_wide_GRASS_post[,2:ncol(jrn_Q_wide_GRASS_post)]), na.rm=TRUE)
###now fill in remaining nas with rowmedians
for (i in 1:nrow(jrn_Q_wide_GRASS_pre)){
  jrn_Q_wide_GRASS_pre[i,which(is.na(jrn_Q_wide_GRASS_pre[i,]))]=rowmedians_GRASS_pre[i]
}

for (i in 1:nrow(jrn_Q_wide_GRASS_post)){
  jrn_Q_wide_GRASS_post[i,which(is.na(jrn_Q_wide_GRASS_post[i,]))]=rowmedians_GRASS_post[i]
}

jrn_Q_wide_GRASS_pre<-as.data.frame(jrn_Q_wide_GRASS_pre)
jrn_Q_wide_GRASS_post<-as.data.frame(jrn_Q_wide_GRASS_post)

rownames(jrn_Q_wide_GRASS_pre)=jrn_Q_wide_GRASS_pre[,1]
rownames(jrn_Q_wide_GRASS_post)=jrn_Q_wide_GRASS_post[,1]

jrn_Q_wide_GRASS_pre=jrn_Q_wide_GRASS_pre[,c(2:ncol(jrn_Q_wide_GRASS_pre))]
jrn_Q_wide_GRASS_post=jrn_Q_wide_GRASS_post[,c(2:ncol(jrn_Q_wide_GRASS_post))]


jrn_Q_wide_mat_GRASS_pre=as.matrix(jrn_Q_wide_GRASS_pre)
jrn_Q_wide_mat_GRASS_post=as.matrix(jrn_Q_wide_GRASS_post)


plot.ids_GRASS_pre<-rownames(jrn_Q_wide_mat_GRASS_pre)
plot.ids_GRASS_post<-rownames(jrn_Q_wide_mat_GRASS_post)


total_pre.cln_GRASS<-cleandat(jrn_Q_wide_mat_GRASS_pre,times=years_pre,clev=5)$cdat

total_post.cln_GRASS<-cleandat(jrn_Q_wide_mat_GRASS_post,times=years_post,clev=5)$cdat


####FORB
jrn_Q_wide_FORBS_pre[,colna_FORBS_pre]=rowMedians(as.matrix(jrn_Q_wide_FORBS_pre[,2:ncol(jrn_Q_wide_FORBS_pre)]), na.rm=TRUE)
jrn_Q_wide_FORBS_post[,colna_FORBS_post]=rowMedians(as.matrix(jrn_Q_wide_FORBS_post[,2:ncol(jrn_Q_wide_FORBS_post)]), na.rm=TRUE)
###now fill in remaining nas with rowmedians
for (i in 1:nrow(jrn_Q_wide_FORBS_pre)){
  jrn_Q_wide_FORBS_pre[i,which(is.na(jrn_Q_wide_FORBS_pre[i,]))]=rowmedians_FORBS_pre[i]
}

for (i in 1:nrow(jrn_Q_wide_FORBS_post)){
  jrn_Q_wide_FORBS_post[i,which(is.na(jrn_Q_wide_FORBS_post[i,]))]=rowmedians_FORBS_post[i]
}

jrn_Q_wide_FORBS_pre<-as.data.frame(jrn_Q_wide_FORBS_pre)
jrn_Q_wide_FORBS_post<-as.data.frame(jrn_Q_wide_FORBS_post)

rownames(jrn_Q_wide_FORBS_pre)=jrn_Q_wide_FORBS_pre[,1]
rownames(jrn_Q_wide_FORBS_post)=jrn_Q_wide_FORBS_post[,1]

jrn_Q_wide_FORBS_pre=jrn_Q_wide_FORBS_pre[,c(2:ncol(jrn_Q_wide_FORBS_pre))]
jrn_Q_wide_FORBS_post=jrn_Q_wide_FORBS_post[,c(2:ncol(jrn_Q_wide_FORBS_post))]

jrn_Q_wide_mat_FORBS_pre=as.matrix(jrn_Q_wide_FORBS_pre)
jrn_Q_wide_mat_FORBS_post=as.matrix(jrn_Q_wide_FORBS_post)

plot.ids_FORBS_pre<-rownames(jrn_Q_wide_mat_FORBS_pre)
plot.ids_FORBS_post<-rownames(jrn_Q_wide_mat_FORBS_post)

total_pre.cln_FORBS<-cleandat(jrn_Q_wide_mat_FORBS_pre,times=years_pre,clev=5,lambdas = seq(-20,20, by=0.01))$cdat


#remove quadrats with constant trends
jrn_Q_wide_mat_FORBS_post=jrn_Q_wide_mat_FORBS_post[c(1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,18,19,20,21),]


total_post.cln_FORBS<-cleandat(jrn_Q_wide_mat_FORBS_post,times=years_post,clev=5)$cdat


#####

long_term_quads=read_sf("F:/NMSU/Spatiotemporal Synchrony/Locations/Study_351_USDA_Permanent_Quadrats.shp")

quadrat_coords=data.frame(long_term_quads$NAME,long_term_quads$POINT_X,long_term_quads$POINT_Y)

colnames(quadrat_coords)<-c("plotID","X","Y")

colnames(precip.drought.raw)

####get pdsi values for each year
pdsibyyear=precip.drought.raw%>%group_by(year)%>%slice_head()%>%ungroup()%>%select(year, PDSI)
PDObyyear=precip.drought.raw%>%group_by(year)%>%slice_head()%>%ungroup()%>%select(year, PDO)
ENSObyyear=precip.drought.raw%>%group_by(year)%>%slice_head()%>%ungroup()%>%select(year, ENSO)


PDSI_pre<-matrix(pdsibyyear$PDSI[1:36], nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=T)
PDSI_post<-matrix(pdsibyyear$PDSI[43:65], nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=T)

PDO_pre<-matrix(PDObyyear$PDO[1:36], nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=T)
PDO_post<-matrix(PDObyyear$PDO[43:65], nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=T)

ENSO_pre<-matrix(ENSObyyear$ENSO[1:36], nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=T)
ENSO_post<-matrix(ENSObyyear$ENSO[43:65], nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=T)





dat0_pre <- precip.drought.raw[precip.drought.raw$year==1915|precip.drought.raw$year==1916|
                                 precip.drought.raw$year==1917|precip.drought.raw$year==1918|
                                 precip.drought.raw$year==1919|precip.drought.raw$year==1920|
                                 precip.drought.raw$year==1921|precip.drought.raw$year==1922|
                                 precip.drought.raw$year==1923|precip.drought.raw$year==1924|
                                 precip.drought.raw$year==1925|precip.drought.raw$year==1926|
                                 precip.drought.raw$year==1927|precip.drought.raw$year==1928|
                                 precip.drought.raw$year==1929|precip.drought.raw$year==1930|
                                 precip.drought.raw$year==1931|precip.drought.raw$year==1932|
                                 precip.drought.raw$year==1933|precip.drought.raw$year==1934|
                                 precip.drought.raw$year==1935|precip.drought.raw$year==1936|
                                 precip.drought.raw$year==1937|precip.drought.raw$year==1938|
                                 precip.drought.raw$year==1939|precip.drought.raw$year==1940|
                                 precip.drought.raw$year==1941|precip.drought.raw$year==1942|
                                 precip.drought.raw$year==1943|precip.drought.raw$year==1944|
                                 precip.drought.raw$year==1945|precip.drought.raw$year==1946|
                                 precip.drought.raw$year==1947|precip.drought.raw$year==1948|
                                 precip.drought.raw$year==1949|precip.drought.raw$year==1950,] %>%tbl_df() 

dat0_post <- precip.drought.raw[precip.drought.raw$year==1957|precip.drought.raw$year==1958|
                                  precip.drought.raw$year==1959|precip.drought.raw$year==1960|
                                  precip.drought.raw$year==1961|precip.drought.raw$year==1962|
                                  precip.drought.raw$year==1963|precip.drought.raw$year==1964|
                                  precip.drought.raw$year==1965|precip.drought.raw$year==1966|
                                  precip.drought.raw$year==1967|precip.drought.raw$year==1968|
                                  precip.drought.raw$year==1969|precip.drought.raw$year==1970|
                                  precip.drought.raw$year==1971|precip.drought.raw$year==1972|
                                  precip.drought.raw$year==1973|precip.drought.raw$year==1974|
                                  precip.drought.raw$year==1975|precip.drought.raw$year==1976|
                                  precip.drought.raw$year==1977|precip.drought.raw$year==1978|
                                  precip.drought.raw$year==1979,] %>%tbl_df() 

growingdat_pre <- dat0_pre %>%
  filter(month == 7 | month==8| month==9|month == 10)  %>%
  group_by(year) %>%
  summarize(precip = sum(prec_in)) 

growingdat_post <- dat0_post %>%
  filter(month == 7 | month==8| month==9|month == 10)  %>%
  group_by(year) %>% 
  summarize(precip = sum(prec_in)) 

dormantdat_pre <- dat0_pre %>%
  filter(month == 11 |month == 12| month == 1 | month == 2|
           month == 3|month == 4|month == 5|month==6)  %>%
  group_by(year) %>%
  summarize(precip = sum(prec_in))

dormantdat_post <- dat0_post %>%
  filter(month == 11 |month == 12| month == 1 | month == 2|
           month == 3|month == 4|month == 5|month==6)  %>%
  group_by(year) %>%
  summarize(precip = sum(prec_in))

totalprecipdat_pre <- dat0_pre %>%
  filter(month == 1 |month == 2| month == 3 | month == 4|
           month == 5|month == 6|month == 7|month==8|
           month==9|month==10|month==11|month==12)  %>%
  group_by(year) %>%
  summarize(precip = sum(prec_in))

totalprecipdat_post <- dat0_post %>%
  filter(month == 1 |month == 2| month == 3 | month == 4|
           month == 5|month == 6|month == 7|month==8|
           month==9|month==10|month==11|month==12)  %>%
  group_by(year) %>%
  summarize(precip = sum(prec_in))


growindprcp_pre<-matrix(growingdat_pre$precip, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow = T)
growindprcp_post<-matrix(growingdat_post$precip, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow = T)

dormantprcp_pre<-matrix(dormantdat_pre$precip, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow = T)
dormantprcp_post<-matrix(dormantdat_post$precip, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow = T)

totalprcp_pre<-matrix(totalprecipdat_pre$precip, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow = T)
totalprcp_post<-matrix(totalprecipdat_post$precip, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow = T)



PDSIpre.cln<-cleandat(PDSI_pre, times=years_pre, clev=5)$cdat
row.names(PDSIpre.cln)<-rownames(total_pre.cln)
colnames(PDSIpre.cln)<-colnames(total_pre.cln)
PDSIpost.cln<-cleandat(PDSI_post, times=years_post, clev=5)$cdat
row.names(PDSIpost.cln)<-rownames(total_post.cln)
colnames(PDSIpost.cln)<-colnames(total_post.cln)

PDOpre.cln<-cleandat(PDO_pre, times=years_pre, clev=5)$cdat
row.names(PDOpre.cln)<-rownames(total_pre.cln)
colnames(PDOpre.cln)<-colnames(total_pre.cln)
PDOpost.cln<-cleandat(PDO_post, times=years_post, clev=5)$cdat
row.names(PDOpost.cln)<-rownames(total_post.cln)
colnames(PDOpost.cln)<-colnames(total_post.cln)

ENSOpre.cln<-cleandat(ENSO_pre, times=years_pre, clev=5)$cdat
row.names(ENSOpre.cln)<-rownames(total_pre.cln)
colnames(ENSOpre.cln)<-colnames(total_pre.cln)
ENSOpost.cln<-cleandat(ENSO_post, times=years_post, clev=5)$cdat
row.names(ENSOpost.cln)<-rownames(total_post.cln)
colnames(ENSOpost.cln)<-colnames(total_post.cln)


growingprcp_pre.cln<-cleandat(growindprcp_pre, times=years_pre, clev=5)$cdat
row.names(growingprcp_pre.cln)<-rownames(total_pre.cln)
colnames(growingprcp_pre.cln)<-colnames(total_pre.cln)

growingprcp_post.cln<-cleandat(growindprcp_post, times=years_post, clev=5)$cdat
row.names(growingprcp_post.cln)<-rownames(total_post.cln)
colnames(growingprcp_post.cln)<-colnames(total_post.cln)

dormantprcp_pre.cln<-cleandat(dormantprcp_pre, times=years_pre, clev=5)$cdat
row.names(dormantprcp_pre.cln)<-rownames(total_pre.cln)
colnames(dormantprcp_pre.cln)<-colnames(total_pre.cln)

dormantprcp_post.cln<-cleandat(dormantprcp_post, times=years_post, clev=5)$cdat
row.names(dormantprcp_post.cln)<-rownames(total_post.cln)
colnames(dormantprcp_post.cln)<-colnames(total_post.cln)

totalprcp_pre.cln<-cleandat(totalprcp_pre, times=years_pre, clev=5)$cdat
row.names(totalprcp_pre.cln)<-rownames(total_pre.cln)
colnames(totalprcp_pre.cln)<-colnames(total_pre.cln)

totalprcp_post.cln<-cleandat(totalprcp_post, times=years_post, clev=5)$cdat
row.names(totalprcp_post.cln)<-rownames(total_post.cln)
colnames(totalprcp_post.cln)<-colnames(total_post.cln)

###########prcpboer

PDSI_pre_BOER<-matrix(pdsibyyear$PDSI[1:36], nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=T)
PDSI_post_BOER<-matrix(pdsibyyear$PDSI[43:65], nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=T)

PDO_pre_BOER<-matrix(PDObyyear$PDO[1:36], nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=T)
PDO_post_BOER<-matrix(PDObyyear$PDO[43:65], nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=T)

ENSO_pre_BOER<-matrix(ENSObyyear$ENSO[1:36], nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=T)
ENSO_post_BOER<-matrix(ENSObyyear$ENSO[43:65], nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=T)


growindprcp_pre_BOER<-matrix(growingdat_pre$precip, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow = T)
growindprcp_post_BOER<-matrix(growingdat_post$precip, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow = T)

dormantprcp_pre_BOER<-matrix(dormantdat_pre$precip, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow = T)
dormantprcp_post_BOER<-matrix(dormantdat_post$precip, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow = T)

totalprcp_pre_BOER<-matrix(totalprecipdat_pre$precip, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow = T)
totalprcp_post_BOER<-matrix(totalprecipdat_post$precip, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow = T)

PDSIpre.cln_BOER<-cleandat(PDSI_pre_BOER, times=years_pre, clev=5)$cdat
row.names(PDSIpre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(PDSIpre.cln_BOER)<-colnames(total_pre.cln_BOER)

PDSIpost.cln_BOER<-cleandat(PDSI_post_BOER, times=years_post, clev=5)$cdat
row.names(PDSIpost.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(PDSIpost.cln_BOER)<-colnames(total_post.cln_BOER)

PDOpre.cln_BOER<-cleandat(PDO_pre_BOER, times=years_pre, clev=5)$cdat
row.names(PDOpre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(PDOpre.cln_BOER)<-colnames(total_pre.cln_BOER)

PDOpost.cln_BOER<-cleandat(PDO_post_BOER, times=years_post, clev=5)$cdat
row.names(PDOpost.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(PDOpost.cln_BOER)<-colnames(total_post.cln_BOER)

ENSOpre.cln_BOER<-cleandat(ENSO_pre_BOER, times=years_pre, clev=5)$cdat
row.names(ENSOpre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(ENSOpre.cln_BOER)<-colnames(total_pre.cln_BOER)

ENSOpost.cln_BOER<-cleandat(ENSO_post_BOER, times=years_post, clev=5)$cdat
row.names(ENSOpost.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(ENSOpost.cln_BOER)<-colnames(total_post.cln_BOER)

growingprcp_pre.cln_BOER<-cleandat(growindprcp_pre_BOER, times=years_pre, clev=5)$cdat
row.names(growingprcp_pre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(growingprcp_pre.cln_BOER)<-colnames(total_pre.cln_BOER)

growingprcp_post.cln_BOER<-cleandat(growindprcp_post_BOER, times=years_post, clev=5)$cdat
row.names(growingprcp_post.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(growingprcp_post.cln_BOER)<-colnames(total_post.cln_BOER)

dormantprcp_pre.cln_BOER<-cleandat(dormantprcp_pre_BOER, times=years_pre, clev=5)$cdat
row.names(dormantprcp_pre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(dormantprcp_pre.cln_BOER)<-colnames(total_pre.cln_BOER)

dormantprcp_post.cln_BOER<-cleandat(dormantprcp_post_BOER, times=years_post, clev=5)$cdat
row.names(dormantprcp_post.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(dormantprcp_post.cln_BOER)<-colnames(total_post.cln_BOER)

totalprcp_pre.cln_BOER<-cleandat(totalprcp_pre_BOER, times=years_pre, clev=5)$cdat
row.names(totalprcp_pre.cln_BOER)<-rownames(total_pre.cln_BOER)
colnames(totalprcp_pre.cln_BOER)<-colnames(total_pre.cln_BOER)

totalprcp_post.cln_BOER<-cleandat(totalprcp_post_BOER, times=years_post, clev=5)$cdat
row.names(totalprcp_post.cln_BOER)<-rownames(total_post.cln_BOER)
colnames(totalprcp_post.cln_BOER)<-colnames(total_post.cln_BOER)

###########prcpgrass
PDSI_pre_GRASS<-matrix(pdsibyyear$PDSI[1:36], nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=T)
PDSI_post_GRASS<-matrix(pdsibyyear$PDSI[43:65], nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=T)

PDO_pre_GRASS<-matrix(PDObyyear$PDO[1:36], nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=T)
PDO_post_GRASS<-matrix(PDObyyear$PDO[43:65], nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=T)

ENSO_pre_GRASS<-matrix(ENSObyyear$ENSO[1:36], nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=T)
ENSO_post_GRASS<-matrix(ENSObyyear$ENSO[43:65], nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=T)

growindprcp_pre_GRASS<-matrix(growingdat_pre$precip, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow = T)
growindprcp_post_GRASS<-matrix(growingdat_post$precip, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow = T)

dormantprcp_pre_GRASS<-matrix(dormantdat_pre$precip, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow = T)
dormantprcp_post_GRASS<-matrix(dormantdat_post$precip, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow = T)

totalprcp_pre_GRASS<-matrix(totalprecipdat_pre$precip, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow = T)
totalprcp_post_GRASS<-matrix(totalprecipdat_post$precip, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow = T)

PDSIpre.cln_GRASS<-cleandat(PDSI_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(PDSIpre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(PDSIpre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

PDSIpost.cln_GRASS<-cleandat(PDSI_post_GRASS, times=years_post, clev=5)$cdat
row.names(PDSIpost.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(PDSIpost.cln_GRASS)<-colnames(total_post.cln_GRASS)

PDOpre.cln_GRASS<-cleandat(PDO_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(PDOpre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(PDOpre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

PDOpost.cln_GRASS<-cleandat(PDO_post_GRASS, times=years_post, clev=5)$cdat
row.names(PDOpost.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(PDOpost.cln_GRASS)<-colnames(total_post.cln_GRASS)

ENSOpre.cln_GRASS<-cleandat(ENSO_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(ENSOpre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(ENSOpre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

ENSOpost.cln_GRASS<-cleandat(ENSO_post_GRASS, times=years_post, clev=5)$cdat
row.names(ENSOpost.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(ENSOpost.cln_GRASS)<-colnames(total_post.cln_GRASS)


growingprcp_pre.cln_GRASS<-cleandat(growindprcp_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(growingprcp_pre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(growingprcp_pre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

growingprcp_post.cln_GRASS<-cleandat(growindprcp_post_GRASS, times=years_post, clev=5)$cdat
row.names(growingprcp_post.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(growingprcp_post.cln_GRASS)<-colnames(total_post.cln_GRASS)

dormantprcp_pre.cln_GRASS<-cleandat(dormantprcp_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(dormantprcp_pre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(dormantprcp_pre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

dormantprcp_post.cln_GRASS<-cleandat(dormantprcp_post_GRASS, times=years_post, clev=5)$cdat
row.names(dormantprcp_post.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(dormantprcp_post.cln_GRASS)<-colnames(total_post.cln_GRASS)

totalprcp_pre.cln_GRASS<-cleandat(totalprcp_pre_GRASS, times=years_pre, clev=5)$cdat
row.names(totalprcp_pre.cln_GRASS)<-rownames(total_pre.cln_GRASS)
colnames(totalprcp_pre.cln_GRASS)<-colnames(total_pre.cln_GRASS)

totalprcp_post.cln_GRASS<-cleandat(totalprcp_post_GRASS, times=years_post, clev=5)$cdat
row.names(totalprcp_post.cln_GRASS)<-rownames(total_post.cln_GRASS)
colnames(totalprcp_post.cln_GRASS)<-colnames(total_post.cln_GRASS)

###########prcpforb
PDSI_pre_FORBS<-matrix(pdsibyyear$PDSI[1:36], nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=T)
PDSI_post_FORBS<-matrix(pdsibyyear$PDSI[43:65], nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=T)

PDO_pre_FORBS<-matrix(PDObyyear$PDO[1:36], nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=T)
PDO_post_FORBS<-matrix(PDObyyear$PDO[43:65], nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=T)

ENSO_pre_FORBS<-matrix(ENSObyyear$ENSO[1:36], nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=T)
ENSO_post_FORBS<-matrix(ENSObyyear$ENSO[43:65], nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=T)

growindprcp_pre_FORBS<-matrix(growingdat_pre$precip, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow = T)
growindprcp_post_FORBS<-matrix(growingdat_post$precip, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow = T)

dormantprcp_pre_FORBS<-matrix(dormantdat_pre$precip, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow = T)
dormantprcp_post_FORBS<-matrix(dormantdat_post$precip, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow = T)

totalprcp_pre_FORBS<-matrix(totalprecipdat_pre$precip, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow = T)
totalprcp_post_FORBS<-matrix(totalprecipdat_post$precip, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow = T)

PDSIpre.cln_FORBS<-cleandat(PDSI_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(PDSIpre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(PDSIpre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

PDSIpost.cln_FORBS<-cleandat(PDSI_post_FORBS, times=years_post, clev=5)$cdat
row.names(PDSIpost.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(PDSIpost.cln_FORBS)<-colnames(total_post.cln_FORBS)

PDOpre.cln_FORBS<-cleandat(PDO_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(PDOpre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(PDOpre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

PDOpost.cln_FORBS<-cleandat(PDO_post_FORBS, times=years_post, clev=5)$cdat
row.names(PDOpost.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(PDOpost.cln_FORBS)<-colnames(total_post.cln_FORBS)

ENSOpre.cln_FORBS<-cleandat(ENSO_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(ENSOpre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(ENSOpre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

ENSOpost.cln_FORBS<-cleandat(ENSO_post_FORBS, times=years_post, clev=5)$cdat
row.names(ENSOpost.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(ENSOpost.cln_FORBS)<-colnames(total_post.cln_FORBS)


growingprcp_pre.cln_FORBS<-cleandat(growindprcp_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(growingprcp_pre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(growingprcp_pre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

growingprcp_post.cln_FORBS<-cleandat(growindprcp_post_FORBS, times=years_post, clev=5)$cdat
row.names(growingprcp_post.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(growingprcp_post.cln_FORBS)<-colnames(total_post.cln_FORBS)

dormantprcp_pre.cln_FORBS<-cleandat(dormantprcp_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(dormantprcp_pre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(dormantprcp_pre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

dormantprcp_post.cln_FORBS<-cleandat(dormantprcp_post_FORBS, times=years_post, clev=5)$cdat
row.names(dormantprcp_post.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(dormantprcp_post.cln_FORBS)<-colnames(total_post.cln_FORBS)

totalprcp_pre.cln_FORBS<-cleandat(totalprcp_pre_FORBS, times=years_pre, clev=5)$cdat
row.names(totalprcp_pre.cln_FORBS)<-rownames(total_pre.cln_FORBS)
colnames(totalprcp_pre.cln_FORBS)<-colnames(total_pre.cln_FORBS)

totalprcp_post.cln_FORBS<-cleandat(totalprcp_post_FORBS, times=years_post, clev=5)$cdat
row.names(totalprcp_post.cln_FORBS)<-rownames(total_post.cln_FORBS)
colnames(totalprcp_post.cln_FORBS)<-colnames(total_post.cln_FORBS)




####now we will repeat for soil variables
colnames(plotsandsoil)
###first, lets just select the quadrats we are using for this work
plots_soil_select=plotsandsoil[plotsandsoil$plotID=="I2"|plotsandsoil$plotID=="I7"|plotsandsoil$plotID=="I1"|
                                 plotsandsoil$plotID=="I4"|plotsandsoil$plotID=="I3"|plotsandsoil$plotID=="I5"|
                                 plotsandsoil$plotID=="I6"|plotsandsoil$plotID=="J1"|plotsandsoil$plotID=="J8"|
                                 plotsandsoil$plotID=="H1"|plotsandsoil$plotID=="H2"|plotsandsoil$plotID=="B1"|
                                 plotsandsoil$plotID=="B2"|plotsandsoil$plotID=="B3"|plotsandsoil$plotID=="N5"|
                                 plotsandsoil$plotID=="H3"|plotsandsoil$plotID=="J12"|plotsandsoil$plotID=="N4"|
                                 plotsandsoil$plotID=="R2"|plotsandsoil$plotID=="R3"|plotsandsoil$plotID=="J9",]
plots_soil_select[plots_soil_select$plotID=="H3",17:21]=0

colnames(plots_soil_select)


#############now we will matrify and clean them TotalBiomass
soil_meandepth_pre<-matrix(plots_soil_select$mean_depth, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_meandepth_pre)=plots_soil_select$plotID
colnames(soil_meandepth_pre)=years_pre
soil_meandepth_pre<-soil_meandepth_pre[order(rownames(soil_meandepth_pre)),]



soil_meandepth_post<-matrix(plots_soil_select$mean_depth, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_meandepth_post)=plots_soil_select$plotID
colnames(soil_meandepth_post)=years_post
soil_meandepth_post<-soil_meandepth_post[order(rownames(soil_meandepth_post)),]

soil_SHALSAND_pre<-matrix(plots_soil_select$SHAL_pct_sand, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSAND_pre)=plots_soil_select$plotID
colnames(soil_SHALSAND_pre)=years_pre
soil_SHALSAND_pre<-soil_SHALSAND_pre[order(rownames(soil_SHALSAND_pre)),]

soil_SHALSAND_post<-matrix(plots_soil_select$SHAL_pct_sand, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSAND_post)=plots_soil_select$plotID
colnames(soil_SHALSAND_post)=years_post
soil_SHALSAND_post<-soil_SHALSAND_post[order(rownames(soil_SHALSAND_post)),]


soil_SHALSILT_pre<-matrix(plots_soil_select$SHAL_pct_silt, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSILT_pre)=plots_soil_select$plotID
colnames(soil_SHALSILT_pre)=years_pre
soil_SHALSILT_pre<-soil_SHALSILT_pre[order(rownames(soil_SHALSILT_pre)),]

soil_SHALSILT_post<-matrix(plots_soil_select$SHAL_pct_silt, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSILT_post)=plots_soil_select$plotID
colnames(soil_SHALSILT_post)=years_post
soil_SHALSILT_post<-soil_SHALSILT_post[order(rownames(soil_SHALSILT_post)),]


soil_SHALCLAY_pre<-matrix(plots_soil_select$SHAL_pct_clay, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCLAY_pre)=plots_soil_select$plotID
colnames(soil_SHALCLAY_pre)=years_pre
soil_SHALCLAY_pre<-soil_SHALCLAY_pre[order(rownames(soil_SHALCLAY_pre)),]

soil_SHALCLAY_post<-matrix(plots_soil_select$SHAL_pct_clay, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCLAY_post)=plots_soil_select$plotID
colnames(soil_SHALCLAY_post)=years_post
soil_SHALCLAY_post<-soil_SHALCLAY_post[order(rownames(soil_SHALCLAY_post)),]

soil_SHALVFS_pre<-matrix(plots_soil_select$SHAL_pct_vfs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVFS_pre)=plots_soil_select$plotID
colnames(soil_SHALVFS_pre)=years_pre
soil_SHALVFS_pre<-soil_SHALVFS_pre[order(rownames(soil_SHALVFS_pre)),]

soil_SHALVFS_post<-matrix(plots_soil_select$SHAL_pct_vfs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVFS_post)=plots_soil_select$plotID
colnames(soil_SHALVFS_post)=years_post
soil_SHALVFS_post<-soil_SHALVFS_post[order(rownames(soil_SHALVFS_post)),]

soil_SHALFS_pre<-matrix(plots_soil_select$SHAL_pct_fs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALFS_pre)=plots_soil_select$plotID
colnames(soil_SHALFS_pre)=years_pre
soil_SHALFS_pre<-soil_SHALFS_pre[order(rownames(soil_SHALFS_pre)),]

soil_SHALFS_post<-matrix(plots_soil_select$SHAL_pct_fs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALFS_post)=plots_soil_select$plotID
colnames(soil_SHALFS_post)=years_post
soil_SHALFS_post<-soil_SHALFS_post[order(rownames(soil_SHALFS_post)),]


soil_SHALMS_pre<-matrix(plots_soil_select$SHAL_pct_ms, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALMS_pre)=plots_soil_select$plotID
colnames(soil_SHALMS_pre)=years_pre
soil_SHALMS_pre<-soil_SHALMS_pre[order(rownames(soil_SHALMS_pre)),]

soil_SHALMS_post<-matrix(plots_soil_select$SHAL_pct_ms, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALMS_post)=plots_soil_select$plotID
colnames(soil_SHALMS_post)=years_post
soil_SHALMS_post<-soil_SHALMS_post[order(rownames(soil_SHALMS_post)),]


soil_SHALCOS_pre<-matrix(plots_soil_select$SHAL_pct_cos, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCOS_pre)=plots_soil_select$plotID
colnames(soil_SHALCOS_pre)=years_pre
soil_SHALCOS_pre<-soil_SHALCOS_pre[order(rownames(soil_SHALCOS_pre)),]

soil_SHALCOS_post<-matrix(plots_soil_select$SHAL_pct_cos, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCOS_post)=plots_soil_select$plotID
colnames(soil_SHALCOS_post)=years_post
soil_SHALCOS_post<-soil_SHALCOS_post[order(rownames(soil_SHALCOS_post)),]

soil_SHALVCS_pre<-matrix(plots_soil_select$SHAL_pct_vcs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVCS_pre)=plots_soil_select$plotID
colnames(soil_SHALVCS_pre)=years_pre
soil_SHALVCS_pre<-soil_SHALVCS_pre[order(rownames(soil_SHALVCS_pre)),]

soil_SHALVCS_post<-matrix(plots_soil_select$SHAL_pct_vcs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVCS_post)=plots_soil_select$plotID
colnames(soil_SHALVCS_post)=years_post
soil_SHALVCS_post<-soil_SHALVCS_post[order(rownames(soil_SHALVCS_post)),]

soil_DEEPSAND_pre<-matrix(plots_soil_select$DEEP_pct_sand, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSAND_pre)=plots_soil_select$plotID
colnames(soil_DEEPSAND_pre)=years_pre
soil_DEEPSAND_pre<-soil_DEEPSAND_pre[order(rownames(soil_DEEPSAND_pre)),]

soil_DEEPSAND_post<-matrix(plots_soil_select$DEEP_pct_sand, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSAND_post)=plots_soil_select$plotID
colnames(soil_DEEPSAND_post)=years_post
soil_DEEPSAND_post<-soil_DEEPSAND_post[order(rownames(soil_DEEPSAND_post)),]

soil_DEEPSILT_pre<-matrix(plots_soil_select$DEEP_pct_silt, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSILT_pre)=plots_soil_select$plotID
colnames(soil_DEEPSILT_pre)=years_pre
soil_DEEPSILT_pre<-soil_DEEPSILT_pre[order(rownames(soil_DEEPSILT_pre)),]

soil_DEEPSILT_post<-matrix(plots_soil_select$DEEP_pct_silt, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSILT_post)=plots_soil_select$plotID
colnames(soil_DEEPSILT_post)=years_post
soil_DEEPSILT_post<-soil_DEEPSILT_post[order(rownames(soil_DEEPSILT_post)),]

soil_DEEPCLAY_pre<-matrix(plots_soil_select$DEEP_pct_clay, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCLAY_pre)=plots_soil_select$plotID
colnames(soil_DEEPCLAY_pre)=years_pre
soil_DEEPCLAY_pre<-soil_DEEPCLAY_pre[order(rownames(soil_DEEPCLAY_pre)),]

soil_DEEPCLAY_post<-matrix(plots_soil_select$DEEP_pct_clay, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCLAY_post)=plots_soil_select$plotID
colnames(soil_DEEPCLAY_post)=years_post
soil_DEEPCLAY_post<-soil_DEEPCLAY_post[order(rownames(soil_DEEPCLAY_post)),]

soil_DEEPVFS_pre<-matrix(plots_soil_select$DEEP_pct_vfs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVFS_pre)=plots_soil_select$plotID
colnames(soil_DEEPVFS_pre)=years_pre
soil_DEEPVFS_pre<-soil_DEEPVFS_pre[order(rownames(soil_DEEPVFS_pre)),]

soil_DEEPVFS_post<-matrix(plots_soil_select$DEEP_pct_vfs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVFS_post)=plots_soil_select$plotID
colnames(soil_DEEPVFS_post)=years_post
soil_DEEPVFS_post<-soil_DEEPVFS_post[order(rownames(soil_DEEPVFS_post)),]

soil_DEEPFS_pre<-matrix(plots_soil_select$DEEP_pct_fs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPFS_pre)=plots_soil_select$plotID
colnames(soil_DEEPFS_pre)=years_pre
soil_DEEPFS_pre<-soil_DEEPFS_pre[order(rownames(soil_DEEPFS_pre)),]

soil_DEEPFS_post<-matrix(plots_soil_select$DEEP_pct_fs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPFS_post)=plots_soil_select$plotID
colnames(soil_DEEPFS_post)=years_post
soil_DEEPFS_post<-soil_DEEPFS_post[order(rownames(soil_DEEPFS_post)),]

soil_DEEPMS_pre<-matrix(plots_soil_select$DEEP_pct_ms, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPMS_pre)=plots_soil_select$plotID
colnames(soil_DEEPMS_pre)=years_pre
soil_DEEPMS_pre<-soil_DEEPMS_pre[order(rownames(soil_DEEPMS_pre)),]


soil_DEEPMS_post<-matrix(plots_soil_select$DEEP_pct_ms, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPMS_post)=plots_soil_select$plotID
colnames(soil_DEEPMS_post)=years_post
soil_DEEPMS_post<-soil_DEEPMS_post[order(rownames(soil_DEEPMS_post)),]

soil_DEEPCOS_pre<-matrix(plots_soil_select$DEEP_pct_cos, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCOS_pre)=plots_soil_select$plotID
colnames(soil_DEEPCOS_pre)=years_pre
soil_DEEPCOS_pre<-soil_DEEPCOS_pre[order(rownames(soil_DEEPCOS_pre)),]

soil_DEEPCOS_post<-matrix(plots_soil_select$DEEP_pct_cos, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCOS_post)=plots_soil_select$plotID
colnames(soil_DEEPCOS_post)=years_post
soil_DEEPCOS_post<-soil_DEEPCOS_post[order(rownames(soil_DEEPCOS_post)),]

soil_DEEPVCS_pre<-matrix(plots_soil_select$DEEP_pct_vcs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVCS_pre)=plots_soil_select$plotID
colnames(soil_DEEPVCS_pre)=years_pre
soil_DEEPVCS_pre<-soil_DEEPVCS_pre[order(rownames(soil_DEEPVCS_pre)),]

soil_DEEPVCS_post<-matrix(plots_soil_select$DEEP_pct_vcs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVCS_post)=plots_soil_select$plotID
colnames(soil_DEEPVCS_post)=years_post
soil_DEEPVCS_post<-soil_DEEPVCS_post[order(rownames(soil_DEEPVCS_post)),]

##BOER
#############now we will matrify and clean them BOER
###did we have to cut out any transects due to constant trends?
rownames(total_pre.cln)
rownames(total_pre.cln_BOER) #######yes! I3 was removed
rownames(total_post.cln_BOER) #######yes! B2, H1, I1, I2, I3, I6, J1, J12, J8, J9, N4, N5, R2
plots_soil_select_pre_BOER=plots_soil_select[!plots_soil_select$plotID=="I3",]
plots_soil_select_post_BOER=plots_soil_select[!plots_soil_select$plotID=="B2"&
                                                !plots_soil_select$plotID=="H1"&
                                                !plots_soil_select$plotID=="I1"&
                                                !plots_soil_select$plotID=="I2"&
                                                !plots_soil_select$plotID=="I3"&
                                                !plots_soil_select$plotID=="I6"&
                                                !plots_soil_select$plotID=="J1"&
                                                !plots_soil_select$plotID=="J12"&
                                                !plots_soil_select$plotID=="J8"&
                                                !plots_soil_select$plotID=="J9"&
                                                !plots_soil_select$plotID=="N4"&
                                                !plots_soil_select$plotID=="N5"&
                                                !plots_soil_select$plotID=="R2",]


soil_meandepth_pre_BOER<-matrix(plots_soil_select_pre_BOER$mean_depth, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_meandepth_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_meandepth_pre_BOER)=years_pre
soil_meandepth_pre_BOER<-soil_meandepth_pre_BOER[order(rownames(soil_meandepth_pre_BOER)),]

soil_meandepth_post_BOER<-matrix(plots_soil_select_post_BOER$mean_depth, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_meandepth_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_meandepth_post_BOER)=years_post
soil_meandepth_post_BOER<-soil_meandepth_post_BOER[order(rownames(soil_meandepth_post_BOER)),]

soil_SHALSAND_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_sand, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSAND_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALSAND_pre_BOER)=years_pre
soil_SHALSAND_pre_BOER<-soil_SHALSAND_pre_BOER[order(rownames(soil_SHALSAND_pre_BOER)),]

soil_SHALSAND_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_sand, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSAND_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALSAND_post_BOER)=years_post
soil_SHALSAND_post_BOER<-soil_SHALSAND_post_BOER[order(rownames(soil_SHALSAND_post_BOER)),]

soil_SHALSILT_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_silt, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSILT_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALSILT_pre_BOER)=years_pre
soil_SHALSILT_pre_BOER<-soil_SHALSILT_pre_BOER[order(rownames(soil_SHALSILT_pre_BOER)),]

soil_SHALSILT_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_silt, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSILT_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALSILT_post_BOER)=years_post
soil_SHALSILT_post_BOER<-soil_SHALSILT_post_BOER[order(rownames(soil_SHALSILT_post_BOER)),]

soil_SHALCLAY_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_clay, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCLAY_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALCLAY_pre_BOER)=years_pre
soil_SHALCLAY_pre_BOER<-soil_SHALCLAY_pre_BOER[order(rownames(soil_SHALCLAY_pre_BOER)),]

soil_SHALCLAY_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_clay, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCLAY_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALCLAY_post_BOER)=years_post
soil_SHALCLAY_post_BOER<-soil_SHALCLAY_post_BOER[order(rownames(soil_SHALCLAY_post_BOER)),]

soil_SHALVFS_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_vfs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVFS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALVFS_pre_BOER)=years_pre
soil_SHALVFS_pre_BOER<-soil_SHALVFS_pre_BOER[order(rownames(soil_SHALVFS_pre_BOER)),]

soil_SHALVFS_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_vfs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVFS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALVFS_post_BOER)=years_post
soil_SHALVFS_post_BOER<-soil_SHALVFS_post_BOER[order(rownames(soil_SHALVFS_post_BOER)),]

soil_SHALFS_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_fs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALFS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALFS_pre_BOER)=years_pre
soil_SHALFS_pre_BOER<-soil_SHALFS_pre_BOER[order(rownames(soil_SHALFS_pre_BOER)),]

soil_SHALFS_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_fs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALFS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALFS_post_BOER)=years_post
soil_SHALFS_post_BOER<-soil_SHALFS_post_BOER[order(rownames(soil_SHALFS_post_BOER)),]

soil_SHALMS_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_ms, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALMS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALMS_pre_BOER)=years_pre
soil_SHALMS_pre_BOER<-soil_SHALMS_pre_BOER[order(rownames(soil_SHALMS_pre_BOER)),]

soil_SHALMS_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_ms, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALMS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALMS_post_BOER)=years_post
soil_SHALMS_post_BOER<-soil_SHALMS_post_BOER[order(rownames(soil_SHALMS_post_BOER)),]

soil_SHALCOS_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_cos, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCOS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALCOS_pre_BOER)=years_pre
soil_SHALCOS_pre_BOER<-soil_SHALCOS_pre_BOER[order(rownames(soil_SHALCOS_pre_BOER)),]

soil_SHALCOS_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_cos, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCOS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALCOS_post_BOER)=years_post
soil_SHALCOS_post_BOER<-soil_SHALCOS_post_BOER[order(rownames(soil_SHALCOS_post_BOER)),]

soil_SHALVCS_pre_BOER<-matrix(plots_soil_select_pre_BOER$SHAL_pct_vcs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVCS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_SHALVCS_pre_BOER)=years_pre
soil_SHALVCS_pre_BOER<-soil_SHALVCS_pre_BOER[order(rownames(soil_SHALVCS_pre_BOER)),]

soil_SHALVCS_post_BOER<-matrix(plots_soil_select_post_BOER$SHAL_pct_vcs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVCS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_SHALVCS_post_BOER)=years_post
soil_SHALVCS_post_BOER<-soil_SHALVCS_post_BOER[order(rownames(soil_SHALVCS_post_BOER)),]

soil_DEEPSAND_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_sand, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSAND_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPSAND_pre_BOER)=years_pre
soil_DEEPSAND_pre_BOER<-soil_DEEPSAND_pre_BOER[order(rownames(soil_DEEPSAND_pre_BOER)),]

soil_DEEPSAND_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_sand, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSAND_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPSAND_post_BOER)=years_post
soil_DEEPSAND_post_BOER<-soil_DEEPSAND_post_BOER[order(rownames(soil_DEEPSAND_post_BOER)),]

soil_DEEPSILT_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_silt, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSILT_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPSILT_pre_BOER)=years_pre
soil_DEEPSILT_pre_BOER<-soil_DEEPSILT_pre_BOER[order(rownames(soil_DEEPSILT_pre_BOER)),]

soil_DEEPSILT_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_silt, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSILT_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPSILT_post_BOER)=years_post
soil_DEEPSILT_post_BOER<-soil_DEEPSILT_post_BOER[order(rownames(soil_DEEPSILT_post_BOER)),]

soil_DEEPCLAY_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_clay, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCLAY_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPCLAY_pre_BOER)=years_pre
soil_DEEPCLAY_pre_BOER<-soil_DEEPCLAY_pre_BOER[order(rownames(soil_DEEPCLAY_pre_BOER)),]

soil_DEEPCLAY_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_clay, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCLAY_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPCLAY_post_BOER)=years_post
soil_DEEPCLAY_post_BOER<-soil_DEEPCLAY_post_BOER[order(rownames(soil_DEEPCLAY_post_BOER)),]

soil_DEEPVFS_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_vfs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVFS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPVFS_pre_BOER)=years_pre
soil_DEEPVFS_pre_BOER<-soil_DEEPVFS_pre_BOER[order(rownames(soil_DEEPVFS_pre_BOER)),]

soil_DEEPVFS_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_vfs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVFS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPVFS_post_BOER)=years_post
soil_DEEPVFS_post_BOER<-soil_DEEPVFS_post_BOER[order(rownames(soil_DEEPVFS_post_BOER)),]

soil_DEEPFS_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_fs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPFS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPFS_pre_BOER)=years_pre
soil_DEEPFS_pre_BOER<-soil_DEEPFS_pre_BOER[order(rownames(soil_DEEPFS_pre_BOER)),]

soil_DEEPFS_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_fs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPFS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPFS_post_BOER)=years_post
soil_DEEPFS_post_BOER<-soil_DEEPFS_post_BOER[order(rownames(soil_DEEPFS_post_BOER)),]

soil_DEEPMS_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_ms, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPMS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPMS_pre_BOER)=years_pre
soil_DEEPMS_pre_BOER<-soil_DEEPMS_pre_BOER[order(rownames(soil_DEEPMS_pre_BOER)),]

soil_DEEPMS_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_ms, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPMS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPMS_post_BOER)=years_post
soil_DEEPMS_post_BOER<-soil_DEEPMS_post_BOER[order(rownames(soil_DEEPMS_post_BOER)),]

soil_DEEPCOS_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_cos, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCOS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPCOS_pre_BOER)=years_pre
soil_DEEPCOS_pre_BOER<-soil_DEEPCOS_pre_BOER[order(rownames(soil_DEEPCOS_pre_BOER)),]

soil_DEEPCOS_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_cos, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCOS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPCOS_post_BOER)=years_post
soil_DEEPCOS_post_BOER<-soil_DEEPCOS_post_BOER[order(rownames(soil_DEEPCOS_post_BOER)),]

soil_DEEPVCS_pre_BOER<-matrix(plots_soil_select_pre_BOER$DEEP_pct_vcs, nrow=length(row.names(total_pre.cln_BOER)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVCS_pre_BOER)=plots_soil_select_pre_BOER$plotID
colnames(soil_DEEPVCS_pre_BOER)=years_pre
soil_DEEPVCS_pre_BOER<-soil_DEEPVCS_pre_BOER[order(rownames(soil_DEEPVCS_pre_BOER)),]

soil_DEEPVCS_post_BOER<-matrix(plots_soil_select_post_BOER$DEEP_pct_vcs, nrow=length(row.names(total_post.cln_BOER)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVCS_post_BOER)=plots_soil_select_post_BOER$plotID
colnames(soil_DEEPVCS_post_BOER)=years_post
soil_DEEPVCS_post_BOER<-soil_DEEPVCS_post_BOER[order(rownames(soil_DEEPVCS_post_BOER)),]

#################GRASS
rownames(total_pre.cln)
rownames(total_pre.cln_GRASS) #######yes! I3 was removed
rownames(total_post.cln_GRASS) #######yes! B2, H1, I1, I2, I3, I6, J1, J12, J8, J9, N4, N5, R2
plots_soil_select_pre_GRASS=plots_soil_select
plots_soil_select_post_GRASS=plots_soil_select

soil_meandepth_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$mean_depth, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_meandepth_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_meandepth_pre_GRASS)=years_pre
soil_meandepth_pre_GRASS<-soil_meandepth_pre_GRASS[order(rownames(soil_meandepth_pre_GRASS)),]

soil_meandepth_post_GRASS<-matrix(plots_soil_select_post_GRASS$mean_depth, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_meandepth_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_meandepth_post_GRASS)=years_post
soil_meandepth_post_GRASS<-soil_meandepth_post_GRASS[order(rownames(soil_meandepth_post_GRASS)),]

soil_SHALSAND_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_sand, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSAND_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALSAND_pre_GRASS)=years_pre
soil_SHALSAND_pre_GRASS<-soil_SHALSAND_pre_GRASS[order(rownames(soil_SHALSAND_pre_GRASS)),]

soil_SHALSAND_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_sand, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSAND_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALSAND_post_GRASS)=years_post
soil_SHALSAND_post_GRASS<-soil_SHALSAND_post_GRASS[order(rownames(soil_SHALSAND_post_GRASS)),]

soil_SHALSILT_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_silt, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSILT_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALSILT_pre_GRASS)=years_pre
soil_SHALSILT_pre_GRASS<-soil_SHALSILT_pre_GRASS[order(rownames(soil_SHALSILT_pre_GRASS)),]

soil_SHALSILT_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_silt, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSILT_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALSILT_post_GRASS)=years_post
soil_SHALSILT_post_GRASS<-soil_SHALSILT_post_GRASS[order(rownames(soil_SHALSILT_post_GRASS)),]

soil_SHALCLAY_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_clay, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCLAY_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALCLAY_pre_GRASS)=years_pre
soil_SHALCLAY_pre_GRASS<-soil_SHALCLAY_pre_GRASS[order(rownames(soil_SHALCLAY_pre_GRASS)),]

soil_SHALCLAY_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_clay, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCLAY_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALCLAY_post_GRASS)=years_post
soil_SHALCLAY_post_GRASS<-soil_SHALCLAY_post_GRASS[order(rownames(soil_SHALCLAY_post_GRASS)),]

soil_SHALVFS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_vfs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVFS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALVFS_pre_GRASS)=years_pre
soil_SHALVFS_pre_GRASS<-soil_SHALVFS_pre_GRASS[order(rownames(soil_SHALVFS_pre_GRASS)),]

soil_SHALVFS_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_vfs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVFS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALVFS_post_GRASS)=years_post
soil_SHALVFS_post_GRASS<-soil_SHALVFS_post_GRASS[order(rownames(soil_SHALVFS_post_GRASS)),]

soil_SHALFS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_fs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALFS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALFS_pre_GRASS)=years_pre
soil_SHALFS_pre_GRASS<-soil_SHALFS_pre_GRASS[order(rownames(soil_SHALFS_pre_GRASS)),]

soil_SHALFS_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_fs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALFS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALFS_post_GRASS)=years_post
soil_SHALFS_post_GRASS<-soil_SHALFS_post_GRASS[order(rownames(soil_SHALFS_post_GRASS)),]

soil_SHALMS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_ms, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALMS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALMS_pre_GRASS)=years_pre
soil_SHALMS_pre_GRASS<-soil_SHALMS_pre_GRASS[order(rownames(soil_SHALMS_pre_GRASS)),]

soil_SHALMS_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_ms, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALMS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALMS_post_GRASS)=years_post
soil_SHALMS_post_GRASS<-soil_SHALMS_post_GRASS[order(rownames(soil_SHALMS_post_GRASS)),]

soil_SHALCOS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_cos, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCOS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALCOS_pre_GRASS)=years_pre
soil_SHALCOS_pre_GRASS<-soil_SHALCOS_pre_GRASS[order(rownames(soil_SHALCOS_pre_GRASS)),]

soil_SHALCOS_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_cos, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCOS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALCOS_post_GRASS)=years_post
soil_SHALCOS_post_GRASS<-soil_SHALCOS_post_GRASS[order(rownames(soil_SHALCOS_post_GRASS)),]

soil_SHALVCS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$SHAL_pct_vcs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVCS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_SHALVCS_pre_GRASS)=years_pre
soil_SHALVCS_pre_GRASS<-soil_SHALVCS_pre_GRASS[order(rownames(soil_SHALVCS_pre_GRASS)),]

soil_SHALVCS_post_GRASS<-matrix(plots_soil_select_post_GRASS$SHAL_pct_vcs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVCS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_SHALVCS_post_GRASS)=years_post
soil_SHALVCS_post_GRASS<-soil_SHALVCS_post_GRASS[order(rownames(soil_SHALVCS_post_GRASS)),]

soil_DEEPSAND_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_sand, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSAND_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPSAND_pre_GRASS)=years_pre
soil_DEEPSAND_pre_GRASS<-soil_DEEPSAND_pre_GRASS[order(rownames(soil_DEEPSAND_pre_GRASS)),]

soil_DEEPSAND_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_sand, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSAND_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPSAND_post_GRASS)=years_post
soil_DEEPSAND_post_GRASS<-soil_DEEPSAND_post_GRASS[order(rownames(soil_DEEPSAND_post_GRASS)),]

soil_DEEPSILT_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_silt, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSILT_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPSILT_pre_GRASS)=years_pre
soil_DEEPSILT_pre_GRASS<-soil_DEEPSILT_pre_GRASS[order(rownames(soil_DEEPSILT_pre_GRASS)),]

soil_DEEPSILT_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_silt, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSILT_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPSILT_post_GRASS)=years_post
soil_DEEPSILT_post_GRASS<-soil_DEEPSILT_post_GRASS[order(rownames(soil_DEEPSILT_post_GRASS)),]

soil_DEEPCLAY_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_clay, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCLAY_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPCLAY_pre_GRASS)=years_pre
soil_DEEPCLAY_pre_GRASS<-soil_DEEPCLAY_pre_GRASS[order(rownames(soil_DEEPCLAY_pre_GRASS)),]

soil_DEEPCLAY_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_clay, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCLAY_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPCLAY_post_GRASS)=years_post
soil_DEEPCLAY_post_GRASS<-soil_DEEPCLAY_post_GRASS[order(rownames(soil_DEEPCLAY_post_GRASS)),]

soil_DEEPVFS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_vfs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVFS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPVFS_pre_GRASS)=years_pre
soil_DEEPVFS_pre_GRASS<-soil_DEEPVFS_pre_GRASS[order(rownames(soil_DEEPVFS_pre_GRASS)),]

soil_DEEPVFS_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_vfs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVFS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPVFS_post_GRASS)=years_post
soil_DEEPVFS_post_GRASS<-soil_DEEPVFS_post_GRASS[order(rownames(soil_DEEPVFS_post_GRASS)),]

soil_DEEPFS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_fs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPFS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPFS_pre_GRASS)=years_pre
soil_DEEPFS_pre_GRASS<-soil_DEEPFS_pre_GRASS[order(rownames(soil_DEEPFS_pre_GRASS)),]

soil_DEEPFS_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_fs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPFS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPFS_post_GRASS)=years_post
soil_DEEPFS_post_GRASS<-soil_DEEPFS_post_GRASS[order(rownames(soil_DEEPFS_post_GRASS)),]

soil_DEEPMS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_ms, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPMS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPMS_pre_GRASS)=years_pre
soil_DEEPMS_pre_GRASS<-soil_DEEPMS_pre_GRASS[order(rownames(soil_DEEPMS_pre_GRASS)),]

soil_DEEPMS_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_ms, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPMS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPMS_post_GRASS)=years_post
soil_DEEPMS_post_GRASS<-soil_DEEPMS_post_GRASS[order(rownames(soil_DEEPMS_post_GRASS)),]

soil_DEEPCOS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_cos, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCOS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPCOS_pre_GRASS)=years_pre
soil_DEEPCOS_pre_GRASS<-soil_DEEPCOS_pre_GRASS[order(rownames(soil_DEEPCOS_pre_GRASS)),]

soil_DEEPCOS_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_cos, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCOS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPCOS_post_GRASS)=years_post
soil_DEEPCOS_post_GRASS<-soil_DEEPCOS_post_GRASS[order(rownames(soil_DEEPCOS_post_GRASS)),]

soil_DEEPVCS_pre_GRASS<-matrix(plots_soil_select_pre_GRASS$DEEP_pct_vcs, nrow=length(row.names(total_pre.cln_GRASS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVCS_pre_GRASS)=plots_soil_select_pre_GRASS$plotID
colnames(soil_DEEPVCS_pre_GRASS)=years_pre
soil_DEEPVCS_pre_GRASS<-soil_DEEPVCS_pre_GRASS[order(rownames(soil_DEEPVCS_pre_GRASS)),]

soil_DEEPVCS_post_GRASS<-matrix(plots_soil_select_post_GRASS$DEEP_pct_vcs, nrow=length(row.names(total_post.cln_GRASS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVCS_post_GRASS)=plots_soil_select_post_GRASS$plotID
colnames(soil_DEEPVCS_post_GRASS)=years_post
soil_DEEPVCS_post_GRASS<-soil_DEEPVCS_post_GRASS[order(rownames(soil_DEEPVCS_post_GRASS)),]

###################FORBS
rownames(total_pre.cln)
rownames(total_pre.cln_FORBS) #######yes! I3 was removed
rownames(total_post.cln_FORBS) #######yes! B2, H1, I1, I2, I3, I6, J1, J12, J8, J9, N4, N5, R2
plots_soil_select_pre_FORBS=plots_soil_select
plots_soil_select_post_FORBS=plots_soil_select[  !plots_soil_select$plotID=="I3"&
                                                   !plots_soil_select$plotID=="I4",]


soil_meandepth_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$mean_depth, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_meandepth_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_meandepth_pre_FORBS)=years_pre
soil_meandepth_pre_FORBS<-soil_meandepth_pre_FORBS[order(rownames(soil_meandepth_pre_FORBS)),]

soil_meandepth_post_FORBS<-matrix(plots_soil_select_post_FORBS$mean_depth, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_meandepth_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_meandepth_post_FORBS)=years_post
soil_meandepth_post_FORBS<-soil_meandepth_post_FORBS[order(rownames(soil_meandepth_post_FORBS)),]

soil_SHALSAND_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_sand, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSAND_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALSAND_pre_FORBS)=years_pre
soil_SHALSAND_pre_FORBS<-soil_SHALSAND_pre_FORBS[order(rownames(soil_SHALSAND_pre_FORBS)),]

soil_SHALSAND_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_sand, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSAND_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALSAND_post_FORBS)=years_post
soil_SHALSAND_post_FORBS<-soil_SHALSAND_post_FORBS[order(rownames(soil_SHALSAND_post_FORBS)),]

soil_SHALSILT_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_silt, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALSILT_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALSILT_pre_FORBS)=years_pre
soil_SHALSILT_pre_FORBS<-soil_SHALSILT_pre_FORBS[order(rownames(soil_SHALSILT_pre_FORBS)),]

soil_SHALSILT_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_silt, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALSILT_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALSILT_post_FORBS)=years_post
soil_SHALSILT_post_FORBS<-soil_SHALSILT_post_FORBS[order(rownames(soil_SHALSILT_post_FORBS)),]

soil_SHALCLAY_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_clay, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCLAY_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALCLAY_pre_FORBS)=years_pre
soil_SHALCLAY_pre_FORBS<-soil_SHALCLAY_pre_FORBS[order(rownames(soil_SHALCLAY_pre_FORBS)),]

soil_SHALCLAY_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_clay, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCLAY_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALCLAY_post_FORBS)=years_post
soil_SHALCLAY_post_FORBS<-soil_SHALCLAY_post_FORBS[order(rownames(soil_SHALCLAY_post_FORBS)),]

soil_SHALVFS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_vfs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVFS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALVFS_pre_FORBS)=years_pre
soil_SHALVFS_pre_FORBS<-soil_SHALVFS_pre_FORBS[order(rownames(soil_SHALVFS_pre_FORBS)),]

soil_SHALVFS_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_vfs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVFS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALVFS_post_FORBS)=years_post
soil_SHALVFS_post_FORBS<-soil_SHALVFS_post_FORBS[order(rownames(soil_SHALVFS_post_FORBS)),]

soil_SHALFS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_fs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALFS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALFS_pre_FORBS)=years_pre
soil_SHALFS_pre_FORBS<-soil_SHALFS_pre_FORBS[order(rownames(soil_SHALFS_pre_FORBS)),]

soil_SHALFS_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_fs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALFS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALFS_post_FORBS)=years_post
soil_SHALFS_post_FORBS<-soil_SHALFS_post_FORBS[order(rownames(soil_SHALFS_post_FORBS)),]

soil_SHALMS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_ms, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALMS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALMS_pre_FORBS)=years_pre
soil_SHALMS_pre_FORBS<-soil_SHALMS_pre_FORBS[order(rownames(soil_SHALMS_pre_FORBS)),]

soil_SHALMS_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_ms, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALMS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALMS_post_FORBS)=years_post
soil_SHALMS_post_FORBS<-soil_SHALMS_post_FORBS[order(rownames(soil_SHALMS_post_FORBS)),]

soil_SHALCOS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_cos, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALCOS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALCOS_pre_FORBS)=years_pre
soil_SHALCOS_pre_FORBS<-soil_SHALCOS_pre_FORBS[order(rownames(soil_SHALCOS_pre_FORBS)),]

soil_SHALCOS_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_cos, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALCOS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALCOS_post_FORBS)=years_post
soil_SHALCOS_post_FORBS<-soil_SHALCOS_post_FORBS[order(rownames(soil_SHALCOS_post_FORBS)),]

soil_SHALVCS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$SHAL_pct_vcs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_SHALVCS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_SHALVCS_pre_FORBS)=years_pre
soil_SHALVCS_pre_FORBS<-soil_SHALVCS_pre_FORBS[order(rownames(soil_SHALVCS_pre_FORBS)),]

soil_SHALVCS_post_FORBS<-matrix(plots_soil_select_post_FORBS$SHAL_pct_vcs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_SHALVCS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_SHALVCS_post_FORBS)=years_post
soil_SHALVCS_post_FORBS<-soil_SHALVCS_post_FORBS[order(rownames(soil_SHALVCS_post_FORBS)),]

soil_DEEPSAND_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_sand, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSAND_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPSAND_pre_FORBS)=years_pre
soil_DEEPSAND_pre_FORBS<-soil_DEEPSAND_pre_FORBS[order(rownames(soil_DEEPSAND_pre_FORBS)),]

soil_DEEPSAND_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_sand, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSAND_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPSAND_post_FORBS)=years_post
soil_DEEPSAND_post_FORBS<-soil_DEEPSAND_post_FORBS[order(rownames(soil_DEEPSAND_post_FORBS)),]

soil_DEEPSILT_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_silt, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPSILT_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPSILT_pre_FORBS)=years_pre
soil_DEEPSILT_pre_FORBS<-soil_DEEPSILT_pre_FORBS[order(rownames(soil_DEEPSILT_pre_FORBS)),]

soil_DEEPSILT_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_silt, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPSILT_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPSILT_post_FORBS)=years_post
soil_DEEPSILT_post_FORBS<-soil_DEEPSILT_post_FORBS[order(rownames(soil_DEEPSILT_post_FORBS)),]

soil_DEEPCLAY_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_clay, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCLAY_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPCLAY_pre_FORBS)=years_pre
soil_DEEPCLAY_pre_FORBS<-soil_DEEPCLAY_pre_FORBS[order(rownames(soil_DEEPCLAY_pre_FORBS)),]

soil_DEEPCLAY_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_clay, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCLAY_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPCLAY_post_FORBS)=years_post
soil_DEEPCLAY_post_FORBS<-soil_DEEPCLAY_post_FORBS[order(rownames(soil_DEEPCLAY_post_FORBS)),]

soil_DEEPVFS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_vfs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVFS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPVFS_pre_FORBS)=years_pre
soil_DEEPVFS_pre_FORBS<-soil_DEEPVFS_pre_FORBS[order(rownames(soil_DEEPVFS_pre_FORBS)),]

soil_DEEPVFS_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_vfs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVFS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPVFS_post_FORBS)=years_post
soil_DEEPVFS_post_FORBS<-soil_DEEPVFS_post_FORBS[order(rownames(soil_DEEPVFS_post_FORBS)),]

soil_DEEPFS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_fs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPFS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPFS_pre_FORBS)=years_pre
soil_DEEPFS_pre_FORBS<-soil_DEEPFS_pre_FORBS[order(rownames(soil_DEEPFS_pre_FORBS)),]

soil_DEEPFS_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_fs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPFS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPFS_post_FORBS)=years_post
soil_DEEPFS_post_FORBS<-soil_DEEPFS_post_FORBS[order(rownames(soil_DEEPFS_post_FORBS)),]

soil_DEEPMS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_ms, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPMS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPMS_pre_FORBS)=years_pre
soil_DEEPMS_pre_FORBS<-soil_DEEPMS_pre_FORBS[order(rownames(soil_DEEPMS_pre_FORBS)),]

soil_DEEPMS_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_ms, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPMS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPMS_post_FORBS)=years_post
soil_DEEPMS_post_FORBS<-soil_DEEPMS_post_FORBS[order(rownames(soil_DEEPMS_post_FORBS)),]

soil_DEEPCOS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_cos, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPCOS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPCOS_pre_FORBS)=years_pre
soil_DEEPCOS_pre_FORBS<-soil_DEEPCOS_pre_FORBS[order(rownames(soil_DEEPCOS_pre_FORBS)),]

soil_DEEPCOS_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_cos, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPCOS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPCOS_post_FORBS)=years_post
soil_DEEPCOS_post_FORBS<-soil_DEEPCOS_post_FORBS[order(rownames(soil_DEEPCOS_post_FORBS)),]

soil_DEEPVCS_pre_FORBS<-matrix(plots_soil_select_pre_FORBS$DEEP_pct_vcs, nrow=length(row.names(total_pre.cln_FORBS)), ncol=length(years_pre), byrow=F)
rownames(soil_DEEPVCS_pre_FORBS)=plots_soil_select_pre_FORBS$plotID
colnames(soil_DEEPVCS_pre_FORBS)=years_pre
soil_DEEPVCS_pre_FORBS<-soil_DEEPVCS_pre_FORBS[order(rownames(soil_DEEPVCS_pre_FORBS)),]

soil_DEEPVCS_post_FORBS<-matrix(plots_soil_select_post_FORBS$DEEP_pct_vcs, nrow=length(row.names(total_post.cln_FORBS)), ncol=length(years_post), byrow=F)
rownames(soil_DEEPVCS_post_FORBS)=plots_soil_select_post_FORBS$plotID
colnames(soil_DEEPVCS_post_FORBS)=years_post
soil_DEEPVCS_post_FORBS<-soil_DEEPVCS_post_FORBS[order(rownames(soil_DEEPVCS_post_FORBS)),]

###################################
##################################
####################Pick up here 5_27
## Geography of Synchrony Analyses

#Create coherence matrices and clustering; plot distance decay of synchrony and networks; matrix regression analyses


# -----------------------------------------------------------------------
# 1. Geography of synchrony
# -----------------------------------------------------------------------
##select only quadrat coordinates from the quadrats we are using
##################
#################
#################
quadrat_coords_s=quadrat_coords[quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I7"|quadrat_coords$plotID=="I1"|
                                  quadrat_coords$plotID=="I4"|quadrat_coords$plotID=="I3"|quadrat_coords$plotID=="I5"|
                                  quadrat_coords$plotID=="I6"|quadrat_coords$plotID=="J1"|quadrat_coords$plotID=="J8"|
                                  quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="B1"|
                                  quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|quadrat_coords$plotID=="N5"|
                                  quadrat_coords$plotID=="H3"|quadrat_coords$plotID=="J12"|quadrat_coords$plotID=="N4"|
                                  quadrat_coords$plotID=="R2"|quadrat_coords$plotID=="R3"|quadrat_coords$plotID=="J9",]

###not sure if it matches by plotid, so lets sort to make sure
quadrat_coords_s <- quadrat_coords_s[order(quadrat_coords_s$plotID),]

total_pre.cln <-total_pre.cln[order(rownames(total_pre.cln)),]
total_post.cln<-total_post.cln[order(rownames(total_post.cln)),]

clust.totabio.st_pre<-clust(total_pre.cln,years_pre,quadrat_coords_s,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales predrought
rownames(clust.totabio.st_pre$adj)<-rownames(clust.totabio.st_pre$dat)
colnames(clust.totabio.st_pre$adj)<-rownames(clust.totabio.st_pre$dat)
clust.totabio.st_post<-clust(total_post.cln,years_post,quadrat_coords_s,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales postdrought

clust.totabio.lt_pre<-clust(total_pre.cln,years_pre,quadrat_coords_s,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales predrought
clust.totabio.lt_post<-clust(total_post.cln,years_post,quadrat_coords_s,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales postdrought

###########BOER
rownames(soil_DEEPCOS_pre_BOER)
rownames(soil_DEEPCOS_post_BOER)

quadrat_coords_s_BOER_PRE=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|
                                           quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                           quadrat_coords$plotID=="I1"|quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I4"|
                                           quadrat_coords$plotID=="I5"|quadrat_coords$plotID=="I6"|quadrat_coords$plotID=="I7"|
                                           quadrat_coords$plotID=="J1"|quadrat_coords$plotID=="J12"|quadrat_coords$plotID=="J8"|
                                           quadrat_coords$plotID=="J9"|quadrat_coords$plotID=="N4"|quadrat_coords$plotID=="N5"|
                                           quadrat_coords$plotID=="R2"|quadrat_coords$plotID=="R3",]

quadrat_coords_s_BOER_POST=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B3"|
                                            quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                            quadrat_coords$plotID=="I4"|quadrat_coords$plotID=="I5"|quadrat_coords$plotID=="I7"|
                                            quadrat_coords$plotID=="R3",]



###not sure if it matches by plotid, so lets sort to make sure
quadrat_coords_s_BOER_PRE <- quadrat_coords_s_BOER_PRE[order(quadrat_coords_s_BOER_PRE$plotID),]
quadrat_coords_s_BOER_POST <- quadrat_coords_s_BOER_POST[order(quadrat_coords_s_BOER_POST$plotID),]


total_pre.cln_BOER <-total_pre.cln_BOER[order(rownames(total_pre.cln_BOER)),]
total_post.cln_BOER<-total_post.cln_BOER[order(rownames(total_post.cln_BOER)),]

clust.totabio.st_pre_BOER<-clust(total_pre.cln_BOER,years_pre,quadrat_coords_s_BOER_PRE,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales predrought
rownames(clust.totabio.st_pre_BOER$adj)<-rownames(clust.totabio.st_pre_BOER$dat)
colnames(clust.totabio.st_pre_BOER$adj)<-rownames(clust.totabio.st_pre_BOER$dat)
clust.totabio.st_post_BOER<-clust(total_post.cln_BOER,years_post,quadrat_coords_s_BOER_POST,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales postdrought

clust.totabio.lt_pre_BOER<-clust(total_pre.cln_BOER,years_pre,quadrat_coords_s_BOER_PRE,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales predrought
clust.totabio.lt_post_BOER<-clust(total_post.cln_BOER,years_post,quadrat_coords_s_BOER_POST,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales postdrought

#############

############FORB
rownames(soil_DEEPCOS_pre_FORBS)
rownames(soil_DEEPCOS_post_FORBS)

quadrat_coords_s_FORBS_PRE=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|
                                            quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                            quadrat_coords$plotID=="I1"|quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I3"|
                                            quadrat_coords$plotID=="I4"|quadrat_coords$plotID=="I5"|quadrat_coords$plotID=="I6"|
                                            quadrat_coords$plotID=="I7"|quadrat_coords$plotID=="J1"|quadrat_coords$plotID=="J12"|
                                            quadrat_coords$plotID=="J8"|quadrat_coords$plotID=="J9"|quadrat_coords$plotID=="N4"|
                                            quadrat_coords$plotID=="N5"|quadrat_coords$plotID=="R2"|quadrat_coords$plotID=="R3",]

quadrat_coords_s_FORBS_POST=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|
                                             quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                             quadrat_coords$plotID=="I1"|quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I5"|
                                             quadrat_coords$plotID=="I6"|quadrat_coords$plotID=="I7"|quadrat_coords$plotID=="J1"|
                                             quadrat_coords$plotID=="J12"|quadrat_coords$plotID=="J8"|quadrat_coords$plotID=="J9"|
                                             quadrat_coords$plotID=="N4"|quadrat_coords$plotID=="N5"|quadrat_coords$plotID=="R2"|
                                             quadrat_coords$plotID=="R3",]



###not sure if it matches by plotid, so lets sort to make sure
quadrat_coords_s_FORBS_PRE <- quadrat_coords_s_FORBS_PRE[order(quadrat_coords_s_FORBS_PRE$plotID),]
quadrat_coords_s_FORBS_POST <- quadrat_coords_s_FORBS_POST[order(quadrat_coords_s_FORBS_POST$plotID),]

total_pre.cln_FORBS <-total_pre.cln_FORBS[order(rownames(total_pre.cln_FORBS)),]
total_post.cln_FORBS<-total_post.cln_FORBS[order(rownames(total_post.cln_FORBS)),]

clust.totabio.st_pre_FORBS<-clust(total_pre.cln_FORBS,years_pre,quadrat_coords_s_FORBS_PRE,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales predrought
rownames(clust.totabio.st_pre_FORBS$adj)<-rownames(clust.totabio.st_pre_FORBS$dat)
colnames(clust.totabio.st_pre_FORBS$adj)<-rownames(clust.totabio.st_pre_FORBS$dat)
clust.totabio.st_post_FORBS<-clust(total_post.cln_FORBS,years_post,quadrat_coords_s_FORBS_POST,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales postdrought

clust.totabio.lt_pre_FORBS<-clust(total_pre.cln_FORBS,years_pre,quadrat_coords_s_FORBS_PRE,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales predrought
clust.totabio.lt_post_FORBS<-clust(total_post.cln_FORBS,years_post,quadrat_coords_s_FORBS_POST,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales postdrought


###############
############GRASS
rownames(soil_DEEPCOS_pre_GRASS)
rownames(soil_DEEPCOS_post_GRASS)

quadrat_coords_s_GRASS_PRE=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|
                                            quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                            quadrat_coords$plotID=="I1"|quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I3"|
                                            quadrat_coords$plotID=="I4"|quadrat_coords$plotID=="I5"|quadrat_coords$plotID=="I6"|
                                            quadrat_coords$plotID=="I7"|quadrat_coords$plotID=="J1"|quadrat_coords$plotID=="J12"|
                                            quadrat_coords$plotID=="J8"|quadrat_coords$plotID=="J9"|quadrat_coords$plotID=="N4"|
                                            quadrat_coords$plotID=="N5"|quadrat_coords$plotID=="R2"|quadrat_coords$plotID=="R3",]

quadrat_coords_s_GRASS_POST=quadrat_coords[quadrat_coords$plotID=="B1"|quadrat_coords$plotID=="B2"|quadrat_coords$plotID=="B3"|
                                             quadrat_coords$plotID=="H1"|quadrat_coords$plotID=="H2"|quadrat_coords$plotID=="H3"|
                                             quadrat_coords$plotID=="I1"|quadrat_coords$plotID=="I2"|quadrat_coords$plotID=="I3"|
                                             quadrat_coords$plotID=="I4"|quadrat_coords$plotID=="I5"|quadrat_coords$plotID=="I6"|
                                             quadrat_coords$plotID=="I7"|quadrat_coords$plotID=="J1"|quadrat_coords$plotID=="J12"|
                                             quadrat_coords$plotID=="J8"|quadrat_coords$plotID=="J9"|quadrat_coords$plotID=="N4"|
                                             quadrat_coords$plotID=="N5"|quadrat_coords$plotID=="R2"|quadrat_coords$plotID=="R3",]



###not sure if it matches by plotid, so lets sort to make sure
quadrat_coords_s_GRASS_PRE <- quadrat_coords_s_GRASS_PRE[order(quadrat_coords_s_GRASS_PRE$plotID),]
quadrat_coords_s_GRASS_POST <- quadrat_coords_s_GRASS_POST[order(quadrat_coords_s_GRASS_POST$plotID),]

total_pre.cln_GRASS <-total_pre.cln_GRASS[order(rownames(total_pre.cln_GRASS)),]
total_post.cln_GRASS<-total_post.cln_GRASS[order(rownames(total_post.cln_GRASS)),]

clust.totabio.st_pre_GRASS<-clust(total_pre.cln_GRASS,years_pre,quadrat_coords_s_GRASS_PRE,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales predrought
rownames(clust.totabio.st_pre_GRASS$adj)<-rownames(clust.totabio.st_pre_GRASS$dat)
colnames(clust.totabio.st_pre_GRASS$adj)<-rownames(clust.totabio.st_pre_GRASS$dat)
clust.totabio.st_post_GRASS<-clust(total_post.cln_GRASS,years_post,quadrat_coords_s_GRASS_POST,method="ReXWT",tsrange=c(2,6),weighted=T) #short timescales postdrought

clust.totabio.lt_pre_GRASS<-clust(total_pre.cln_GRASS,years_pre,quadrat_coords_s_GRASS_PRE,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales predrought
clust.totabio.lt_post_GRASS<-clust(total_post.cln_GRASS,years_post,quadrat_coords_s_GRASS_POST,method="ReXWT",tsrange=c(6,Inf),weighted=T) #short timescales postdrought

#################


# ----------------------------------------------------------------------
# 1a. Distance-decay of synchrony
rownames(quadrat_coords_s)<-quadrat_coords_s$plotID

geog.dist<-dist(quadrat_coords_s[,2:3])

#define function for doing spline fitting
spline.fit<-function(dists,zmat,nresamp=1000,quantiles=c(0.025,0.975)){
  xemp<-c(dists)
  triang<-lower.tri(zmat)
  yemp<-zmat[triang]
  dfs=sqrt(nrow(zmat))
  distmat<-full(dists)
  out<-list()
  
  emp.spline<-smooth.spline(xemp,yemp,df=dfs)
  out$emp.spline<-emp.spline
  
  resamp.splines<-matrix(NA, nrow=nresamp, ncol=length(emp.spline$y))
  for(ii in 1:nresamp){
    shuffle<-sample(1:nrow(zmat), size=nrow(zmat), replace=TRUE)
    xres<-distmat[shuffle,shuffle][triang]
    yres<-zmat[shuffle,shuffle][triang]
    drop.NaNs<-!is.na(yres)
    xres<-xres[drop.NaNs]; yres<-yres[drop.NaNs]
    res.spline<-smooth.spline(xres,yres,df=dfs)
    resamp.splines[ii,]<-predict(res.spline, x=emp.spline$x)$y
  }
  out$resamp.splines<-resamp.splines
  out$spline.quantiles<-apply(resamp.splines,2,quantile,probs=quantiles)
  return(out)
}

#do spline fits
spline.totcov.st_pre<-spline.fit(geog.dist,clust.totabio.st_pre$adj) #short timescales
spline.totcov.st_post<-spline.fit(geog.dist,clust.totabio.st_post$adj) #short timescales

spline.totcov.lt_pre<-spline.fit(geog.dist,clust.totabio.lt_pre$adj) #long timescales
spline.totcov.lt_post<-spline.fit(geog.dist,clust.totabio.lt_post$adj) #long timescales

colors=c("red","blue") #blue for in-module, red for out-of-module

mod.in.out<-function(mod.ids){
  xx<-matrix(1, nrow=length(mod.ids), ncol=length(mod.ids))
  for(i in 1:length(mod.ids)){
    for(j in 1:length(mod.ids)){
      if(mod.ids[i]==mod.ids[j]){xx[i,j]<-2}
    }
  }
  return(xx)
}

inout.pler.st_pre<-mod.in.out(clust.totabio.st_pre$clusters[[length(clust.totabio.st_pre$clusters)]])
inout.pler.st_post<-mod.in.out(clust.totabio.st_post$clusters[[length(clust.totabio.st_post$clusters)]])

inout.pler.lt_pre<-mod.in.out(clust.totabio.lt_pre$clusters[[length(clust.totabio.lt_pre$clusters)]])
inout.pler.lt_post<-mod.in.out(clust.totabio.lt_post$clusters[[length(clust.totabio.lt_post$clusters)]])

#################BOER
rownames(quadrat_coords_s_BOER_PRE)<-quadrat_coords_s_BOER_PRE$plotID
rownames(quadrat_coords_s_BOER_POST)<-quadrat_coords_s_BOER_POST$plotID

geog.dist_BOER_PRE<-dist(quadrat_coords_s_BOER_PRE[,2:3])
geog.dist_BOER_POST<-dist(quadrat_coords_s_BOER_POST[,2:3])
####################

###################GRASS
rownames(quadrat_coords_s_GRASS_PRE)<-quadrat_coords_s_GRASS_PRE$plotID
rownames(quadrat_coords_s_GRASS_POST)<-quadrat_coords_s_GRASS_POST$plotID

geog.dist_GRASS_PRE<-dist(quadrat_coords_s_GRASS_PRE[,2:3])
geog.dist_GRASS_POST<-dist(quadrat_coords_s_GRASS_POST[,2:3])

#######################

#####################FORBS
rownames(quadrat_coords_s_FORBS_PRE)<-quadrat_coords_s_FORBS_PRE$plotID
rownames(quadrat_coords_s_FORBS_POST)<-quadrat_coords_s_FORBS_POST$plotID

geog.dist_FORBS_PRE<-dist(quadrat_coords_s_FORBS_PRE[,2:3])
geog.dist_FORBS_POST<-dist(quadrat_coords_s_FORBS_POST[,2:3])


#####################


-------------------------------------------------------
  # 1b. Networks and modularity
  
  source("F:/NMSU/Spatiotemporal Synchrony/plotClusterMap_JRG.R")

#Networks, short timescales, plants
tiff("F:/NMSU/Spatiotemporal Synchrony/Pre_post_maps/totalcover.tif", units="in", res=300, width = 6.5, height=4)
par(mfrow=c(2,2))

plotClusterMap(clust.totabio.st_pre, nodewgt="mod.decomp", edgewgt=0.9, title="Total_pre_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"a)")
plotClusterMap(clust.totabio.st_post, nodewgt="mod.decomp", edgewgt=0.9, title="Total_post_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"b)")
plotClusterMap(clust.totabio.lt_pre, nodewgt="mod.decomp", edgewgt=0.9, title="Total_pre_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"c)")
plotClusterMap(clust.totabio.lt_post, nodewgt="mod.decomp", edgewgt=0.9, title="Total_post_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"d)")

dev.off()

################BOER
source("F:/NMSU/Spatiotemporal Synchrony/plotClusterMap_JRG.R")

#Networks, short timescales, plants
tiff("F:/NMSU/Spatiotemporal Synchrony/Pre_post_maps/BOERcover.tif", units="in", res=300, width = 6.5, height=4)
par(mfrow=c(2,2))

plotClusterMap(clust.totabio.st_pre_BOER, nodewgt="mod.decomp", edgewgt=0.9, title="BOER_pre_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"a)")
plotClusterMap(clust.totabio.st_post_BOER, nodewgt="mod.decomp", edgewgt=0.9, title="BOER_post_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"b)")
plotClusterMap(clust.totabio.lt_pre_BOER, nodewgt="mod.decomp", edgewgt=0.9, title="BOER_pre_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"c)")
plotClusterMap(clust.totabio.lt_post_BOER, nodewgt="mod.decomp", edgewgt=0.9, title="BOER_post_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"d)")

dev.off()


####################

################GRASS
source("F:/NMSU/Spatiotemporal Synchrony/plotClusterMap_JRG.R")

#Networks, short timescales, plants
tiff("F:/NMSU/Spatiotemporal Synchrony/Pre_post_maps/GRASScover.tif", units="in", res=300, width = 6.5, height=4)
par(mfrow=c(2,2))

plotClusterMap(clust.totabio.st_pre_GRASS, nodewgt="mod.decomp", edgewgt=0.9, title="GRASS_pre_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"a)")
plotClusterMap(clust.totabio.st_post_GRASS, nodewgt="mod.decomp", edgewgt=0.9, title="GRASS_post_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"b)")
plotClusterMap(clust.totabio.lt_pre_GRASS, nodewgt="mod.decomp", edgewgt=0.9, title="GRASS_pre_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"c)")
plotClusterMap(clust.totabio.lt_post_GRASS, nodewgt="mod.decomp", edgewgt=0.9, title="GRASS_post_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"d)")

dev.off()


#####################

##################FORBS
source("F:/NMSU/Spatiotemporal Synchrony/plotClusterMap_JRG.R")

#Networks, short timescales, plants
tiff("F:/NMSU/Spatiotemporal Synchrony/Pre_post_maps/FORBScover.tif", units="in", res=300, width = 6.5, height=4)
par(mfrow=c(2,2))

plotClusterMap(clust.totabio.st_pre_FORBS, nodewgt="mod.decomp", edgewgt=0.9, title="FORBS_pre_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"a)")
plotClusterMap(clust.totabio.st_post_FORBS, nodewgt="mod.decomp", edgewgt=0.9, title="FORBS_post_st", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"b)")
plotClusterMap(clust.totabio.lt_pre_FORBS, nodewgt="mod.decomp", edgewgt=0.9, title="FORBS_pre_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"c)")
plotClusterMap(clust.totabio.lt_post_FORBS, nodewgt="mod.decomp", edgewgt=0.9, title="FORBS_post_lt", nodesize=c(0.7,2), edgesize = c(0.5,0.5))
text(1.5,28.5,"d)")

dev.off()


######################

# ----------------------------------------------------------------------
# 1c. Matrix regression
soil_meandepth_pre<-matrix(plots_soil_select$mean_depth, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_meandepth_post<-matrix(plots_soil_select$mean_depth, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALSAND_pre<-matrix(plots_soil_select$SHAL_pct_sand, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALSAND_post<-matrix(plots_soil_select$SHAL_pct_sand, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALSILT_pre<-matrix(plots_soil_select$SHAL_pct_silt, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALSILT_post<-matrix(plots_soil_select$SHAL_pct_silt, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALCLAY_pre<-matrix(plots_soil_select$SHAL_pct_clay, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALCLAY_post<-matrix(plots_soil_select$SHAL_pct_clay, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALVFS_pre<-matrix(plots_soil_select$SHAL_pct_vfs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALVFS_post<-matrix(plots_soil_select$SHAL_pct_vfs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALFS_pre<-matrix(plots_soil_select$SHAL_pct_fs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALFS_post<-matrix(plots_soil_select$SHAL_pct_fs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALMS_pre<-matrix(plots_soil_select$SHAL_pct_ms, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALMS_post<-matrix(plots_soil_select$SHAL_pct_ms, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALCOS_pre<-matrix(plots_soil_select$SHAL_pct_cos, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALCOS_post<-matrix(plots_soil_select$SHAL_pct_cos, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_SHALVCS_pre<-matrix(plots_soil_select$SHAL_pct_vcs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_SHALVCS_post<-matrix(plots_soil_select$SHAL_pct_vcs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPSAND_pre<-matrix(plots_soil_select$DEEP_pct_sand, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPSAND_post<-matrix(plots_soil_select$DEEP_pct_sand, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPSILT_pre<-matrix(plots_soil_select$DEEP_pct_silt, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPSILT_post<-matrix(plots_soil_select$DEEP_pct_silt, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPCLAY_pre<-matrix(plots_soil_select$DEEP_pct_clay, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPCLAY_post<-matrix(plots_soil_select$DEEP_pct_clay, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPVFS_pre<-matrix(plots_soil_select$DEEP_pct_vfs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPVFS_post<-matrix(plots_soil_select$DEEP_pct_vfs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPFS_pre<-matrix(plots_soil_select$DEEP_pct_fs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPFS_post<-matrix(plots_soil_select$DEEP_pct_fs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPMS_pre<-matrix(plots_soil_select$DEEP_pct_ms, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPMS_post<-matrix(plots_soil_select$DEEP_pct_ms, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPCOS_pre<-matrix(plots_soil_select$DEEP_pct_cos, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPCOS_post<-matrix(plots_soil_select$DEEP_pct_cos, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soil_DEEPVCS_pre<-matrix(plots_soil_select$DEEP_pct_vcs, nrow=length(row.names(total_pre.cln)), ncol=length(years_pre), byrow=F)
soil_DEEPVCS_post<-matrix(plots_soil_select$DEEP_pct_vcs, nrow=length(row.names(total_post.cln)), ncol=length(years_post), byrow=F)

soild.sim<-1-(dist((soil_meandepth_pre[,1]))/max(dist((soil_meandepth_pre[,1])), na.rm=TRUE))

shalsand.sim<-1-(dist((soil_SHALSAND_post[,1]))/max(dist((soil_SHALSAND_post[,1])), na.rm=TRUE))

shalsilt.sim<-1-(dist((soil_SHALSILT_pre[,1]))/max(dist((soil_SHALSILT_pre[,1])), na.rm=TRUE))

shalclay.sim<-1-(dist((soil_SHALCLAY_pre[,1]))/max(dist((soil_SHALCLAY_pre[,1])), na.rm=TRUE))

shalvfs.sim<-1-(dist((soil_SHALVFS_post[,1]))/max(dist((soil_SHALVFS_post[,1])), na.rm=TRUE))

shalfs.sim<-1-(dist((soil_SHALFS_pre[,1]))/max(dist((soil_SHALFS_pre[,1])), na.rm=TRUE))

shalms.sim<-1-(dist((soil_SHALMS_pre[,1]))/max(dist((soil_SHALMS_pre[,1])), na.rm=TRUE))

shalcos.sim<-1-(dist((soil_SHALCOS_pre[,1]))/max(dist((soil_SHALCOS_pre[,1])), na.rm=TRUE))

shalvcs.sim<-1-(dist((soil_SHALVCS_pre[,1]))/max(dist((soil_SHALVCS_pre[,1])), na.rm=TRUE))

deepsand.sim<-1-(dist((soil_DEEPSAND_pre[,1]))/max(dist((soil_DEEPSAND_pre[,1])), na.rm=TRUE))

deepsilt.sim<-1-(dist((soil_DEEPSILT_pre[,1]))/max(dist((soil_DEEPSILT_pre[,1])), na.rm=TRUE))

deepclay.sim<-1-(dist((soil_DEEPCLAY_pre[,1]))/max(dist((soil_DEEPCLAY_pre[,1])), na.rm=TRUE))

deepvfs.sim<-1-(dist((soil_DEEPVFS_pre[,1]))/max(dist((soil_DEEPVFS_pre[,1])), na.rm=TRUE))

deepfs.sim<-1-(dist((soil_DEEPFS_pre[,1]))/max(dist((soil_DEEPFS_pre[,1])), na.rm=TRUE))

deepms.sim<-1-(dist((soil_DEEPMS_pre[,1]))/max(dist((soil_DEEPMS_pre[,1])), na.rm=TRUE))

deepcos.sim<-1-(dist((soil_DEEPCOS_pre[,1]))/max(dist((soil_DEEPCOS_pre[,1])), na.rm=TRUE))

deepvcs.sim<-1-(dist((soil_DEEPVCS_pre[,1]))/max(dist((soil_DEEPVCS_pre[,1])), na.rm=TRUE))

geog.prox<-1-(geog.dist/max(geog.dist))

######BOER
soild.sim_pre_BOER<-1-(dist((soil_meandepth_pre_BOER[,1]))/max(dist((soil_meandepth_pre_BOER[,1])), na.rm=TRUE))
soild.sim_post_BOER<-1-(dist((soil_meandepth_post_BOER[,1]))/max(dist((soil_meandepth_post_BOER[,1])), na.rm=TRUE))

shalsand.sim_pre_BOER<-1-(dist((soil_SHALSAND_pre_BOER[,1]))/max(dist((soil_SHALSAND_pre_BOER[,1])), na.rm=TRUE))
shalsand.sim_post_BOER<-1-(dist((soil_SHALSAND_post_BOER[,1]))/max(dist((soil_SHALSAND_post_BOER[,1])), na.rm=TRUE))

shalsilt.sim_pre_BOER<-1-(dist((soil_SHALSILT_pre_BOER[,1]))/max(dist((soil_SHALSILT_pre_BOER[,1])), na.rm=TRUE))
shalsilt.sim_post_BOER<-1-(dist((soil_SHALSILT_post_BOER[,1]))/max(dist((soil_SHALSILT_post_BOER[,1])), na.rm=TRUE))

shalclay.sim_pre_BOER<-1-(dist((soil_SHALCLAY_pre_BOER[,1]))/max(dist((soil_SHALCLAY_pre_BOER[,1])), na.rm=TRUE))
shalclay.sim_post_BOER<-1-(dist((soil_SHALCLAY_post_BOER[,1]))/max(dist((soil_SHALCLAY_post_BOER[,1])), na.rm=TRUE))

shalvfs.sim_pre_BOER<-1-(dist((soil_SHALVFS_pre_BOER[,1]))/max(dist((soil_SHALVFS_pre_BOER[,1])), na.rm=TRUE))
shalvfs.sim_post_BOER<-1-(dist((soil_SHALVFS_post_BOER[,1]))/max(dist((soil_SHALVFS_post_BOER[,1])), na.rm=TRUE))

shalfs.sim_pre_BOER<-1-(dist((soil_SHALFS_pre_BOER[,1]))/max(dist((soil_SHALFS_pre_BOER[,1])), na.rm=TRUE))
shalfs.sim_post_BOER<-1-(dist((soil_SHALFS_post_BOER[,1]))/max(dist((soil_SHALFS_post_BOER[,1])), na.rm=TRUE))

shalms.sim_pre_BOER<-1-(dist((soil_SHALMS_pre_BOER[,1]))/max(dist((soil_SHALMS_pre_BOER[,1])), na.rm=TRUE))
shalms.sim_post_BOER<-1-(dist((soil_SHALMS_post_BOER[,1]))/max(dist((soil_SHALMS_post_BOER[,1])), na.rm=TRUE))

shalcos.sim_pre_BOER<-1-(dist((soil_SHALCOS_pre_BOER[,1]))/max(dist((soil_SHALCOS_pre_BOER[,1])), na.rm=TRUE))
shalcos.sim_post_BOER<-1-(dist((soil_SHALCOS_post_BOER[,1]))/max(dist((soil_SHALCOS_post_BOER[,1])), na.rm=TRUE))

shalvcs.sim_pre_BOER<-1-(dist((soil_SHALVCS_pre_BOER[,1]))/max(dist((soil_SHALVCS_pre_BOER[,1])), na.rm=TRUE))
shalvcs.sim_post_BOER<-1-(dist((soil_SHALVCS_post_BOER[,1]))/max(dist((soil_SHALVCS_post_BOER[,1])), na.rm=TRUE))

deepsand.sim_pre_BOER<-1-(dist((soil_DEEPSAND_pre_BOER[,1]))/max(dist((soil_DEEPSAND_pre_BOER[,1])), na.rm=TRUE))
deepsand.sim_post_BOER<-1-(dist((soil_DEEPSAND_post_BOER[,1]))/max(dist((soil_DEEPSAND_post_BOER[,1])), na.rm=TRUE))

deepsilt.sim_pre_BOER<-1-(dist((soil_DEEPSILT_pre_BOER[,1]))/max(dist((soil_DEEPSILT_pre_BOER[,1])), na.rm=TRUE))
deepsilt.sim_post_BOER<-1-(dist((soil_DEEPSILT_post_BOER[,1]))/max(dist((soil_DEEPSILT_post_BOER[,1])), na.rm=TRUE))

deepclay.sim_pre_BOER<-1-(dist((soil_DEEPCLAY_pre_BOER[,1]))/max(dist((soil_DEEPCLAY_pre_BOER[,1])), na.rm=TRUE))
deepclay.sim_post_BOER<-1-(dist((soil_DEEPCLAY_post_BOER[,1]))/max(dist((soil_DEEPCLAY_post_BOER[,1])), na.rm=TRUE))

deepvfs.sim_pre_BOER<-1-(dist((soil_DEEPVFS_pre_BOER[,1]))/max(dist((soil_DEEPVFS_pre_BOER[,1])), na.rm=TRUE))
deepvfs.sim_post_BOER<-1-(dist((soil_DEEPVFS_post_BOER[,1]))/max(dist((soil_DEEPVFS_post_BOER[,1])), na.rm=TRUE))

deepfs.sim_pre_BOER<-1-(dist((soil_DEEPFS_pre_BOER[,1]))/max(dist((soil_DEEPFS_pre_BOER[,1])), na.rm=TRUE))
deepfs.sim_post_BOER<-1-(dist((soil_DEEPFS_post_BOER[,1]))/max(dist((soil_DEEPFS_post_BOER[,1])), na.rm=TRUE))

deepms.sim_pre_BOER<-1-(dist((soil_DEEPMS_pre_BOER[,1]))/max(dist((soil_DEEPMS_pre_BOER[,1])), na.rm=TRUE))
deepms.sim_post_BOER<-1-(dist((soil_DEEPMS_post_BOER[,1]))/max(dist((soil_DEEPMS_post_BOER[,1])), na.rm=TRUE))

deepcos.sim_pre_BOER<-1-(dist((soil_DEEPCOS_pre_BOER[,1]))/max(dist((soil_DEEPCOS_pre_BOER[,1])), na.rm=TRUE))
deepcos.sim_post_BOER<-1-(dist((soil_DEEPCOS_post_BOER[,1]))/max(dist((soil_DEEPCOS_post_BOER[,1])), na.rm=TRUE))

deepvcs.sim_pre_BOER<-1-(dist((soil_DEEPVCS_pre_BOER[,1]))/max(dist((soil_DEEPVCS_pre_BOER[,1])), na.rm=TRUE))
deepvcs.sim_post_BOER<-1-(dist((soil_DEEPVCS_post_BOER[,1]))/max(dist((soil_DEEPVCS_post_BOER[,1])), na.rm=TRUE))

geog.prox_pre_BOER<-1-(geog.dist_BOER_PRE/max(geog.dist_BOER_PRE))
geog.prox_post_BOER<-1-(geog.dist_BOER_POST/max(geog.dist_BOER_POST))

##########

#########GRASS
soild.sim_pre_GRASS<-1-(dist((soil_meandepth_pre_GRASS[,1]))/max(dist((soil_meandepth_pre_GRASS[,1])), na.rm=TRUE))
soild.sim_post_GRASS<-1-(dist((soil_meandepth_post_GRASS[,1]))/max(dist((soil_meandepth_post_GRASS[,1])), na.rm=TRUE))

shalsand.sim_pre_GRASS<-1-(dist((soil_SHALSAND_pre_GRASS[,1]))/max(dist((soil_SHALSAND_pre_GRASS[,1])), na.rm=TRUE))
shalsand.sim_post_GRASS<-1-(dist((soil_SHALSAND_post_GRASS[,1]))/max(dist((soil_SHALSAND_post_GRASS[,1])), na.rm=TRUE))

shalsilt.sim_pre_GRASS<-1-(dist((soil_SHALSILT_pre_GRASS[,1]))/max(dist((soil_SHALSILT_pre_GRASS[,1])), na.rm=TRUE))
shalsilt.sim_post_GRASS<-1-(dist((soil_SHALSILT_post_GRASS[,1]))/max(dist((soil_SHALSILT_post_GRASS[,1])), na.rm=TRUE))

shalclay.sim_pre_GRASS<-1-(dist((soil_SHALCLAY_pre_GRASS[,1]))/max(dist((soil_SHALCLAY_pre_GRASS[,1])), na.rm=TRUE))
shalclay.sim_post_GRASS<-1-(dist((soil_SHALCLAY_post_GRASS[,1]))/max(dist((soil_SHALCLAY_post_GRASS[,1])), na.rm=TRUE))

shalvfs.sim_pre_GRASS<-1-(dist((soil_SHALVFS_pre_GRASS[,1]))/max(dist((soil_SHALVFS_pre_GRASS[,1])), na.rm=TRUE))
shalvfs.sim_post_GRASS<-1-(dist((soil_SHALVFS_post_GRASS[,1]))/max(dist((soil_SHALVFS_post_GRASS[,1])), na.rm=TRUE))

shalfs.sim_pre_GRASS<-1-(dist((soil_SHALFS_pre_GRASS[,1]))/max(dist((soil_SHALFS_pre_GRASS[,1])), na.rm=TRUE))
shalfs.sim_post_GRASS<-1-(dist((soil_SHALFS_post_GRASS[,1]))/max(dist((soil_SHALFS_post_GRASS[,1])), na.rm=TRUE))

shalms.sim_pre_GRASS<-1-(dist((soil_SHALMS_pre_GRASS[,1]))/max(dist((soil_SHALMS_pre_GRASS[,1])), na.rm=TRUE))
shalms.sim_post_GRASS<-1-(dist((soil_SHALMS_post_GRASS[,1]))/max(dist((soil_SHALMS_post_GRASS[,1])), na.rm=TRUE))

shalcos.sim_pre_GRASS<-1-(dist((soil_SHALCOS_pre_GRASS[,1]))/max(dist((soil_SHALCOS_pre_GRASS[,1])), na.rm=TRUE))
shalcos.sim_post_GRASS<-1-(dist((soil_SHALCOS_post_GRASS[,1]))/max(dist((soil_SHALCOS_post_GRASS[,1])), na.rm=TRUE))

shalvcs.sim_pre_GRASS<-1-(dist((soil_SHALVCS_pre_GRASS[,1]))/max(dist((soil_SHALVCS_pre_GRASS[,1])), na.rm=TRUE))
shalvcs.sim_post_GRASS<-1-(dist((soil_SHALVCS_post_GRASS[,1]))/max(dist((soil_SHALVCS_post_GRASS[,1])), na.rm=TRUE))

deepsand.sim_pre_GRASS<-1-(dist((soil_DEEPSAND_pre_GRASS[,1]))/max(dist((soil_DEEPSAND_pre_GRASS[,1])), na.rm=TRUE))
deepsand.sim_post_GRASS<-1-(dist((soil_DEEPSAND_post_GRASS[,1]))/max(dist((soil_DEEPSAND_post_GRASS[,1])), na.rm=TRUE))

deepsilt.sim_pre_GRASS<-1-(dist((soil_DEEPSILT_pre_GRASS[,1]))/max(dist((soil_DEEPSILT_pre_GRASS[,1])), na.rm=TRUE))
deepsilt.sim_post_GRASS<-1-(dist((soil_DEEPSILT_post_GRASS[,1]))/max(dist((soil_DEEPSILT_post_GRASS[,1])), na.rm=TRUE))

deepclay.sim_pre_GRASS<-1-(dist((soil_DEEPCLAY_pre_GRASS[,1]))/max(dist((soil_DEEPCLAY_pre_GRASS[,1])), na.rm=TRUE))
deepclay.sim_post_GRASS<-1-(dist((soil_DEEPCLAY_post_GRASS[,1]))/max(dist((soil_DEEPCLAY_post_GRASS[,1])), na.rm=TRUE))

deepvfs.sim_pre_GRASS<-1-(dist((soil_DEEPVFS_pre_GRASS[,1]))/max(dist((soil_DEEPVFS_pre_GRASS[,1])), na.rm=TRUE))
deepvfs.sim_post_GRASS<-1-(dist((soil_DEEPVFS_post_GRASS[,1]))/max(dist((soil_DEEPVFS_post_GRASS[,1])), na.rm=TRUE))

deepfs.sim_pre_GRASS<-1-(dist((soil_DEEPFS_pre_GRASS[,1]))/max(dist((soil_DEEPFS_pre_GRASS[,1])), na.rm=TRUE))
deepfs.sim_post_GRASS<-1-(dist((soil_DEEPFS_post_GRASS[,1]))/max(dist((soil_DEEPFS_post_GRASS[,1])), na.rm=TRUE))

deepms.sim_pre_GRASS<-1-(dist((soil_DEEPMS_pre_GRASS[,1]))/max(dist((soil_DEEPMS_pre_GRASS[,1])), na.rm=TRUE))
deepms.sim_post_GRASS<-1-(dist((soil_DEEPMS_post_GRASS[,1]))/max(dist((soil_DEEPMS_post_GRASS[,1])), na.rm=TRUE))

deepcos.sim_pre_GRASS<-1-(dist((soil_DEEPCOS_pre_GRASS[,1]))/max(dist((soil_DEEPCOS_pre_GRASS[,1])), na.rm=TRUE))
deepcos.sim_post_GRASS<-1-(dist((soil_DEEPCOS_post_GRASS[,1]))/max(dist((soil_DEEPCOS_post_GRASS[,1])), na.rm=TRUE))

deepvcs.sim_pre_GRASS<-1-(dist((soil_DEEPVCS_pre_GRASS[,1]))/max(dist((soil_DEEPVCS_pre_GRASS[,1])), na.rm=TRUE))
deepvcs.sim_post_GRASS<-1-(dist((soil_DEEPVCS_post_GRASS[,1]))/max(dist((soil_DEEPVCS_post_GRASS[,1])), na.rm=TRUE))

geog.prox_pre_GRASS<-1-(geog.dist_GRASS_PRE/max(geog.dist_GRASS_PRE))
geog.prox_post_GRASS<-1-(geog.dist_GRASS_POST/max(geog.dist_GRASS_POST))


##############

########FORBS
soild.sim_pre_FORBS<-1-(dist((soil_meandepth_pre_FORBS[,1]))/max(dist((soil_meandepth_pre_FORBS[,1])), na.rm=TRUE))
soild.sim_post_FORBS<-1-(dist((soil_meandepth_post_FORBS[,1]))/max(dist((soil_meandepth_post_FORBS[,1])), na.rm=TRUE))

shalsand.sim_pre_FORBS<-1-(dist((soil_SHALSAND_pre_FORBS[,1]))/max(dist((soil_SHALSAND_pre_FORBS[,1])), na.rm=TRUE))
shalsand.sim_post_FORBS<-1-(dist((soil_SHALSAND_post_FORBS[,1]))/max(dist((soil_SHALSAND_post_FORBS[,1])), na.rm=TRUE))

shalsilt.sim_pre_FORBS<-1-(dist((soil_SHALSILT_pre_FORBS[,1]))/max(dist((soil_SHALSILT_pre_FORBS[,1])), na.rm=TRUE))
shalsilt.sim_post_FORBS<-1-(dist((soil_SHALSILT_post_FORBS[,1]))/max(dist((soil_SHALSILT_post_FORBS[,1])), na.rm=TRUE))

shalclay.sim_pre_FORBS<-1-(dist((soil_SHALCLAY_pre_FORBS[,1]))/max(dist((soil_SHALCLAY_pre_FORBS[,1])), na.rm=TRUE))
shalclay.sim_post_FORBS<-1-(dist((soil_SHALCLAY_post_FORBS[,1]))/max(dist((soil_SHALCLAY_post_FORBS[,1])), na.rm=TRUE))

shalvfs.sim_pre_FORBS<-1-(dist((soil_SHALVFS_pre_FORBS[,1]))/max(dist((soil_SHALVFS_pre_FORBS[,1])), na.rm=TRUE))
shalvfs.sim_post_FORBS<-1-(dist((soil_SHALVFS_post_FORBS[,1]))/max(dist((soil_SHALVFS_post_FORBS[,1])), na.rm=TRUE))

shalfs.sim_pre_FORBS<-1-(dist((soil_SHALFS_pre_FORBS[,1]))/max(dist((soil_SHALFS_pre_FORBS[,1])), na.rm=TRUE))
shalfs.sim_post_FORBS<-1-(dist((soil_SHALFS_post_FORBS[,1]))/max(dist((soil_SHALFS_post_FORBS[,1])), na.rm=TRUE))

shalms.sim_pre_FORBS<-1-(dist((soil_SHALMS_pre_FORBS[,1]))/max(dist((soil_SHALMS_pre_FORBS[,1])), na.rm=TRUE))
shalms.sim_post_FORBS<-1-(dist((soil_SHALMS_post_FORBS[,1]))/max(dist((soil_SHALMS_post_FORBS[,1])), na.rm=TRUE))

shalcos.sim_pre_FORBS<-1-(dist((soil_SHALCOS_pre_FORBS[,1]))/max(dist((soil_SHALCOS_pre_FORBS[,1])), na.rm=TRUE))
shalcos.sim_post_FORBS<-1-(dist((soil_SHALCOS_post_FORBS[,1]))/max(dist((soil_SHALCOS_post_FORBS[,1])), na.rm=TRUE))

shalvcs.sim_pre_FORBS<-1-(dist((soil_SHALVCS_pre_FORBS[,1]))/max(dist((soil_SHALVCS_pre_FORBS[,1])), na.rm=TRUE))
shalvcs.sim_post_FORBS<-1-(dist((soil_SHALVCS_post_FORBS[,1]))/max(dist((soil_SHALVCS_post_FORBS[,1])), na.rm=TRUE))

deepsand.sim_pre_FORBS<-1-(dist((soil_DEEPSAND_pre_FORBS[,1]))/max(dist((soil_DEEPSAND_pre_FORBS[,1])), na.rm=TRUE))
deepsand.sim_post_FORBS<-1-(dist((soil_DEEPSAND_post_FORBS[,1]))/max(dist((soil_DEEPSAND_post_FORBS[,1])), na.rm=TRUE))

deepsilt.sim_pre_FORBS<-1-(dist((soil_DEEPSILT_pre_FORBS[,1]))/max(dist((soil_DEEPSILT_pre_FORBS[,1])), na.rm=TRUE))
deepsilt.sim_post_FORBS<-1-(dist((soil_DEEPSILT_post_FORBS[,1]))/max(dist((soil_DEEPSILT_post_FORBS[,1])), na.rm=TRUE))

deepclay.sim_pre_FORBS<-1-(dist((soil_DEEPCLAY_pre_FORBS[,1]))/max(dist((soil_DEEPCLAY_pre_FORBS[,1])), na.rm=TRUE))
deepclay.sim_post_FORBS<-1-(dist((soil_DEEPCLAY_post_FORBS[,1]))/max(dist((soil_DEEPCLAY_post_FORBS[,1])), na.rm=TRUE))

deepvfs.sim_pre_FORBS<-1-(dist((soil_DEEPVFS_pre_FORBS[,1]))/max(dist((soil_DEEPVFS_pre_FORBS[,1])), na.rm=TRUE))
deepvfs.sim_post_FORBS<-1-(dist((soil_DEEPVFS_post_FORBS[,1]))/max(dist((soil_DEEPVFS_post_FORBS[,1])), na.rm=TRUE))

deepfs.sim_pre_FORBS<-1-(dist((soil_DEEPFS_pre_FORBS[,1]))/max(dist((soil_DEEPFS_pre_FORBS[,1])), na.rm=TRUE))
deepfs.sim_post_FORBS<-1-(dist((soil_DEEPFS_post_FORBS[,1]))/max(dist((soil_DEEPFS_post_FORBS[,1])), na.rm=TRUE))

deepms.sim_pre_FORBS<-1-(dist((soil_DEEPMS_pre_FORBS[,1]))/max(dist((soil_DEEPMS_pre_FORBS[,1])), na.rm=TRUE))
deepms.sim_post_FORBS<-1-(dist((soil_DEEPMS_post_FORBS[,1]))/max(dist((soil_DEEPMS_post_FORBS[,1])), na.rm=TRUE))

deepcos.sim_pre_FORBS<-1-(dist((soil_DEEPCOS_pre_FORBS[,1]))/max(dist((soil_DEEPCOS_pre_FORBS[,1])), na.rm=TRUE))
deepcos.sim_post_FORBS<-1-(dist((soil_DEEPCOS_post_FORBS[,1]))/max(dist((soil_DEEPCOS_post_FORBS[,1])), na.rm=TRUE))

deepvcs.sim_pre_FORBS<-1-(dist((soil_DEEPVCS_pre_FORBS[,1]))/max(dist((soil_DEEPVCS_pre_FORBS[,1])), na.rm=TRUE))
deepvcs.sim_post_FORBS<-1-(dist((soil_DEEPVCS_post_FORBS[,1]))/max(dist((soil_DEEPVCS_post_FORBS[,1])), na.rm=TRUE))

geog.prox_pre_FORBS<-1-(geog.dist_FORBS_PRE/max(geog.dist_FORBS_PRE))
geog.prox_post_FORBS<-1-(geog.dist_FORBS_POST/max(geog.dist_FORBS_POST))

## Short Timescales ##################################distance vs soil types

#Short Timescales
geog.prox<-as.matrix(geog.prox)
clust.totabio.st_pre$adj<-as.matrix(clust.totabio.st_pre$adj)
###try changing NAs to 0s
clust.totabio.st_pre$adj[is.na(clust.totabio.st_pre$adj)] <- 0

###remove all packages to remove conflict

##biomass v geography
totbiogeog.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(geog.prox),nperm=10000) #p=.0004, R2=.1757954
totbiogeog.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(geog.prox),nperm=10000) #p= .137, R2=.01426735

#biomass v soildepth
totbiomeandepth.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(soild.sim),nperm=10000)#p=.6808, R2=.00385
totbiomeandepth.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(soild.sim),nperm=10000)#p=.8054, R2=.000682

#biomass v shallow sand
totbioshallowsand.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalsand.sim),nperm=10000)#p=.158, R2=.04908
totbioshallowsand.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalsand.sim),nperm=10000)#p=.6819, R2=.001832

#biomass v shallow silt
totbioshallowsilt.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalsilt.sim),nperm=10000)#p=0.1185000, R2=.060473
totbioshallowsilt.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalsilt.sim),nperm=10000)#p=0.5923, R2=0.002726398

###biomass v shallow clay
totbioshallowclay.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalclay.sim),nperm=10000)#p=.244700, R2=.03275
totbioshallowclay.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalclay.sim),nperm=10000)#p=.6959000, R2=.00167

###biomass v shallow vfs
totbioshallowvfs.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalvfs.sim),nperm=10000)#p=.1857000, R2=.032878
totbioshallowvfs.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalvfs.sim),nperm=10000)#p=.9828000, R2=.000003727275

###biomass v shallow fs
totbioshallowfs.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalfs.sim),nperm=10000)#p=0.8961000, R2=0.0001855631
totbioshallowfs.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalfs.sim),nperm=10000)#p=0.7399, R2=0.0006933

####biomass v shallow ms
totbioshallowms.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalms.sim),nperm=10000)#p=0.5691, R2=0.002902989
totbioshallowms.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalms.sim),nperm=10000)#p=.7061, R2=0.0007816

####biomass v shallow cos
totbioshallowcos.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalcos.sim),nperm=10000)#p=.9924, R2=.000000614
totbioshallowcos.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalcos.sim),nperm=10000)#p=0.8979, R2=.00008456

####biomass v shallow vcs
totbioshallowvcs.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(shalvcs.sim),nperm=10000)#p=0.5463, R2=0.003178757
totbioshallowvcs.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(shalvcs.sim),nperm=10000)#p=0.7864, R2=0.000437

#biomass v deep sand
totbiodeepsand.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(deepsand.sim),nperm=10000)#p=0.2367, R2=0.03569462
totbiodeepsand.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(deepsand.sim),nperm=10000)#p=0.8714, R2=0.0003700796

#biomass v deep silt
totbiodeepsilt.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(deepsilt.sim),nperm=10000)#p=0.2225000, R2=0.03297778
totbiodeepsilt.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(deepsilt.sim),nperm=10000)#p=0.762000, R2=0.0008721314

#biomass v deep clay
totbiodeepclay.st_pre<-
  MRM(lower(clust.totabio.st_pre$adj)~lower(deepclay.sim),nperm=10000)#p=0.3596000, R2=0.0200544
totbiodeepclay.st_post<-
  MRM(lower(clust.totabio.st_post$adj)~lower(deepclay.sim),nperm=10000)#p=0.9048, R2=.0002495153

#biomass v deep vfs
MRM(lower(clust.totabio.st_pre$adj)~lower(deepvfs.sim),nperm=10000)#p=0.7758000, R2=0.002100088
MRM(lower(clust.totabio.st_post$adj)~lower(deepvfs.sim),nperm=10000)#p=0.4941, R2=.004365142

#biomass v deep fs
MRM(lower(clust.totabio.st_pre$adj)~lower(deepfs.sim),nperm=10000)#p=0.0626000, R2=0.1016816
MRM(lower(clust.totabio.st_post$adj)~lower(deepfs.sim),nperm=10000)#p=0.2487000, R2=.0.01077147

#biomass v deep ms
MRM(lower(clust.totabio.st_pre$adj)~lower(deepms.sim),nperm=10000)#p=.06880000, R2=.08941002
MRM(lower(clust.totabio.st_post$adj)~lower(deepms.sim),nperm=10000)#p=0.2838, R2=0.008973531

#biomass v deep cos
MRM(lower(clust.totabio.st_pre$adj)~lower(deepcos.sim),nperm=10000)#p=.3115000, R2=0.015533
MRM(lower(clust.totabio.st_post$adj)~lower(deepcos.sim),nperm=10000)#p=0.16900, R2=0.01340668

#biomass v deep vcs
MRM(lower(clust.totabio.st_pre$adj)~lower(deepvcs.sim),nperm=10000)#p=.6203000, R2=0.005638806
MRM(lower(clust.totabio.st_post$adj)~lower(deepvcs.sim),nperm=10000)#p=.9745, R2=.000009532532

######################BOER
#Short Timescales
geog.prox_pre_BOER<-as.matrix(geog.prox_pre_BOER)
geog.prox_post_BOER<-as.matrix(geog.prox_post_BOER)

clust.totabio.st_pre_BOER$adj<-as.matrix(clust.totabio.st_pre_BOER$adj)
clust.totabio.st_post_BOER$adj<-as.matrix(clust.totabio.st_post_BOER$adj)

###try changing NAs to 0s
clust.totabio.st_pre_BOER$adj[is.na(clust.totabio.st_pre_BOER$adj)] <- 0
clust.totabio.st_post_BOER$adj[is.na(clust.totabio.st_post_BOER$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(geog.prox_pre_BOER),nperm=10000)#p=0.0015, R2=0.1459782

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(geog.prox_post_BOER),nperm=10000)#p=0.2888, R2=0.03851029 
#biomass v soildepth

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(soild.sim_pre_BOER),nperm=10000)#p=0.8131, R2=0.001329827

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(soild.sim_post_BOER),nperm=10000)#p=0.093200, R2=0.1627571

#biomass v shallow sand

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalsand.sim_pre_BOER),nperm=10000)#p=0.9325, R2=0.0001294764

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalsand.sim_post_BOER),nperm=10000)#p=0.8871, R2=0.0006960695

#biomass v shallow silt

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalsilt.sim_pre_BOER),nperm=10000)#p=0.5929000, R2=0.005735764

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalsilt.sim_post_BOER),nperm=10000)#p=0.5032,R2=0.01628968

###biomass v shallow clay

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalclay.sim_pre_BOER),nperm=10000)#p=0.3401, R2=0.0173632

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalclay.sim_post_BOER),nperm=10000)#p=0.1248, R2=0.07874774

###biomass v shallow vfs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalvfs.sim_pre_BOER),nperm=10000)#p=0.8477, R2=0.0005881851
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalvfs.sim_post_BOER),nperm=10000)#p=0.9108, R2=0.0004398699

###biomass v shallow fs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalfs.sim_pre_BOER),nperm=10000)#p=0.5569, R2=0.003771282
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalfs.sim_post_BOER),nperm=10000)#p=0.3662, R2=0.02984055

####biomass v shallow ms
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalms.sim_pre_BOER),nperm=10000)#p=0.7739, R2=0.0006742633
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalms.sim_post_BOER),nperm=10000)#p=0.6787, R2=0.005170354
####biomass v shallow cos
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalcos.sim_pre_BOER),nperm=10000)#p=0.583, R2=0.002005387
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalcos.sim_post_BOER),nperm=10000)#p=0.8996, R2=0.0006555398

####biomass v shallow vcs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(shalvcs.sim_pre_BOER),nperm=10000)#p=0.7379, R2=0.00094284111
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalvcs.sim_post_BOER),nperm=10000)#p=0.4592, R2=0.01689682

#biomass v deep sand

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepsand.sim_pre_BOER),nperm=10000)#p=0.1213, R2=0.04017296

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepsand.sim_post_BOER),nperm=10000)#p=0.3888, R2=0.0304623

#biomass v deep silt

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepsilt.sim_pre_BOER),nperm=10000)#p=0.5197, R2=0.005549649

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepsilt.sim_post_BOER),nperm=10000)#p=0.3698, R2=0.0286206

#biomass v deep clay

MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepclay.sim_pre_BOER),nperm=10000)#p=0.017, R2=0.123096

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepclay.sim_post_BOER),nperm=10000)#p=0.6885. R2=0.007801779

#biomass v deep vfs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepvfs.sim_pre_BOER),nperm=10000)#p=0.1795, R2=0.04517745
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepvfs.sim_post_BOER),nperm=10000)#p=0.4482, R2=0.0299115

#biomass v deep fs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepfs.sim_pre_BOER),nperm=10000)#p=0.1736, R2=0.05003863
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepfs.sim_post_BOER),nperm=10000)#p=0.3086, R2=0.04178753

#biomass v deep ms
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepms.sim_pre_BOER),nperm=10000)#p=0.088, R2=0.07219317
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepms.sim_post_BOER),nperm=10000)#p=0.6177, R2=0.01763225

#biomass v deep cos
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepcos.sim_pre_BOER),nperm=10000)#p=0.6525, R2=0.003376594
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepcos.sim_post_BOER),nperm=10000)#p=0.9069000, R2=0.0004110328

#biomass v deep vcs
MRM(lower(clust.totabio.st_pre_BOER$adj)~lower(deepvcs.sim_pre_BOER),nperm=10000)#p=0.815, R2=0.001359272
MRM(lower(clust.totabio.st_post_BOER$adj)~lower(deepvcs.sim_post_BOER),nperm=10000)#p=0.440700, R2=0.02379562

############################

###########################GRASS
#Short Timescales
geog.prox_pre_GRASS<-as.matrix(geog.prox_pre_GRASS)
geog.prox_post_GRASS<-as.matrix(geog.prox_post_GRASS)

clust.totabio.st_pre_GRASS$adj<-as.matrix(clust.totabio.st_pre_GRASS$adj)
clust.totabio.st_post_GRASS$adj<-as.matrix(clust.totabio.st_post_GRASS$adj)

###try changing NAs to 0s
clust.totabio.st_pre_GRASS$adj[is.na(clust.totabio.st_pre_GRASS$adj)] <- 0
clust.totabio.st_post_GRASS$adj[is.na(clust.totabio.st_post_GRASS$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(geog.prox_pre_GRASS),nperm=10000)#p=0.0003, R2=0.1817023

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(geog.prox_post_GRASS),nperm=10000) #p=0.030200, R2=0.03502907
#biomass v soildepth

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(soild.sim_pre_GRASS),nperm=10000)#p=0.71, R2=0.00301294

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(soild.sim_post_GRASS),nperm=10000)#p=0.9455, R2=0.00006548637

#biomass v shallow sand

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalsand.sim_pre_GRASS),nperm=10000)#p=0.1547, R2=0.04964324

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalsand.sim_post_GRASS),nperm=10000)#p=0.7858000, R2=0.0007095577

#biomass v shallow silt

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalsilt.sim_pre_GRASS),nperm=10000)#p=0.11970, R2=0.0606374

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalsilt.sim_post_GRASS),nperm=10000)#p=0.9837, R2=0.000004083974

###biomass v shallow clay

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalclay.sim_pre_GRASS),nperm=10000)#p=0.24690, R2=0.03320823

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalclay.sim_post_GRASS),nperm=10000)#p=0.7602, R2=0.0009259395

###biomass v shallow vfs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalvfs.sim_pre_GRASS),nperm=10000)#p=0.1851, R2=0.03313275
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalvfs.sim_post_GRASS),nperm=10000)#p=0.9076, R2=0.0001265522

###biomass v shallow fs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalfs.sim_pre_GRASS),nperm=10000)#p=0.9212, R2=0.0001288397
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalfs.sim_post_GRASS),nperm=10000)#p=0.2482, R2=0.009359812

####biomass v shallow ms
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalms.sim_pre_GRASS),nperm=10000)#p=0.5631, R2=0.00289042
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalms.sim_post_GRASS),nperm=10000)#p=0.8375, R2=0.0002435036
####biomass v shallow cos
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalcos.sim_pre_GRASS),nperm=10000)#p=0.9891, R2=0.000001766146
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalcos.sim_post_GRASS),nperm=10000)#p=0.06130, R2=0.01818125

####biomass v shallow vcs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(shalvcs.sim_pre_GRASS),nperm=10000)#p=0.5258, R2=0.003496541
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalvcs.sim_post_GRASS),nperm=10000)#p=0.8436, R2=0.0002452193

#biomass v deep sand

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepsand.sim_pre_GRASS),nperm=10000)#p=0.2413, R2=0.03612598

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepsand.sim_post_GRASS),nperm=10000)#p=0.719, 0.001312021

#biomass v deep silt

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepsilt.sim_pre_GRASS),nperm=10000)#p=0.2361, R2=0.03337158

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepsilt.sim_post_GRASS),nperm=10000)#p=0.6585, R2=0.001934152

#biomass v deep clay

MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepclay.sim_pre_GRASS),nperm=10000)#p=0.3483, R2=0.02110076

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepclay.sim_post_GRASS),nperm=10000)#p=0.7663, R2=0.0009049197

#biomass v deep vfs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepvfs.sim_pre_GRASS),nperm=10000)#p=0.7605, R2=0.002488539
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepvfs.sim_post_GRASS),nperm=10000)#p=0.5121, R2=0.00543073

#biomass v deep fs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepfs.sim_pre_GRASS),nperm=10000)#p=0.05830, R2=0.1076086
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepfs.sim_post_GRASS),nperm=10000)#p=0.1014, R2=0.03175219

#biomass v deep ms
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepms.sim_pre_GRASS),nperm=10000)#p=0.0595, R2=0.0951272
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepms.sim_post_GRASS),nperm=10000)#p=0.223, R2=0.01626596

#biomass v deep cos
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepcos.sim_pre_GRASS),nperm=10000)#p=0.3001, R2=0.01699145
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepcos.sim_post_GRASS),nperm=10000)#p=0.00830, R2=0.06194688

#biomass v deep vcs
MRM(lower(clust.totabio.st_pre_GRASS$adj)~lower(deepvcs.sim_pre_GRASS),nperm=10000)#p=0.6296, R2=0.005187025
MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(deepvcs.sim_post_GRASS),nperm=10000)#p=0.8673, R2=0.0002723267

##############################

################FORBS
#Short Timescales
geog.prox_pre_FORBS<-as.matrix(geog.prox_pre_FORBS)
geog.prox_post_FORBS<-as.matrix(geog.prox_post_FORBS)

clust.totabio.st_pre_FORBS$adj<-as.matrix(clust.totabio.st_pre_FORBS$adj)
clust.totabio.st_post_FORBS$adj<-as.matrix(clust.totabio.st_post_FORBS$adj)

###try changing NAs to 0s
clust.totabio.st_pre_FORBS$adj[is.na(clust.totabio.st_pre_FORBS$adj)] <- 0
clust.totabio.st_post_FORBS$adj[is.na(clust.totabio.st_post_FORBS$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(geog.prox_pre_FORBS),nperm=10000)#p=0.3343, R2=0.007536984

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(geog.prox_post_FORBS),nperm=10000) #p=0.290100, R2=0.006494848
#biomass v soildepth

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(soild.sim_pre_FORBS),nperm=10000)#p=0.3085, R2=0.01233296

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(soild.sim_post_FORBS),nperm=10000)#p=0.3749, R2=0.006019827

#biomass v shallow sand

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalsand.sim_pre_FORBS),nperm=10000)#p=0.8917000, R2=0.0002752665

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalsand.sim_post_FORBS),nperm=10000)#p=0.0057, R2=0.05173915

#biomass v shallow silt

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalsilt.sim_pre_FORBS),nperm=10000)#p=0.8482, R2=0.0004730482

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalsilt.sim_post_FORBS),nperm=10000)#p=0.0126, R2=0.03724474

###biomass v shallow clay

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalclay.sim_pre_FORBS),nperm=10000)#p=0.564200, R2=0.004908207

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalclay.sim_post_FORBS),nperm=10000)#p=0.0372, R2=0.02600693

###biomass v shallow vfs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalvfs.sim_pre_FORBS),nperm=10000)#p=0.2954, R2=0.01138324
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalvfs.sim_post_FORBS),nperm=10000)#p=0.0086, R2=0.04979943

###biomass v shallow fs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalfs.sim_pre_FORBS),nperm=10000)#p=0.4462, R2=0.004189217
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalfs.sim_post_FORBS),nperm=10000)#0.4806, R2=0.002970107

####biomass v shallow ms
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalms.sim_pre_FORBS),nperm=10000)#p=0.5915, R2=0.001713485
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalms.sim_post_FORBS),nperm=10000)#p=0.7871, R2=0.0004426034
####biomass v shallow cos
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalcos.sim_pre_FORBS),nperm=10000)#p=0.2134, R2=0.008346818
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalcos.sim_post_FORBS),nperm=10000)#p=0.4000, R2=0.003857545

####biomass v shallow vcs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(shalvcs.sim_pre_FORBS),nperm=10000)#p=0.0161, R2=0.03816935
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalvcs.sim_post_FORBS),nperm=10000)#p=0.6774, R2=0.001020402

#biomass v deep sand

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepsand.sim_pre_FORBS),nperm=10000)#p=0.9723, R2=0.00002720271

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepsand.sim_post_FORBS),nperm=10000)#p=0.0161, R2=0.03756451

#biomass v deep silt

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepsilt.sim_pre_FORBS),nperm=10000)#p=0.7257, R2=0.00165989

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepsilt.sim_post_FORBS),nperm=10000)#p=0.181800, R2=0.009917062

#biomass v deep clay

MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepclay.sim_pre_FORBS),nperm=10000)#p=0.8581, R2=0.0006406363

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepclay.sim_post_FORBS),nperm=10000)#p=0.0099,R2=0.04572562

#biomass v deep vfs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepvfs.sim_pre_FORBS),nperm=10000)#p=0.8262, R2=0.0006434591
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepvfs.sim_post_FORBS),nperm=10000)#p=0.2241, R2=0.009028334

#biomass v deep fs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepfs.sim_pre_FORBS),nperm=10000)#p=0.8403, R2=0.000657342
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepfs.sim_post_FORBS),nperm=10000)#p=0.2563, R2=0.007870597

#biomass v deep ms
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepms.sim_pre_FORBS),nperm=10000)#p=0.5366, 0.004776549
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepms.sim_post_FORBS),nperm=10000)#p=0.2597, R2=0.007625077

#biomass v deep cos
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepcos.sim_pre_FORBS),nperm=10000)#p=0.09330, R2=0.02472199
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepcos.sim_post_FORBS),nperm=10000)#p=0.1223, R2=0.01406232

#biomass v deep vcs
MRM(lower(clust.totabio.st_pre_FORBS$adj)~lower(deepvcs.sim_pre_FORBS),nperm=10000)#p=0.505100, R2=0.005282958
MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(deepvcs.sim_post_FORBS),nperm=10000)#p=0.259500, R2=0.007813946


######################




#################
#####now with long timescales
totbiogeog.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(geog.prox),nperm=10000) #p=.0143, R2=.07106
totbiogeog.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(geog.prox),nperm=10000) #p= .149100, R2=.01380667

#biomass v soildepth
totbiomeandepth.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(soild.sim),nperm=10000)#p=0.0022000, R2=.2023073
totbiomeandepth.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(soild.sim),nperm=10000)#p=.876100, R2=0.0002295912

#biomass v shallow sand
totbioshallowsand.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalsand.sim),nperm=10000)#p=.836, R2=0.0009135349
totbioshallowsand.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalsand.sim),nperm=10000)#p=.3756000, R2=0.0114399

#biomass v shallow silt
totbioshallowsilt.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalsilt.sim),nperm=10000)#p=.661100, R2=.004106542
totbioshallowsilt.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalsilt.sim),nperm=10000)#p=.4303, R2=0.008846327

###biomass v shallow clay
totbioshallowclay.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalclay.sim),nperm=10000)#p=.9529, R2=.00007225093
totbioshallowclay.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalclay.sim),nperm=10000)#p=0.3293, R2=0.01294999

###biomass v shallow vfs
totbioshallowvfs.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalvfs.sim),nperm=10000)#p=0.5676, R2=0.005373529
totbioshallowvfs.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalvfs.sim),nperm=10000)#p=0.68920000, R2=.001930862

###biomass v shallow fs
totbioshallowfs.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalfs.sim),nperm=10000)#p=0.4768, R2=0.005280802
totbioshallowfs.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalfs.sim),nperm=10000)#p=0.735000, R2=0.0007998142

####biomass v shallow ms
totbioshallowms.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalms.sim),nperm=10000)#p=0.22880000, R2=0.01093348
totbioshallowms.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalms.sim),nperm=10000)#p=0.276300, R2=0.006526503

####biomass v shallow cos
totbioshallowcos.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalcos.sim),nperm=10000)#p=0.85760000, R2=0.0002092811
totbioshallowcos.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalcos.sim),nperm=10000)#p=0.408200000, R2=0.4082000

####biomass v shallow vcs
totbioshallowvcs.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(shalvcs.sim),nperm=10000)#p=0.14610000, R2=0.01567237
totbioshallowvcs.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(shalvcs.sim),nperm=10000)#p=0.26220000, R2=0.00601551

#biomass v deep sand
totbiodeepsand.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(deepsand.sim),nperm=10000)#p=0.9293, R2=0.0001708328
totbiodeepsand.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(deepsand.sim),nperm=10000)#p=0.5264000, R2=0.007234905

#biomass v deep silt
totbiodeepsilt.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(deepsilt.sim),nperm=10000)#p=0.9177, R2=0.0001994683
totbiodeepsilt.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(deepsilt.sim),nperm=10000)#p=0.7133, R2=0.002410289

#biomass v deep clay
totbiodeepclay.lt_pre<-
  MRM(lower(clust.totabio.lt_pre$adj)~lower(deepclay.sim),nperm=10000)#p=0.8985000, R2=0.0003861484
totbiodeepclay.lt_post<-
  MRM(lower(clust.totabio.lt_post$adj)~lower(deepclay.sim),nperm=10000)#p=0.464, R2=0.00936281

#biomass v deep vfs
MRM(lower(clust.totabio.lt_pre$adj)~lower(deepvfs.sim),nperm=10000)#p=0.3452, R2=0.02094788
MRM(lower(clust.totabio.lt_post$adj)~lower(deepvfs.sim),nperm=10000)#p=0.6174, R2=0.003746802

#biomass v deep fs
MRM(lower(clust.totabio.lt_pre$adj)~lower(deepfs.sim),nperm=10000)#p=0.06990000, R2=0.07573482
MRM(lower(clust.totabio.lt_post$adj)~lower(deepfs.sim),nperm=10000)#p=0.4241, R2=0.009824874

#biomass v deep ms
MRM(lower(clust.totabio.lt_pre$adj)~lower(deepms.sim),nperm=10000)#p=0.055, R2=0.09665348
MRM(lower(clust.totabio.lt_post$adj)~lower(deepms.sim),nperm=10000)#p=0.918, R2=0.000236201

#biomass v deep cos
MRM(lower(clust.totabio.lt_pre$adj)~lower(deepcos.sim),nperm=10000)#p=0.0513, R2=0.05140581
MRM(lower(clust.totabio.lt_post$adj)~lower(deepcos.sim),nperm=10000)#p=0.8967, R2=0.0001435857

#biomass v deep vcs
MRM(lower(clust.totabio.lt_pre$adj)~lower(deepvcs.sim),nperm=10000)#p=0.1242, R2=0.04481923
MRM(lower(clust.totabio.lt_post$adj)~lower(deepvcs.sim),nperm=10000)#p=0.6316000, R2=0.003369392

#####################BOER LONG TERM

clust.totabio.lt_pre_BOER$adj<-as.matrix(clust.totabio.lt_pre_BOER$adj)
clust.totabio.lt_post_BOER$adj<-as.matrix(clust.totabio.lt_post_BOER$adj)

###try changing NAs to 0s
clust.totabio.lt_pre_BOER$adj[is.na(clust.totabio.lt_pre_BOER$adj)] <- 0
clust.totabio.lt_post_BOER$adj[is.na(clust.totabio.lt_post_BOER$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(geog.prox_pre_BOER),nperm=10000)###p=0.1476, R2=0.02302104

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(geog.prox_post_BOER),nperm=10000) ###p=0.311, R2=0.03549146
#biomass v soildepth

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(soild.sim_pre_BOER),nperm=10000)##p=0.1712, R2=0.02848063

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(soild.sim_post_BOER),nperm=10000)##p=0.12170, R2=0.1596492

#biomass v shallow sand

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalsand.sim_pre_BOER),nperm=10000)##p=0.7777, R2=0.001277238

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalsand.sim_post_BOER),nperm=10000)#p=0.321200, R2=0.04327985

#biomass v shallow silt

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalsilt.sim_pre_BOER),nperm=10000)###p=0.5343, R2=0.006778398

MRM(lower(clust.totabio.st_post_BOER$adj)~lower(shalsilt.sim_post_BOER),nperm=10000)#p=0.502300, R2=0.01628968

###biomass v shallow clay

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalclay.sim_pre_BOER),nperm=10000)###p=0.04210, R2=0.05886352

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalclay.sim_post_BOER),nperm=10000)#p=0.4584, R2=0.01228086

###biomass v shallow vfs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalvfs.sim_pre_BOER),nperm=10000)#p=0.877, R2=0.0003249107
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalvfs.sim_post_BOER),nperm=10000)#p=0.5954, R2=0.01551353

###biomass v shallow fs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalfs.sim_pre_BOER),nperm=10000)#p=0.47, R2=0.004741368
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalfs.sim_post_BOER),nperm=10000)#p=0.9059, R2=0.0007140934

####biomass v shallow ms
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalms.sim_pre_BOER),nperm=10000)#p=0.1727, R2=0.01336329
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalms.sim_post_BOER),nperm=10000)#p=0.180400, R2=0.04822878
####biomass v shallow cos
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalcos.sim_pre_BOER),nperm=10000)#p=0.6797, R2=0.001049684
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalcos.sim_post_BOER),nperm=10000)#p=0.607, R2=0.006035244

####biomass v shallow vcs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(shalvcs.sim_pre_BOER),nperm=10000)#p=0.9092, R2=0.00009481948
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(shalvcs.sim_post_BOER),nperm=10000)#p=0.2055, R2=0.04014561

#biomass v deep sand

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepsand.sim_pre_BOER),nperm=10000)#p=0.02810, R2=0.06504214

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepsand.sim_post_BOER),nperm=10000)#p=0.6729, R2=0.01288857

#biomass v deep silt

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepsilt.sim_pre_BOER),nperm=10000)#p=0.22720, R2=0.01605036

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepsilt.sim_post_BOER),nperm=10000)#p=0.9494, R2=0.0002736318

#biomass v deep clay

MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepclay.sim_pre_BOER),nperm=10000)#p=0.0036, R2=0.142499

MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepclay.sim_post_BOER),nperm=10000)#p=0.3318, R2=0.6553183

#biomass v deep vfs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepvfs.sim_pre_BOER),nperm=10000)#p=0.1952, R2=0.3476918
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepvfs.sim_post_BOER),nperm=10000)#p=0.127100, R2=0.1591327

#biomass v deep fs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepfs.sim_pre_BOER),nperm=10000)#p=0.312400, R2=0.02337646
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepfs.sim_post_BOER),nperm=10000)#p=0.0495, R2=0.2216234

#biomass v deep ms
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepms.sim_pre_BOER),nperm=10000)#p=0.10130, R2=0.05943184
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepms.sim_post_BOER),nperm=10000)#p=0.0346, R2=0.2301464

#biomass v deep cos
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepcos.sim_pre_BOER),nperm=10000)#p=0.1529, R2=0.02807471
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepcos.sim_post_BOER),nperm=10000)#p=0.1589, R2=0.06887021

#biomass v deep vcs
MRM(lower(clust.totabio.lt_pre_BOER$adj)~lower(deepvcs.sim_pre_BOER),nperm=10000)#p=0.6839, R2=0.003710532
MRM(lower(clust.totabio.lt_post_BOER$adj)~lower(deepvcs.sim_post_BOER),nperm=10000)#p=0.401800, R2=0.0453869

########################

#####################GRASS
clust.totabio.lt_pre_GRASS$adj<-as.matrix(clust.totabio.lt_pre_GRASS$adj)
clust.totabio.lt_post_GRASS$adj<-as.matrix(clust.totabio.lt_post_GRASS$adj)

###try changing NAs to 0s
clust.totabio.lt_pre_GRASS$adj[is.na(clust.totabio.lt_pre_GRASS$adj)] <- 0
clust.totabio.lt_post_GRASS$adj[is.na(clust.totabio.lt_post_GRASS$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(geog.prox_pre_GRASS),nperm=10000)#p=0.0161, R2=0.0681229

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(geog.prox_post_GRASS),nperm=10000)#p=0.36880, R2=0.007537561 
#biomass v soildepth

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(soild.sim_pre_GRASS),nperm=10000)#p=0.00300, R2=0.200857

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(soild.sim_post_GRASS),nperm=10000)#p=0.7046000, R2=0.00198986

#biomass v shallow sand

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalsand.sim_pre_GRASS),nperm=10000)#p=0.8267, R2=0.0009485065

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalsand.sim_post_GRASS),nperm=10000)#p=0.7507, R2=0.003633384

#biomass v shallow silt

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalsilt.sim_pre_GRASS),nperm=10000)#p=0.6585, R2=0.004136248

MRM(lower(clust.totabio.st_post_GRASS$adj)~lower(shalsilt.sim_post_GRASS),nperm=10000)#p=0.983, R2=0.00000483974

###biomass v shallow clay

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalclay.sim_pre_GRASS),nperm=10000)#p=0.958, R2=0.00004455346

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalclay.sim_post_GRASS),nperm=10000)#p=0.8576, R2=0.001721321

###biomass v shallow vfs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalvfs.sim_pre_GRASS),nperm=10000)#p=0.5535, R2=0.005546016
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalvfs.sim_post_GRASS),nperm=10000)#p=0.9864, R2=0.000005849257

###biomass v shallow fs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalfs.sim_pre_GRASS),nperm=10000)#p=0.4483, R2=0.006010165
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalfs.sim_post_GRASS),nperm=10000)#p=0.5496, R2=0.003184202

####biomass v shallow ms
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalms.sim_pre_GRASS),nperm=10000)#p=0.2013, R2=0.01215282
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalms.sim_post_GRASS),nperm=10000)#p=0.8094, R2=0.0004079745
####biomass v shallow cos
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalcos.sim_pre_GRASS),nperm=10000)#p=0.7948, R2=0.0004464946
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalcos.sim_post_GRASS),nperm=10000)#p=0.8833, R2=0.0001203175

####biomass v shallow vcs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(shalvcs.sim_pre_GRASS),nperm=10000)#p=0.14220, R2=0.01562222
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(shalvcs.sim_post_GRASS),nperm=10000)#p=0.0916, R2=0.01588378

#biomass v deep sand

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepsand.sim_pre_GRASS),nperm=10000)#p=0.9249, R2=0.0001834212

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepsand.sim_post_GRASS),nperm=10000)#p=0.9731, R2=0.00006638432

#biomass v deep silt

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepsilt.sim_pre_GRASS),nperm=10000)#p=0.9204, R2=0.0001842466

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepsilt.sim_post_GRASS),nperm=10000)#p=0.6419, R2=0.006056318

#biomass v deep clay

MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepclay.sim_pre_GRASS),nperm=10000)#p=0.8854, R2=0.0003907382

MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepclay.sim_post_GRASS),nperm=10000)#p=0.8475, R2=0.002331176

#biomass v deep vfs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepvfs.sim_pre_GRASS),nperm=10000)#p=0.3368, R2=0.02084723
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepvfs.sim_post_GRASS),nperm=10000)#p=0.609100, R2=0.006097606

#biomass v deep fs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepfs.sim_pre_GRASS),nperm=10000)#p=0.07090, R2=0.07563208
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepfs.sim_post_GRASS),nperm=10000)#p=0.409200, R2=0.01360326

#biomass v deep ms
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepms.sim_pre_GRASS),nperm=10000)#p=0.05090, R2=0.09742495
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepms.sim_post_GRASS),nperm=10000)#p=0.8035, R2=0.002660604

#biomass v deep cos
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepcos.sim_pre_GRASS),nperm=10000)#p=0.05230, R2=0.05148073
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepcos.sim_post_GRASS),nperm=10000)#p=0.8258, R2=0.0006171493

#biomass v deep vcs
MRM(lower(clust.totabio.lt_pre_GRASS$adj)~lower(deepvcs.sim_pre_GRASS),nperm=10000)#p=0.1195, R2=0.04444938
MRM(lower(clust.totabio.lt_post_GRASS$adj)~lower(deepvcs.sim_post_GRASS),nperm=10000)#p=0.8351, R2=0.001190896
#########################

####################FORBS
clust.totabio.lt_pre_FORBS$adj<-as.matrix(clust.totabio.lt_pre_FORBS$adj)
clust.totabio.lt_post_FORBS$adj<-as.matrix(clust.totabio.lt_post_FORBS$adj)

###try changing NAs to 0s
clust.totabio.lt_pre_FORBS$adj[is.na(clust.totabio.lt_pre_FORBS$adj)] <- 0
clust.totabio.lt_post_FORBS$adj[is.na(clust.totabio.lt_post_FORBS$adj)] <- 0

##biomass v geography

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(geog.prox_pre_FORBS),nperm=10000)#p= 0.4024, R2=0.00673837

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(geog.prox_post_FORBS),nperm=10000)#p=0.4386, R2=0.007200111
#biomass v soildepth

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(soild.sim_pre_FORBS),nperm=10000)#p=0.0989, R2=0.05375288

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(soild.sim_post_FORBS),nperm=10000)#p=0.4073, R2=0.01466009

#biomass v shallow sand

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalsand.sim_pre_FORBS),nperm=10000)#p=0.6831, R2=0.004097669

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalsand.sim_post_FORBS),nperm=10000)#p=0.00290, R2=0.237112

#biomass v shallow silt

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalsilt.sim_pre_FORBS),nperm=10000)#p=0.5803, R2=0.006806447

MRM(lower(clust.totabio.st_post_FORBS$adj)~lower(shalsilt.sim_post_FORBS),nperm=10000)#p=0.0171, R2=0.03734474

###biomass v shallow clay

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalclay.sim_pre_FORBS),nperm=10000)#p=0.7308, R2=0.002956731

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalclay.sim_post_FORBS),nperm=10000)#p=0.0040, R2=0.1854342

###biomass v shallow vfs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalvfs.sim_pre_FORBS),nperm=10000)#p=0.4376, R2=0.009644462
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalvfs.sim_post_FORBS),nperm=10000)#p=0.01690, R2=0.1126197

###biomass v shallow fs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalfs.sim_pre_FORBS),nperm=10000)#p=0.2965, R2=0.009436678
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalfs.sim_post_FORBS),nperm=10000)#p=0.8102, R2=0.0007973057

####biomass v shallow ms
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalms.sim_pre_FORBS),nperm=10000)#p=0.012, R2=0.04571185
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalms.sim_post_FORBS),nperm=10000)#p=0.5964, R2=0.002865732
####biomass v shallow cos
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalcos.sim_pre_FORBS),nperm=10000)#p=0.0981, R2=0.01476431
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalcos.sim_post_FORBS),nperm=10000)#p=0.75, R2=0.0007549137

####biomass v shallow vcs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(shalvcs.sim_pre_FORBS),nperm=10000)#p=0.4744, R2=0.003309034
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(shalvcs.sim_post_FORBS),nperm=10000)#p=0.0384, R2=0.03431309

#biomass v deep sand

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepsand.sim_pre_FORBS),nperm=10000)#p=0.7328, R2=0.003142513

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepsand.sim_post_FORBS),nperm=10000)#p=0.00370, R2=0.1745996

#biomass v deep silt

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepsilt.sim_pre_FORBS),nperm=10000)#p=0.7751, R2=0.002045349

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepsilt.sim_post_FORBS),nperm=10000)#p=0.2512, R2=0.01617245

#biomass v deep clay

MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepclay.sim_pre_FORBS),nperm=10000)#p=0.758, R2=0.002645482

MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepclay.sim_post_FORBS),nperm=10000)#p=0.00280, R2=0.2666074

#biomass v deep vfs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepvfs.sim_pre_FORBS),nperm=10000)#p=0.5518, R2=0.007475373
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepvfs.sim_post_FORBS),nperm=10000)#p=0.8094, R2=0.001587395

#biomass v deep fs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepfs.sim_pre_FORBS),nperm=10000)#p=0.9994, R2=0.000000007991917
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepfs.sim_post_FORBS),nperm=10000)#p=0.8511, R2=0.001039595

#biomass v deep ms
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepms.sim_pre_FORBS),nperm=10000)#p=0.8409, R2=0.0009915774
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepms.sim_post_FORBS),nperm=10000)#p=0.9158, R2=0.000323552

#biomass v deep cos
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepcos.sim_pre_FORBS),nperm=10000)#p=0.6071, R2=0.003211434
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepcos.sim_post_FORBS),nperm=10000)#p=0.4850, 0.06651538

#biomass v deep vcs
MRM(lower(clust.totabio.lt_pre_FORBS$adj)~lower(deepvcs.sim_pre_FORBS),nperm=10000)#p=0.3538, R2=0.01635926
MRM(lower(clust.totabio.lt_post_FORBS$adj)~lower(deepvcs.sim_post_FORBS),nperm=10000)#p=0.1753, R2=0.03280788

########################




## Multivariate Wavelet Coherence Models

#Test coherence in multivariate models and compute fractions of synchrony explained and cross-terms.

#First, test bivariate coherences with control variables

st<-c(2,6)
lt<-c(6,Inf)

#droughtXtotbio_st_lt_pre
totalXpdsi<-coh(total_pre.cln, PDSIpre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXpdsi<-bandtest(totalXpdsi, st); totalXpdsi<-bandtest(totalXpdsi, lt)
print(totalXpdsi$bandp)

#ts_low_bd ts_hi_bd      p_val     mn_phs
#1         2        6 0.04149585 -0.1916681
#2         6      Inf 0.86311369  0.1719154

#droughtXtotbio_st_lt_post
totalXpdsi<-coh(total_post.cln, PDSIpost.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXpdsi<-bandtest(totalXpdsi, st); totalXpdsi<-bandtest(totalXpdsi, lt)
print(totalXpdsi$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.1545845 -1.841257
#2         6      Inf 0.2725727  0.943993

#PDOXtotbio_st_lt_pre
totalXpdo<-coh(total_pre.cln, PDOpre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXpdo<-bandtest(totalXpdo, st); totalXpdo<-bandtest(totalXpdo, lt)
print(totalXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.4576542 -2.503606
#2         6      Inf 0.2283772 -1.943073



#PDOXtotbio_st_lt_post
totalXpdo<-coh(total_post.cln, PDOpost.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXpdo<-bandtest(totalXpdo, st); totalXpdo<-bandtest(totalXpdo, lt)
print(totalXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.6168383 -2.470397
#2         6      Inf 0.6560344  2.095846


#ENSOXtotbio_st_lt_pre
totalXenso<-coh(total_pre.cln, ENSOpre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXenso<-bandtest(totalXenso, st); totalXenso<-bandtest(totalXenso, lt)
print(totalXenso$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.2107789 -2.0933988
#2         6      Inf 0.3652635 -0.4977538

#ENSOXtotbio_st_lt_post
totalXenso<-coh(total_post.cln, ENSOpost.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXenso<-bandtest(totalXenso, st); totalXenso<-bandtest(totalXenso, lt)
print(totalXenso$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.6326367 -2.559644
#2         6      Inf 0.2472753 -2.930561

################pick up here, repeat with grass, forb, BOER
####totprecipXtotbio_ST_LT_PRE
totalXtotal<-coh(total_pre.cln, totalprcp_pre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXtotal<-bandtest(totalXtotal, st); totalXtotal<-bandtest(totalXtotal, lt)
print(totalXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.1315868 -1.6298233
#2         6      Inf 0.8541146 -0.3162321

####totprecipXtotbio_ST_LT_POST
totalXtotal<-coh(total_post.cln, totalprcp_post.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXtotal<-bandtest(totalXtotal, st); totalXtotal<-bandtest(totalXtotal, lt)
print(totalXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.2212779 -1.5445979
#2         6      Inf 0.3131687  0.5766398

####growingprecipXtotbio_ST_LT_PRE
totalXgrprcp<-coh(total_pre.cln, growingprcp_pre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXgrprcp<-bandtest(totalXgrprcp, st); totalXgrprcp<-bandtest(totalXgrprcp, lt)
print(totalXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.3798620 -1.239328
#2         6      Inf 0.7056294 -1.051036

####growingprecipXtotbio_ST_LT_POST
totalXgrprcp<-coh(total_post.cln, growingprcp_post.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXgrprcp<-bandtest(totalXgrprcp, st); totalXgrprcp<-bandtest(totalXgrprcp, lt)
print(totalXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val   mn_phs
#1         2        6 0.4042596 2.188088
#2         6      Inf 0.6977302 2.447874


####dormantprecipXtotbio_ST_LT_PRE
totalXdorprcp<-coh(total_pre.cln, dormantprcp_pre.cln, years_pre, norm="powall", sigmethod="fast", nrand=10000)
totalXdorprcp<-bandtest(totalXdorprcp, st); totalXdorprcp<-bandtest(totalXdorprcp, lt)
print(totalXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.6436356 -1.9263038
#2         6      Inf 0.6866313  0.6216603

####dormantprecipXtotbio_ST_LT_POST
totalXdorprcp<-coh(total_post.cln, dormantprcp_post.cln, years_post, norm="powall", sigmethod="fast", nrand=10000)
totalXdorprcp<-bandtest(totalXdorprcp, st); totalXdorprcp<-bandtest(totalXdorprcp, lt)
print(totalXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.2071793 2.8050045
#2         6      Inf 0.5352465 0.4334391

####repeat for boer, forb, grass, interactions
################BOER
#PDOXboer_st_lt_pre
boerXpdo<-coh(total_pre.cln_BOER, PDOpre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
boerXpdo<-bandtest(boerXpdo, st); boerXpdo<-bandtest(boerXpdo, lt)
print(boerXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.3015698 -2.440554
#2         6      Inf 0.2589741 -2.128846


#PDOXboer_st_lt_post
boerXpdo<-coh(total_post.cln_BOER, PDOpost.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
boerXpdo<-bandtest(boerXpdo, st); boerXpdo<-bandtest(boerXpdo, lt)
print(boerXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.6833317 -1.389768
#2         6      Inf 0.4493551  1.972660

#ENSOXboer_st_lt_pre
boerXenso<-coh(total_pre.cln_BOER, ENSOpre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
boerXenso<-bandtest(boerXenso, st); boerXenso<-bandtest(boerXenso, lt)
print(boerXenso$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.1194881 -2.1281730
#2         6      Inf 0.3991601 -0.6934393

#ENSOXboer_st_lt_post
boerXenso<-coh(total_post.cln_BOER, ENSOpost.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
boerXenso<-bandtest(boerXenso, st); boerXenso<-bandtest(boerXenso, lt)
print(boerXenso$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.9545045  2.843524
#2         6      Inf 0.8579142 -2.516959

#droughtXtotbio_st_lt_pre
BOERXpdsi<-coh(total_pre.cln_BOER, PDSIpre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
BOERXpdsi<-bandtest(BOERXpdsi, st); BOERXpdsi<-bandtest(BOERXpdsi, lt)
print(BOERXpdsi$bandp)

#ts_low_bd ts_hi_bd      p_val      mn_phs
#1         2        6 0.01889811 -0.24673653
#2         6      Inf 0.86191381  0.03461046


#droughtXtotbio_st_lt_post
BOERpostXpdsi<-coh(total_post.cln_BOER, PDSIpost.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
BOERpostXpdsi<-bandtest(BOERpostXpdsi, st); BOERpostXpdsi<-bandtest(BOERpostXpdsi, lt)
print(BOERpostXpdsi$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.4704530 -1.411190
#2         6      Inf 0.4492551  1.467718

####totprecipXtotbio_ST_LT_PRE
BOERPREXtotal<-coh(total_pre.cln_BOER, totalprcp_pre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
BOERPREXtotal<-bandtest(BOERPREXtotal, st); BOERPREXtotal<-bandtest(BOERPREXtotal, lt)
print(BOERPREXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.1354865 -1.6453113
#2         6      Inf 0.8857114 -0.5361276

####totprecipXtotbio_ST_LT_POST
BOERPOSTXtotal<-coh(total_post.cln_BOER, totalprcp_post.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
BOERPOSTXtotal<-bandtest(BOERPOSTXtotal, st); BOERPOSTXtotal<-bandtest(BOERPOSTXtotal, lt)
print(BOERPOSTXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.6245375 1.0671205
#2         6      Inf 0.6834317 0.9719343


####growingprecipXtotbio_ST_LT_PRE
BOERPREXgrprcp<-coh(total_pre.cln_BOER, growingprcp_pre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
BOERPREXgrprcp<-bandtest(BOERPREXgrprcp, st); BOERPREXgrprcp<-bandtest(BOERPREXgrprcp, lt)
print(BOERPREXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.3878612 -1.246398
#2         6      Inf 0.7547245 -1.234264


####growingprecipXtotbio_ST_LT_POST
BOERPOSTXgrprcp<-coh(total_post.cln_BOER, growingprcp_post.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
BOERPOSTXgrprcp<-bandtest(BOERPOSTXgrprcp, st); BOERPOSTXgrprcp<-bandtest(BOERPOSTXgrprcp, lt)
print(BOERPOSTXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val   mn_phs
#1         2        6 0.3281672 1.565257
#2         6      Inf 0.0979902 2.936289


####dormantprecipXtotbio_ST_LT_PRE
BOERPREXdorprcp<-coh(total_pre.cln_BOER, dormantprcp_pre.cln_BOER, years_pre, norm="powall", sigmethod="fast", nrand=10000)
BOERPREXdorprcp<-bandtest(BOERPREXdorprcp, st); BOERPREXdorprcp<-bandtest(BOERPREXdorprcp, lt)
print(BOERPREXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.5162484 -1.9100341
#2         6      Inf 0.6767323  0.4121384



####dormantprecipXtotbio_ST_LT_POST
BOERPOSTXdorprcp<-coh(total_post.cln_BOER, dormantprcp_post.cln_BOER, years_post, norm="powall", sigmethod="fast", nrand=10000)
BOERPOSTXdorprcp<-bandtest(BOERPOSTXdorprcp, st); BOERPOSTXdorprcp<-bandtest(BOERPOSTXdorprcp, lt)
print(BOERPOSTXdorprcp$bandp)


#ts_low_bd ts_hi_bd      p_val     mn_phs
#1         2        6 0.12858714 -1.4859379
#2         6      Inf 0.08659134  0.8857282

####################

###################GRASS
#PDOXgrass_st_lt_pre
grassXpdo<-coh(total_pre.cln_GRASS, PDOpre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
grassXpdo<-bandtest(grassXpdo, st); grassXpdo<-bandtest(grassXpdo, lt)
print(grassXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.4609539 -2.504815
#2         6      Inf 0.2279772 -1.947799

#PDOXgrass_st_lt_post
grassXpdo<-coh(total_post.cln_GRASS, PDOpost.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
grassXpdo<-bandtest(grassXpdo, st); grassXpdo<-bandtest(grassXpdo, lt)
print(grassXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.5587441 -2.395118
#2         6      Inf 0.2810719  2.174764

#ENSOXgrass_st_lt_pre
grassXenso<-coh(total_pre.cln_GRASS, ENSOpre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
grassXenso<-bandtest(grassXenso, st); grassXenso<-bandtest(grassXenso, lt)
print(grassXenso$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.1960804 -2.0922926
#2         6      Inf 0.3610639 -0.5044839

#ENSOXgrass_st_lt_post
grassXenso<-coh(total_post.cln_GRASS, ENSOpost.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
grassXenso<-bandtest(grassXenso, st); grassXenso<-bandtest(grassXenso, lt)
print(grassXenso$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.7823218 -2.894557
#2         6      Inf 0.3809619 -2.623205

#droughtXtotbio_st_lt_pre
GRASSXpdsi<-coh(total_pre.cln_GRASS, PDSIpre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
GRASSXpdsi<-bandtest(GRASSXpdsi, st); GRASSXpdsi<-bandtest(GRASSXpdsi, lt)
print(GRASSXpdsi$bandp)

#ts_low_bd ts_hi_bd      p_val     mn_phs
#1         2        6 0.03809619 -0.1881530
#2         6      Inf 0.86261374  0.1715451

#droughtXtotbio_st_lt_post
GRASSpostXpdsi<-coh(total_post.cln_GRASS, PDSIpost.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
GRASSpostXpdsi<-bandtest(GRASSpostXpdsi, st); GRASSpostXpdsi<-bandtest(GRASSpostXpdsi, lt)
print(GRASSpostXpdsi$bandp)

#ts_low_bd ts_hi_bd     p_val   mn_phs
#1         2        6 0.5061494 3.070827
#2         6      Inf 0.6993301 1.394015

####totprecipXtotbio_ST_LT_PRE
GRASSPREXtotal<-coh(total_pre.cln_GRASS, totalprcp_pre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
GRASSPREXtotal<-bandtest(GRASSPREXtotal, st); GRASSPREXtotal<-bandtest(GRASSPREXtotal, lt)
print(GRASSPREXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.1312869 -1.6295018
#2         6      Inf 0.8554145 -0.3143846

####totprecipXtotbio_ST_LT_POST
GRASSPOSTXtotal<-coh(total_post.cln_GRASS, totalprcp_post.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
GRASSPOSTXtotal<-bandtest(GRASSPOSTXtotal, st); GRASSPOSTXtotal<-bandtest(GRASSPOSTXtotal, lt)
print(GRASSPOSTXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.3473653 -2.2790027
#2         6      Inf 0.7108289  0.9168907

####growingprecipXtotbio_ST_LT_PRE
GRASSPREXgrprcp<-coh(total_pre.cln_GRASS, growingprcp_pre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
GRASSPREXgrprcp<-bandtest(GRASSPREXgrprcp, st); GRASSPREXgrprcp<-bandtest(GRASSPREXgrprcp, lt)
print(GRASSPREXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.3799620 -1.239845
#2         6      Inf 0.6919308 -1.051701

####growingprecipXtotbio_ST_LT_POST
GRASSPOSTXgrprcp<-coh(total_post.cln_GRASS, growingprcp_post.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
GRASSPOSTXgrprcp<-bandtest(GRASSPOSTXgrprcp, st); GRASSPOSTXgrprcp<-bandtest(GRASSPOSTXgrprcp, lt)
print(GRASSPOSTXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val   mn_phs
#1         2        6 0.4022598 1.509519
#2         6      Inf 0.6208379 2.888853

####dormantprecipXtotbio_ST_LT_PRE
GRASSPREXdorprcp<-coh(total_pre.cln_GRASS, dormantprcp_pre.cln_GRASS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
GRASSPREXdorprcp<-bandtest(GRASSPREXdorprcp, st); GRASSPREXdorprcp<-bandtest(GRASSPREXdorprcp, lt)
print(GRASSPREXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.6409359 -1.9245619
#2         6      Inf 0.6809319  0.6158648

####dormantprecipXtotbio_ST_LT_POST
GRASSPOSTXdorprcp<-coh(total_post.cln_GRASS, dormantprcp_post.cln_GRASS, years_post, norm="powall", sigmethod="fast", nrand=10000)
GRASSPOSTXdorprcp<-bandtest(GRASSPOSTXdorprcp, st); GRASSPOSTXdorprcp<-bandtest(GRASSPOSTXdorprcp, lt)
print(GRASSPOSTXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val   mn_phs
#1         2        6 0.3651635 2.818793
#2         6      Inf 0.4688531 0.827307

######################

##################FORBS
#PDOXforbs_st_lt_pre
forbXpdo<-coh(total_pre.cln_FORBS, PDOpre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
forbXpdo<-bandtest(forbXpdo, st); forbXpdo<-bandtest(forbXpdo, lt)
print(forbXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.3934607 -1.772359
#2         6      Inf 0.6777322  0.739080

#PDOXforbs_st_lt_post
forbXpdo<-coh(total_post.cln_FORBS, PDOpost.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
forbXpdo<-bandtest(forbXpdo, st); forbXpdo<-bandtest(forbXpdo, lt)
print(forbXpdo$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.2973703  1.0804751
#2         6      Inf 0.2894711 -0.3390865

#ENSOXforbs_st_lt_pre
forbXenso<-coh(total_pre.cln_FORBS, ENSOpre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
forbXenso<-bandtest(forbXenso, st); forbXenso<-bandtest(forbXenso, lt)
print(forbXenso$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.8423158 -0.2994098
#2         6      Inf 0.3475652  2.3171877

#ENSOXforbs_st_lt_post
forbXenso<-coh(total_post.cln_FORBS, ENSOpost.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
forbXenso<-bandtest(forbXenso, st); forbXenso<-bandtest(forbXenso, lt)
print(forbXenso$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.7166283 -0.972271
#2         6      Inf 0.5189481  1.282750

#droughtXtotbio_st_lt_pre
FORBSXpdsi<-coh(total_pre.cln_FORBS, PDSIpre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
FORBSXpdsi<-bandtest(FORBSXpdsi, st); FORBSXpdsi<-bandtest(FORBSXpdsi, lt)
print(FORBSXpdsi$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.8453155  0.4871281
#2         6      Inf 0.5522448 -2.9859948

#droughtXtotbio_st_lt_post
FORBSpostXpdsi<-coh(total_post.cln_FORBS, PDSIpost.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
FORBSpostXpdsi<-bandtest(FORBSpostXpdsi, st); FORBSpostXpdsi<-bandtest(FORBSpostXpdsi, lt)
print(FORBSpostXpdsi$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.6767323 -1.109835
#2         6      Inf 0.6390361 -1.030696

####totprecipXtotbio_ST_LT_PRE
FORBSPREXtotal<-coh(total_pre.cln_FORBS, totalprcp_pre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
FORBSPREXtotal<-bandtest(FORBSPREXtotal, st); FORBSPREXtotal<-bandtest(FORBSPREXtotal, lt)
print(FORBSPREXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.8931107 -0.8019087
#2         6      Inf 0.3679632  2.9597288

####totprecipXtotbio_ST_LT_POST
FORBSPOSTXtotal<-coh(total_post.cln_FORBS, totalprcp_post.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
FORBSPOSTXtotal<-bandtest(FORBSPOSTXtotal, st); FORBSPOSTXtotal<-bandtest(FORBSPOSTXtotal, lt)
print(FORBSPOSTXtotal$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.7692231 -3.107692
#2         6      Inf 0.4185581 -1.522897

####growingprecipXtotbio_ST_LT_PRE
FORBSPREXgrprcp<-coh(total_pre.cln_FORBS, growingprcp_pre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
FORBSPREXgrprcp<-bandtest(FORBSPREXgrprcp, st); FORBSPREXgrprcp<-bandtest(FORBSPREXgrprcp, lt)
print(FORBSPREXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.6513349 -0.8857592
#2         6      Inf 0.6058394  1.9187135

####growingprecipXtotbio_ST_LT_POST
FORBSPOSTXgrprcp<-coh(total_post.cln_FORBS, growingprcp_post.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
FORBSPOSTXgrprcp<-bandtest(FORBSPOSTXgrprcp, st); FORBSPOSTXgrprcp<-bandtest(FORBSPOSTXgrprcp, lt)
print(FORBSPOSTXgrprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.3852615 -1.8525184
#2         6      Inf 0.4693531  0.4455261

####dormantprecipXtotbio_ST_LT_PRE
FORBSPREXdorprcp<-coh(total_pre.cln_FORBS, dormantprcp_pre.cln_FORBS, years_pre, norm="powall", sigmethod="fast", nrand=10000)
FORBSPREXdorprcp<-bandtest(FORBSPREXdorprcp, st); FORBSPREXdorprcp<-bandtest(FORBSPREXdorprcp, lt)
print(FORBSPREXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val     mn_phs
#1         2        6 0.4676532 -0.3775359
#2         6      Inf 0.3465653 -2.8874884

####dormantprecipXtotbio_ST_LT_POST
FORBSPOSTXdorprcp<-coh(total_post.cln_FORBS, dormantprcp_post.cln_FORBS, years_post, norm="powall", sigmethod="fast", nrand=10000)
FORBSPOSTXdorprcp<-bandtest(FORBSPOSTXdorprcp, st); FORBSPOSTXdorprcp<-bandtest(FORBSPOSTXdorprcp, lt)
print(FORBSPOSTXdorprcp$bandp)

#ts_low_bd ts_hi_bd     p_val    mn_phs
#1         2        6 0.4952505 -2.377648
#2         6      Inf 0.2559744 -1.613594

#####################

########temporal synchrony
library(tsvr)
library(tidyverse)
library(matrixStats)
jrn_quadrats=read.csv("F:/NMSU/Spatiotemporal Synchrony/JQP.csv")
#remove duplicate sampling events from beginning of time series
##If quadratxproject year not unique, choose later sampling date
jrn_quads_latest=jrn_quadrats%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%ungroup()

##here is the code for making a counts instead of cover dataset, in case we want to go down that route later
##JRNQUAD_Counts=jrn_quads_latest%>%group_by(QYear,species_code)%>%mutate(n=length(species_code))%>%slice_head()%>%ungroup()



jrnQuads80=jrn_quads_latest[jrn_quads_latest$quadrat=="I2"|jrn_quads_latest$quadrat=="I7"|jrn_quads_latest$quadrat=="I1"|
                              jrn_quads_latest$quadrat=="I4"|jrn_quads_latest$quadrat=="I3"|jrn_quads_latest$quadrat=="I5"|
                              jrn_quads_latest$quadrat=="I6"|jrn_quads_latest$quadrat=="J1"|jrn_quads_latest$quadrat=="J8"|
                              jrn_quads_latest$quadrat=="H1"|jrn_quads_latest$quadrat=="H2"|jrn_quads_latest$quadrat=="B1"|
                              jrn_quads_latest$quadrat=="B2"|jrn_quads_latest$quadrat=="B3"|jrn_quads_latest$quadrat=="N5"|
                              jrn_quads_latest$quadrat=="H3"|jrn_quads_latest$quadrat=="J12"|jrn_quads_latest$quadrat=="N4"|
                              jrn_quads_latest$quadrat=="R2"|jrn_quads_latest$quadrat=="R3"|jrn_quads_latest$quadrat=="J9",]


JRN_PreDrought<-jrnQuads80[jrnQuads80$project_year<1951,]
JRN_PostDrought<-jrnQuads80[jrnQuads80$project_year>=1957 & jrnQuads80$project_year<=1979,]

###subset by transect, set up species by year transect for each
quadrat_list=unique(JRN_PreDrought$quadrat)

TSVR_results=data.frame(matrix(nrow=length(quadrat_list),ncol=13))##################data frame to store results
colnames(TSVR_results)=c("species_code", "Pre_Long_cvcom", "Pre_Long_cvcomip2","Pre_long_vr", "Pre_Short_cvcom", "Pre_Short_cvcomip2","Pre_Short_vr",
                         "Post_Long_cvcom", "Post_Long_cvcomip2","Post_Long_vr", "Post_Short_cvcom", "Post_Short_cvcomip2", "Post_Short_vr")

for (i in 1:length(quadrat_list)){
  JRN_PreDrought_select=JRN_PreDrought[JRN_PreDrought$quadrat==quadrat_list[i],]
  JRN_PostDrought_select=JRN_PostDrought[JRN_PostDrought$quadrat==quadrat_list[i],]
  ####pull out quadrat, sum total areas together
  jrn_pre_summed=JRN_PreDrought_select%>%group_by(project_year, species_code)%>%mutate(spsum=sum(area))%>%slice_head()%>%ungroup()
  jrn_post_summed=JRN_PostDrought_select%>%group_by(project_year, species_code)%>%mutate(spsum=sum(area))%>%slice_head()%>%ungroup()
  ##remove individuals without area measurements, unknowns
  jrn_pre_unkrm=jrn_pre_summed[!(jrn_pre_summed$species_code=="UNKF" | jrn_pre_summed$species_code=="UNKG" | jrn_pre_summed$species_code=="UNKN"),]   ##remove unknown species
  jrn_post_unkrm=jrn_post_summed[!(jrn_post_summed$species_code=="UNKF" | jrn_post_summed$species_code=="UNKG" | jrn_post_summed$species_code=="UNKN"),]   ##remove unknown species
  
  ###now we need to transform into a species by time rxc matrix
  jrn_pre_relevant=jrn_pre_unkrm[,c(2,5,12)]
  jrn_post_relevant=jrn_post_unkrm[,c(2,5,12)]
  
  jrnwidepre=spread(jrn_pre_relevant, key='project_year', value='spsum')
  jrnwidepost=spread(jrn_post_relevant, key='project_year', value='spsum')
  
  ####here we need to take out rows (species) with no cover for any year
  jrnwidepre_narm<-jrnwidepre[rowSums(is.na(jrnwidepre[,2:ncol(jrnwidepre)])) != ncol(jrnwidepre)-1, ]
  jrnwidepost_narm<-jrnwidepost[rowSums(is.na(jrnwidepost[,2:ncol(jrnwidepost)])) != ncol(jrnwidepost)-1, ]
  
  years_of_quad_pre=c("species_code",1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,
                      1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950)
  
  years_of_quad_post=c("species_code",1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,
                       1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979)
  
  Missing_pre <- setdiff(years_of_quad_pre, names(jrnwidepre_narm))  # Find names of missing columns
  Missing_post <- setdiff(years_of_quad_post, names(jrnwidepost_narm))  # Find names of missing columns
  
  
  jrnwidepre_narm[Missing_pre] <- NA                    # Add them, filled with 'NA's
  jrnwidepost_narm[Missing_post] <- NA                    # Add them, filled with 'NA's
  
  jrnwidepre_narm <- jrnwidepre_narm[years_of_quad_pre]
  jrnwidepost_narm <- jrnwidepost_narm[years_of_quad_post]
  ####search through data frame, if year is empty, replace with median of species time series
  
  colna_pre= colnames(jrnwidepre_narm[, colSums(is.na(jrnwidepre_narm)) == nrow(jrnwidepre_narm)])
  colna_post= colnames(jrnwidepost_narm[, colSums(is.na(jrnwidepost_narm)) == nrow(jrnwidepost_narm)])
  
  jrnwidepre_narm[,colna_pre]=rowMedians(as.matrix(jrnwidepre_narm[,2:ncol(jrnwidepre_narm)]), na.rm=TRUE)
  jrnwidepost_narm[,colna_post]=rowMedians(as.matrix(jrnwidepost_narm[,2:ncol(jrnwidepost_narm)]), na.rm=TRUE)
  
  jrnwidepre_narm<-as.data.frame(jrnwidepre_narm)
  jrnwidepost_narm<-as.data.frame(jrnwidepost_narm)
  
  rownames(jrnwidepre_narm)=jrnwidepre_narm[,1]
  rownames(jrnwidepost_narm)=jrnwidepost_narm[,1]
  
  jrnwidepre_narm=jrnwidepre_narm[,c(2:ncol(jrnwidepre_narm))]
  jrnwidepost_narm=jrnwidepost_narm[,c(2:ncol(jrnwidepost_narm))]
  
  #######change all remaining NAs to 0s
  jrnwidepre_narm[is.na(jrnwidepre_narm)] = 0
  jrnwidepost_narm[is.na(jrnwidepost_narm)] = 0
  
  jrnwidepre_narm=as.matrix(jrnwidepre_narm)
  jrnwidepost_narm=as.matrix(jrnwidepost_narm)
  
  respre<-tsvreq_classic(jrnwidepre_narm)
  respost<-tsvreq_classic(jrnwidepost_narm)
  
  aggresLong_Pre<-aggts(respre, respre$ts[respre$ts>=4])
  aggresShort_Pre<-aggts(respre, respre$ts[respre$ts<4])
  
  aggresLong_Post<-aggts(respost, respost$ts[respost$ts>=4])
  aggresShort_Post<-aggts(respost, respost$ts[respre$ts<4])
  
  
  
  ###store results in table
  
  TSVR_results[i,]<-c(quadrat_list[i], aggresLong_Pre$com, aggresLong_Pre$comnull,aggresLong_Pre$vr,aggresShort_Pre$com, aggresShort_Pre$comnull,aggresShort_Pre$vr,
                      aggresLong_Post$com, aggresLong_Post$comnull,aggresLong_Post$vr, aggresShort_Post$com, aggresShort_Post$comnull, aggresShort_Post$vr)
  
  
}

####ok, now lets do significance testing on our results
###paired t test: long timescale vs short timescale for before, long timescale vs short timescale after (aggregate population variability)
##null hypothesis= long term dynamics dominant pre drought, short term dynamics dominant post drought


d <- with(TSVR_results, 
          as.numeric(Pre_Long_cvcomip2) - as.numeric(Pre_Short_cvcomip2))
# Shapiro-Wilk normality test for the differences
shapiro.test(d) 
###non-normal, use wilcox test

t.test(x=as.numeric(TSVR_results$Pre_Long_cvcomip2), y=as.numeric(TSVR_results$Pre_Short_cvcomip2), paired = TRUE, alternative = "greater")

wilcox.test(x=as.numeric(TSVR_results$Pre_Long_cvcomip2), y=as.numeric(TSVR_results$Pre_Short_cvcomip2), paired=TRUE, alternative = "greater")


mean(as.numeric(TSVR_results$Pre_Long_cvcomip2))
mean(as.numeric(TSVR_results$Pre_Short_cvcomip2))

#long-time period dynamics dominant in pre drought period

###now after drought

e <- with(TSVR_results, 
          as.numeric(Post_Long_cvcomip2) - as.numeric(Post_Short_cvcomip2))
# Shapiro-Wilk normality test for the differences
shapiro.test(e) 
#non-normal, use wilcox test

t.test(x=as.numeric(TSVR_results$Post_Long_cvcomip2), y=as.numeric(TSVR_results$Post_Short_cvcomip2), paired = TRUE, alternative = "less")

wilcox.test(x=as.numeric(TSVR_results$Post_Long_cvcomip2), y=as.numeric(TSVR_results$Post_Short_cvcomip2), paired=TRUE, alternative="less")
###short dynamics dominant in post drought period
mean(as.numeric(TSVR_results$Post_Long_cvcomip2))
mean(as.numeric(TSVR_results$Post_Short_cvcomip2))



####repeat with cvcom

f <- with(TSVR_results, 
          as.numeric(Pre_Long_cvcom) - as.numeric(Pre_Short_cvcom))
# Shapiro-Wilk normality test for the differences
shapiro.test(f) 


t.test(x=as.numeric(TSVR_results$Pre_Long_cvcom), y=as.numeric(TSVR_results$Pre_Short_cvcom), paired = TRUE, alternative = "greater")


mean(as.numeric(TSVR_results$Pre_Long_cvcom))
mean(as.numeric(TSVR_results$Pre_Short_cvcom))

#long-time period dynamics dominant in pre drought period

###now after drought

g <- with(TSVR_results, 
          as.numeric(Post_Long_cvcom) - as.numeric(Post_Short_cvcom))
# Shapiro-Wilk normality test for the differences
shapiro.test(g) 


t.test(x=as.numeric(TSVR_results$Post_Long_cvcom), y=as.numeric(TSVR_results$Post_Short_cvcom), paired = TRUE, alternative = "less")

wilcox.test(as.numeric(TSVR_results$Post_Long_cvcom), as.numeric(TSVR_results$Post_Short_cvcom), paired=TRUE, alternative = 'less')
###short dynamics dominant in post drought period
mean(as.numeric(TSVR_results$Post_Long_cvcom))
mean(as.numeric(TSVR_results$Post_Short_cvcom))

###repeat with vr
h <- with(TSVR_results, 
          as.numeric(Pre_long_vr) - as.numeric(Pre_Short_vr))
# Shapiro-Wilk normality test for the differences
shapiro.test(h) 


t.test(x=as.numeric(TSVR_results$Pre_long_vr), y=as.numeric(TSVR_results$Pre_Short_vr), paired = TRUE, alternative = "less")
wilcox.test(as.numeric(TSVR_results$Pre_long_vr), as.numeric(TSVR_results$Pre_Short_vr), paired=TRUE, alternative='less')


mean(as.numeric(TSVR_results$Pre_long_vr))
mean(as.numeric(TSVR_results$Pre_Short_vr))

#short term dynamics more synchronous pre drought

###now after drought

i <- with(TSVR_results, 
          as.numeric(Post_Long_vr) - as.numeric(Post_Short_vr))
# Shapiro-Wilk normality test for the differences
shapiro.test(i) 


t.test(x=as.numeric(TSVR_results$Post_Long_vr), y=as.numeric(TSVR_results$Post_Short_vr), paired = TRUE, alternative = "less")

wilcox.test(as.numeric(TSVR_results$Post_Long_vr), as.numeric(TSVR_results$Post_Short_vr), paired=TRUE, alternative='less')

mean(as.numeric(TSVR_results$Post_Long_vr))
mean(as.numeric(TSVR_results$Post_Short_vr))

####equally synchronous at short and long scales post drought

###Now repeat, but compare pre- and post- drought for comip2, com, and vr
##cvcomip2 short timescales
j <- with(TSVR_results, 
          as.numeric(Pre_Short_cvcomip2) - as.numeric(Post_Short_cvcomip2))
# Shapiro-Wilk normality test for the differences
shapiro.test(j) 


t.test(x=as.numeric(TSVR_results$Pre_Short_cvcomip2), y=as.numeric(TSVR_results$Post_Short_cvcomip2), paired = TRUE, alternative = "less")

wilcox.test(x=as.numeric(TSVR_results$Pre_Short_cvcomip2), y=as.numeric(TSVR_results$Post_Short_cvcomip2), paired=TRUE, alternative = "less")


mean(as.numeric(TSVR_results$Pre_Short_cvcomip2))
mean(as.numeric(TSVR_results$Post_Short_cvcomip2))
##more agg comm var at short timescales pre drought


###cvcomip2 long timescales
k <- with(TSVR_results, 
          as.numeric(Pre_Long_cvcomip2) - as.numeric(Post_Long_cvcomip2))
# Shapiro-Wilk normality test for the differences
shapiro.test(k) 


t.test(x=as.numeric(TSVR_results$Pre_Long_cvcomip2), y=as.numeric(TSVR_results$Post_Long_cvcomip2), paired = TRUE, alternative = "less")

wilcox.test(x=as.numeric(TSVR_results$Pre_Long_cvcomip2), y=as.numeric(TSVR_results$Post_Long_cvcomip2), paired=TRUE, alternative = "less")


mean(as.numeric(TSVR_results$Pre_Long_cvcomip2))
mean(as.numeric(TSVR_results$Post_Long_cvcomip2))

###before is less variable than after drought   Check all interpretations
###cvcom short timescales
l <- with(TSVR_results, 
          as.numeric(Pre_Short_cvcom) - as.numeric(Post_Short_cvcom))
# Shapiro-Wilk normality test for the differences
shapiro.test(l) 


t.test(x=as.numeric(TSVR_results$Pre_Short_cvcom), y=as.numeric(TSVR_results$Post_Short_cvcom), paired = TRUE, alternative = "less")

wilcox.test(x=as.numeric(TSVR_results$Pre_Short_cvcom), y=as.numeric(TSVR_results$Post_Short_cvcom), paired=TRUE, alternative = "less")


mean(as.numeric(TSVR_results$Pre_Short_cvcom))
mean(as.numeric(TSVR_results$Post_Short_cvcom))


###cvcom long timescales
m <- with(TSVR_results, 
          as.numeric(Pre_Long_cvcom) - as.numeric(Post_Long_cvcom))
# Shapiro-Wilk normality test for the differences
shapiro.test(m) 


t.test(x=as.numeric(TSVR_results$Pre_Long_cvcom), y=as.numeric(TSVR_results$Post_Long_cvcom), paired = TRUE, alternative = "less")

wilcox.test(x=as.numeric(TSVR_results$Pre_Short_cvcom), y=as.numeric(TSVR_results$Post_Short_cvcom), paired=TRUE, alternative = "less")


mean(as.numeric(TSVR_results$Pre_Long_cvcom))
mean(as.numeric(TSVR_results$Post_Long_cvcom))

###now vr time
##vr short
n <- with(TSVR_results, 
          as.numeric(Pre_Short_vr) - as.numeric(Post_Short_vr))
# Shapiro-Wilk normality test for the differences
shapiro.test(n) 


t.test(x=as.numeric(TSVR_results$Pre_Short_vr), y=as.numeric(TSVR_results$Post_Short_vr), paired = TRUE, alternative = "greater")

wilcox.test(x=as.numeric(TSVR_results$Pre_Short_vr), y=as.numeric(TSVR_results$Post_Short_vr), paired=TRUE, alternative = "greater")


mean(as.numeric(TSVR_results$Pre_Short_vr))
mean(as.numeric(TSVR_results$Post_Short_vr))

###greater synchrony pre drought at short time scales
#vrlong
o <- with(TSVR_results, 
          as.numeric(Pre_long_vr) - as.numeric(Post_Long_vr))
# Shapiro-Wilk normality test for the differences
shapiro.test(o) 


t.test(x=as.numeric(TSVR_results$Pre_long_vr), y=as.numeric(TSVR_results$Post_Long_vr), paired = TRUE, alternative = "two.sided")

wilcox.test(x=as.numeric(TSVR_results$Pre_long_vr), y=as.numeric(TSVR_results$Post_Long_vr), paired=TRUE, alternative = "two.sided")


mean(as.numeric(TSVR_results$Pre_long_vr))
mean(as.numeric(TSVR_results$Post_Long_vr))

##long-term synchrony does not change over long timescales pre and post drought


#############looking at recruitment vs cover change over time




###Reading in veg quadrat data
library(tidyverse)
quads=read.csv('F:/NMSU/Spatiotemporal Synchrony/JQPNoannuals15_79.csv')

str(quads)

###total cover vs total abundance over time

jrn_quads_summed_total=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%filter(project_year<=1979)%>%
  mutate(spsum=sum(area, na.rm = TRUE),absum=sum(Abundance, na.rm=TRUE))%>%slice_head()%>%ungroup()

ggplot(jrn_quads_summed_total, aes(x=project_year))+geom_smooth(aes(y=scale(spsum)), fill='blue')+
  geom_smooth(aes(y=scale(absum)), fill='red')+theme_classic()


###boer cover vs abundance over time
jrn_quads_summed_BOER=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%filter(project_year<=1979&species_code=='BOER4')%>%
  mutate(spsum=sum(area, na.rm = TRUE),absum=sum(Abundance, na.rm=TRUE))%>%slice_head()%>%ungroup()

ggplot(jrn_quads_summed_BOER, aes(x=project_year))+geom_smooth(aes(y=scale(spsum)), fill='blue')+
  geom_smooth(aes(y=scale(absum)), fill='red')+theme_classic()

###grass cover vs abundance over time
jrn_quads_summed_grass=quads%>%mutate(QYear=paste(quadrat, project_year, sep = "_"))%>%
  group_by(QYear)%>%slice_max(month, with_ties = TRUE)%>%ungroup()%>%
  filter(species_code=='ARPUL'|species_code=='ARPU9'|
           species_code=='ARAD'|species_code=='ARTE3'|
           species_code=='ARIST'|species_code=='ARPA9'|
           species_code=='BOER4'|species_code=='BOGR2'|
           species_code=='BOHI2'|species_code=='BOAR'|
           species_code=='BOCU'|species_code=='BOBA2'|
           species_code=='BOUTE'|species_code=='CESP4'|
           species_code=='CHVI4'|species_code=='CYRE14'|
           species_code=='CYPER'|species_code=='DAPU7'|
           species_code=='DICA8'|species_code=='ECCO2'|
           species_code=='ENDE'|species_code=='ERLE'|
           species_code=='ERCI'|species_code=='ERPE'|
           species_code=='ERAGR'|species_code=='LYSE3'|
           species_code=='LYPH'|species_code=='MUPO2'|
           species_code=='MUAR'|species_code=='MUAR2'|
           species_code=='MUHLE'|species_code=='MUSQ3'|
           species_code=='PAHA'|species_code=='PAHI5'|
           species_code=='PANIC'|species_code=='PAOB'|
           species_code=='PACA6'|species_code=='PLMU3'|
           species_code=='SCAR'|species_code=='SCBR2'|
           species_code=='SELE6'|species_code=='SPAI'|
           species_code=='SPNE'|species_code=='SPFL2'|
           species_code=='SPCR'|species_code=='SPCO4'|
           species_code=='SPORO'|species_code=='TRBE'|
           species_code=='TRMU'|species_code=='UNKAG'|
           species_code=='UNKG'|species_code=='UNKPG')%>%group_by(QYear)%>%
  mutate(spsum=sum(area, na.rm = TRUE),absum=sum(Abundance, na.rm=TRUE))%>%slice_head()%>%ungroup()

ggplot(jrn_quads_summed_grass, aes(x=project_year))+geom_smooth(aes(y=scale(spsum)), fill='blue')+
  geom_smooth(aes(y=scale(absum)), fill='red')+theme_classic()
