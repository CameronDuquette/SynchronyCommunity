library(vegan)
community_ord=read.csv("F:/NMSU/Spatiotemporal Synchrony/widecom.csv")

community_ord[is.na(community_ord)] <- 0
community_ord$decade=substr(community_ord$project_decade,nchar(community_ord$project_decade)-3, nchar(community_ord$project_decade))

ord_res=metaMDS(community_ord[,2:(ncol(community_ord)-1)], k=2, distance="bray", trymax = 10000)
ord_res2=metaMDS(community_ord[,2:(ncol(community_ord)-1)], k=2, distance="bray", trymax = 10000, previous.best = ord_res)

####let's plot richness over time
community_ord=read.csv("F:/NMSU/Spatiotemporal Synchrony/widecom.csv")

community_ord[is.na(community_ord)] <- 0
community_ord$decade=substr(community_ord$project_decade,nchar(community_ord$project_decade)-3, nchar(community_ord$project_decade))

community_ord$richness