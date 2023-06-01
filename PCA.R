library(data.table)

rils<-as.data.table(read.csv("rils_pheno_alltrts_AGI.csv"))
rils$X<-NULL
rils$ID<-NULL
rils$ID<-"Ril"
parents<-as.data.table(read.csv("parents_pheno_alltrts_AGI.csv"))
parents$X<-NULL
parents$rep<-NULL
colnames(parents)[1]<-"ID"
parentsavg<-parents[,lapply(.SD,mean),by = c("ID","treatment")]

dt<-rbind(parentsavg,rils[treatment != "delta"])
#for Salicylic treatment group
sa<-dt[treatment == 'SW',-c('treatment'),with=FALSE]
id<-sa[,1]
sa_pca_scaled<-prcomp(sa[,-1],scale=T)
plot(sa_pca_scaled, type = "l")
biplot(sa_pca_scaled)

pcs<-data.frame(sa_pca_scaled$x)
str(pcs)
pcs$ID<-sa$ID
ggplot(pcs,aes(x=PC1,y=PC2,col=ID))+
  geom_point()+
  theme_bw()+
  #geom_polygon(stat = "ellipse",aes(fill=ID), alpha = 0.3)+
  ggtitle("Silwet")
