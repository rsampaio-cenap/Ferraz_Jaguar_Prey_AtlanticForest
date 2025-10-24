library(here)
library(tidyverse)
library(rjags)
library(brms)



#### load necessary data
covs<-read.csv2(here("data","covs.csv"))
### load model
out<-readRDS(here("data", "RN_multitaxa_jaguar_prey.rds"))

#### Constructing DF of data from the output model
DF<-as.data.frame(out$BUGSoutput$summary)

##### Creating the data.frame with the aggregated ABUNDANCE BY SITE AND REGIONS
ABU<-DF[c(1,3,7)] %>% filter(grepl("AB", row.names(DF)))%>%
  mutate_at(1:3, round, 2)%>%
  mutate(Ponto=c(seq(1:52),
                 seq(1:59),
                 seq(1:60),
                 seq(1:51),
                 seq(1:60),
                 seq(1:60),
                 seq(1:55),
                 seq(1:40),
                 seq(1:59)))%>%
  mutate(region=c(rep("APA-SFX",52),
                  rep("EEB",59),
                  rep("LA",60),
                  rep("NITA", 51),
                  rep("NSV", 60),
                  rep("PECB", 60),
                  rep("PETAR", 55),
                  rep("PNG", 40),
                  rep("PNI", 59)))%>%
  mutate(region = fct_relevel(region, "APA-SFX","EEB","LA","PNG","NITA", "NSV", "PETAR", "PECB", "PNI"))%>%
  mutate(species=c(rep("AB", 496)))%>%
  mutate(deployment=paste(region,Ponto,sep="_"))%>%
  relocate(deployment,region,species,mean,"2.5%","97.5%")%>%
  rename(lowerCI="2.5%",
         upperCI="97.5%")%>%                      
  print()

### add the predictors
ABU.1<-merge(ABU,covs[-1],by="deployment")

###GLMM
##### ABUNDANCE
ABU.model<-brm(mean ~ AccessCost + Elevation + EVIdiv_shan+ (1|region), 
               data=ABU.1,
               chains = 4, # nb of chains
               iter = 100000, # nb of iterations, including burnin
               warmup = 50000, # burnin
               thin = 100)

# Save results  
saveRDS(ABU.model, here("data", "ABU.model.rds"))


##### Creating the data.frame with the aggregated BIOMASS BY SITE AND REGIONS
BI<-DF[c(1,3,7)] %>% filter(grepl("BI", row.names(DF)))%>%
  .[1:496,]%>% ### Selecting only the aggregated biomass
  mutate_at(1:3, round, 2)%>%
  mutate(Ponto=c(seq(1:52),
                 seq(1:59),
                 seq(1:60),
                 seq(1:51),
                 seq(1:60),
                 seq(1:60),
                 seq(1:55),
                 seq(1:40),
                 seq(1:59)))%>%
  mutate(region=c(rep("APA-SFX",52),
                  rep("EEB",59),
                  rep("LA",60),
                  rep("NITA", 51),
                  rep("NSV", 60),
                  rep("PECB", 60),
                  rep("PETAR", 55),
                  rep("PNG", 40),
                  rep("PNI", 59)))%>%
  mutate(region = fct_relevel(region, "APA-SFX","EEB","LA","PNG","NITA", "NSV", "PETAR", "PECB", "PNI"))%>%
  mutate(species=c(rep("BI", 496)))%>%
  mutate(deployment=paste(region,Ponto,sep="_"))%>%
  relocate(deployment,region,species,mean,"2.5%","97.5%")%>%
  rename(lowerCI="2.5%",
         upperCI="97.5%")                
  
BI.1<-merge(BI,cov[-1],by="deployment")
names(BI.1)
##### SR
BI.model<-brm(log(mean) ~ AccessCost + Elevation + EVIdiv_shan+ (1|region), 
              data=BI.1,
              chains = 4, # nb of chains
              iter = 100000, # nb of iterations, including burnin
              warmup = 50000, # burnin
              thin = 100)
# Save results  
saveRDS(BI.model, here("data", "BI.model.rds"))

