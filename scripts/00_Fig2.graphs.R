library(here)
library(tidyverse)
library(rjags) 
library(R2jags)
library(dplyr)
library(ggpubr)
library(viridis)
library(ggbreak)


#### Load necessary data
covs<-read.csv2(here("data","covs.csv"))
DF<-read.csv2(here("data","DF.MR_MSOM_RN_results.csv"))
rownames(DF)<-DF$X
#### EXPLORING VARIATION IN THE SPECIES ABUNDANCE
species.names<- c("Cabass","Cunpac","Dasypu", "Mazama", "Nasnas", "Taypec", "Tapter", "Tamtet", "Didelp", "Dictaj", "Procan", "Dasypr", "Certho", "Mirtri")
region.names<-c("APA-SFX","EEB","LA","NITA","NSV","PECB","PETAR","PNG","PNI")

#### CREATING GRAPHS FOR FIGURE 2 - THE AGGREGATED ABUNDANCE BY REGIONS
##### EXPLORING THE AGGREGATED VALUES OF ABUNDANCE BY SITE AND REGIONS
ABU<-DF[c(2,4,8)] %>% filter(grepl("AB", row.names(DF)))%>%
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
  mutate(region = fct_relevel(region,"PNI","PECB","PNG","NSV","PETAR","NITA","LA","EEB","APA-SFX"))%>%
  mutate(species=c(rep("AB", 496)))%>%
  mutate(deployment=paste(region,Ponto,sep="_"))%>%
  relocate(deployment,region,species,mean,"X2.5.","X97.5.")%>%
  rename(lowerCI="X2.5.",
         upperCI="X97.5.")%>%                      
  print()

###### FIGURE 2A
(ABU.g<-ABU%>% 
    ggplot(aes(y=mean, x=region, fill = region, colour = region))+
    geom_boxplot(alpha = 0.4)+
    geom_jitter(width = 0.05,size=1)+
    xlab("") + 
    ylab("Aggregated abundance") +
    ylim(0,41)+
    scale_fill_viridis_d(option="cividis")+
    scale_colour_viridis_d(option="cividis")+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(size=10, angle = 90, face="bold"),
           axis.text.y = element_text(size=10),
           axis.title.y = element_text(size = 10),
           legend.position = "none",
           legend.title=element_blank(),
           plot.margin = margin(.1, .1, -0.4, .4, "cm")))

#### FIGURE 2B
##### GRAPH THE AGGREGATED BIOMASS - DATA WERE FILTERED UP TO 400 kg, TO IMPROVE VISUALIZATION OF THE MOST CONSUMED SPECIES
(BIO<-DF[c(2,4,8)] %>% filter(grepl("BI", row.names(DF)))%>%
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
    mutate(region = fct_relevel(region,"PNI","PECB","PNG","NSV","PETAR","NITA","LA","EEB","APA-SFX"))%>%
    mutate(species=c(rep("BI", 496)))%>%
    mutate(deployment=paste(region,Ponto,sep="_"))%>%
    relocate(deployment,region,species,mean,"X2.5.","X97.5.")%>%
    rename(lowerCI="X2.5.",
           upperCI="X97.5.")%>%                  
    ggplot(aes(y=mean, x=region, fill = region, colour = region))+
    geom_boxplot(alpha = 0.4)+
    geom_jitter(width = 0.05,size=1)+
    xlab("") + 
    ylab("Aggregated biomass") +
    scale_fill_viridis_d(option="cividis")+
    scale_colour_viridis_d(option="cividis")+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(size=10, angle = 90, face="bold"),
           axis.text.y = element_text(size=10),
           axis.title.y = element_text(size = 10),
           legend.position = "none",
           legend.title=element_blank(),
           plot.margin = margin(.1, .1, -0.4, .1, "cm")))

######### Figure 2
ggarrange(ABU.g,         BIO,
          nrow = 2,
          labels = c("a)","b)"), vjust=1, hjust=0.05)


