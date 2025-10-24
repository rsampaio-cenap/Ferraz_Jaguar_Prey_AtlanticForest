library(here)
library(tidyverse)
library(rjags) 
library(dplyr)
library(ggpubr)
library(viridis)
library(ggbreak)

rm(list = ls())


#### Load necessary data
covs<-read.csv2(here("data","covs.csv"))
DF<-read.csv2(here("data","DF.MR_MSOM_RN_results.csv"))
rownames(DF)<-DF$X
species.names<- c("Cabass","Cunpac","Dasypu", "Mazama", "Nasnas", "Taypec", "Tapter", "Tamtet", "Didelp", "Dictaj", "Procan", "Dasypr", "Certho", "Myrtri")

#### CREATING A DATAFRAME INCLUDING THE ESTIMATED ABUNDANCE VALUES FOR INDIVIDUAL SPECIES
sp.abundance<-DF[c(2,4,8)] %>% filter(grepl("Z", row.names(DF)))%>%
  #mutate_at(1:3, round, 2)%>%
  mutate(Ponto=c(rep(seq(1:52),14),
                 rep(seq(1:59),14),
                 rep(seq(1:60),14),
                 rep(seq(1:51),14),
                 rep(seq(1:60),14),
                 rep(seq(1:60),14),
                 rep(seq(1:55),14),
                 rep(seq(1:40),14),
                 rep(seq(1:59),14)))%>%
  mutate(region=c(rep("APA-SFX",52*14),
                  rep("EEB",59*14),
                  rep("LA",60*14),
                  rep("NITA", 51*14),
                  rep("NSV", 60*14),
                  rep("PECB", 60*14),
                  rep("PETAR", 55*14),
                  rep("PNG", 40*14),
                  rep("PNI", 59*14)))%>%
  mutate(region = fct_relevel(region,"PNI","PECB","PNG","NSV","PETAR","NITA","LA","EEB","APA-SFX"))%>%
  mutate(species=c(rep(species.names, each=52),
                   rep(species.names, each=59),
                   rep(species.names, each=60),
                   rep(species.names, each=51),
                   rep(species.names, each=60),
                   rep(species.names, each=60),
                   rep(species.names, each=55),
                   rep(species.names, each=40),
                   rep(species.names, each=59)))%>%
  mutate(species = fct_relevel(species, "Cabass","Certho","Tapter","Didelp","Dasypr","Cunpac",
                               "Procan","Myrtri","Tamtet","Dasypu","Mazama","Nasnas","Dictaj","Taypec"))%>%
  mutate(deployment=paste(region,Ponto,sep="_"))%>%
  mutate(diet=ifelse(species %in% c("Dasypu","Mazama","Nasnas","Dictaj","Taypec"),"44%",
                     ifelse(species %in% c("Dasypr","Cunpac","Procan","Myrtri","Tamtet"),"15%","3%")))%>%
  relocate(deployment,region,species,mean,"X2.5.","X97.5.")%>%
  rename(lowerCI="X2.5.",
         upperCI="X97.5.")%>%                      
  print()

##### INDIVIDUAL SPECIES ABUNDANCES VALUES BY REGIONS GRAPH
# CREATING A DATA.FRAMA INCLUING THE MEAN SIZE OF FOCAL SPECIES
species.names
size <- c(6,9.5,6,24,5.1,32,160,4.5,1.09,25,5.4,4.5,6.5,30.5)
names<- as.data.frame(cbind(species.names,size))
names(names)[1]<-"species"
names$size<-as.numeric(names$size)

###### Plot Figure 3A
(sp.abundance.g<-sp.abundance%>% 
    left_join(names, by="species")%>%
    #mutate(species = fct_reorder(species, size)) %>%
    ggplot(aes(y=mean, x=fct_reorder(species, size), fill=fct_reorder(species, size), colour = fct_reorder(species, size)))+ 
    geom_boxplot(alpha=0.4)+
    coord_flip()+
    xlab("") + 
    ylab("Relative abundances") +
    facet_wrap(~region, ncol=9)+
    scale_colour_viridis_d(option="cividis",direction = -1)+
    scale_fill_viridis_d(option="cividis",direction = -1)+
    #facet_grid(species ~ region, scales="free")+
    #scale_color_manual(values = c(cor1,cor2,cor3))+
    theme (#strip.text = element_blank(),
      strip.text = element_text(size=9, face ="bold"),
      strip.background = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size=8, angle = 90),
      axis.text.y = element_text(size=9),
      legend.position="none",
      plot.margin = margin(0, .4, .1, 0, "cm")))

###### GETTING MEAN ABUNDANCE VALUE BY SPECIES BY REGION
names(sp.abundance)
abunda_regio_sp<-sp.abundance%>%
  group_by(region, species)%>%
  summarise(mean_value = mean(mean),
            sd_value = sd(mean),
            count_obs = n())
write.csv(abunda_regio_sp,here("data", "abu.region.sp.csv"))

####### EXPLORING THE BIOMASS OF INDIVIDUAL SPECIES - ABUNDANCE * MEAN SIZE
# CREATING A DATA.FRAMA INCLUING THE MEAN SIZE OF FOCAL SPECIES
size <- c(6,9.5,6,24,5.1,32,160,4.5,1.09,25,5.4,4.5,6.5,30.5)
biomass<- as.data.frame(cbind(species.names,size))
biomass$size<-as.numeric(biomass$size)
names(biomass)[1]<-"species"

#### GRAPH THE BIOMASS OF INDIVIDUAL SPECIES BY REGION
sp.biomass.g<-sp.abundance%>%
  left_join(biomass, by="species")%>%
  mutate(biomass=mean*size)

###### Figure 3B
(sp.biomass.g<-sp.abundance%>%
    left_join(biomass, by="species")%>%
    mutate(biomass=mean*size)%>%
    select(deployment,region,species,size,diet,biomass)%>% 
    ggplot(aes(y=biomass+1, x=fct_reorder(species, size), fill=fct_reorder(species, size), colour = fct_reorder(species, size)))+ 
    geom_boxplot(alpha=0.4)+
    coord_flip()+
    xlab("Species") + 
    ylab("Biomass") +
    scale_colour_viridis_d(option="cividis",direction = -1)+
    scale_fill_viridis_d(option="cividis",direction = -1)+
    scale_y_log10()+
    facet_wrap(~region, ncol=9)+
    theme (strip.text = element_blank(),
           panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(size=8, angle = 90),
           axis.text.y = element_text(size=9),
           plot.margin = margin(0, .4, .1, .1, "cm"),
           legend.position="none"))


######### Figure 3
ggarrange(sp.abundance.g, sp.biomass.g, 
          nrow=2,
          labels = c("a)","b)"), vjust=1, hjust=0.05)
