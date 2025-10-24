library(here)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(viridis)
rm(list=ls()) #


###### EVALUATING THE EFFECTS OF PREDICTORS ON THE AGGREGATED ABUNDANCE AND BIOMASS - DERIVED PARAMETERS
### load models
ABU.model<-readRDS(here("data", "ABU.model.rds"))
BI.model<-readRDS(here("data", "BI.model.rds"))

###### AGREGATED ABUNDANCE
summary(ABU.model)$fixed
coef.ABU<-as.data.frame(summary(ABU.model)$fixed)
coef.ABU$variables<-row.names(coef.ABU)
names(coef.ABU)[c(3,4)]<-c("lowerCI","upperCI")
coef.ABU$color<-ifelse(coef.ABU$lowerCI>0 & coef.ABU$upperCI>0 ,"a",
                       ifelse(coef.ABU$lowerCI<0 & coef.ABU$upperCI<0, "b", "c"))
coef.ABU$variables<-c("Intercept","Cost","Elevation","Shannon")
coef.ABU$region<-"ABUNDANCE"
names(coef.ABU)[c(1)]<-"mean"
ABU.ag<-coef.ABU[-1,]%>%
  select(region,variables,mean,lowerCI,upperCI,color)

#### GLOBAL VARIABLE EFFECTS
#### load necessary data
source(file = here("Scripts","00_data_preparation.r"))
### load model
out<-readRDS(here("data", "RN_multitaxa_jaguar_prey.rds"))
#### Constructing DF of data from the output model
DF<-as.data.frame(out$BUGSoutput$summary)
#### Constructiong DF of predictors effects
var.ef<-DF[c(1,3,7)] %>% filter(grepl("mu.a0.global|mu.a1.global|mu.a2.global|mu.a3.global", row.names(DF)))%>%
  mutate(variables=c("Intercept", "Cost", "Elevation", "Shannon"))%>%
  mutate(region=rep("All",4))%>%
  relocate(region,variables,mean,"2.5%","97.5%")%>%
  mutate_at(3:5, round, 2)%>%
  print()
#### Regional variable effects
region.names<-sort(unique(all.data$region))
reg.var.ef.1<-DF[c(1,3,7)] %>% filter(grepl("a0.region|a1.region|a2.region|a3.region", row.names(DF)))%>%
  mutate_at(1:3, round, 2)%>%
  mutate(variables=c(rep("Intercept",9), 
                     rep("Cost",9), 
                     rep("Elevation",9), 
                     rep("Shannon",9)))%>%
  mutate(region=rep(region.names,4))%>%
  relocate(region,variables,mean,"2.5%","97.5%")%>%
  print()
### Ading all data to regional
reg.var.ef<-rbind(reg.var.ef.1,var.ef)
ABU.sp<-reg.var.ef%>% mutate(color=ifelse(reg.var.ef$`2.5%`>0 & reg.var.ef$`97.5%`>0 ,"a",
                                            ifelse(reg.var.ef$`2.5%`<0 & reg.var.ef$`97.5%`<0, "b", "c")))%>%
    filter(!variables %in% "Intercept")%>%
    rename(lowerCI="2.5%",
           upperCI="97.5%")%>%
  print()

#### Merge all abundance data    
ABUNDANCES<-rbind(ABU.sp,ABU.ag)

#### Data frame of the predictor effects of aggregated biomas
###### AGREGATED ABUNDANCE
summary(BI.model)$fixed
coef.BI<-as.data.frame(summary(BI.model)$fixed)
coef.BI$variables<-row.names(coef.BI)
names(coef.BI)[c(3,4)]<-c("lowerCI","upperCI")
coef.BI$color<-ifelse(coef.BI$lowerCI>0 & coef.BI$upperCI>0 ,"a",
                       ifelse(coef.BI$lowerCI<0 & coef.BI$upperCI<0, "b", "c"))
coef.BI$variables<-c("Intercept","Cost","Elevation","Shannon")
coef.BI$region<-"BIOMASS"
names(coef.BI)[c(1)]<-"mean"
ABU.ag<-coef.BI[-1,]%>%
  select(region,variables,mean,lowerCI,upperCI,color)

#### Merge all abundance data with biomass    
TOTAL<-rbind(ABUNDANCES,ABU.ag)


#### Graph
TOTAL%>%
  mutate(region = fct_relevel(region,"BIOMASS","ABUNDANCE","APA-SFX","EEB","LA","PNG","NITA", "NSV", "PETAR", "PECB", "PNI","All"),
         color = fct_relevel(color, "a","c","b"))%>%
  ggplot(aes(y=mean, x=region, ymin=lowerCI, ymax=upperCI, colour = color)) +
  geom_point()+
  geom_linerange(linewidth=0.5)+
  coord_flip() +
  xlab("") + 
  ylab("Bayesian Credible Intervals (± 95% BCI)") +
  geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
  geom_vline(xintercept = 2.5, 
             color = "black")+
  #scale_color_manual(values=c("blue","red","black"))+
  scale_colour_viridis_d(option="cividis")+
  
  facet_wrap(~variables, scales = "free",
             labeller = labeller(variables = 
                                   c("Cost" = "a) Cost",
                                     "Elevation" = "b) Elevation",
                                     "Shannon" = "c) Shannon")))+
  #facet_grid(species ~ region, scales="free")+
  #scale_color_manual(values = c(cor1,cor2,cor3))+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         strip.background =element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=10),
         axis.title = element_text(size=10, face="bold"),
         strip.text = element_text(size=12, face="bold"),
         strip.text.x = element_text(hjust = 0, vjust=0),
         #plot.margin=unit(c(0,0.25,0,0),"cm"),
         #legend.title=element_blank(),
         legend.position="none")









##### Other scripts
(g.ab<-ggplot(coef.ABU[-1,], aes(y=Estimate, x=variables, ymin=lowerCI, ymax=upperCI, colour = color)) +  
  geom_point(size=1.5)+
  geom_linerange(size=0.5)+
  coord_flip() +
  xlab("")+ 
  ylab("Regression coefficients (± 95% CI)") +
  geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
  scale_color_manual(values=c("blue","red","black"))+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=10),
         axis.text.y = element_text(size=10),
         axis.title = element_text(size=10, face = "bold"),
         plot.margin=unit(c(0,0.25,0,0),"cm"),
         plot.title = element_text(face = "bold", hjust = 0.5, size = 10))+
  guides(color = "none"))


#### GLOBAL VARIABLE EFFECTS
#### load necessary data
source(file = here("Scripts","00_data_preparation.r"))
### load model
out<-readRDS(here("data", "RN_multitaxa_jaguar_prey.rds"))
#### Constructing DF of data from the output model
DF<-as.data.frame(out$BUGSoutput$summary)
#### Constructiong DF of predictors effects
var.ef<-DF[c(1,3,7)] %>% filter(grepl("mu.a0.global|mu.a1.global|mu.a2.global|mu.a3.global", row.names(DF)))%>%
  mutate(variables=c("Intercept", "Cost", "Elevation", "Shannon"))%>%
  mutate(region=rep("ALL",4))%>%
  relocate(region,variables,mean,"2.5%","97.5%")%>%
  mutate_at(3:5, round, 2)%>%
  print()
#### Regional variable effects
region.names<-sort(unique(all.data$region))
reg.var.ef.1<-DF[c(1,3,7)] %>% filter(grepl("a0.region|a1.region|a2.region|a3.region", row.names(DF)))%>%
  mutate_at(1:3, round, 2)%>%
  mutate(variables=c(rep("Intercept",9), 
                     rep("Cost",9), 
                     rep("Elevation",9), 
                     rep("Shannon",9)))%>%
  mutate(region=rep(region.names,4))%>%
  relocate(region,variables,mean,"2.5%","97.5%")%>%
  print()
### Ading all data to regional
reg.var.ef<-rbind(reg.var.ef.1,var.ef)

#Graphs
names(reg.var.ef)
(g.ab.sp<-reg.var.ef%>% mutate(color=ifelse(reg.var.ef$`2.5%`>0 & reg.var.ef$`97.5%`>0 ,"a",
                                  ifelse(reg.var.ef$`2.5%`<0 & reg.var.ef$`97.5%`<0, "b", "c")))%>%
filter(!variables %in% "Intercept")%>%
  rename(lowerCI="2.5%",
         upperCI="97.5%")%>% 
  #ggplot(aes(y=mean, x=variables, ymin=lowerCI, ymax=upperCI)) +  
  #geom_linerange(size=1)+
  ggplot(aes(y=mean, x=region, ymin=lowerCI, ymax=upperCI, colour = color)) +
  geom_point()+
  geom_linerange(linewidth=0.5)+
  coord_flip() +
  xlab("") + 
  ylab("") +
  geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
  scale_color_manual(values=c("blue","red","black"))+
  facet_wrap(~variables, scales = "free")+
  #facet_grid(species ~ region, scales="free")+
  #scale_color_manual(values = c(cor1,cor2,cor3))+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=10),
         strip.text = element_text(size=10),
         plot.margin=unit(c(0,0.25,0,0),"cm"),
         legend.title=element_blank(),
         legend.position="none"))


####### CREATING THE FIGUE 3 - INCLUDING THESE FOUR PLOTS
ggarrange(g.ab.sp,g.ab, nrow = 1, labels = c("a)","b)"))



#### Regional variable effects over individual specieshttp://127.0.0.1:42255/graphics/plot_zoom_png?width=780&height=438
species.names<-colnames(all.data[3:16])
reg.var.sp.ef<-DF[c(1,3,7)] %>% filter(grepl("a0|a1|a2|a3", row.names(DF)),http://127.0.0.1:42255/graphics/plot_zoom_png?width=887&height=334
                                       !grepl("a0.region|a1.region|a2.region|a3.region",row.names(DF)),
                                       !grepl("mu.a0.global|mu.a1.global|mu.a2.global|mu.a3.global",row.names(DF)))%>%
  mutate_at(1:3, round, 2)%>%
  mutate(variables=c(rep("Intercept",9*14),
                     rep("AccessCost",9*14), 
                     rep("Elevation",9*14), 
                     rep("EVIdiv_shan",9*14)))%>%
  mutate(region=rep(rep(region.names,each=14),4))%>%
  mutate(species=rep(species.names,9*4))%>%
  relocate(region,species,variables,mean,"2.5%","97.5%")%>%
  rename(lowerCI="2.5%",
         upperCI="97.5%")%>%                      
  print()
library(ggplot2)  
head(reg.var.sp.ef)
tail(reg.var.sp.ef)
names(reg.var.sp.ef)
reg.var.sp.ef%>% filter(!variables %in% "Intercept")%>%
  #ggplot(aes(y=mean, x=variables, ymin=lowerCI, ymax=upperCI)) +  
  #geom_linerange(size=1)+
  ggplot(aes(y=mean, x=region, ymin=lowerCI, ymax=upperCI, 
             colour = species, fill=species)) +  
  geom_linerange(size=1, position = position_dodge(width =1))+
  coord_flip() +
  xlab("") + 
  ylab("") +
  geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
  facet_wrap(~variables, scales = "free")+
  #facet_grid(species ~ region, scales="free")+
  #scale_color_manual(values = c(cor1,cor2,cor3))+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=10),
         plot.margin=unit(c(0,0.25,0,0),"cm"),
         legend.title=element_blank(),
         legend.position="top")



reg.var.sp.ef%>% filter(!variables %in% "Intercept")%>%
  #ggplot(aes(y=mean, x=variables, ymin=lowerCI, ymax=upperCI)) +  
  #geom_linerange(size=1)+
  ggplot(aes(y=mean, x=species, ymin=lowerCI, ymax=upperCI, 
             colour = region, fill=region)) +  
  geom_linerange(size=1, position = position_dodge(width =1))+
  coord_flip() +
  xlab("") + 
  ylab("") +
  geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
  facet_wrap(~variables, scales = "free")+
  #facet_grid(species ~ region, scales="free")+
  #scale_color_manual(values = c(cor1,cor2,cor3))+
  theme (panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=8),
         plot.margin=unit(c(0,0.25,0,0),"cm"),
         legend.title=element_blank(),
         legend.position="top")



##### BIOMASS
### Coefs
summary(BI.model)$fixed
coef.BI<-as.data.frame(summary(BI.model)$fixed)
coef.BI$variables<-row.names(coef.BI)
names(coef.BI)[c(3,4)]<-c("lowerCI","upperCI")
coef.BI$color<-ifelse(coef.BI$lowerCI>0 & coef.BI$upperCI>0 ,"a",
                      ifelse(coef.BI$lowerCI<0 & coef.BI$upperCI<0, "b", "c"))
coef.BI$variables<-c("Intercept","Cost","Elevation","Shannon")

(g.bi<-ggplot(coef.BI[-1,], aes(y=Estimate, x=variables, ymin=lowerCI, ymax=upperCI, colour = color)) +  
    geom_point(size=1.5)+
    geom_linerange(size=0.5)+
    coord_flip() +
    xlab("")+ 
    ylab("Regression coefficients (± 95% CI)") +
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values=c("blue","black"))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(size=10),
           axis.text.y = element_text(size=10),
           axis.title = element_text(size=10, face = "bold"),
           plot.margin=unit(c(0,0.25,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 10))+
    guides(color = "none"))


