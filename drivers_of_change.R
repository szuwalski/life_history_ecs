#=======================================================================
# reltionship between avg sst, F, and parameters
#=======================================================================
#==RUN life_history_plot.R first==
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(mgcv)
library(reshape2)
library(png)
data <- read_xlsx("adj_dat.xlsx")
sst_lme<-read.csv("sst_lme.csv")
#==count the number of entries for a given species   
count_entries<-function(species,parameter)
{
  temp<-data %>% filter(Species == species & Parameter_name == parameter)
  return(nrow(temp))
}

spec_name<-unique(data$Species)
length(unique(data$Reference))

counted<-spec_name
for(x in 1:length(spec_name))
  counted[x]<-count_entries(species=spec_name[x],parameter="Linf")

#==plot species with more entries than X (here 3)
use_spec<-spec_name[which(as.numeric(counted)>3)]
ggplot(data=sst_lme)+
  geom_line(aes(x=year,y=mean_SST,group=LME,col=LME))+theme_bw()

#=================================
#==make data set for regression
#================================
sm_dat<-data[,c(1,3,5,8,10,13)]
#==remove unused parameters
par_names<-unique(sm_dat$Parameter_name)
sm_dat<-sm_dat[!is.na(match(sm_dat$Parameter_name,par_names[c(1,2,4)])),]
#==remove species with too few obs
sm_dat<-sm_dat[!is.na(match(sm_dat$Species,use_spec[c(2,3,4,5)])),]

#==temps only go to 1960
sm_dat$Begin_study_date[sm_dat$Begin_study_date<1960]<-1960

#==remove those that do not have LME
sm_dat<-sm_dat[-which(sm_dat$Large_marine_ecosystem=="NA"),]
sm_dat<-sm_dat[-which(is.na(match(sm_dat$Large_marine_ecosystem,c("East China Sea","Yellow Sea")))),]

#==add fishing mortality
sm_dat$fishing_mortality<-NA
for(x in 1:nrow(sm_dat))
  sm_dat$fishing_mortality[x]<-save_F[match(sm_dat$Species[x],rownames(save_F)),match(sm_dat$Begin_study_date[x],colnames(save_F))]

#==add sst
sm_dat$sst<-NA
for(x in 1:nrow(sm_dat))
  sm_dat$sst[x]<-sst_lme$mean_SST[which(sm_dat$Large_marine_ecosystem[x]==sst_lme$LME & sm_dat$Begin_study_date[x]==sst_lme$year)]






#==GAM with year and AR process
#==GAM with SST and fishing mortality interaction, all species
#==THIS DONT WORK GREAT. SMALL SAMPLE SIZES
library(mgcv)
library(visreg)
library(grid)
library(gridExtra)
sm_spec<-unique(sm_dat$Species)
sm_par <-unique(sm_dat$Parameter_name)
#pdf('gam_output.pdf')
for(x in 1: 3)

for(y in 1:length(sm_par))
{
 temp<- sm_dat %>% filter(Species == sm_spec[x] & Parameter_name == sm_par[y] & Large_marine_ecosystem=="East China Sea")  
 # if(x<3)
 #  mod<-gam(Value~s(sst,k=3)+s(fishing_mortality,k=3)+as.factor(Large_marine_ecosystem),data=temp) 
 # if(x==3)
   mod<-gam(Value~s(sst,k=3)+s(fishing_mortality,k=3),data=temp) 
 plot(mod,pages=1,residuals=TRUE,cex=2)
 summary(mod)
 AIC(mod)

 null_mod<-gam(Value~1,data=temp)
 summary(null_mod)
 AIC(null_mod)
 
 if(y==1)
 {
   sst1<-visreg(mod,"sst",  gg = TRUE, plot = TRUE) +
   theme_bw() 
   eff1<-visreg(mod,"fishing_mortality",  gg = TRUE, plot = TRUE) +
     theme_bw() 
 }
 if(y==2)
 {
   sst2<-visreg(mod,"sst",  gg = TRUE, plot = TRUE) +
     theme_bw() 
   eff2<-visreg(mod,"fishing_mortality",  gg = TRUE, plot = TRUE) +
     theme_bw() 
 }
 if(y==3)
 {
   sst3<-visreg(mod,"sst",  gg = TRUE, plot = TRUE) +
     theme_bw() 
   eff3<-visreg(mod,"fishing_mortality",  gg = TRUE, plot = TRUE) +
     theme_bw() 
 }
 }
library(patchwork)
(sst1|eff1)/(sst2|eff2)/(sst3|eff3)

#dev.off()


sm_spec<-unique(sm_dat$Species)
sm_par<-unique(sm_dat$Parameter_name)
pdf('gam_output.pdf')
for(x in 1: 3)
  for(y in 1:length(sm_par))
  {
    temp<- sm_dat %>% filter(Species == sm_spec[x] & Parameter_name == sm_par[y])  
    mod<-gam(Value~s(sst,fishing_mortality,k=10),data=temp) 
    plot(mod,too.far=2,scheme=2,cex=3)
    mtext(side=3,c(sm_spec[x]," ",sm_par[y]))
    print(c(sm_spec[x]," ",sm_par[y]))
    print(summary(mod))
    
  }
dev.off()


#==plot a figure that shows the trajectory of the parameter if there was constant fishing
#==constant temempearture to show the relative effects of the changes in temperature and fishing for each
#==how does one interpret this?



#==scale parametesr within species and parameter type
unq_species<-unique(sm_dat$Species)
unq_pars<-unique(sm_dat$Parameter_name)
for(x in 1:length(unq_species))
  for(y in 1:length(unq_pars))
  {
    sm_dat[ sm_dat$Species==unq_species[x] &  sm_dat$Species==unq_species[x] ]  
  }

sm_dat[sm_dat$Species==unq_species[7],]
sst_in<-rep(NA,nrow(sm_dat))

data[data$Species==unq_species[7],]

for(x in 1:length(sst_in))
  try(sst_in[x]<-in_sst$mean_SST[!is.na(match(in_sst$year,sm_dat$Begin_study_date[x]))],silent=TRUE)

sm_dat$sst<-sst_in

#==scale all variables within species and within parameter type



gam(Value~s(sst)+s(fmort) + Parameter_name+Large_marine_ecosystem)
