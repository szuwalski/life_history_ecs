
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(mgcv)
library(reshape2)
library(png)
data <- read_xlsx("adj_dat.xlsx")

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
unq_par<-unique(data$Parameter_name)
unq_par<-unq_par[-c(5,8,11,12)]
unq_par[6]<-"M"
unq_par[4]<-"a"
unq_par[5]<-'b'

use_unq_par<-c(1,2,6,8)

#use_LME<-unique(data$Large_marine_ecosystem)
use_LME<-c("East China Sea","Yellow Sea","South China Sea","Bohai")
use_LME<-c("East China Sea","Yellow Sea")


unq_par_names<-c(expression(italic("L"[infinity])),
                 expression(italic("k")),
                 expression("t"[0]),
                 expression(italic(alpha)),
                 expression(italic(beta)),
                 "M","Z","F")

styr<-1960
endyr<-2014

library(RColorBrewer)
incol<-brewer.pal(5,"Set1")
for(x in 1:length(incol))
  incol[x]<-paste(incol[x],77,sep="")

png("figures/figure_2.png",res=1200,units='in',width=8,height=6)
par(mfcol=c(5,4),mar=c(.2,.1,.21,.1),oma=c(4,6,2,1))

#==storage
dev_expl<-matrix(NA,5,5)
p_val<-matrix(NA,5,5)
save_F<-matrix(ncol=length(seq(styr,endyr)),nrow=5)
rownames(save_F)<-use_spec[2:6]
colnames(save_F)<-seq(styr,endyr)
ylim_in<-c(800,0.9,1,2.5,22)
saved_stuff<-list(list())

weight_at_length_a<-matrix(ncol=4,nrow=length(seq(styr,endyr)))
weight_at_length_b<-matrix(ncol=4,nrow=length(seq(styr,endyr)))
vonbert_t0<-matrix(ncol=4,nrow=length(seq(styr,endyr)))
for(x in c(2:5))
{
  temp_list<-list()
  temp<-data[data$Species==use_spec[x] & !is.na(match(data$Large_marine_ecosystem,use_LME)),]
  counter<-1
  for(y in use_unq_par)
  {
    temp2<-temp[temp$Parameter_name==unq_par[y],]
    temp2_baselme<-unique(temp2$Large_marine_ecosystem)[1]
    if(nrow(temp2)>0)
    {
      ymin<-0
      if(min(as.numeric(temp2$Value),na.rm=T)<0)
        ymin<-min(as.numeric(temp2$Value),na.rm=T)
      obs_n<-sum(!is.na(as.numeric(temp2$Value)))
      if(obs_n>3)
      {
        temp2$Value <- as.numeric(temp2$Value)
        temp2$End_study_date <- as.numeric(temp2$End_study_date)
        if(length(temp2_baselme)>1)
        {
          mod<-gam(Value~s(End_study_date,k=3)+Large_marine_ecosystem,data=temp2)
          tempnew <- list(End_study_date=seq(from=styr, to = endyr),Large_marine_ecosystem=rep(use_LME[1],length(seq(styr,endyr))))
          preds<- predict(mod,newdata=tempnew,se=TRUE)
        } else
        {
          mod<-gam(Value~s(End_study_date,k=3),data=temp2)
          tempnew <- list(End_study_date=seq(from=styr, to = endyr))
          preds<- predict(mod,newdata=tempnew,se=TRUE)
        }
        temp_list[[y]]<-preds$fit
        dev_expl[counter,x-1]<-summary(mod)$dev.expl
        p_val[counter,x-1]<-summary(mod)$s.pv
        if(y== 8)
          save_F[x-1,]<-preds$fit      
        #in_ylim<-c(0,max(preds$fit+(2*preds$se.fit)))
        
        in_ylim<-c(0,ylim_in[counter])
        plot(0,type='n',xlim=c(styr,endyr),xaxt='n',xlab=NA,
             ylab=NA,ylim=in_ylim,bty='n',las=1,yaxt='n')
        if(x==2)
          axis(side=2,las=1)
        polygon(x=c(seq(styr,endyr),rev(seq(styr,endyr))),
                y=c(preds$fit+(2*preds$se.fit),rev(preds$fit-(2*preds$se.fit))),
                col=incol[x-1],border=F)
        lines(x=styr:endyr, preds$fit,type="l",lwd=2)
        points(y=temp2$Value, x=temp2$End_study_date, cex=1.2, pch=19,col='dark grey')
        
      }else
      {
        
        if(y==length(unq_par))
        {
          plot(-1000,yaxt='n',xaxt='n',xlim=c(styr,endyr),bty='n')
        } else
          plot.new() 
      }     
      if(nrow(temp2)<1)
      {
        if(y==length(unq_par))
        {
          plot(-1000,yaxt='n',xaxt='n',xlim=c(styr,endyr),bty='n')
        } else
          plot.new() 
      }
      if(y==1)
        mtext(side=3,use_spec[x],cex=.7,line=.25)
      if(x==2)
        mtext(side=2,unq_par_names[y],las=1,line=3)
    }
    if(x==6 &  y==8)
      plot.new()
    counter<-counter+1
  }
  
  #=======================================
  # Plot weight at Linf
  #+=====================================
  tempnew <- list(End_study_date=seq(from=styr, to = endyr))
  
  temp_a<-temp[temp$Parameter_name=="a" & temp$sex=="both" & temp$Life_history_process=="weight at length",]
  temp_a$Value <- as.numeric(temp_a$Value)
  temp_a$End_study_date <- as.numeric(temp_a$End_study_date)

  temp_b<-temp[temp$Parameter_name=="b"& temp$sex=="both" & temp$Life_history_process=="weight at length",]
  temp_b$Value <- as.numeric(temp_b$Value)
  temp_b$End_study_date <- as.numeric(temp_b$End_study_date)
 
  temp_t0<-temp[temp$Parameter_name=="t0"& temp$sex=="both",]
  temp_t0$Value <- as.numeric(temp_t0$Value)
  temp_t0$End_study_date <- as.numeric(temp_t0$End_study_date)
  
  
  #==fit to alpha and beta parameters for YPR and save
  tempnew <- list(End_study_date=seq(from=styr, to = endyr))
  
  use_temp_a<-filter(temp_a,Value<0.001)
  mod_a<-gam(data=use_temp_a,Value~s(End_study_date,k=3))
  summary(mod_a)
  #plot(mod_a,residuals=TRUE,cex=2)
  
  use_temp_b<-filter(temp_a,Value<0.001)
  mod_b<-gam(data=use_temp_b,Value~s(End_study_date,k=3))
  summary(mod_b)
  #plot(mod_b)
  
  mod_t0<-gam(data=temp_t0,Value~s(End_study_date,k=3))
  summary(mod_t0)
  #plot(mod_t0)

  preds_a<- predict(mod_a,newdata=tempnew,se=TRUE)
  preds_b<- predict(mod_b,newdata=tempnew,se=TRUE)
  preds_t0<- predict(mod_t0,newdata=tempnew,se=TRUE)
  
  weight_at_length_a[,x-1]<-preds_a$fit
  weight_at_length_b[,x-1]<-preds_b$fit 
  vonbert_t0[,x-1]<-preds_t0$fit 
  
  wt40<-temp_a$Value*40^temp_b$Value
  
  #==this is a somewhat distasteful way of accounting for differences
  #==in reporting by study: those with changes by a factor of 10 were mm vs. cm
  #==thos with a change by factor of 1000 were grams vs. kilograms
  if(x==2)
    wt40[wt40>.2]<-wt40[wt40>.2]*10  
  if(x==3)
    wt40[wt40>.2]<-wt40[wt40>.2]*10 
  if(x==4)
    wt40[wt40>10]<-wt40[wt40>10]/1000
  if(x==5)
    wt40[wt40>5]<-wt40[wt40>5]/10  
  if(x==6)
    wt40[wt40>5]<-wt40[wt40>5]/1000  
  
  indates<-temp_a$End_study_date
  mod<-gam(wt40~s(indates,k=3))
  dev_expl[counter,x-1]<-summary(mod)$dev.expl
  p_val[counter,x-1]<-summary(mod)$s.pv
  tempnew <- list(indates=seq(from=styr, to = endyr))
  preds<- predict(mod,newdata=tempnew,se=TRUE)
  inmin<-max(0,min(preds$fit-(2*preds$se.fit)))
  inmax<-max(preds$fit+(2*preds$se.fit))
  plot(0,type='n',xlim=c(styr,endyr),xaxt='n',xlab=NA,
       ylab=NA,ylim=c(inmin,inmax),bty='n',las=1,yaxt='n')
  if(x==2)
    axis(side=2,las=1)
  polygon(x=c(seq(styr,endyr),rev(seq(styr,endyr))),
          y=c(preds$fit+(2*preds$se.fit),rev(preds$fit-(2*preds$se.fit))),
          col=incol[x-1],border=F)
  lines(x=styr:endyr, preds$fit,type="l",lwd=2)
  points(y=wt40, x=indates, cex=1.2, pch=19,col='dark grey')
  axis(side=1,at=c(1970,1990,2010))

  if(x==2)
    mtext(side=2,line=2.5,expression("W"[40]),las=1)
  saved_stuff[[x]]<-temp_list
}

dev.off()

#===pull together output for YPR analysis
ypr_input<-NULL
ypr_all<-NULL
ages<-seq(1,15)
inds<-c(1,2,6,8)
for(q in 2:4)
{
#==pull parameters
pull_par<-matrix(ncol=4,nrow=length(seq(styr,endyr)))
rownames(pull_par)<-seq(styr,endyr)
for(z in 1:4) 
  pull_par[,z]<-unlist(saved_stuff[[q]][inds[z]])

#==calculate length at age
temp_l_age<-matrix(nrow=nrow(pull_par),ncol=length(ages))
for(h in 1:ncol(temp_l_age))
  for(j in 1:nrow(temp_l_age))
    temp_l_age[j,h]<-pull_par[j,1]*(1-exp(-1*pull_par[j,2]*(ages[h]-vonbert_t0[j,q-1])))

#==calculate weight at age
weight_at_age<-temp_l_age
rownames(weight_at_age)<-seq(styr,endyr)
for(k in 1:nrow(weight_at_age))
  weight_at_age[k,]<-weight_at_length_a[k,q-1]* weight_at_age[k,]^weight_at_length_b[k,q-1]

if(q==4)
  weight_at_age<-weight_at_age/1000
ypr_inputs<-melt(weight_at_age)
colnames(ypr_inputs)<-c("Year","Age","Weight")

#==natural mortality
ypr_inputs$M<-rep(pull_par[,3],length(ages))

#==calculate selectivity at size (in order for figure 2)
#==values taken from Szuwalski et al. High fishery catches in China and extrapolated from REF
l50<-c(13.4,9.5,17.8,13.9)
l25<-c(16.3,11.3,21.7,16.8)

#==calculate selectivity at age
sr <-l50[q-1] -l25[q-1]
s1<-l50[q-1]*log(3)/sr
s2<-s1/l50[q-1]
sel_at_age<-1-1/(1+exp(s1-(s2*temp_l_age/10)))
rownames(sel_at_age)<-seq(styr,endyr)
melt(sel_at_age)
ypr_inputs$Sel<-melt(sel_at_age)[,3]
ypr_inputs$Species<-use_spec[q]
ypr_inputs$F<-rep(pull_par[,4],length(ages))
ypr_all<-rbind(ypr_all,ypr_inputs)
}
ypr_all$prob_mat<-1
ypr_in<-ypr_all[,c(2,1,3,5,7,4,6)]
colnames(ypr_in)<-c("age","year","weight","fish_sel","prob_mat","nat_m","species")
take_yr<-c(seq(1960,2015,7),2014)
ypr_in<-filter(ypr_in,year%in%take_yr)

#==peform YPR analysis for three stocks
source("C:/Users/cody.szuwalski/Work/global_ypr/functions.R")
specs<-data.frame(ypr_in)
proj_yr<-100
input_f<-seq(0.00,1.5,0.03)

unq_stock<-unique(specs$species)
big_out<-data.frame(ypr=NULL,sbpr=NULL,f=NULL,year=NULL,stock=NULL)
big_out_sum<-data.frame(max_ypr=NULL,fmax=NULL,B40=NULL,F40=NULL,year=NULL,stock=NULL)
for(x in 1:length(unq_stock))
{
  specs_use<-dplyr::filter(specs, species == unq_stock[x])
  unq_years<-unique(specs_use$year)
  for(y in 1:length(unq_years))
  {
    #==ypr and sbpr curves
    specs_use2<-dplyr::filter(specs_use, year == unq_years[y])
    outs<-ypr_age(specs_in=specs_use2,proj_yr=100,input_f=input_f)
    big_out<-rbind(big_out,data.frame(ypr=outs$ypr,sbpr=outs$sbpr,f=outs$f,year=rep(unq_years[y],length(input_f)),
                                      stock=rep(unq_stock[x],length(input_f))))
    #==reference points
    #=find f_max and max_ypr
    max_ypr<-max(outs$ypr)
    f_max<-outs$f[which(outs$ypr==max(outs$ypr))]
    
    #=find B40 and F40
    B_40<-outs$sbpr[1]*.40
    F_40<-outs$f[which(abs(outs$sbpr-B_40)==min(abs(outs$sbpr-B_40)))]
    
    big_out_sum<-rbind(big_out_sum,data.frame(max_ypr=max_ypr,f_max=f_max,B40=B_40,F40=F_40,year=unq_years[y],
                                              stock=unq_stock[x]))
  }
}
big_out$year<-as.character(big_out$year)
big_out_sum$year<-as.character(big_out_sum$year)

#==scale ypr for each species relative to maximum
use_out<-NULL
teep<-filter(big_out,stock=="Trichiurus lepturus")
teep$ypr<-teep$ypr/max(teep$ypr)
use_out<-rbind(use_out,teep)
teep<-filter(big_out,stock=="Scomber japonicus")
teep$ypr<-teep$ypr/max(teep$ypr)
use_out<-rbind(use_out,teep)
teep<-filter(big_out,stock=="Larimichthys polyactis")
teep$ypr<-teep$ypr/max(teep$ypr)
use_out<-rbind(use_out,teep)

#================================
# Plots
#================================
library(ggplot2)
require(ggplot2); require(grid); require(png); require(RCurl)
.THEME    = theme_bw(base_size = 12, base_family = "") +
  theme(strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white"))

annotation_custom2 <-   function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf,ymax = Inf, data){ layer(data = data, 
                                                                                                      stat = StatIdentity, 
                                                                                                      position = PositionIdentity,
                                                                                                      geom = ggplot2:::GeomCustomAnn,
                                                                                                      inherit.aes = TRUE, 
                                                                                                      params = list(grob = grob,xmin = xmin, xmax = xmax,
                                                                                                                    ymin = ymin, ymax = ymax))}
syc_png<-readPNG('figures/syc.png')
hair_png<-readPNG('figures/hairtail.png')
scad_png<-readPNG('figures/scad.png')
chub_png<-readPNG('figures/chubmack.png')

use_out$ypr<-as.numeric(use_out$ypr)

#==YPR
p<-ggplot(as.data.frame(use_out))+
  geom_line(aes(x=f,y=ypr,color=year))+
  facet_wrap(~stock, scales ='free',ncol=1) +
  .THEME+xlab("Fishing mortality")+
  ylab("Relative yield per recruit")

a1 = annotation_custom2(rasterGrob(hair_png, interpolate=TRUE), xmin=0, xmax=0.5, ymin=0.75, ymax=1, data=use_out[1,])
a2 = annotation_custom2(rasterGrob(chub_png, interpolate=TRUE), xmin=0, xmax=0.5, ymin=0.75, ymax=1, data=use_out[(1+nrow(use_out)/3),])
a3 = annotation_custom2(rasterGrob(syc_png, interpolate=TRUE), xmin=0, xmax=0.5, ymin=0.75, ymax=1, data=use_out[(1+2*nrow(use_out)/3),])


ht<-ggplot(filter(as.data.frame(use_out),stock=="Trichiurus lepturus"))+
  geom_line(aes(x=f,y=ypr,color=year),lwd=2)+
  facet_wrap(~stock, scales ='free',ncol=1) +
  .THEME+xlab("Fishing mortality")+
 scale_colour_brewer(palette = "Reds")+
  ylab("")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position='none')

syc<-ggplot(filter(as.data.frame(use_out),stock=="Larimichthys polyactis"))+
  geom_line(aes(x=f,y=ypr,color=year),lwd=2)+
  facet_wrap(~stock, scales ='free',ncol=1) +
  .THEME+xlab("Fishing mortality")+
  scale_colour_brewer(palette = "Blues")+
  ylab("Relative yield per recruit")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

chub<-ggplot(filter(as.data.frame(use_out),stock=="Scomber japonicus"))+
  geom_line(aes(x=f,y=ypr,color=year),lwd=2)+
  facet_wrap(~stock, scales ='free',ncol=1) +
  .THEME+xlab("Fishing mortality")+
  ylab("")+theme(legend.position='none')+
 scale_colour_brewer(palette = "Greens")


library(patchwork)
png(paste("figures/figure_3.png",sep=""),height=6,width=4,res=500,units='in')
(ht+a1)/(syc+a3)/(chub+a2) + plot_layout(guides = "collect") 
dev.off()
##calculate % change in yield per recruit
##plug in different mortality trend for scomber japonicus to see if can flip the direction
## put in the graphics for each fish









#=====================================
#==small yellow croaker analysis
#=====================================
syc<-data[data$Species=="Larimichthys polyactis",]
syc[syc$Life_history_process=="age at maturity",]$Value
syc$Value[syc$Units=='cm' & syc$Parameter_name=="Linf"] <- as.numeric(syc$Value[syc$Units=='cm'& syc$Parameter_name=="Linf"])*10
syc$Value[syc$Parameter_name=="t0" & is.na(syc$Value)] <- 0
syc$Value[syc$Parameter_name=="t0" & syc$Value=="N/A"] <- 0
syc$Value[syc$Parameter_name=="t0" & syc$Value=="NA"] <- 0

#==plot length at age over time
#==need to split this by sex
syc_growth<-syc[syc$Life_history_process=="growth",]
ages<-seq(1,20)
unq_study<-unique(syc_growth$Reference)
len_at_age<-matrix(ncol=length(ages),nrow=50)
last_year<-matrix(ncol=3,nrow=50)
counter<-1
for(x in 1:length(unq_study))
{
  temp<-syc_growth[syc_growth$Reference==unq_study[x],]
  unqcomb<- unique(temp[,c('Begin_study_date','End_study_date')])

  for(y in 1:nrow(unqcomb))
  {  
  t0<-NULL
  Linf<-NULL
  grow_k<-NULL
  t0<-as.numeric(temp$Value[temp$Parameter_name=='t0' & temp$End_study_date==as.numeric(unqcomb[y,2]) & temp$Begin_study_date==as.numeric(unqcomb[y,1])])
  Linf<-as.numeric(temp$Value[temp$Parameter_name=='Linf' & temp$End_study_date==as.numeric(unqcomb[y,2]) & temp$Begin_study_date==as.numeric(unqcomb[y,1])])
  grow_k<-as.numeric(temp$Value[temp$Parameter_name=='k' & temp$End_study_date==as.numeric(unqcomb[y,2]) & temp$Begin_study_date==as.numeric(unqcomb[y,1])])
  
  if(length(t0)<1)
    t0<-0
  if(length(grow_k)==1 & length(Linf)==1)
  {
   len_at_age[counter,]<-Linf*(1-exp(-1*grow_k*(ages-t0)))
   last_year[counter,1]<-as.numeric(unqcomb[y,2])
   last_year[counter,2]<-x
   last_year[counter,3]<-y
   counter<-counter+1
  }else
  if(length(grow_k)>1 & length(Linf)>1)
  {
    for(z in 1:length(grow_k))
    {
    len_at_age[counter,]<-Linf[z]*(1-exp(-1*grow_k[z]*(ages-t0[z])))
    last_year[counter,1]<-as.numeric(unqcomb[y,2])
    last_year[counter,2]<-x
    last_year[counter,3]<-y
    counter<-counter+1
    }
  }
 }
 }

#==make colors
library(RColorBrewer)
colrange  <-seq(min(last_year[,1],na.rm=T),max(last_year[,1],na.rm=T),length.out=1000)
cols 	<-colorRampPalette(brewer.pal(9,"RdYlGn"))(length(colrange))

incols<-last_year[,1]
  for(j in 1:length(incols))  
    try(incols[j] <- cols[which(abs(colrange-(last_year[j,1])) == min(abs(colrange-(last_year[j,1]))))] ,silent=T)

len_at_age[31,]<-len_at_age[31,]*10
par(mfrow=c(1,1),mar=c(3,3,.1,1),oma=c(4,3,1,3))
plot(len_at_age[1,]~ages,type='l',ylim=c(0,350),col=incols[1],las=1,ylab='',xlab='n',lwd=2)
for(x in 2:nrow(len_at_age))
  lines(len_at_age[x,]~ages,col=incols[x],lwd=2) 


#===================================================
# look at relationship between life history, F, and sst
#=====================================================
all_par<-NULL
for(q in 2:4)
{
  #==pull parameters
  pull_par<-matrix(ncol=4,nrow=length(seq(styr,endyr)))
  rownames(pull_par)<-seq(styr,endyr)
  for(z in 1:4) 
    pull_par[,z]<-unlist(saved_stuff[[q]][inds[z]])
  pull_par<-as.data.frame(pull_par)
  pull_par$species<-unq_stock[q-1]
 all_par<-rbind(pull_par,all_par)
}
colnames(all_par)<-c("Linf (mm)","k","M","F","Species")
all_par$Year<-seq(1960,2014)
sst_dat<-read.csv("sst_lme.csv")
temp_sst<-filter(sst_dat,LME=="East China Sea")
colnames(temp_sst)[1]<-"Year"
comp_dat<-merge(all_par,temp_sst)
input<-melt(filter(comp_dat,mean_SST>20)[,-8],id.vars=list("Year","F","Species","mean_SST"))

tmp<-input %>% group_by(Species , variable) %>% mutate(scaled = scale(value))
colnames(tmp)[7]<-'value_scale'
ggplot(data=input)+
  geom_point(aes(x=value,y=F))+
  facet_wrap(Species~variable,scales='free_x')+
  theme_bw()+
  theme(strip.placement='outside')

ggplot(data=as.data.frame(tmp))+
  geom_point(aes(col=(value_scale),x=mean_SST,y=F))+
  facet_wrap(Species~variable,scales='free_y')+
  theme_bw()+
  theme(strip.placement='outside')+expand_limits(y=c(0))

ggplot(data=as.data.frame(tmp))+
  geom_point(aes(col=(value_scale),x=mean_SST,y=F))+
 # geom_density_2d_filled(aes(z=(value_scale),x=mean_SST,y=F,fill=value_scale))+
  facet_wrap(Species~variable,scales='free')+
  theme_bw()+
  theme(strip.placement='outside')+expand_limits(y=c(0))

#==not what I want, but kind of neat
ggplot(tmp, aes(x = mean_SST, y = F)) +
  geom_density2d_filled(aes(alpha = as.numeric(..nlevel..),fill  = value), 
                        bins = 40) +
  geom_point() +
  stat_density_2d(geom = "contour", color = "black", alpha = .25, show.legend = FALSE)+
  # scale_alpha(range = c(0.05,0.5)) +
  # scale_fill_manual(values = rep(c("green4", "gray1", "royalblue1"), each = 2)) +
  # stat_quadrant_counts(quadrants = 0L, label.x = "right", 
  #                      aes(label = sprintf("%i observations", stat(count)))) +
  facet_wrap(Species~variable, scales = "free") +theme_bw()

png("figures/figure_4.png",res=1200,units='in',width=8,height=8)
ggplot(data=input,aes(x=value,y=mean_SST))+
  geom_point()+
  facet_wrap(Species~variable,scales='free_x')+
  theme_bw()+geom_smooth()
dev.off()

#==figure showing the potential yield per recruit curves if SST is driving the bus
library(mgcViz)
uniq_sp<-unique(tmp$Species)
uniq_sp<-uniq_sp[c(3,2,1)]
uniq_par<-unique(tmp$variable )
use_brew<-c("Reds","Blues","Greens")
save_plots<-list(list())
save_reform<-matrix(ncol=3,nrow=)
counter<-1
png("figures/figure_4.png",res=450,units='in',width=8,height=8)
par(mfcol=c(3,3),mar=c(.1,.1,.1,.1),oma=c(4,4,2,2))
for(x in 1:length(uniq_sp))
  for(y in 1:length(uniq_par))
  {
   yerp<-filter(tmp,Species==uniq_sp[x]&variable==uniq_par[y])  
   mod<-gam(data=yerp,value~s(mean_SST,F))
   save_plots[[counter]]<-summary(mod)$dev.exp
   xaxt_in<-'n'
   yaxt_in<-'n'
   if(x==1)
     yaxt_in<-'s'
   if(y==3)
     xaxt_in<-'s'
   plot(mod,too.far=1,scheme=2,cex=4,hcolors=colorRampPalette(brewer.pal(9,use_brew[x]))(100),
        yaxt=yaxt_in,xaxt=xaxt_in,ylab='',xlab='',las=1)
   counter<-1+counter
  }
mtext(side=2,outer=T,"Fishing mortality",line=2.3)
mtext(side=1,outer=T,"Sea surface temperature",line=2.3)
mtext(side=3,outer=T,adj=0.1,uniq_sp[1])
mtext(side=3,outer=T,adj=0.5,uniq_sp[2])
mtext(side=3,outer=T,adj=0.9,uniq_sp[3])
mtext(side=4,outer=T,adj=0.1,uniq_par[1])
mtext(side=4,outer=T,adj=0.5,uniq_par[2])
mtext(side=4,outer=T,adj=0.9,uniq_par[3])
dev.off()