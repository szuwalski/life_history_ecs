library(dplyr)
library(ggplot2)
library(maps)
library(lattice)
library("rnaturalearth")
library(interp)
library(RColorBrewer)
library(reshape2) # for melt
library(mgcv)  
library(PBSmapping)
library(mapdata)    #some additional hires data
library(maptools)   #useful tools such as reading shapefiles
library(mapproj)
library(cowplot)

in_dat<-read.csv("C:/Users/cody.szuwalski/Work/life_history/Hadley SST/big_hadley.csv")

#==data doofiness
dat<-in_dat %>% 
  mutate(year = substr(time,1,4),
         month = substr(time,6,7))

dat$sst[dat$sst=='NaN']<-NA
dat$longitude<-as.numeric(as.character(dat$longitude))
dat$latitude<-as.numeric(as.character(dat$latitude))
dat$sst<-as.numeric(as.character(dat$sst))

range(dat$longitude,na.rm=T)

#==find average in data
avgs<-dat %>%
  group_by(year) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

plot(avgs,type='l')
in_sst<-avgs[1:length(avgs)]

#==show divisions used for China's respective seas
state.map <- map('worldHires', 
                 xlim=c(min(dat$longitude,na.rm=T), max(dat$longitude,na.rm=T)), 
                 ylim=c(min(dat$latitude,na.rm=T),max(dat$latitude,na.rm=T)),
                 plot = TRUE, fill = TRUE,col='grey85')

abline(h=33.3,lty=2)
abline(h=37.5,lty=2)
abline(h=25.5,lty=2)

state.map <- map('worldHires', 
                 xlim=c(min(dat$longitude,na.rm=T), max(dat$longitude,na.rm=T)), 
                 ylim=c(min(dat$latitude,na.rm=T),max(dat$latitude,na.rm=T)),
                 add = TRUE, fill = TRUE,col='grey85')

#==calculate time series of SST by LME
scs_avg<-dat %>%
  filter(latitude >0 & latitude <25.5) %>%
  group_by(year) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

ecs_avg<-dat %>%
  filter(latitude >=25.5 & latitude <33.3) %>%
  group_by(year) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

ys_avg<-dat %>%
  filter(latitude >=33.3 & latitude <37.5) %>%
  group_by(year) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

bh_avg<-dat %>%
  filter(latitude >=37.5) %>%
  group_by(year) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

plot(scs_avg$mean_SST~scs_avg$year,type='l',ylim=c(10,29),las=1)
lines(ecs_avg$mean_SST~ecs_avg$year)
lines(ys_avg$mean_SST~ecs_avg$year)
lines(bh_avg$mean_SST~ecs_avg$year)

scs_avg$LME<-"South China Sea"
ecs_avg$LME<-"East China Sea"
ys_avg$LME<-"Yellow Sea"
bh_avg$LME<-"Bohai"

all_sea_avgs<-rbind(scs_avg,ecs_avg,ys_avg,bh_avg)

write.csv(all_sea_avgs,"sst_lme.csv",row.names=F)

LME<-rep(NA,nrow(dat))
LME[dat$latitude >0 & dat$latitude <25.5]<-"South China Sea"
LME[dat$latitude >=25.5 & dat$latitude <33.3]<-"East China Sea"
LME[dat$latitude >=33.3 & dat$latitude <37.5]<-"Yellow Sea"
LME[dat$latitude >=37.5 ]<-"Bohai Sea"
dat$LME<-LME

#==plot sst over time spatially
lon_1<- min(dat$longitude,na.rm=T)
lon_2<- max(dat$longitude,na.rm=T)
lat_1<- min(dat$latitude,na.rm=T)
lat_2<- max(dat$latitude,na.rm=T)

world <- ne_countries(scale = "medium", returnclass = "sf")

spatial_avg<-dat %>%
  group_by(year,latitude,longitude) %>%
  summarise(mean_SST=mean(sst,na.rm=T))

png("figures/sst_china_map.png")
p<-ggplot() + 
  geom_tile(data=spatial_avg, aes(x = longitude, y = latitude, fill = mean_SST)) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  scale_fill_distiller(palette="Spectral", na.value="white") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6),
        strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(color="white",fill="white"))
print(p)
dev.off()

#==all poitns?
#==just CIs?
q<-ggplot(dat) +
  geom_line(aes(x=year,y=sst,color=LME))
print(q)


plot_grid(p,p)



png("figures/sst_time_china.png")
p<-ggplot() + 
  geom_tile(data=dat, aes(x = longitude, y = latitude, fill = sst)) +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE) +
  scale_fill_distiller(palette="Spectral", na.value="white") +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 6),
        strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(color="white",fill="white"))
print(p)
dev.off()


tmp<-readLines('C:/Users/cody.szuwalski/Work/life_history/Hadley SST/HadISST1_SST_2017.txt')

hist(unlist(tmp))

