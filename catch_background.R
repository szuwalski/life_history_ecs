fish_dat<-read.csv("catch_data.csv")

library(dplyr)
library(RColorBrewer)
#======================================
# Figure with anchovies
#===========================================
Totals<- fish_dat%>%
  group_by(Year) %>%
  summarise(Power=sum(Kilowatts,na.rm=T)/1000000,
            Catch = sum(all_fisheries_catch,na.rm=T)/1000000,
            hairtail = sum(largehead_hairtail_catch,na.rm=T)/1000000,
            syc = sum(small_yellow_croaker_catch,na.rm=T)/1000000, 
            chub = sum(chub_mackerel_catch,na.rm=T)/1000000,
            scad = sum(round_scad_catch,na.rm=T)/1000000,
            anchovy = sum(anchovy_catch,na.rm=T)/1000000,
            sub = sum(largehead_hairtail_catch,chub_mackerel_catch,small_yellow_croaker_catch,round_scad_catch,
                      anchovy_catch,na.rm=T)/1000000)

incol<-brewer.pal(5,"Set1")
mat<-c(1,2,2,2)
png("catch_history.png",res=1200,units='in',width=4,height=7)
layout(mat)
par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))
Totals[Totals$Year ==1989, ]<-NA
plot(Totals$Catch~Totals$Year,type="l",ylim=c(0,max(Totals$Catch,na.rm=T)),
     las=1,ylab='',xlab='',xaxt='n',bty='n')
lines(Totals$sub~Totals$Year,lty=3)

par(new=TRUE)
plot(Totals$Power~Totals$Year,type="l",ylim=c(0,max(Totals$Power,na.rm=T)),las=1,
     yaxt='n',lty=2,ylab='',xlab='',xaxt='n',bty='n')
mtext(side=4,line=2.5,"Kilowatts (million)")
axis(side=4,las=1)
legend("right",bty='n',lty=c(1,2,3),legend=c("Total catch","Fleet power","Example species"),
       cex=1.35)

plot(unlist(Totals[,4])~Totals$Year,type="l",ylim=c(0,1.5),las=1,ylab='',xlab='',col=incol[1],lwd=2,bty='n')

for(x in c(1:4))
  lines(unlist(Totals[,x+4])~Totals$Year,col=incol[x+1],lwd=2)
mtext(side=2,outer=T,"Catch (million t)",line=2.5)

legend("topleft",bty='n',col=incol,pch=15,
       legend=c("Largehead hairtail","Small yellow croaker","Chub mackerel","Round scad","Anchovy"),
       cex=1.35)

dev.off()



#======================================
# Alternative figure
#===========================================
Totals<- fish_dat%>%
  group_by(Year) %>%
  summarise(Power=sum(Kilowatts,na.rm=T)/1000000,
            Catch = sum(all_fisheries_catch,na.rm=T)/1000000,
            hairtail = sum(largehead_hairtail_catch,na.rm=T)/1000000,
            syc = sum(small_yellow_croaker_catch,na.rm=T)/1000000, 
            chub = sum(chub_mackerel_catch,na.rm=T)/1000000,
            scad = sum(round_scad_catch,na.rm=T)/1000000,
            sub = sum(largehead_hairtail_catch,chub_mackerel_catch,small_yellow_croaker_catch,round_scad_catch,na.rm=T)/1000000)

incol<-brewer.pal(5,"Set1")
mat<-c(1,2,2,2)
png("figures/figure_1.png",res=1200,units='in',width=4,height=7)
layout(mat)
par(mar=c(.1,.1,.1,.1),oma=c(4,4,1,4))
Totals[Totals$Year ==1989, ]<-NA
plot(Totals$Catch~Totals$Year,type="l",ylim=c(0,max(Totals$Catch,na.rm=T)),
     las=1,ylab='',xlab='',xaxt='n',bty='n')
lines(Totals$sub~Totals$Year,lty=3)

par(new=TRUE)
plot(Totals$Power~Totals$Year,type="l",ylim=c(0,max(Totals$Power,na.rm=T)),las=1,
     yaxt='n',lty=2,ylab='',xlab='',xaxt='n',bty='n')
mtext(side=4,line=2.5,"Kilowatts (million)")
axis(side=4,las=1)
legend("right",bty='n',lty=c(1,2,3),legend=c("Total catch","Fleet power","Example species"),
       cex=1.35)

plot(unlist(Totals[,4])~Totals$Year,type="l",ylim=c(0,1.5),las=1,ylab='',xlab='',col=incol[1],lwd=2,bty='n')

for(x in c(1:3))
  lines(unlist(Totals[,x+4])~Totals$Year,col=incol[x+1],lwd=2)
mtext(side=2,outer=T,"Catch (million t)",line=2.5)

legend("topleft",bty='n',col=incol,pch=15,
       legend=c("Largehead hairtail","Small yellow croaker","Chub mackerel","Round scad"),
       cex=1.35)

dev.off()

