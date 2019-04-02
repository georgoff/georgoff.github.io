data<-read.csv(paste0(directory, "outputs/Obs_all.csv"))

# Calculate within-household variance and mean
var<-tapply(data$hbd,data$hhid,var,na.rm=T)
mean<-tapply(data$hbd,data$hhid,mean,na.rm=T)
site<-tapply(as.character(data$siteid),data$hhid,unique)
by_hh<-cbind.data.frame(var,mean,site)

# library(RColorBrewer)
red<-brewer.pal(4,"Reds")[3]
green<-brewer.pal(4,"Greens")[3]
blue<-brewer.pal(4,"Blues")[3]

tororo<-subset(data,data$siteid=="Tororo")
kanungu<-subset(data,data$siteid=="Kanungu")
jinja<-subset(data,data$siteid=="Jinja")
var<-tapply(tororo$hbd,tororo$monthyear,var,na.rm=T)
mean<-tapply(tororo$hbd,tororo$monthyear,mean,na.rm=T)
site<-tapply(as.character(tororo$siteid),tororo$monthyear,unique)
tororo<-cbind(var,mean,site)
var<-tapply(kanungu$hbd,kanungu$monthyear,var,na.rm=T)
mean<-tapply(kanungu$hbd,kanungu$monthyear,mean,na.rm=T)
site<-tapply(as.character(kanungu$siteid),kanungu$monthyear,unique)
kanungu<-cbind(var,mean,site)
var<-tapply(jinja$hbd,jinja$monthyear,var,na.rm=T)
mean<-tapply(jinja$hbd,jinja$monthyear,mean,na.rm=T)
site<-tapply(as.character(jinja$siteid),jinja$monthyear,unique)
jinja<-cbind(var,mean,site)
by_month<-rbind.data.frame(tororo,kanungu,jinja)
by_month$var<-as.numeric(as.character(by_month$var))
by_month$mean<-as.numeric(as.character(by_month$mean))

z<-tapply(data$hbd,list(data$hhid,data$monthyear),unique)
y<-as.numeric(colnames(z))
x<-as.numeric(as.factor(rownames(z)))
months<-c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")
monthlab<-rep(months,length.out=length(y))
tororo_breaks<-c(0,1,5,10,50,100,250,500,1000,1500)
kanungu_breaks<-c(0,1,5,10,50,100,250,500)
jinja_breaks<-c(0,1,5,10,50,100,250)
tororo_colors<-brewer.pal(length(tororo_breaks)-1,"Reds")
kanungu_colors<-brewer.pal(length(kanungu_breaks)-1,"Greens")
jinja_colors<-brewer.pal(length(jinja_breaks)-1,"Blues")

# # pdf"/Volumes/lvc32/Thesis/hbr_key_22_07_16.pdf")
# par(mfrow=c(1,1))
# n<-25
# key1<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
# colnames(key1)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
# rownames(key1)<-1:nrow(key1)
# key1[,"Kanungu"]<-NA
# key1[,"Jinja"]<-NA
# key1[,4:n]<-NA
# key2<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
# colnames(key2)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
# rownames(key2)<-1:nrow(key2)
# key2[,"Tororo"]<-NA
# key2[1:(nrow(key2)-length(kanungu_colors)),"Kanungu"]<-0
# key2[,"Jinja"]<-NA
# key2[,4:n]<-NA
# key3<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
# colnames(key3)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
# rownames(key3)<-1:nrow(key3)
# key3[,"Tororo"]<-NA
# key3[,"Kanungu"]<-NA
# key3[,4:n]<-NA
# key3[1:(nrow(key3)-length(jinja_colors)),"Jinja"]<-0
# barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1.5,space=0,names.arg="n")
# axis(2,0:nrow(key1),tororo_breaks,cex.axis=1.5,las=2)
# barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg="n")
# barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1:6)],add=T,space=0,names.arg="n")
# # dev.off()

#Testing 80-20 rule for HBR
calc_top20<-function(site){
  subset<-data[which(data$siteid==site),]
  sums<-tapply(subset$hbd,subset$monthyear,sum)
  subset<-subset[order(subset$monthyear,subset$hbd, decreasing=TRUE),]
  n<-tapply(subset$hhid,subset$monthyear,length)
  cum_hh<-list(seq(from=0,to=1,length.out=n[1]),seq(from=0,to=1,length.out=n[2]),
               seq(from=0,to=1,length.out=n[3]),seq(from=0,to=1,length.out=n[4]),
               seq(from=0,to=1,length.out=n[5]),seq(from=0,to=1,length.out=n[6]),
               seq(from=0,to=1,length.out=n[7]),seq(from=0,to=1,length.out=n[8]),
               seq(from=0,to=1,length.out=n[9]),seq(from=0,to=1,length.out=n[10]),
               seq(from=0,to=1,length.out=n[11]),seq(from=0,to=1,length.out=n[12]),
               seq(from=0,to=1,length.out=n[13]),seq(from=0,to=1,length.out=n[14]),
               seq(from=0,to=1,length.out=n[15]),seq(from=0,to=1,length.out=n[16]),
               seq(from=0,to=1,length.out=n[17]),seq(from=0,to=1,length.out=n[18]),
               seq(from=0,to=1,length.out=n[19]),seq(from=0,to=1,length.out=n[20]),
               seq(from=0,to=1,length.out=n[21]),seq(from=0,to=1,length.out=n[22]),
               seq(from=0,to=1,length.out=n[23]),seq(from=0,to=1,length.out=n[24]),
               seq(from=0,to=1,length.out=n[25]),seq(from=0,to=1,length.out=n[26]),
               seq(from=0,to=1,length.out=n[27]),seq(from=0,to=1,length.out=n[28]),
               seq(from=0,to=1,length.out=n[29]),seq(from=0,to=1,length.out=n[30]),
               seq(from=0,to=1,length.out=n[31]),seq(from=0,to=1,length.out=n[32]),
               seq(from=0,to=1,length.out=n[33]),seq(from=0,to=1,length.out=n[34]),
               seq(from=0,to=1,length.out=n[35]),seq(from=0,to=1,length.out=n[36]),
               seq(from=0,to=1,length.out=n[37]),seq(from=0,to=1,length.out=n[38]),
               seq(from=0,to=1,length.out=n[39]),seq(from=0,to=1,length.out=n[40]),
               seq(from=0,to=1,length.out=n[41]),seq(from=0,to=1,length.out=n[42]))
  cum_hbr<-tapply(subset$hbd,subset$monthyear,cumsum)
  for (i in 1:42){
    cum_hbr[[i]]<-cum_hbr[[i]]/sums[i]}
  top20<-cum_hbr[[1]][round(0.2*n)[1]]
  for (i in 2:42) {top20<-c(top20,cum_hbr[[i]][round(0.2*n)[i]])}
  all_time_HBR<-tapply(subset$hbd,subset$hhid,sum)/table(subset$hhid)
  all_time_HBR<-all_time_HBR[order(all_time_HBR,decreasing=T)]
  selected_hh<-names(all_time_HBR)[1:round(length(all_time_HBR)*0.2)]
  subsubset<-subset[which(subset$hhid%in%selected_hh),]
  selected_sums<-tapply(subsubset$hbd,subsubset$monthyear,sum)
  top20_mod<-selected_sums/sums
  selected_hh<-unique(subset$hhid[order(subset$weight,decreasing=T)])[1:round(0.2*length(unique(subset$hhid[order(subset$weight,decreasing=T)])))]
  subsubset<-subset[which(subset$hhid%in%selected_hh),]
  selected_sums<-tapply(subsubset$hbd,subsubset$monthyear,sum)
  top20_weights<-selected_sums/sums
  return(list(top20,top20_mod,top20_weights))}

top20t<-calc_top20("Tororo")[[1]]
top20t_mod<-calc_top20("Tororo")[[3]]
top20k<-calc_top20("Kanungu")[[1]]
top20k_mod<-calc_top20("Kanungu")[[3]]
top20j<-calc_top20("Jinja")[[1]]
top20j_mod<-calc_top20("Jinja")[[3]]

z_t<-z[1:107,]
z_t<-z_t[order(rowSums(z_t,na.rm=T)),]
z_k<-z[224:330,]
z_k<-z_k[order(rowSums(z_k,na.rm=T)),]
z_j<-z[108:223,]
z_j<-z_j[order(rowSums(z_j,na.rm=T)),]

lm_ci<-function(conf,fit,xrange){
  xspan<-seq(xrange[1],xrange[2],0.001)
  alpha<-1-conf
  p<-1-alpha/2
  df<-summary(fit)[["df"]][2]
  t<-qt(p,df)
  slope<-summary(fit)[["coefficients"]][2,1]
  int<-summary(fit)[["coefficients"]][1,1]
  slope_low<-slope-t*summary(fit)[["coefficients"]][2,2]
  slope_high<-slope+t*summary(fit)[["coefficients"]][2,2]
  int_low<-int-t*summary(fit)[["coefficients"]][1,2]
  int_high<-int+t*summary(fit)[["coefficients"]][1,2]
  line<-slope*xspan+int
  line1<-slope_low*xspan+int_low
  line2<-slope_high*xspan+int_low
  line3<-slope_low*xspan+int_high
  line4<-slope_high*xspan+int_high
  return(rbind(line,line1,line2,line3,line4))}

plot_ci<-function(conf,fit,xrange,col="light gray"){
  ci<-lm_ci(conf,fit,xrange)
  xspan<-seq(xrange[1],xrange[2],0.001)
  upper<-apply(ci,2,max)
  lower<-apply(ci,2,min)
  polygon(x=c(xspan,rev(xspan)),y=c(upper,rev(ci[1,])),col=alpha(col,0.5),border=NA)
  polygon(x=c(xspan,rev(xspan)),y=c(lower,rev(ci[1,])),col=alpha(col,0.5),border=NA)
}

# # pdf"/Volumes/lvc32/Thesis/Figures/hbr_heat_plot_26_07_16.pdf")
# par(mar=c(4,4,3,1), mfrow=c(2,2))
# y<-as.numeric(colnames(z))
# image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
# image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
# image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
# axis(1,seq(621,661,3),monthlab[seq(1,41,3)],cex.axis=0.7,las=2)
# mtext("Jinja             Kanungu          Tororo",2,0,cex=0.7)
# mtext("Month",1,2,cex=0.8)
# mtext("(a)",3,1,adj=0)
# 
# yrange<-c(floor(min(log10(by_hh$var)[which(is.finite(log10(by_hh$var)))])),ceiling(max(log10(by_hh$var)[which(is.finite(log10(by_hh$var)))])))
# xrange<-c(floor(min(log10(by_hh$mean)[which(is.finite(log10(by_hh$mean)))])),ceiling(max(log10(by_hh$mean)[which(is.finite(log10(by_hh$mean)))])))
# plot(log10(by_hh$mean[which(by_hh$site=="Tororo")]),log10(by_hh$var[which(by_hh$site=="Tororo")]),pch=20,cex=0.8,col=red,xlim=xrange,
#      ylim=yrange,yaxt="n",xaxt="n",xlab="",ylab="Variance")
# lines(-3:6,-3:6,col="purple",lty=2)
# axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=0.9)
# axis(2,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),las=2,cex.axis=0.9)
# legend("topleft",c("Tororo","Kanungu","Jinja"),pch=20,col=c(red,green,blue),bty="n")
# subset<-by_hh[which(by_hh$mean>0),]
# hh_fit<-lm(log10(subset$var)~log10(subset$mean))
# plot_ci(.95,hh_fit,xrange)
# points(log10(by_hh$mean[which(by_hh$site=="Tororo")]),log10(by_hh$var[which(by_hh$site=="Tororo")]),pch=20,cex=0.8,col=red)
# points(log10(by_hh$mean[which(by_hh$site=="Kanungu")]),log10(by_hh$var[which(by_hh$site=="Kanungu")]),pch=20,cex=0.8,col=green)
# points(log10(by_hh$mean[which(by_hh$site=="Jinja")]),log10(by_hh$var[which(by_hh$site=="Jinja")]),pch=20,cex=0.8,col=blue)
# abline(a=hh_fit[[1]][1],b=hh_fit[[1]][2])
# mtext("(b)",3,1,adj=0)
# mtext("Mean",1,2,cex=0.8)
# 
# yrange<-c(floor(min(log10(by_month$var)[which(is.finite(log10(by_month$var)))])),ceiling(max(log10(by_month$var)[which(is.finite(log10(by_month$var)))])))
# xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
# plot(log10(by_month$mean[which(by_month$site=="Tororo")]),log10(by_month$var[which(by_month$site=="Tororo")]),pch=20,col=red,xlim=xrange,
#      ylim=yrange,cex=0.8,yaxt="n",xaxt="n",xlab="",ylab="Variance")
# lines(-3:6,-3:6,col="purple",lty=2)
# axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=0.9)
# axis(2,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),las=2,cex.axis=0.9)
# legend("topleft",c("Tororo","Kanungu","Jinja"),pch=20,col=c(red,green,blue),bty="n")
# subset<-by_month[which(by_month$mean>0),]
# month_fit<-lm(log10(subset$var)~log10(subset$mean))
# plot_ci(.95,month_fit,xrange)
# points(log10(by_month$mean[which(by_month$site=="Tororo")]),log10(by_month$var[which(by_month$site=="Tororo")]),pch=20,cex=0.8,col=red)
# points(log10(by_month$mean[which(by_month$site=="Kanungu")]),log10(by_month$var[which(by_month$site=="Kanungu")]),pch=20,cex=0.8,col=green)
# points(log10(by_month$mean[which(by_month$site=="Jinja")]),log10(by_month$var[which(by_month$site=="Jinja")]),pch=20,cex=0.8,col=blue)
# abline(a=month_fit[[1]][1],b=month_fit[[1]][2])
# text(log10(by_month[which(rownames(by_month)==662),"mean"]),log10(by_month[which(rownames(by_month)==662),"var"]),"IRS",adj=-1)
# points(log10(by_month[which(rownames(by_month)==662),"mean"]),log10(by_month[which(rownames(by_month)==662),"var"]),cex=4)
# mtext("(c)",3,1,adj=0)
# mtext("Mean",1,2,cex=0.8)
# 
# xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
# plot(log10(by_month$mean[1:42]),top20t,ylab="Pareto Index",xlab="",xaxt="n",pch=20,cex=0.8,col=red,las=1,ylim=c(0,1),xlim=xrange)
# axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=0.9)
# x<-log10(by_month$mean)
# y<-c(top20t,top20k,top20j)
# y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
# pareto_fit<-lm(y~x)
# pareto_fit_mod<-lm(y_mod~x)
# plot_ci(.95,pareto_fit,c(-3,4))
# plot_ci(.95,pareto_fit_mod,c(-3,4))
# abline(a=pareto_fit[[1]][1],b=pareto_fit[[1]][2])
# abline(a=pareto_fit_mod[[1]][1],b=pareto_fit_mod[[1]][2],lty=2)
# points(log10(by_month$mean[1:42]),top20t,pch=20,cex=0.8,col=red)
# points(log10(by_month$mean[42+(1:42)]),top20k,pch=20,cex=0.8,col=green)
# points(log10(by_month$mean[84+(1:42)]),top20j,pch=20,cex=0.8,col=blue)
# points(log10(by_month$mean[1:42]),top20t_mod,pch=4,col=red,cex=0.6)
# points(log10(by_month$mean[42+(1:42)]),top20k_mod,pch=4,col=green,cex=0.6)
# points(log10(by_month$mean[84+(1:42)]),top20j_mod,pch=4,col=blue,cex=0.6)
# legend("bottomleft",c("Pareto","Modified Pareto"),pch=c(20,4),col=1,bty="n")
# mtext("(d)",3,1,adj=0)
# mtext("Mean",1,2,cex=0.8)
# dev.off()