setwd("/homes/georgoff/georgoff.github.io/superspreading/")
data<-read.csv("Obs_all.csv")

var_true<-tapply(data$hid,data$hhid,var,na.rm=T)
mean_true<-tapply(data$hid,data$hhid,mean,na.rm=T)
var_mod<-tapply(data$modeled_eir,data$hhid,var,na.rm=T)
mean_mod<-tapply(data$modeled_eir,data$hhid,mean,na.rm=T)
site<-tapply(as.character(data$siteid),data$hhid,unique)
by_hh<-cbind.data.frame(var_true,mean_true,var_mod,mean_mod,site)

site<-tapply(as.character(data$siteid),data$hhid,unique)
by_hh_true<-cbind.data.frame(var,mean,site)

tororo<-subset(data,data$siteid=="Tororo")
var<-tapply(tororo$hid,tororo$hhid,var,na.rm=T)
mean<-tapply(tororo$hid,tororo$hhid,mean,na.rm=T)
tororo_true_by_hh<-cbind.data.frame(var,mean)

library(RColorBrewer)
red<-brewer.pal(4,"Reds")[3]
green<-brewer.pal(4,"Greens")[3]
blue<-brewer.pal(4,"Blues")[3]

tororo<-subset(data,data$siteid=="Tororo")
kanungu<-subset(data,data$siteid=="Kanungu")
jinja<-subset(data,data$siteid=="Jinja")
var<-tapply(tororo$modeled_eir,tororo$monthyear,var,na.rm=T)
mean<-tapply(tororo$modeled_eir,tororo$monthyear,mean,na.rm=T)
site<-tapply(as.character(tororo$siteid),tororo$monthyear,unique)
tororo<-cbind(var,mean,site)
var<-tapply(kanungu$modeled_eir,kanungu$monthyear,var,na.rm=T)
mean<-tapply(kanungu$modeled_eir,kanungu$monthyear,mean,na.rm=T)
site<-tapply(as.character(kanungu$siteid),kanungu$monthyear,unique)
kanungu<-cbind(var,mean,site)
var<-tapply(jinja$modeled_eir,jinja$monthyear,var,na.rm=T)
mean<-tapply(jinja$modeled_eir,jinja$monthyear,mean,na.rm=T)
site<-tapply(as.character(jinja$siteid),jinja$monthyear,unique)
jinja<-cbind(var,mean,site)
by_month<-rbind.data.frame(tororo,kanungu,jinja)
by_month$var<-as.numeric(as.character(by_month$var))
by_month$mean<-as.numeric(as.character(by_month$mean))

tororo<-subset(data,data$siteid=="Tororo")
var_true<-tapply(tororo$hid,tororo$monthyear,var,na.rm=T)
mean_true<-tapply(tororo$hid,tororo$monthyear,mean,na.rm=T)
var_mod<-tapply(tororo$modeled_eir,tororo$monthyear,var,na.rm=T)
mean_mod<-tapply(tororo$modeled_eir,tororo$monthyear,mean,na.rm=T)
site<-tapply(as.character(tororo$siteid),tororo$monthyear,unique)
tororo<-cbind(var_true,mean_true,var_mod,mean_mod,site)

kanungu<-subset(data,data$siteid=="Kanungu")
var_true<-tapply(kanungu$hid,kanungu$monthyear,var,na.rm=T)
mean_true<-tapply(kanungu$hid,kanungu$monthyear,mean,na.rm=T)
var_mod<-tapply(kanungu$modeled_eir,kanungu$monthyear,var,na.rm=T)
mean_mod<-tapply(kanungu$modeled_eir,kanungu$monthyear,mean,na.rm=T)
site<-tapply(as.character(kanungu$siteid),kanungu$monthyear,unique)
kanungu<-cbind(var_true,mean_true,var_mod,mean_mod,site)

jinja<-subset(data,data$siteid=="Jinja")
var_true<-tapply(jinja$hid,jinja$monthyear,var,na.rm=T)
mean_true<-tapply(jinja$hid,jinja$monthyear,mean,na.rm=T)
var_mod<-tapply(jinja$modeled_eir,jinja$monthyear,var,na.rm=T)
mean_mod<-tapply(jinja$modeled_eir,jinja$monthyear,mean,na.rm=T)
site<-tapply(as.character(jinja$siteid),jinja$monthyear,unique)
jinja<-cbind(var_true,mean_true,var_mod,mean_mod,site)

by_month<-rbind.data.frame(tororo,kanungu,jinja)
by_month$var_true<-as.numeric(as.character(by_month$var_true))
by_month$mean_true<-as.numeric(as.character(by_month$mean_true))
by_month$var_mod<-as.numeric(as.character(by_month$var_mod))
by_month$mean_mod<-as.numeric(as.character(by_month$mean_mod))

tororo<-subset(data,data$siteid=="Tororo")
var<-tapply(tororo$hid,tororo$monthyear,var,na.rm=T)
mean<-tapply(tororo$hid,tororo$monthyear,mean,na.rm=T)
tororo_true_by_month<-cbind.data.frame(var,mean)


z<-tapply(data$hid,list(data$hhid,data$monthyear),unique)
y<-as.numeric(colnames(z))
x<-as.numeric(as.factor(rownames(z)))
months<-c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")
monthlab<-rep(months,length.out=length(y))
tororo_breaks<-c(0,1,2,3,4,5,10,20,40,80)
kanungu_breaks<-c(0,1,2,3,4,5,10,20)
jinja_breaks<-c(0,1,2,3)
tororo_colors<-brewer.pal(length(tororo_breaks)-1,"Reds")
kanungu_colors<-brewer.pal(length(kanungu_breaks)-1,"Greens")
jinja_colors<-brewer.pal(length(jinja_breaks)-1,"Blues")

pdf("test1.pdf")
par(mfrow=c(1,1))
n<-25
key1<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
colnames(key1)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
rownames(key1)<-1:nrow(key1)
key1[,"Kanungu"]<-NA
key1[,"Jinja"]<-NA
key1[,4:n]<-NA
key2<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
colnames(key2)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
rownames(key2)<-1:nrow(key2)
key2[,"Tororo"]<-NA
key2[1:(nrow(key2)-length(kanungu_colors)),"Kanungu"]<-0
key2[,"Jinja"]<-NA
key2[,4:n]<-NA
key3<-matrix(data=1,ncol=n,nrow=length(tororo_colors))
colnames(key3)<-c("Tororo","Kanungu","Jinja",rep("",n-3))
rownames(key3)<-1:nrow(key3)
key3[,"Tororo"]<-NA
key3[,"Kanungu"]<-NA
key3[,4:n]<-NA
key3[1:(nrow(key3)-length(jinja_colors)),"Jinja"]<-0
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1.5,space=0,names.arg=NULL)
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1.5,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg=NULL)
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1,1,1,1:3)],add=T,space=0,names.arg=NULL)
dev.off()


# #Testing 80-20 rule for EIR
# calc_top20_eir_modelled<-function(site){
#   subset<-data[which(data$siteid==site),]
#   sums<-tapply(subset$modeled_eir,subset$monthyear,sum)
#   subset<-subset[order(subset$monthyear,subset$modeled_eir, decreasing=TRUE),]
#   n<-tapply(subset$hhid,subset$monthyear,length)
#   cum_hh<-list(seq(from=0,to=1,length.out=n[1]),seq(from=0,to=1,length.out=n[2]),
#                seq(from=0,to=1,length.out=n[3]),seq(from=0,to=1,length.out=n[4]),
#                seq(from=0,to=1,length.out=n[5]),seq(from=0,to=1,length.out=n[6]),
#                seq(from=0,to=1,length.out=n[7]),seq(from=0,to=1,length.out=n[8]),
#                seq(from=0,to=1,length.out=n[9]),seq(from=0,to=1,length.out=n[10]),
#                seq(from=0,to=1,length.out=n[11]),seq(from=0,to=1,length.out=n[12]),
#                seq(from=0,to=1,length.out=n[13]),seq(from=0,to=1,length.out=n[14]),
#                seq(from=0,to=1,length.out=n[15]),seq(from=0,to=1,length.out=n[16]),
#                seq(from=0,to=1,length.out=n[17]),seq(from=0,to=1,length.out=n[18]),
#                seq(from=0,to=1,length.out=n[19]),seq(from=0,to=1,length.out=n[20]),
#                seq(from=0,to=1,length.out=n[21]),seq(from=0,to=1,length.out=n[22]),
#                seq(from=0,to=1,length.out=n[23]),seq(from=0,to=1,length.out=n[24]),
#                seq(from=0,to=1,length.out=n[25]),seq(from=0,to=1,length.out=n[26]),
#                seq(from=0,to=1,length.out=n[27]),seq(from=0,to=1,length.out=n[28]),
#                seq(from=0,to=1,length.out=n[29]),seq(from=0,to=1,length.out=n[30]),
#                seq(from=0,to=1,length.out=n[31]),seq(from=0,to=1,length.out=n[32]),
#                seq(from=0,to=1,length.out=n[33]),seq(from=0,to=1,length.out=n[34]),
#                seq(from=0,to=1,length.out=n[35]),seq(from=0,to=1,length.out=n[36]),
#                seq(from=0,to=1,length.out=n[37]),seq(from=0,to=1,length.out=n[38]),
#                seq(from=0,to=1,length.out=n[39]),seq(from=0,to=1,length.out=n[40]),
#                seq(from=0,to=1,length.out=n[41]),seq(from=0,to=1,length.out=n[42]))
#   cum_eir<-tapply(subset$modeled_eir,subset$monthyear,cumsum)
#   for (i in 1:42){
#     cum_eir[[i]]<-cum_eir[[i]]/sums[i]}
#   top20<-cum_eir[[1]][round(0.2*n)[1]]
#   for (i in 2:42) {top20<-c(top20,cum_eir[[i]][round(0.2*n)[i]])}
#   all_time_eir<-tapply(subset$modeled_eir,subset$hhid,sum)/table(subset$hhid)
#   all_time_eir<-all_time_eir[order(all_time_eir,decreasing=T)]
#   selected_hh<-names(all_time_eir)[1:round(length(all_time_eir)*0.2)]
#   subsubset<-subset[which(subset$hhid%in%selected_hh),]
#   selected_sums<-tapply(subsubset$modeled_eir,subsubset$monthyear,sum)
#   top20_mod<-selected_sums/sums
#   selected_hh<-unique(subset$hhid[order(subset$weight,decreasing=T)])[1:round(0.2*length(unique(subset$hhid[order(subset$weight,decreasing=T)])))]
#   subsubset<-subset[which(subset$hhid%in%selected_hh),]
#   selected_sums<-tapply(subsubset$modeled_eir,subsubset$monthyear,sum)
#   top20_weights<-selected_sums/sums
#   return(list(top20,top20_mod,top20_weights))}
# 
# top20t<-calc_top20_eir_modelled("Tororo")[[1]]
# top20t_mod<-calc_top20_eir_modelled("Tororo")[[3]]
# top20k<-calc_top20_eir_modelled("Kanungu")[[1]]
# top20k_mod<-calc_top20_eir_modelled("Kanungu")[[3]]
# top20j<-calc_top20_eir_modelled("Jinja")[[1]]
# top20j_mod<-calc_top20_eir_modelled("Jinja")[[3]]

#Testing 80-20 rule for EIR
calc_top20_eir<-function(site){
  subset<-data[which(data$siteid==site),]
  sums<-tapply(subset$hid,subset$monthyear,sum)
  subset<-subset[order(subset$monthyear,subset$hid, decreasing=TRUE),]
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
  cum_eir<-tapply(subset$hid,subset$monthyear,cumsum)
  for (i in 1:42){
    cum_eir[[i]]<-cum_eir[[i]]/sums[i]}
  top20<-cum_eir[[1]][round(0.2*n)[1]]
  for (i in 2:42) {top20<-c(top20,cum_eir[[i]][round(0.2*n)[i]])}
  all_time_eir<-tapply(subset$hid,subset$hhid,sum)/table(subset$hhid)
  all_time_eir<-all_time_eir[order(all_time_eir,decreasing=T)]
  selected_hh<-names(all_time_eir)[1:round(length(all_time_eir)*0.2)]
  subsubset<-subset[which(subset$hhid%in%selected_hh),]
  selected_sums<-tapply(subsubset$hid,subsubset$monthyear,sum)
  top20_mod<-selected_sums/sums
  selected_hh<-unique(subset$hhid[order(subset$weight,decreasing=T)])[1:round(0.2*length(unique(subset$hhid[order(subset$weight,decreasing=T)])))]
  subsubset<-subset[which(subset$hhid%in%selected_hh),]
  selected_sums<-tapply(subsubset$hid,subsubset$monthyear,sum)
  top20_weights<-selected_sums/sums
  return(list(top20,top20_mod,top20_weights))}

top20t<-calc_top20_eir("Tororo")[[1]]
top20t_mod<-calc_top20_eir("Tororo")[[3]]
top20k<-calc_top20_eir("Kanungu")[[1]]
top20k_mod<-calc_top20_eir("Kanungu")[[3]]
top20j<-calc_top20_eir("Jinja")[[1]]
top20j_mod<-calc_top20_eir("Jinja")[[3]]

z_t<-z[1:107,]
z_t<-z_t[order(rowSums(z_t,na.rm=T)),]
z_k<-z[224:330,]
z_k<-z_k[order(rowSums(z_k,na.rm=T)),]
z_j<-z[108:223,]
z_j<-z_j[order(rowSums(z_j,na.rm=T)),]

pdf("fig_5_test.pdf")
par(mar=c(4,4,3,1), mfrow=c(2,2))
y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(621,661,3),monthlab[seq(1,41,3)],cex.axis=0.7,las=2)
mtext("Jinja             Kanungu          Tororo",2,0,cex=0.7)
mtext("Month",1,2,cex=0.8)
mtext("(a)",3,1,adj=0)

yrange<-c(floor(min(log10(by_hh$var)[which(is.finite(log10(by_hh$var)))])),ceiling(max(log10(by_hh$var)[which(is.finite(log10(by_hh$var)))])))
xrange<-c(floor(min(log10(by_hh$mean)[which(is.finite(log10(by_hh$mean)))])),ceiling(max(log10(by_hh$mean)[which(is.finite(log10(by_hh$mean)))])))
plot(log10(by_hh$mean[which(by_hh$site=="Tororo")]),log10(by_hh$var[which(by_hh$site=="Tororo")]),pch=20,cex=0.8,col=red,xlim=xrange,
     ylim=yrange,yaxt="n",xaxt="n",xlab="",ylab="Variance")
lines(-7:6,-7:6,col="purple",lty=2)
axis(1,-5:1,c(expression(10^-5),expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=0.9)
axis(2,-8:3,c(expression(10^-8),expression(10^-7),expression(10^-6),expression(10^-5),expression(10^-4),"0.001","0.01","0.1","1","10","100","1000"),las=2,cex.axis=0.9)
legend("topleft",c("Tororo","Kanungu","Jinja"),pch=20,col=c(red,green,blue),bty="n")
subset<-by_hh[which(by_hh$mean>0),]
hh_fit<-lm(log10(subset$var)~log10(subset$mean))
plot_ci(.95,hh_fit,c(-6,3))
points(log10(by_hh$mean[which(by_hh$site=="Tororo")]),log10(by_hh$var[which(by_hh$site=="Tororo")]),pch=20,cex=0.8,col=red)
points(log10(by_hh$mean[which(by_hh$site=="Kanungu")]),log10(by_hh$var[which(by_hh$site=="Kanungu")]),pch=20,cex=0.8,col=green)
points(log10(by_hh$mean[which(by_hh$site=="Jinja")]),log10(by_hh$var[which(by_hh$site=="Jinja")]),pch=20,cex=0.8,col=blue)
points(log10(tororo_true_by_hh$mean),log10(tororo_true_by_hh$var),pch=4,cex=0.6,col=red)
abline(a=hh_fit[[1]][1],b=hh_fit[[1]][2])
mtext("(b)",3,1,adj=0)
mtext("Mean",1,2,cex=0.8)
legend("bottomright",c("Modelled","True"),pch=c(20,4),col=1,bty="n")

yrange<-c(floor(min(log10(by_month$var)[which(is.finite(log10(by_month$var)))])),ceiling(max(log10(by_month$var)[which(is.finite(log10(by_month$var)))])))
xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
plot(log10(by_month$mean[which(by_month$site=="Tororo")]),log10(by_month$var[which(by_month$site=="Tororo")]),pch=20,col=red,xlim=xrange,
     ylim=yrange,cex=0.8,yaxt="n",xaxt="n",xlab="",ylab="Variance")
lines(-6:6,-6:6,col="purple",lty=2)
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=0.9)
axis(2,-7:3,c(expression(10^-7),expression(10^-6),expression(10^-5),expression(10^-4),"0.001","0.01","0.1","1","10","100","1000"),las=2,cex.axis=0.9)
legend("topleft",c("Tororo","Kanungu","Jinja"),pch=20,col=c(red,green,blue),bty="n")
subset<-by_month[which(by_month$mean>0),]
month_fit<-lm(log10(subset$var)~log10(subset$mean))
plot_ci(.95,month_fit,xrange)
points(log10(by_month$mean[which(by_month$site=="Tororo")]),log10(by_month$var[which(by_month$site=="Tororo")]),pch=20,cex=0.8,col=red)
points(log10(by_month$mean[which(by_month$site=="Kanungu")]),log10(by_month$var[which(by_month$site=="Kanungu")]),pch=20,col=green,cex=0.8)
points(log10(by_month$mean[which(by_month$site=="Jinja")]),log10(by_month$var[which(by_month$site=="Jinja")]),pch=20,col=blue,cex=0.8)
points(log10(tororo_true_by_month$mean),log10(tororo_true_by_month$var),pch=4,cex=0.6,col=red)
abline(a=month_fit[[1]][1],b=month_fit[[1]][2])
mtext("(c)",3,1,adj=0)
mtext("Mean",1,2,cex=0.8)
legend("bottomright",c("Modelled","True"),pch=c(20,4),col=1,bty="n")
text(log10(by_month[which(rownames(by_month)==662),"mean"]),log10(by_month[which(rownames(by_month)==662),"var"]),"IRS",adj=-1)
points(log10(by_month[which(rownames(by_month)==662),"mean"]),log10(by_month[which(rownames(by_month)==662),"var"]),cex=4)
points(log10(by_month[which(rownames(by_month)==661),"mean"]),log10(by_month[which(rownames(by_month)==661),"var"]),cex=4)

xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
plot(log10(by_month$mean[1:42]),top20t,ylab="Pareto Index",xlab="",xaxt="n",pch=20,cex=0.8,col=red,las=1,ylim=c(0,1),xlim=xrange)
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=0.9)
x<-log10(by_month$mean)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
fit_data<-cbind.data.frame(x,y,y_mod)
fit_data<-fit_data[complete.cases(fit_data),]
fit_data<-fit_data[is.finite(fit_data$x),]
pareto_fit<-lm(fit_data$y~fit_data$x)
pareto_fit_mod<-lm(fit_data$y_mod~fit_data$x)
plot_ci(.95,pareto_fit,c(-5,3))
plot_ci(.95,pareto_fit_mod,c(-5,3))
points(log10(by_month$mean[1:42]),top20t,pch=20,cex=0.8,col=red)
points(log10(by_month$mean[42+(1:42)]),top20k,pch=20,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),top20j,pch=20,cex=0.8,col=blue)
points(log10(by_month$mean[1:42]),top20t_mod,pch=4,col=red,cex=0.6)
points(log10(by_month$mean[42+(1:42)]),top20k_mod,pch=4,col=green,cex=0.6)
points(log10(by_month$mean[84+(1:42)]),top20j_mod,pch=4,col=blue,cex=0.6)
abline(a=pareto_fit[[1]][1],b=pareto_fit[[1]][2])
abline(a=pareto_fit_mod[[1]][1],b=pareto_fit_mod[[1]][2],lty=2)
legend("bottomleft",c("Pareto","Modified Pareto"),pch=c(20,4),col=1,bty="n")
mtext("(d)",3,1,adj=0)
mtext("Mean",1,2,cex=0.8)
dev.off()