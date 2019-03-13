setwd("/Volumes/lvc32/PhD/Malaria/PLoS Bio Submit")

m <- rbind(c(1,1,1,1,2,3,3,3,3),
           c(1,1,1,1,2,3,3,3,3),
           c(1,1,1,1,2,3,3,3,3),
           c(1,1,1,1,2,3,3,3,3),
           c(4,4,4,4,5,6,6,6,6),
           c(4,4,4,4,5,6,6,6,6 ),
           c(4,4,4,4,5,6,6,6,6 ),
           c(4,4,4,4,5,6,6,6,6 ))
layout(m)
layout.show(6)

png("Figures/NewFigure2_Pareto_IndexA.png",res=330,width=8,height=8,units="in")
source("Code/Figure2/Figure2.R")
# par(mar=c(4,4,3,1), mfrow=c(2,3))
par(mar=c(3,2,3,2))
layout(m)
y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(624,661,12),2012:2015,cex=1.2)
mtext("Jinja                 Kanungu              Tororo",2,0,cex=0.8)
mtext("Year",1,1.5,cex=0.8)
mtext("(a)",3,1,adj=0)

n<-4
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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1:6)],add=T,space=0,names.arg="n")
mtext("Mosquito Count",2,2.5,cex=0.8)

x<-log10(by_month$mean)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)

fit_data<-cbind.data.frame(x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))))
fit_data$y<-log(5)/log(5*fit_data$y)

xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
yrange<-range(fit_data$y[which(is.finite(fit_data$y))],na.rm=T)
plot(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="Pareto Index",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=1)

points(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)
# legend("topleft",c("Pareto","Modified Pareto"),pch=c(1,4),col=1,bty="n")
mtext("(b)",3,1,adj=0)
mtext("Mean HBR",1,2,cex=0.8)
mtext("Pareto Index",2,2.5,cex=0.8)

source("Code/Figure5/Figure5.R")

y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(624,661,12),2012:2015,cex=1.2)
mtext("Jinja                  Kanungu               Tororo",2,0,cex=0.8)
mtext("Year",1,2,cex=0.8)
mtext("(c)",3,1,adj=0)

n<-4
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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1,1,1,1:3)],add=T,space=0,names.arg="n")
mtext("Infectious Mosquito Count",2,2,cex=0.8)

x<-log10(by_month$mean_true)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
fit_data<-cbind.data.frame(x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))))
fit_data$y<-log(5)/log(5*fit_data$y)

xrange<-c(floor(min(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])),ceiling(max(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])))
yrange<-range(fit_data$y,na.rm=T)
plot(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="Pareto Index",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=1)

points(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean_true[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean_true[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)

# legend("topleft",c("Pareto","Modified Pareto"),pch=c(1,4),col=1,bty="n")
mtext("(d)",3,1,adj=0)
mtext("Pareto Index",2,2.5,cex=0.8)
mtext("Mean EIR",1,2,cex=0.8)
dev.off()


png("Figures/NewFigure2_Pareto_IndexB.png",res=330,width=8,height=8,units="in")
source("Code/Figure2/Figure2.R")
# par(mar=c(4,4,3,1), mfrow=c(2,3))
par(mar=c(3,2,3,2))
layout(m)
y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(624,661,12),2012:2015,cex=1.2)
mtext("Jinja                 Kanungu              Tororo",2,0,cex=0.8)
mtext("Year",1,1.5,cex=0.8)
mtext("(a)",3,1,adj=0)

n<-4
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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1:6)],add=T,space=0,names.arg="n")
mtext("Mosquito Count",2,2.5,cex=0.8)

x<-log10(by_month$mean)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)

fit_data<-cbind.data.frame(x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))))
fit_data$y<-log(5)/log(5*fit_data$y)
fit_data$y_mod<-log(5)/log(5*fit_data$y_mod)

xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
yrange<-range(c(range(fit_data$y_mod[which(is.finite(fit_data$y_mod))],na.rm=T),range(fit_data$y[which(is.finite(fit_data$y))],na.rm=T)))
plot(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="Pareto Index",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=1)

points(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)
points(log10(by_month$mean[1:42]),fit_data$y_mod[which(fit_data$siteid=="Tororo")],pch=4,col=red,cex=0.8)
points(log10(by_month$mean[42+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Kanungu")],pch=4,col=green,cex=0.8)
points(log10(by_month$mean[84+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Jinja")],pch=4,col=blue,cex=0.8)
legend("topleft",c("Pareto","Modified Pareto"),pch=c(1,4),col=1,bty="n")
mtext("(b)",3,1,adj=0)
mtext("Mean HBR",1,2,cex=0.8)
mtext("Pareto Index",2,2.5,cex=0.8)

source("Code/Figure5/Figure5.R")

y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(624,661,12),2012:2015,cex=1.2)
mtext("Jinja                  Kanungu               Tororo",2,0,cex=0.8)
mtext("Year",1,2,cex=0.8)
mtext("(c)",3,1,adj=0)

n<-4
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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1,1,1,1:3)],add=T,space=0,names.arg="n")
mtext("Infectious Mosquito Count",2,2,cex=0.8)

x<-log10(by_month$mean_true)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
fit_data<-cbind.data.frame(x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))))
fit_data$y<-log(5)/log(5*fit_data$y)
fit_data$y_mod<-log(5)/log(5*fit_data$y_mod)

xrange<-c(floor(min(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])),ceiling(max(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])))
yrange<-range(c(range(fit_data$y_mod[which(is.finite(fit_data$y_mod))],na.rm=T),range(fit_data$y[which(is.finite(fit_data$y))],na.rm=T)))
plot(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="Pareto Index",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=1)
points(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean_true[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean_true[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)
points(log10(by_month$mean_true[1:42]),fit_data$y_mod[which(fit_data$siteid=="Tororo")],pch=4,col=red,cex=0.8)
points(log10(by_month$mean_true[42+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Kanungu")],pch=4,col=green,cex=0.8)
points(log10(by_month$mean_true[84+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Jinja")],pch=4,col=blue,cex=0.8)

legend("topleft",c("Pareto","Modified Pareto"),pch=c(1,4),col=1,bty="n")
mtext("(d)",3,1,adj=0)
mtext("Pareto Index",2,2.5,cex=0.8)
mtext("Mean EIR",1,2,cex=0.8)
dev.off()


