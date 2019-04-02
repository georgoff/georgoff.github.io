# library(scales, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
m <- rbind(c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8))
layout(m)
layout.show(8)
# source("./Code/Data_prep_LC.R")
# source("/homes/georgoff/georgoff.github.io/superspreading/round_2/code/Data_prep_LC.R")

png(paste0(directory, "outputs/NewFigure2.png"),res=330,width=12,height=8,units="in")
# source("./Code/Figure2/Figure2.R")
# source("/homes/georgoff/georgoff.github.io/superspreading/round_2/code/Figure2/Figure2.R")
# par(mar=c(4,4,3,1), mfrow=c(2,3))
par(mar=c(3,2,3,2))
m <- rbind(c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(1,1,1,1,2,3,3,3,3,4,4,4,4),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8),
           c(5,5,5,5,6,7,7,7,7,8,8,8,8))
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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0)#,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0)#,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1:6)],add=T,space=0)#,names.arg="n")
mtext("Mosquito Count",2,2.5,cex=0.8)

xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
plot(log10(by_month$mean[1:42]),top20t,ylab="",xlab="",xaxt="n",yaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=c(0,1),xlim=xrange)
axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=1)
axis(2,seq(0,1,0.2),seq(0,100,20),cex.axis=1,las=1)

x<-log10(by_month$mean)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
pareto_fit<-lm(y~x)
pareto_fit_mod<-lm(y_mod~x)
# t_fit<-lm(top20t~x[1:42])
# t_fit_mod<-lm(top20t_mod~x[1:42])
# k_fit<-lm(top20k~x[42+(1:42)])
# k_fit_mod<-lm(top20k_mod~x[42+(1:42)])
# j_fit<-lm(top20j~x[84+(1:42)])
# j_fit_mod<-lm(top20j_mod~x[84+(1:42)])
# 
# fits<-list(pareto_fit,pareto_fit_mod,
#           t_fit,t_fit_mod,
#           k_fit,k_fit_mod,
#           j_fit,j_fit_mod)
# 
# i<-which(sapply(fits,function(x){summary(x)$coefficients[2,4]})<0.05)
# fits<-fits[i]
# sapply(fits,function(x){confint(x)[2,]})

# plot_ci(.95,k_fit,c(-5,4),col=kanungu_colors[1])
# # plot_ci(.95,k_fit_mod,c(-5,3))
# plot_ci(.95,j_fit,c(-5,4),col=jinja_colors[1])
# # plot_ci(.95,j_fit_mod,c(-5,3))
# plot_ci(.95,t_fit_mod,c(-5,4),col=tororo_colors[1])
# plot_ci(.95,t_fit,c(-5,4),col=tororo_colors[1])
plot_ci(.95,pareto_fit_mod,c(-3,4))
plot_ci(.95,pareto_fit,c(-3,4))

abline(a=pareto_fit[[1]][1],b=pareto_fit[[1]][2])
abline(a=pareto_fit_mod[[1]][1],b=pareto_fit_mod[[1]][2],lty=2)
# abline(a=t_fit[[1]][1],b=t_fit[[1]][2],col=red)
# abline(a=t_fit_mod[[1]][1],b=t_fit_mod[[1]][2],lty=2,col=red)
# abline(a=k_fit[[1]][1],b=k_fit[[1]][2],col=green)
# # abline(a=k_fit_mod[[1]][1],b=k_fit_mod[[1]][2],lty=2,col=green)
# abline(a=j_fit[[1]][1],b=j_fit[[1]][2],col=blue)
# # abline(a=j_fit_mod[[1]][1],b=j_fit_mod[[1]][2],lty=2,col=blue)

points(log10(by_month$mean[1:42]),top20t,pch=1,cex=0.8,col=red)
points(log10(by_month$mean[42+(1:42)]),top20k,pch=1,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),top20j,pch=1,cex=0.8,col=blue)
points(log10(by_month$mean[1:42]),top20t_mod,pch=4,col=red,cex=0.8)
points(log10(by_month$mean[42+(1:42)]),top20k_mod,pch=4,col=green,cex=0.8)
points(log10(by_month$mean[84+(1:42)]),top20j_mod,pch=4,col=blue,cex=0.8)
legend("bottomright",c("Super-spreading","Super-spreaders"),pch=c(1,4),col=1,bty="n")
mtext("(b)",3,1,adj=0)
mtext("Mean HBR",1,2,cex=0.8)
mtext("Pareto Fraction",2,2.5,cex=0.8)

x<-log10(by_month$mean)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)

fit_data<-cbind.data.frame(mean=by_month$mean,
                           x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))),
                           month=rep(1:42,times=3))
fit_data$fraction<-fit_data$y
fit_data$fraction_mod<-fit_data$y_mod
fit_data$y<-log(5)/log(5*fit_data$y)
fit_data$y_mod<-log(5)/log(5*fit_data$y_mod)

# fit_data$y<-log(1-fit_data$y)/log((1-fit_data$y)/(fit_data$y))
# fit_data$y_mod<-log(1-fit_data$y_mod)/log((1-fit_data$y_mod)/(fit_data$y_mod))

xrange<-c(floor(min(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])),ceiling(max(log10(by_month$mean)[which(is.finite(log10(by_month$mean)))])))
yrange<-c(1,5)
# yrange<-range(c(fit_data$y[which(is.finite(fit_data$y))],fit_data$y_mod[which(is.finite(fit_data$y_mod))]),na.rm=T)
write.csv(fit_data[which(fit_data$y<1|fit_data$y_mod<1|fit_data$y>5|fit_data$y_mod>5|is.na(fit_data$y)|is.na(fit_data$y_mod)),],
          paste0(directory, "outputs/HBR_outliers.csv"))
plot(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-2:5,c("0.01","0.1","1","10","100","1000","10000","100000"),cex.axis=1)

points(log10(by_month$mean[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)
points(log10(by_month$mean[1:42]),fit_data$y_mod[which(fit_data$siteid=="Tororo")],pch=4,col=red,cex=0.8)
points(log10(by_month$mean[42+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Kanungu")],pch=4,cex=0.8,col=green)
points(log10(by_month$mean[84+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Jinja")],pch=4,cex=0.8,col=blue)
abline(h=log(5)/log(4),lty=3)
text(x=max(xrange)-0.1,y=c(log(5)/log(4),log(0.3)/log(0.3/0.7),log(0.45)/log(0.45/0.55))+0.1,labels=c("80-20","70-30","55-45"))
abline(h=log(0.3)/log(0.3/0.7),lty=3)
abline(h=log(0.45)/log(0.45/0.55),lty=3)

mtext("(c)",3,1,adj=0)
mtext("Mean HBR",1,2,cex=0.8)
mtext("Pareto Index",2,2.5,cex=0.8)
legend("topleft",c("Super-spreading","Super-spreaders"),pch=c(1,4),col=1,bty="n")

source(paste0(directory, "scripts/EIR.R"))

y<-as.numeric(colnames(z))
image(y,224:331,t(z_t),ylim=c(1,331),ylab="Household",xlab="",yaxt="n",xaxt="n",breaks=tororo_breaks,col=tororo_colors)
image(y,117:223,t(z_k),add=T,breaks=kanungu_breaks,col=kanungu_colors)
image(y,1:116,t(z_j),add=T,breaks=jinja_breaks,col=jinja_colors)
axis(1,seq(624,661,12),2012:2015,cex=1.2)
mtext("Jinja                  Kanungu               Tororo",2,0,cex=0.8)
mtext("Year",1,2,cex=0.8)
mtext("(d)",3,1,adj=0)

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
barplot(key1,beside=F,yaxt="n",col=tororo_colors,cex.lab=1,space=0)#,names.arg="n")
axis(2,0:nrow(key1),tororo_breaks,cex.axis=1,las=2)
barplot(key2,beside=F,yaxt="n",col=kanungu_colors[c(1,1,1:7)],add=T,space=0)#,names.arg="n")
barplot(key3,beside=F,yaxt="n",col=jinja_colors[c(1,1,1,1,1,1,1:3)],add=T,space=0)#,names.arg="n")
mtext("Infectious Mosquito Count",2,2,cex=0.8)

xrange<-c(floor(min(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])),ceiling(max(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])))
plot(log10(by_month$mean_true[1:42]),top20t,ylab="",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=c(0,1),xlim=xrange,yaxt="n")
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=1)
axis(2,seq(0,1,0.2),seq(0,100,20),cex.axis=1,las=1)
x<-log10(by_month$mean_true)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
fit_data<-cbind.data.frame(mean=by_month$mean_true,x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))),
                           month=rep(1:42,times=3))
fit_data<-fit_data[complete.cases(fit_data),]
fit_data<-fit_data[is.finite(fit_data$x),]
pareto_fit<-lm(fit_data$y~fit_data$x)
pareto_fit_mod<-lm(fit_data$y_mod~fit_data$x)
# t_fit<-lm(y~x,fit_data[which(fit_data$siteid=="Tororo"),])
# t_fit_mod<-lm(y_mod~x,fit_data[which(fit_data$siteid=="Tororo"),])
# k_fit<-lm(y~x,fit_data[which(fit_data$siteid=="Kanungu"),])
# k_fit_mod<-lm(y_mod~x,fit_data[which(fit_data$siteid=="Kanungu"),])
# j_fit<-lm(y~x,fit_data[which(fit_data$siteid=="Jinja"),])
# j_fit_mod<-lm(y_mod~x,fit_data[which(fit_data$siteid=="Jinja"),])

# fits<-list(pareto_fit,pareto_fit_mod,
#            t_fit,t_fit_mod,
#            k_fit,k_fit_mod,
#            j_fit,j_fit_mod)
# 
# i<-which(sapply(fits,function(x){summary(x)$coefficients[2,4]})<0.05)
# fits<-fits[i]
# sapply(fits,function(x){confint(x)[2,]})

plot_ci(.95,pareto_fit_mod,c(-5,3))
# plot_ci(.95,t_fit,c(-5,3),col=tororo_colors[1])
# # plot_ci(.95,t_fit_mod,c(-5,3))
# # plot_ci(.95,k_fit,c(-5,3))
# # plot_ci(.95,k_fit_mod,c(-5,3))
# # plot_ci(.95,j_fit,c(-5,3))
# # plot_ci(.95,j_fit_mod,c(-5,3))
plot_ci(.95,pareto_fit,c(-5,3))

points(log10(by_month$mean_true[1:42]),top20t,pch=1,cex=0.8,col=red)
points(log10(by_month$mean_true[42+(1:42)]),top20k,pch=1,cex=0.8,col=green)
points(log10(by_month$mean_true[84+(1:42)]),top20j,pch=1,cex=0.8,col=blue)
points(log10(by_month$mean_true[1:42]),top20t_mod,pch=4,col=red,cex=0.8)
points(log10(by_month$mean_true[42+(1:42)]),top20k_mod,pch=4,col=green,cex=0.8)
points(log10(by_month$mean_true[84+(1:42)]),top20j_mod,pch=4,col=blue,cex=0.8)

abline(a=pareto_fit[[1]][1],b=pareto_fit[[1]][2])
abline(a=pareto_fit_mod[[1]][1],b=pareto_fit_mod[[1]][2],lty=2)
# abline(a=t_fit[[1]][1],b=t_fit[[1]][2],col=red)
# # abline(a=t_fit_mod[[1]][1],b=t_fit_mod[[1]][2],lty=2,col=red)
# # abline(a=k_fit[[1]][1],b=k_fit[[1]][2],col=green)
# # abline(a=k_fit_mod[[1]][1],b=k_fit_mod[[1]][2],lty=2,col=green)
# # abline(a=j_fit[[1]][1],b=j_fit[[1]][2],col=blue)
# # abline(a=j_fit_mod[[1]][1],b=j_fit_mod[[1]][2],lty=2,col=blue)

legend("bottomright",c("Super-spreading","Super-spreaders"),pch=c(1,4),col=1,bty="n")
mtext("(e)",3,1,adj=0)
mtext("Pareto Fraction",2,2.5,cex=0.8)
mtext("Mean EIR",1,2,cex=0.8)

x<-log10(by_month$mean_true)
y<-c(top20t,top20k,top20j)
y_mod<-c(top20t_mod,top20k_mod,top20j_mod)
fit_data<-cbind.data.frame(mean=by_month$mean_true,
                           x,y,y_mod,siteid=c(rep("Tororo",length(top20t)),rep("Kanungu",length(top20k)),rep("Jinja",length(top20j))),
                           month=rep(1:42,times=3))
fit_data$fraction<-fit_data$y
fit_data$fraction_mod<-fit_data$y_mod
fit_data$y<-log(5)/log(5*fit_data$y)
fit_data$y_mod<-log(5)/log(5*fit_data$y_mod)
# fit_data$y<-log(1-fit_data$y)/log((1-fit_data$y)/(fit_data$y))
# fit_data$y_mod<-log(1-fit_data$y_mod)/log((1-fit_data$y_mod)/(fit_data$y_mod))

xrange<-c(floor(min(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])),ceiling(max(log10(by_month$mean_true)[which(is.finite(log10(by_month$mean_true)))])))
yrange<-c(1,5)
# yrange<-range(c(fit_data$y[which(is.finite(fit_data$y))],fit_data$y_mod[which(is.finite(fit_data$y_mod))]),na.rm=T)
write.csv(fit_data[which(fit_data$y<1|fit_data$y_mod<1|fit_data$y>5|fit_data$y_mod>5|is.na(fit_data$y)|is.na(fit_data$y_mod)),],
          paste0(directory, "outputs/EIR_outliers.csv"))
plot(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],ylab="Pareto Index",xlab="",xaxt="n",pch=1,cex=0.8,col=red,las=1,ylim=yrange,xlim=xrange)
axis(1,-4:1,c(expression(10^-4),"0.001","0.01","0.1","1","10"),cex.axis=1)

points(log10(by_month$mean_true[1:42]),fit_data$y[which(fit_data$siteid=="Tororo")],pch=1,cex=0.8,col=red)
points(log10(by_month$mean_true[42+(1:42)]),fit_data$y[which(fit_data$siteid=="Kanungu")],pch=1,cex=0.8,col=green)
points(log10(by_month$mean_true[84+(1:42)]),fit_data$y[which(fit_data$siteid=="Jinja")],pch=1,cex=0.8,col=blue)
points(log10(by_month$mean_true[1:42]),fit_data$y_mod[which(fit_data$siteid=="Tororo")],pch=4,cex=0.8,col=red)
points(log10(by_month$mean_true[42+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Kanungu")],pch=4,cex=0.8,col=green)
points(log10(by_month$mean_true[84+(1:42)]),fit_data$y_mod[which(fit_data$siteid=="Jinja")],pch=4,cex=0.8,col=blue)
abline(h=log(5)/log(4),lty=3)
abline(h=log(0.3)/log(0.3/0.7),lty=3)
abline(h=log(0.45)/log(0.45/0.55),lty=3)
text(x=max(xrange)-0.1,y=c(log(5)/log(4),log(0.3)/log(0.3/0.7),log(0.45)/log(0.45/0.55))+0.1,labels=c("80-20","70-30","55-45"))


legend("topleft",c("Super-spreading","Super-spreaders"),pch=c(1,4),col=1,bty="n")
mtext("(f)",3,1,adj=0)
mtext("Pareto Index",2,2.5,cex=0.8)
mtext("Mean EIR",1,2,cex=0.8)
dev.off()

# Outliers table
HBR<-read.csv(paste0(directory, "outputs/HBR_outliers.csv"))
EIR<-read.csv(paste0(directory, "outputs/EIR_outliers.csv"))
HBR$type<-"HBR"
EIR$type<-"EIR"
outlier<-rbind.data.frame(HBR,EIR)
monthnames<-c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
yearnames<-c("2011", "2012", "2013", "2014", "2015")
m<-monthnames
y<-yearnames
monthyearnames<-(c(paste(m[10],y[1]),
                   paste(m[11],y[1]),
                   paste(m[12],y[1]),
                   paste(m[1],y[2]),
                   paste(m[2],y[2]),
                   paste(m[3],y[2]),
                   paste(m[4],y[2]),
                   paste(m[5],y[2]),
                   paste(m[6],y[2]),
                   paste(m[7],y[2]),
                   paste(m[8],y[2]),
                   paste(m[9],y[2]),
                   paste(m[10],y[2]),
                   paste(m[11],y[2]),
                   paste(m[12],y[2]),
                   paste(m[1],y[3]),
                   paste(m[2],y[3]),
                   paste(m[3],y[3]),
                   paste(m[4],y[3]),
                   paste(m[5],y[3]),
                   paste(m[6],y[3]),
                   paste(m[7],y[3]),
                   paste(m[8],y[3]),
                   paste(m[9],y[3]),
                   paste(m[10],y[3]),
                   paste(m[11],y[3]),
                   paste(m[12],y[3]),
                   paste(m[1],y[4]),
                   paste(m[2],y[4]),
                   paste(m[3],y[4]),
                   paste(m[4],y[4]),
                   paste(m[5],y[4]),
                   paste(m[6],y[4]),
                   paste(m[7],y[4]),
                   paste(m[8],y[4]),
                   paste(m[9],y[4]),
                   paste(m[10],y[4]),
                   paste(m[11],y[4]),
                   paste(m[12],y[4]),
                   paste(m[1],y[5]),
                   paste(m[2],y[5]),
                   paste(m[3],y[5])
))
outlier$month<-factor(outlier$month,1:42,monthyearnames)
# For the period of October 2013 to March 2014, for which there were low overall mosquito counts and no sporozoite-positive mosquitoes, there is no EIR or SR data
outlier[-which(outlier$type=='EIR'&as.numeric(outlier$month)%in%25:30),]
write.csv(outlier[-which(outlier$type=='EIR'&as.numeric(outlier$month)%in%25:30),],
          paste0(directory, "outputs/outlier_table.csv"))
