library("foreign")
library(MASS)
setwd("/homes/georgoff/georgoff.github.io/superspreading/")
#Load data
trap_data<-read.dta("CDC light trap data through March 2015.dta")
# Weights from Su
tororo_weights<-read.table("Biting_weight_Tororo.txt")
kanungu_weights<-read.table("Biting_weight_Kanungu.txt")
jinja_weights<-read.table("Biting_weight_Jinja.txt")
weights<-rbind.data.frame(tororo_weights,kanungu_weights,jinja_weights)

#Fix months
x<-trap_data$monthyear/12
y<-x+1960
year<-trunc(y)
month<-round((y-year)*12)+1
trap_data<-data.frame(trap_data, month, year)
rm(month, year, x, y)
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

thirtymonth<-as.factor(c(m[10], m[11], m[12], 
                         m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12], 
                         m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12],
                         m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12],
                         m[1], m[2], m[3]))
thirtymonth <- factor(thirtymonth,levels(thirtymonth)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
thirtylabel<-c("10/11","11/11","12/11","1/12","2/12","3/12","4/12","5/12","6/12","7/12","8/12","9/12",
               "10/12","11/12","12/12","1/13","2/13","3/13","4/13","5/13","6/13","7/13","8/13","9/13",
               "10/13","11/13","12/13","1/14","2/14","3/14","4/14","5/14","6/14","7/14","8/14","9/14",
               "10/14","11/14","12/14","1/15","2/15","3/15")
twelvemonth <-as.factor(monthnames)
twelvemonth <- factor(twelvemonth,levels(twelvemonth)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
months<-as.factor(c(m[10], m[11], m[12], 
                    m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12], 
                    m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12],
                    m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9],m[10], m[11], m[12],
                    m[1], m[2], m[3]))
months <- factor(months,levels(months)[c(5,4,8,1,9,7,6,2,12,11,10,3)])

rm(m,y) 


#Drop irrelevant variables
data<-subset(trap_data,select=c(1:3,5,21,25,26,27:32))

#Drop all entries for which traps were not checked
data<-data[which(data$collected=="Yes"),]

#Find numerator by adding up all positive mosquitos for a given month, year and site
numerator<-tapply(data$numberpositive, list(data$monthyear, data$siteid),sum, na.rm=TRUE)
#Find denomenator by adding up all tested mosquitos for a given month and site
denomenator<-tapply(data$numbertested, list(data$monthyear, data$siteid),sum, na.rm=TRUE)

#Find SR
sr_monthyear<-numerator/denomenator
rownames(sr_monthyear)<-monthyearnames

tapply(data$totalanopheles, list(data$monthyear, data$siteid),sum, na.rm=TRUE)
tapply(data$numbercaught, list(data$monthyear, data$siteid),sum, na.rm=TRUE)

# sr_monthyear["Oct 2014","Kanungu"]<-mean(sr_monthyear[c("Oct 2011","Oct 2012","Oct 2013"),"Kanungu"])
# sr_monthyear["Nov 2014","Kanungu"]<-mean(sr_monthyear[c("Nov 2011","Nov 2012","Nov 2013"),"Kanungu"])
# 
# sr_monthyear["Dec 2014","Tororo"]<-mean(sr_monthyear[c("Dec 2011","Dec 2012","Dec 2013"),"Tororo"])
# sr_monthyear["Dec 2014","Kanungu"]<-mean(sr_monthyear[c("Dec 2011","Dec 2012","Dec 2013"),"Kanungu"])
# sr_monthyear["Dec 2014","Jinja"]<-mean(sr_monthyear[c("Dec 2011","Dec 2012","Dec 2013"),"Jinja"])
# 
# sr_monthyear["Jan 2015","Tororo"]<-mean(sr_monthyear[c("Jan 2012","Jan 2013","Jan 2014"),"Tororo"])
# sr_monthyear["Jan 2015","Kanungu"]<-mean(sr_monthyear[c("Jan 2012","Jan 2013","Jan 2014"),"Kanungu"])
# sr_monthyear["Jan 2015","Jinja"]<-mean(sr_monthyear[c("Jan 2012","Jan 2013","Jan 2014"),"Jinja"])
# 
# sr_monthyear["Feb 2015","Tororo"]<-mean(sr_monthyear[c("Feb 2012","Feb 2013","Feb 2014"),"Tororo"])
# sr_monthyear["Feb 2015","Kanungu"]<-mean(sr_monthyear[c("Feb 2012","Feb 2013","Feb 2014"),"Kanungu"])
# sr_monthyear["Feb 2015","Jinja"]<-mean(sr_monthyear[c("Feb 2012","Feb 2013","Feb 2014"),"Jinja"])
# 
# sr_monthyear["Mar 2015","Tororo"]<-mean(sr_monthyear[c("Mar 2012","Mar 2013","Mar 2014"),"Tororo"])
# sr_monthyear["Mar 2015","Kanungu"]<-mean(sr_monthyear[c("Mar 2012","Mar 2013","Mar 2014"),"Kanungu"])
# sr_monthyear["Mar 2015","Jinja"]<-mean(sr_monthyear[c("Mar 2012","Mar 2013","Mar 2014"),"Jinja"])

# variance<-tapply(data$numberpositive/data$numbertested, list(data$monthyear, data$siteid),var, na.rm=TRUE)
# n<-table(data$monthyear, data$siteid)
# error_monthyear<-sqrt(variance)/sqrt(n)

#Find avg sitewide SR
# sr_avg<-tapply(data$numberpositive, data$siteid,sum, na.rm=TRUE)/tapply(data$numbertested, data$siteid,sum, na.rm=TRUE)

#Add to data frame
site_month_sr<-rep(NA,nrow(data))
data<-cbind.data.frame(data,site_month_sr)
for (i in 621:662){
  data$site_month_sr[which(data$siteid=="Jinja" & data$monthyear==i)]<-sr_monthyear[i-620,1]
  data$site_month_sr[which(data$siteid=="Kanungu" & data$monthyear==i)]<-sr_monthyear[i-620,2]
  data$site_month_sr[which(data$siteid=="Tororo" & data$monthyear==i)]<-sr_monthyear[i-620,3]
}

data$numbertested[which(data$totalanopheles==0&is.na(data$numbertested))]<-0
data$numberpositive[which(data$totalanopheles==0&is.na(data$numberpositive))]<-0

# Find the expected hPfEIR
data$modeled_hh_eir<-data$site_month_sr*data$totalanopheles

# Find the true hPfEIR
data$true_hh_eir<-data$numberpositive/data$numbertested*data$totalanopheles
data$true_hh_eir[which(data$totalanopheles==0)]<-0


colnames(data)[4]<-"hbd"
colnames(data)[16]<-"hid"
colnames(data)[15]<-"modeled_eir"

data<-merge(data,weights,by.x="hhid",by.y="uhhid")

data<-data[,c("siteid","hhid","date","monthyear","month","year","hbd","numbertested","numberpositive","modeled_eir","hid","weight")]

write.csv(data,"Obs_all.csv")

signif(tapply(data$hbd>50,data$siteid,mean)*100,2)
tapply(data$hbd>50,data$siteid,sum)
