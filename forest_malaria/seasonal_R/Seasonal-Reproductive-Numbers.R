library(RColorBrewer)
colN <- 20

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,colN), col=sample(col_vector, colN))


makeVCrel = function(pattern="sin", Nyr = 5, showit=FALSE){
  VC = switch(pattern, 
              sin = pmax(0.5 + cos (2*pi*c(1:365)/365),0), 
              sin1 = 1.1 + cos (2*pi*c(1:365)/365), 
              block = c(rep(2,183), rep(.1,182)))
  VC = VC/mean(VC)
  VCrel = rep(VC, Nyr)
  
  if(showit == TRUE) 
    plot(1:(365*Nyr)/365, VCrel, type = "l", xlab = "Year", 
         ylim = c(0, max(VCrel)), ylab = "Seasonal Pattern")
  VCrel
}


VCrel = makeVCrel(pattern="sin", Nyr = 10, showit=TRUE)
VCrel = makeVCrel(pattern="block", showit=TRUE)





makeVCtime = function(EIP=12, p=.9, a=.3, tol = 0.001, plot=FALSE){
  # EIP: Entomological Innoculation Rate - time until mosquitoes become infectious
  # p: daily probability of survival of a mosquito
  # a: blood feeting rate
  # tol: smallest acceptable value for vectorial capacity
  
  # calculate VCn
  # this is the number of days it takes until vectorial
  # capacity reaches its minimum value (tol)
  VCn =  log(tol)/log(p)
  
  # first set VC to zero until the EIP is over,
  # then start killing off mosquitoes according
  # to their daily probability of survival
  # then scale by human biting rate to get VC
  VCtime = c(rep(0, EIP-1), p^c(1:VCn)*a)
  
  # shows how much of the total VC is "left"
  VCtime = VCtime/sum(VCtime)
  
  # plot
  if(plot == TRUE) plotVCtime(VCtime, EIP)
  VCtime
}

plotVCtime = function(VCtime, EIP){
  ix = 1:length(VCtime)
  
  # plot index of VCtime on x axis,
  # VCtime scaled to maximum VCtime on y axis
  plot(ix, VCtime/max(VCtime), type ="n", 
       xlab = "Day", ylab = expression(VC(t)), main = "Temporal Dispersion of VC")
  vix = ix[-c(1:(EIP-1))]
  lines(vix, VCtime[vix]/max(VCtime))
  segments(EIP,.2,EIP,1, lty = 2)
  text(EIP, .1, "EIP")
}


VCtime = makeVCtime(plot=TRUE)





makeDtime = function(D=38, plot=FALSE){
  # Human Transmission Capacity (HTC)
  
  # wait for the end of the EIP and then create
  # a gamma distribution
  Dtime = c(rep(0, 17), dgamma(1:365, 2, .05))
  
  # plot proportion of total HTC over time
  if(plot==TRUE)
    plot(1:365, Dtime[1:365]/max(Dtime), type = "l", ylab = expression(D(t)), xlab = "Time (Days)")
  D*Dtime/sum(Dtime)
}
Dtime = makeDtime(plot=TRUE) 




makeRtime = function(Re = 1, VCtime=makeVCtime(), Dtime=makeDtime(), plot=FALSE){
  Rt = 1:(365+length(Dtime))
  Rtime = Rt*0
  ix = 1:length(VCtime)
  ixx = 1:length(Dtime)
  #for(t in ix) Rtime[t+ixx] = Rtime[t+ixx] + VCtime[t]*Dtime
  for(t in ixx) Rtime[t+ix] = Rtime[t+ix] + VCtime*Dtime[t]
  
  Rtime = Rtime[1:365]
  Rtime = Re*Rtime/sum(Rtime)
  if (plot == TRUE){
    plot(c(1:365), Re*Rtime/max(Rtime), type = "l", ylab = expression(R[0](t)), xlab = "Time (Days)", col = "blue")
    lines(1:365, Re*cumsum(Rtime/sum(Rtime)))
  }
  Rtime
}

Rtime = makeRtime(2, plot=TRUE)
