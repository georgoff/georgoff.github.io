colN <- 20

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
global_col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

makeVCrel = function(pattern="sin", VCes=rep(1,365), Nyr = 5, showit=FALSE){
  VC = switch(pattern,
              sin = pmax(0.5 + cos (2*pi*c(1:365)/365),0),
              sin1 = 1.1 + cos (2*pi*c(1:365)/365),
              block = c(rep(2,183), rep(.1,182)))
  VC = VC/mean(VC)
  VC = VC/VCes
  VCrel = rep(VC, Nyr)
  
  if(showit == TRUE)
    plot(1:(365*Nyr)/365, VCrel, type = "l", xlab = "Year",
         ylim = c(0, max(VCrel)), ylab = "Seasonal Pattern")
  VCrel
}

makeVCes = function(pattern = "sig", maxES = 10, timing=0){
  tt = 1:365
  VCes = switch(pattern,
                sig = 1+maxES*(exp(-.02*tt)/(1+exp(-.02*tt))),
                block = c(rep(maxES, 180), rep(1, 180)),
                rep(1,365)
  )
  c(VCes, VCes)[365+tt-timing]
}

# sparse_seasonal_input = function(file)

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

makeRtime = function(Re = 1, VCtime=makeVCtime(), Dtime=makeDtime(), plot=FALSE){
  # vector of integers with length 1 year plus length of HTC
  Rt = 1:(365+length(Dtime))
  
  # set all values of Rt to zero
  Rtime = Rt*0
  
  # integers with length of vector transmission
  ix = 1:length(VCtime)
  
  # integers with length of human transmission
  ixx = 1:length(Dtime)
  
  # alternate method:
  #for(t in ix) Rtime[t+ixx] = Rtime[t+ixx] + VCtime[t]*Dtime
  
  # technically this is a convolution
  # once a mosquito becomes infectious, we add on a scaled version
  # of the HTC curve (scaled by how likely an infectious mosquito bite
  # is)
  # we then repeat this over and over
  for(t in ixx) Rtime[t+ix] = Rtime[t+ix] + VCtime*Dtime[t]
  
  # subset to just one year
  Rtime = Rtime[1:365]
  
  # scale by the effective R value that was an input
  Rtime = Re*Rtime/sum(Rtime)
  
  if (plot == TRUE){
    plot(c(1:365), Re*Rtime/max(Rtime), type = "l", ylab = expression(R[0](t)), xlab = "Time (Days)", col = "blue")
    lines(1:365, Re*cumsum(Rtime/sum(Rtime)))
  }
  Rtime
}

# TODO: make this more customizable:
generations_graph <- function(Re, num_gen, Rtime, VCrel, col_vector = global_col_vector) {
  # colN <- 20
  # 
  # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  # col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # # pie(rep(1,colN), col=sample(col_vector, colN))
  
  Rtime = Re*Rtime/sum(Rtime)
  RRe = sum(Rtime)
  plot(1:length(VCrel), 8*VCrel/max(VCrel), type = "l", lwd=1, col = grey(0.5),
       ylim = c(0,9), ylab = "", yaxt = "n", xaxt = "n", xlab = "Time (Years)",
       main = paste0(as.character(num_gen), " Generations"))
  axis(1, c(0:10)*365, c(0:10))
  segments(1,0,1,1)
  genN = c(0,1, rep(0,3648))
  
  for(i in 1:num_gen){
    genN = nextGen(genN, Rtime, VCrel, yoffset=i/6, mag=3.5, clr=col_vector[i%%colN +1], norm=TRUE, flr=0.008)
    RRe = c(RRe, sum(genN))
  } 
}

nextGen = function(gen, Rtime, VCrel, clr = "black", yoffset=0, mag=1, norm = FALSE, flr= .005){
  L = length(Rtime)
  xix = 1:L
  genN = c(1:3650)*0
  for(t in 1:3650){
    # how many days into the year is it? add 1 to that
    tt = t%%365+1
    
    # don't know what's going on here!
    genN[t+xix] = genN[t+xix] + Rtime*gen[t]*VCrel[tt]
  }
  genN = genN[1:3650]
  ix = which(genN > flr)
  if(norm == TRUE)
    lines(c(1:3650)[ix], mag*genN[ix]/max(genN)+yoffset, col = clr)
  if(norm == FALSE)
    lines(c(1:3650)[ix], mag*genN[ix]+yoffset, col = clr)
  genN
}

nextGen1 = function(gen, Rtime, VCrel, clr = "black", xoffset=0, yoffset=0, mag=.95, lines=FALSE, nextline=FALSE){
  xix = 1:365
  genN = c(1:2000)*0
  for(t in 1:1635){
    tt = t%%365+1
    genN[t+xix] = genN[t+xix] + Rtime*gen[tt]*VCrel[tt]
  }
  genN = genN[366:730]
  if(lines == TRUE)
    lines(1:365 + xoffset, mag*genN/max(genN) + yoffset, col = clr, lwd=2)
  if(nextline == TRUE)
    lines(1:365 + xoffset, mag*genN/max(genN) + yoffset-1, col = grey(0.85), lwd=2)
  genN
}

plotEigen = function(NGEN, Rtime, VCrel, col_vector = global_col_vector){
  
  plot(1:365,NGEN+2+.95*(VCrel[1:365]/max(VCrel[1:365])), lwd=2, type = "l", ylim = c(0,NGEN+2.5), col = "darkblue", xaxt = "n", xlab = "Month of Year", ylab = "Seasonal Pattern by Generation", yaxt = "n", main = "Generations               ")
  segments(0,NGEN+2,0,NGEN+2.95, lwd=2)
  segments(0,NGEN+1,0,NGEN+1.95, lwd=2, col = grey(0.55))
  axis(1, c(0:12)*365/12, c(0:12)) 
  axis(2, c(0:(NGEN+2))+.5, c(paste(">", NGEN*2), paste(">", NGEN), c((NGEN):0)))
  
  
  genN = c(1, rep(0, 364))
  RRe = sum(genN)
  for(i in 1:NGEN){
    genN = nextGen1(genN, Rtime, VCrel, yoffset = NGEN+2-i, nextline=TRUE, lines=TRUE,col_vector[i])
    RRe = c(RRe, sum(genN))
  } 
  for(i in 1:NGEN){
    genN = nextGen1(genN, Rtime, VCrel, clr = grey(0.5), yoffset = 1, lines=TRUE)
    RRe = c(RRe, sum(genN))
  } 
  
  for(i in -1:(NGEN+2))
    segments(0, i-0.02, 365, i-0.02, col = grey(0.8))
  
  genN = nextGen1(genN, Rtime, VCrel, clr = grey(0.5), lines=TRUE)
  c(RRe, sum(genN))
  eigs = RRe[-1]/RRe[-length(RRe)]
}

nextGenSeas = function(gen, Rtime, VCrel,norm=FALSE){
  L = length(Rtime)
  xix = 1:L
  tt = 1:length(gen)
  genN = tt*0
  for(t in tt){
    ty = t%%365+1
    genN[t+xix] = genN[t+xix] + Rtime*gen[t]*VCrel[ty]
  }
  genN=genN[tt]
  if(norm==TRUE) genN = genN/sum(genN, na.rm=TRUE)
  genN
}

nextGenSeasMat = function(gen, Rtime, VCrel, Nyr, NN=40, norm=FALSE, showit=TRUE){
  for(i in 1:NN){
    # genN = nextGenSeas(genN, Rtime, VCrel, norm)
    genN = nextGenSeas(gen, Rtime, VCrel, norm)
    gen = rbind(gen, genN)
  }
  if(showit==TRUE) generationsPlot(gen, Nyr, VCrel)
  gen
}

generationsPlot = function(gen, Nyr, VCrel, col_vector = global_col_vector){
  tot = colSums(gen[-1,], na.rm=T)
  tt = 1:dim(gen)[2]
  plot(tt, tot, type = "l", xaxt = "n", xlab = "Time (Years)", ylab = "Generations", ylim = c(0,1.15*max(tot, na.rm=T))) 
  axis(1, 365*c(0:Nyr), c(0:Nyr))
  NN = dim(gen)[1]
  for(i in NN:1){
    if(i==1) toti = gen[1,]
    if(i>1) toti = colSums(gen[1:i,], na.rm=T)
    xx = c(tt, max(tt), 1)
    yy = c(toti, 0, 0) 
    clr = col_vector[i%%colN +1]
    polygon(xx, yy, col= clr, border=clr)
    xpos = max(tt)*(i+2)/(NN+4)
    ypos = 1.05*max(tot)
    points(xpos, ypos, pch = 15, col = clr) 
    if(i%%5 == 1) text(xpos, ypos, i-1, pos=3)
  }
  lines(tt, .8*max(tot)*VCrel/max(VCrel), col = grey(0.55))
}

intervention = function(start_month, duration) {
  
}