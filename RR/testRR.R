

data<-read.csv("www/4tech.csv")
prices<-as.timeSeries(data)
returns<-timeSeries::returns(prices, method="discrete")
symbols<-names(prices)

par(mfrow=c(1,1))
hexHist<-hexBinning(returns[,c("FB","AAPL")], bin=20)
plot(hexHist, xlab="FB", ylab="AAPL", col=rev(greyPalette(20)))

sqrHist<-squareBinning(returns[,c("FB","AAPL")], bin=30)
plot(sqrHist, xlab="FB", ylab="AAPL", col=rev(greyPalette(30)))


FB<-as.vector(returns[,"FB"])
sorted.FB<-sort(FB)

AAPL<-as.vector(returns[,"AAPL"])
sorted.AAPL<-sort(AAPL)

#vpar<-par(mfrow=c(1,2))

bin<-30

plot(FB,AAPL,type="n")

#points(FB,AAPL,type="p", pch=19, cex=0.1, col="darkblue")

FB.min<-min(FB)
FB.max<-max(FB)

xbinsize<-(FB.max-FB.min)/bin

AAPL.min<-min(AAPL)
AAPL.max<-max(AAPL)
ybinsize<-(AAPL.max-AAPL.min)/bin

freqmatrix<-matrix(rep(0, len=bin*bin), nrow=bin, ncol=bin)


for (xdata in 1:length(FB)) {
  xbin<-1
  while ((FB.min+xbinsize*xbin) < FB[xdata]) {
      xbin<-xbin+1
  }
  ybin<-1
  while ((AAPL.min+ybinsize*ybin) < AAPL[xdata]) {
      ybin<-ybin+1
  }
  #print(paste("FB[xdata]=",FB[xdata],"AAPL[xdata]=",AAPL[xdata],"xbin=",xbin,"ybin=",ybin))
  freqmatrix[xbin-1,ybin-1]<-freqmatrix[xbin-1,ybin-1] + 1
}

# Hay que calcular el máximo y el mínimo
# y dividir los colores entre el n´úmero de bins
# darle a cada frecuencia su tono de color
fmmax<-max(freqmatrix)

bincolors<-rev(greyPalette(fmmax))

colormatrix<-matrix(rep("#ffffff", len=bin*bin), nrow=bin, ncol=bin)

for (j in 1:bin) {
  for (i in 1:bin) {
    if (freqmatrix[i,j] != 0)
      colormatrix[i,j]<-bincolors[freqmatrix[i,j]]
  }
}


for (y in 0:(bin-1)) {
  for (x in 0:(bin-1)) {
    xs<-c(x*xbinsize, (x+1)*xbinsize, (x+1)*xbinsize, x*xbinsize)+FB.min
    ys<-c(y*ybinsize, y*ybinsize, (y+1)*ybinsize, (y+1)*ybinsize)+AAPL.min
    polygon(xs, ys, xpd = NA, col = colormatrix[x,y], border = NA)
  }
}
points(FB,AAPL,type="p", pch=19, cex=0.01, col="black")
rug(sorted.FB, ticksize = 0.01, side = 3)
rug(sorted.AAPL, ticksize = 0.01, side = 4)

sqrHist<-squareBinning(returns[,c("FB","AAPL")], bin=30)
plot(sqrHist, xlab="FB", ylab="AAPL", col=rev(greyPalette(30)))

#par(vpar)

#quantile(sorted.FB, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
#length(which(sorted.FB>0.025000978))/length(FB)

plot(FB,AAPL,type="p", pch=19, cex=0.1, col="darkblue")
rug(sorted.FB, ticksize = 0.01, side = 3)
rug(sorted.AAPL, ticksize = 0.01, side = 4)

hist(sorted.FB, breaks = 100)
rug(sorted.FB, ticksize = 0.01, side = 1)

hist(sorted.AAPL, breaks = 100)
rug(sorted.AAPL, ticksize = 0.01, side = 1)

#
pairdistance<-abs(FB-AAPL)
hist(pairdistance, breaks = 100)
rug(pairdistance, ticksize = 0.01, side = 1)

pairdistance<-FB-AAPL
hist(pairdistance, breaks = 100)
rug(pairdistance, ticksize = 0.01, side = 1)

# Calcular las distancias entre los puntos
#
edistance<-function(x,y,abs=TRUE) {
  if (abs) abs(x-y)
  else x-y
}

dmatrix<-matrix(rep(0,len=length(FB)*length(AAPL)), nrow=length(FB), ncol=length(AAPL))

for (i in 1:length(FB)) {
  for (j in 1:length(AAPL)) {
    dmatrix[i,j]<-edistance(FB[i], AAPL[j], abs = FALSE)
  }
}

dvector<-as.vector(dmatrix)
hist(dvector, breaks=100)
rug(dvector, ticksize = 0.01, side = 1)
