

data<-read.csv("www/4tech.csv")
prices<-as.timeSeries(data)
returns<-timeSeries::returns(prices, method="discrete")
symbols<-names(prices)

hexHist<-hexBinning(returns[,c("FB","AAPL")], bin=20)
plot(hexHist, xlab="FB", ylab="AAPL", col=rev(greyPalette(20)))

sqrHist<-squareBinning(returns[,c("FB","AAPL")], bin=20)
plot(sqrHist, xlab="FB", ylab="AAPL", col=rev(greyPalette(20)))


FB<-as.vector(returns[,"FB"])
sorted.FB<-sort(FB)

AAPL<-as.vector(returns[,"AAPL"])
sorted.AAPL<-sort(AAPL)

bin<-20

plot(FB,AAPL,type="n")

FB.min<-min(FB)
FB.max<-max(FB)

quantile(FB, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))



# Cuantil por cuantil cuantos puntos caen en cada cuantil
#
#



# Calcular las distancias entre los puntos
#
#

