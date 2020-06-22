beta <- c("0.19-0.1","0.28-0.08","0.31-0.1","0.22-0.02")
listMultiply<- c(1,2,3,4)
networkName <- "LAZEGA"
statistic <- NULL
networkStatistic <- NULL
for(m in 1:length(listMultiply))
{mnoznik <-listMultiply[m]
  for(i in 1:length(beta))
  {    
  zmienna <- paste(paste (beta[i],mnoznik,sep="-"),".dat",sep="")
files <- c("SIRonly","SIS&SIR_blocking","SIS&SIR_default")
only<-read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-",paste(files[1],networkName, sep=""),sep=""),zmienna,sep="-"), sep=";")
blocking <- read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-",paste(files[2],networkName, sep=""),sep=""), zmienna,sep="-"), sep=";")
default <- read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-",paste(files[3],networkName, sep=""),sep=""),zmienna,sep="-"), sep=";")

maxI_SIR <- round(max(only$allI),2)
maxI_default <- round( max(default$allI),2)
maxI_blocking <-round(max(blocking$allI), 2)
maxi_SIR <- min(which(only$allI == max(only$allI)))
maxi_default <- min(which(default$allI == max(default$allI)))
maxi_blocking <- min(which(blocking$allI == max(blocking$allI)))

statistic <- cbind(listMultiply[m],beta[i], maxI_SIR,maxI_blocking,maxI_default, maxi_SIR, maxi_blocking, maxi_default)
title<- paste(paste("Wykres - Brutalresize-now", networkName, sep="-"), paste (beta[i],mnoznik,sep="-"), sep="")
print(statistic)

png(file=paste(title, ".png", sep=""), 800, 700)
matplot(1:length(only$allI), only$allI,xlab= "dzieñ epidemii (iteracja)", ylab="liczba zainfekowanych aktorów [%]",xlim=c(0,110), ylim=c(0,100),pch=10, col="#FF0000")
box()
axis(side=2, at=seq(0,100,5))
axis(side=1, at=seq(0,110,5))
arrows(1:length(only$allI),only$allI+only$sdAllI,1:length(only$allI),only$allI-only$sdAllI,  angle=90, code=3, length=0.1, col ="#ff9a9a")
arrows(1:length(blocking$allI),blocking$allI+blocking$sdAllI,1:length(blocking$allI),blocking$allI-blocking$sdAllI,  angle=90, code=3, length=0.1, col ="#cc9aff")
arrows(1:length(default$allI),default$allI+default$sdAllI,1:length(default$allI),default$allI-default$sdAllI,  angle=90, code=3, length=0.1, col ="#37f437")
points(1:length(blocking$allI), blocking$allI,pch=10,col="#8000FF")
points(1:length(default$allI), default$allI, col= "#088A08",pch=10)
points(1:length(only$allI), only$allI,pch=10, col="#FF0000")
legend(50,40 ,legend=c( "SIR","SIR z blokowaniem UAU ", "SIR i UAU"),col=c("#FF0000","#8000FF","#088A08"), pch=10, cex=1.2) #, cex= 1.3)

dev.off()
#dev.off()
networkStatistic <- rbind(networkStatistic,statistic)

# bmp(file=paste(title, "_errorbars.bmp", sep=""), 1300, 1000)
# plot(1:length(only$allI), only$allI,xlab= "czas epidemii (iteracja)", ylab="% liczba zainfekowanych aktorów [%]",xlim=c(0,100), ylim=c(0,100),pch=1, col="#FF0000", main=title)
# points(1:length(blocking$allI), blocking$allI,pch=1,col="#8000FF")
# points(1:length(default$allI), default$allI, col= "#088A08",pch=1)
# axis(side=2, at=seq(0,150,5))
# axis(side=1, at=seq(0,150,5))
# legend(60, 70, legend=c( "SIR","SIR z blokowaniem UAU ", "SIR i UAU"),col=c("#FF0000","#8000FF","#088A08"), pch=1, cex= .8)
# arrows(1:length(only$allI),only$allI+only$sdAllI,1:length(only$allI),only$allI-only$sdAllI,  angle=90, code=3, length=0.1, col ="#F78181")
# arrows(1:length(blocking$allI),blocking$allI+blocking$sdAllI,1:length(blocking$allI),blocking$allI-blocking$sdAllI,  angle=90, code=3, length=0.1, col ="#ECCEF5")
# arrows(1:length(default$allI),default$allI+default$sdAllI,1:length(default$allI),default$allI-default$sdAllI,  angle=90, code=3, length=0.1, col ="#CEF6CE")
# dev.off()
  }}

write.table(networkStatistic, file = paste(paste("networkSummary",networkName, sep = ""),".dat",sep=""))
