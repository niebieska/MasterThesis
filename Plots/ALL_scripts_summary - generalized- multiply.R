beta <-c("0.19-0.1","0.28-0.08","0.22-0.02","0.31-0.1" )
listMultiply<- c(1,2,3,4)
networkName <- "MoscowAthletics2013"
statistic <- NULL
networkStatistic <- NULL
  for(i in 1:length(beta))
  {    

files <- c(paste(beta[i],"-1.dat",sep=""),paste(beta[i],"-2.dat",sep=""),paste(beta[i],"-3.dat",sep=""),paste(beta[i],"-4.dat",sep=""))
first<-read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-SIS&SIR_default",paste(networkName,files[1], sep="-"),sep=""),sep=""),sep=";")
second<-read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-SIS&SIR_default",paste(networkName,files[2], sep="-"),sep=""),sep=""),sep=";")
third <- read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-SIS&SIR_default",paste(networkName,files[3], sep="-"),sep=""),sep=""),sep=";")
fourth <- read.csv(paste(paste("C:/Users/Paulina/Desktop/Plots/results_SIR-SIS&SIR_default",paste(networkName,files[4], sep="-"),sep=""),sep=""),sep=";")

maxI_first <- round(max(first$allI),2)
maxI_second<-round(max(second$allI), 2)
maxI_third<-round(max(third$allI), 2)
maxI_fourth <- round( max(fourth$allI),2)

maxi_first<- min(which(first$allI == max(first$allI)))
maxi_second <- min(which(second$allI == max(second$allI)))
maxi_third <- min(which(third$allI == max(third$allI)))
maxi_fourth <- min(which(fourth$allI == max(fourth$allI)))


statistic <- cbind(listMultiply[m],beta[i], maxI_first,maxI_second,maxI_third,maxI_fourth, maxi_first, maxi_second,maxi_third, maxi_fourth)
title<- paste(paste("Wykres - mnoznik -pwt", networkName, sep="-"), beta[i], sep="")
print(statistic)
png(file=paste(title, ".png", sep=""), 800, 700)

matplot(1:length(first$allI), first$allI,xlab= "dzieñ epidemii (iteracja)", ylab="liczba zainfekowanych aktorów [%]",xlim=c(0,100), ylim=c(0,100),pch=10, col="#00FF00")

axis(side=2, at=seq(0,100,5))
axis(side=1, at=seq(0,100,5))

arrows(1:length(first$allI),first$allI+first$sdAllI,1:length(first$allI),first$allI-first$sdAllI,  angle=90, code=3, length=0.1, col ="#39a139")
#arrows(1:length(third$allI),third$allI+third$sdAllI,1:length(third$allI),third$allI-third$sdAllI,  angle=90, code=3, length=0.1, col ="#a6ff4c")
arrows(1:length(second$allI),second$allI+second$sdAllI,1:length(second$allI),second$allI-second$sdAllI,  angle=90, code=3, length=0.1, col ="#4cdb93")
arrows(1:length(fourth$allI),fourth$allI+fourth$sdAllI,1:length(fourth$allI),fourth$allI-fourth$sdAllI,  angle=90, code=3, length=0.1, col ="#80ff00")


points(1:length(first$allI), first$allI,pch=10, col="#088A08")
points(1:length(second$allI), second$allI, col="#00cc66",pch=10)
# points(1:length(third$allI), third$allI,pch=10,col="#ffa500")#ffa500
points(1:length(fourth$allI), fourth$allI, col="#80ff00",pch=10)
legend(70, 50, legend=c( "SIR i UAU dla  x=1","SIR i UAU dla  x=2", "SIR i UAU dla  x=4"),col=c("#00cc66","#088A08","#80ff00"), pch=10, cex= .8)
#legend(60, 50, legend=c( "SIR i UAU dla  x=1","SIR i UAU dla  x=2", "SIR i UAU dla  x=3","SIR i UAU dla  x=4"),col=c("#00FF00","#7CFC00","#7CFC00","#006400"), pch=10, cex= .8)
dev.off()
networkStatistic <- rbind(networkStatistic,statistic)

# bmp(file=paste(title, "_errorbars.bmp", sep=""), 1500, 1000)
# plot(1:length(first$allI), first$allI,xlab= "time", ylab="% actors",xlim=c(0,100), ylim=c(0,100),pch=1, col="#FF0000", main=title)
# points(1:length(third$allI), third$allI,pch=1,col="#8000FF")
# points(1:length(fourth$allI), fourth$allI, col= "#088A08",pch=1)
# axis(side=2, at=seq(0,150,5))
# axis(side=1, at=seq(0,100,5))
# legend(60, 70, legend=c( "SIR","blokowanie", "symbioza"),col=c("#FF0000","#8000FF","#088A08"), pch=1, cex= .8)
# arrows(1:length(first$allI),first$allI+first$sdAllI,1:length(first$allI),first$allI-first$sdAllI,  angle=90, code=3, length=0.1, col ="#F78181")
# arrows(1:length(third$allI),third$allI+third$sdAllI,1:length(third$allI),third$allI-third$sdAllI,  angle=90, code=3, length=0.1, col ="#ECCEF5")
# arrows(1:length(fourth$allI),fourth$allI+fourth$sdAllI,1:length(fourth$allI),fourth$allI-fourth$sdAllI,  angle=90, code=3, length=0.1, col ="#CEF6CE")
# dev.off()
  }
write.table(networkStatistic, file = paste(paste("networkSummary-multiply",networkName, sep = ""),".dat",sep=""))
