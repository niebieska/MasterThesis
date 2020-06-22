beta <-c("0.19-0.1","0.28-0.08","0.22-0.02","0.31-0.1" )
listMultiply<- c(1,2,3,4)

for(m in 1:length(listMultiply))
{mnoznik <-listMultiply[m]
  for(i in 1:length(beta))
  {    
  zmienna <- paste(paste (beta[i],mnoznik,sep="-"),".dat",sep="")

only<-read.csv(paste("~/MasterThesis/Eksperiments/Plots/data/results_SIR-SIRonlyEU",zmienna,sep="-"), sep=";")
blocking <- read.csv(paste("~/MasterThesis/Eksperiments/Plots/data/results_SIR-SIS&SIR_blockingEU", zmienna,sep="-"), sep=";")
default <- read.csv(paste("~/MasterThesis/Eksperiments/Plots/data/results_SIR-SIS&SIR_defaultEU",zmienna,sep="-"), sep=";")


title<- paste("wykres sieci EU", paste (beta[i],mnoznik,sep="-"), sep="")

bmp(file=paste(title, ".bmp", sep=""), 1300, 1000)
plot(1:length(only$allI), only$allI,xlab= "time", ylab="% actors",xlim=c(0,150), ylim=c(0,100),pch=1, col="#FF0000", main = title)
axis(side=2, at=seq(0,150,5))
axis(side=1, at=seq(0,100,5))
points(1:length(blocking$allI), blocking$allI,pch=1,col="#8000FF")
points(1:length(default$allI), default$allI, col= "#088A08",pch=1)

legend(60, 70, legend=c( "SIR","blokowanie", "symbioza"),col=c("#FF0000","#8000FF","#088A08"), pch=1, cex= .8)
dev.off()
 

bmp(file=paste(title, "_errorbars.bmp", sep=""), 1500, 1000)
plot(1:length(only$allI), only$allI,xlab= "time", ylab="% actors",xlim=c(0,100), ylim=c(0,100),pch=1, col="#FF0000", main=title)
points(1:length(blocking$allI), blocking$allI,pch=1,col="#8000FF")
points(1:length(default$allI), default$allI, col= "#088A08",pch=1)
axis(side=2, at=seq(0,150,5))
axis(side=1, at=seq(0,100,5))
legend(60, 70, legend=c( "SIR","blokowanie", "symbioza"),col=c("#FF0000","#8000FF","#088A08"), pch=1, cex= .8)
arrows(1:length(only$allI),only$allI+only$sdAllI,1:length(only$allI),only$allI-only$sdAllI,  angle=90, code=3, length=0.1, col ="#F78181")
arrows(1:length(blocking$allI),blocking$allI+blocking$sdAllI,1:length(blocking$allI),blocking$allI-blocking$sdAllI,  angle=90, code=3, length=0.1, col ="#ECCEF5")
arrows(1:length(default$allI),default$allI+default$sdAllI,1:length(default$allI),default$allI-default$sdAllI,  angle=90, code=3, length=0.1, col ="#CEF6CE")
dev.off()
}}
