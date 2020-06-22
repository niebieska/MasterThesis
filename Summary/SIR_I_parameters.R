beta <-c("0.19-0.1","0.28-0.08","0.22-0.01","0.31-0.1" )
networkName <- "EU"#c("Lazega-blocking","AUCS", "MoscowAthletics2013","EU")
mainDirectory <- beta[1]
scritpType<-"SIS&SIR_blocking"
countryDirectory <-"MultiplyBy1"
experimentsMainDirectory<- paste("SIR&SIS",networkName[1], sep="-")
file <-paste(paste(paste(paste(paste("C:/Users/Paulina/Desktop/Eksperymenty/",experimentsMainDirectory,sep=""),countryDirectory,sep="/"),mainDirectory, sep="/"),scritpType, sep="/"),"/Summary_SIR-", sep ="")

#title =paste("Uœredniony-wykres",paste(scritpType,paste(networkName,paste(mainDirectory, countryDirectory, sep="-"), sep = "-"),sep = "-"),sep ="-")


MaxIter<-NULL
for(i in 1:20) { 
  nam <- paste("d", i, sep = "")
  assign(nam, read.csv(paste(paste(file, i, sep=""),".dat", sep=""), sep=";"))
}   


# wczytanie danych

MaxIter<-cbind(max(d1$i),max(d2$i),max(d3$i),max(d4$i),max(d5$i),max(d6$i),max(d7$i),max(d8$i),max(d9$i),max(d10$i),max(d11$i),max(d12$i),max(d13$i),max(d14$i),max(d15$i),max(d16$i),max(d17$i),max(d18$i),max(d19$i),max(d20$i)) 
size <- max(MaxIter)

NumberOfInfected <-cbind(d1$numberOfInfected[1:size],d2$numberOfInfected[1:size],d3$numberOfInfected[1:size],d4$numberOfInfected[1:size],
                         d5$numberOfInfected[1:size],d6$numberOfInfected[1:size],d7$numberOfInfected[1:size],d8$numberOfInfected[1:size],
                         d9$numberOfInfected[1:size],d10$numberOfInfected[1:size],d11$numberOfInfected[1:size],d12$numberOfInfected[1:size],
                         d13$numberOfInfected[1:size],d14$numberOfInfected[1:size],d15$numberOfInfected[1:size],d16$numberOfInfected[1:size],
                         d17$numberOfInfected[1:size],d18$numberOfInfected[1:size],d19$numberOfInfected[1:size],d20$numberOfInfected[1:size]) 

# NumberOfSusceptible <-cbind(d1$numberOfSusceptible[1:size],d2$numberOfSusceptible[1:size],d3$numberOfSusceptible[1:size],d4$numberOfSusceptible[1:size],
#                             d5$numberOfSusceptible[1:size],d6$numberOfSusceptible[1:size],d7$numberOfSusceptible[1:size],d8$numberOfSusceptible[1:size],
#                             d9$numberOfSusceptible[1:size],d10$numberOfSusceptible[1:size],d11$numberOfSusceptible[1:size],d12$numberOfSusceptible[1:size],
#                             d13$numberOfSusceptible[1:size],d14$numberOfSusceptible[1:size],d15$numberOfSusceptible[1:size],d16$numberOfSusceptible[1:size],
#                             d17$numberOfSusceptible[1:size],d18$numberOfSusceptible[1:size],d19$numberOfSusceptible[1:size],d20$numberOfSusceptible[1:size]) 
# 
# 
# NumberOfRecovered <-cbind(d1$numberOfRecovered[1:size],d2$numberOfRecovered[1:size],d3$numberOfRecovered[1:size],d4$numberOfRecovered[1:size],
#                           d5$numberOfRecovered[1:size],d6$numberOfRecovered[1:size],d7$numberOfRecovered[1:size],d8$numberOfRecovered[1:size],
#                           d9$numberOfRecovered[1:size],d10$numberOfRecovered[1:size],d11$numberOfRecovered[1:size],d12$numberOfRecovered[1:size],
#                           d13$numberOfRecovered[1:size],d14$numberOfRecovered[1:size],d15$numberOfRecovered[1:size],d16$numberOfRecovered[1:size],
#                           d17$numberOfRecovered[1:size],d18$numberOfRecovered[1:size],d19$numberOfRecovered[1:size],d20$numberOfRecovered[1:size]) 




allS<-max(d1$SIR_Sum)
allI<- 0
allR <- 0
sdAllS<-0
sdAllI<-0
sdAllR<-0
for(i in 1:size)

{
  I<- round(mean(NumberOfInfected[i,1:20],na.rm = TRUE))
  allI <-rbind(allI,I)
  sdI <- sd(NumberOfInfected[i,1:20],na.rm=TRUE)
  sdAllI <-rbind(sdAllI, sdI)
  
  # S <- round(mean(NumberOfSusceptible[i,1:20],na.rm = TRUE))
  # allS<- rbind(allS,S)
  # sdS <- sd(NumberOfSusceptible[i,1:20],na.rm=TRUE)
  # sdAllS <-rbind(sdAllS, sdS)
  # 
  # sumR <- trunc(mean(NumberOfRecovered[i,1:20],na.rm = TRUE))
  # allR<- rbind(allR,sumR)
  # sdR <- sd(NumberOfRecovered[i,1:20],na.rm=TRUE)
  # sdAllR <-rbind(sdAllR, sdR)
  
}
sdAllS[4]/70*100


# Przeliczenie na procenty
#allS<- (allS[1:size]/max(d1$SIR_Sum)*100)
allI<- (allI[1:size]/max(d1$SIR_Sum)*100)
#allR<-(allR[1:size]/max(d1$SIR_Sum)*100)
#sdAllS<- (sdAllS[1:size]/max(d1$SIR_Sum)*100)
sdAllI<- (sdAllI[1:size]/max(d1$SIR_Sum)*100)
#sdAllR<-(sdAllR[1:size]/max(d1$SIR_Sum)*100)
#sdAllS*100
plot(1:size, allI[1:size],xlab= "time", ylab="% actors",xlim=c(0,150), ylim=c(0,80),pch=19, col="red")

points(1:size, allI, col= "black")
axis(side=2, at=seq(0,100,5))
axis(side=1, at=seq(0,size,5))
legend(130, 90, legend=c( "S","I", "R"),col=c("blue", "red", "green"), pch=19)

arrows(1:size,allI[1:size]+sdAllI[1:size],1:size,allI[1:size]-sdAllI[1:size],  angle=90, code=3, length=0.1, col="blue")
#arrows(1:size,allS[1:size]+sdAllS[1:size],1:size,allS[1:size]-sdAllS[1:size],  angle=90, code=3, length=0.1)
