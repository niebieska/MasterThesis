beta <-c("0.19-0.1","0.28-0.08","0.22-0.02","0.31-0.1" )
listMultiply<- c(1,2,3,4)
type <-c("SIS&SIR_default","SIS&SIR_blocking")
networkName <- "EU"
for(t in 1: length(type)){
  scritpType<-type[t]

for(w in 1:length(beta))
{ mainDirectory <- beta[w]
  for(q in 1:length(listMultiply))
{

countryDirectory <- paste("Mnoznikx", listMultiply[q], sep ="")
experimentsMainDirectory<- paste("SIR&SIS",networkName, sep="-")
file <-paste(paste(paste(paste(paste("~/MasterThesis/Eksperiments/",experimentsMainDirectory,sep=""),countryDirectory,sep="/"),mainDirectory, sep="/"),scritpType, sep="/"),"/Summary_SIR-", sep ="")

title =paste("Uœredniony-wykres",paste(scritpType,paste(networkName,paste(mainDirectory, countryDirectory, sep="-"), sep = "-"),sep = "-"),sep ="-")


MaxIter<-NULL
for(i in 1:20) { 
  nam <- paste("d", i, sep = "")
  assign(nam, read.csv(paste(paste(file, i, sep=""),".dat", sep=""), sep=";"))
}   



MaxIter<-cbind(max(d1$i),max(d2$i),max(d3$i),max(d4$i),max(d5$i),max(d6$i),max(d7$i),max(d8$i),max(d9$i),max(d10$i),max(d11$i),max(d12$i),max(d13$i),max(d14$i),max(d15$i),max(d16$i),max(d17$i),max(d18$i),max(d19$i),max(d20$i)) 
size <- max(MaxIter)

NumberOFIntected <-cbind(d1$numberOfInfected[1:size],d2$numberOfInfected[1:size],d3$numberOfInfected[1:size],d4$numberOfInfected[1:size],
                         d5$numberOfInfected[1:size],d6$numberOfInfected[1:size],d7$numberOfInfected[1:size],d8$numberOfInfected[1:size],
                         d9$numberOfInfected[1:size],d10$numberOfInfected[1:size],d11$numberOfInfected[1:size],d12$numberOfInfected[1:size],
                         d13$numberOfInfected[1:size],d14$numberOfInfected[1:size],d15$numberOfInfected[1:size],d16$numberOfInfected[1:size],
                         d17$numberOfInfected[1:size],d18$numberOfInfected[1:size],d19$numberOfInfected[1:size],d20$numberOfInfected[1:size]) 

test <- NULL
odchyl <-NULL
for(i in 1:size)
  
{
  temp <- trunc(mean(NumberOFIntected[i,1:20],na.rm = TRUE))
  test <-rbind(test,temp)
  iter <- sd(NumberOFIntected[i,1:20],na.rm=TRUE)
  odchyl <-rbind(odchyl, iter)
}
length(test)
length(odchyl)
test<- (test/70)*100

setwd("~/MasterThesis/Eksperiments/Plots/")
jpeg(paste(title,".jpg",sep=""), 1200,1000)
plot(1:size, test, main=paste("Uœredniony wykres zainfekowanych ",paste(scritpType,paste(networkName,paste(mainDirectory, countryDirectory, sep=" "), sep = " "),sep = " "), sep = ""),xlab= "time", ylab="% actors", pch=19,  col="red")
dev.off()
  }
}
  }