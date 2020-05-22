library(multinet)
#Wykres
setwd("~/MasterThesis/Eksperiments/Plots/")
fileNumber <-20
for(n in 1:fileNumber)
{ jpeg(paste(n,"wykres_SIS&SIR_Lazega_Polska.jpg", sep=""), 1200,1000)
#data <-  read.csv( paste(paste("~/MasterThesis/Eksperiments/SIR&SIS_Lazega/ModelForPoland-SIRonly/Summary_SIR", n, sep=""),"_eksperyment-0.31.dat", sep =""), sep=";")
data<- read.csv(paste(paste("~/MasterThesis/Eksperiments/SIR&SIS_Lazega/ModelForPoland-default/Summary_SIR", n, sep=""),"_eksperyment 0.31-0.31.dat", sep =""), sep=";")
attach(data)
plot(i,((numberOfSusceptible/SIR_Sum) *100) , main=paste("Wizaulizacja SIR & SIS Polska b=0.31 g=0.1",n),xlab= "time", ylab="% actors", pch=19, col="blue")
points(i,((numberOfInfected/SIR_Sum) *100) , pch=19, col="red")
points(i,((numberOfRecovered/SIR_Sum) *100) , pch=19, col="green")
abline(h = 0, v = 0, col = "gray60")
abline(h = 100, v = max(i), col = "gray60")
dev.off()
}

