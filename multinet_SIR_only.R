# biblioteka 
library(multinet)

# SIR only file - for exsperiments without SIS ----------------------------
# FolderRoboczy 
setwd("C:/Users/Paulina/Desktop/Repository")
getwd()

#zmienne pomocniecze do zapisu
experimentFolder<- "OnlySIR_AUCS"
#experimentDescription <- "net_SIR"
directory <- paste("experiments",experimentFolder, sep="")
#folder dla eksperymentów 
dir.create(directory)
setwd(paste("C:/Users/Paulina/Desktop/Repository/", directory,sep=""))

experimentNumber <- 20
# definicja seed #set.seed(1313)
for(e in 1:experimentNumber)
{ 
   # wczytanie sieci 
net <- ml_aucs()

fileConn <- file(paste(paste(e,"AUCS", sep=""),"Data.txt",sep =""))
layerName <- "lunch"
#writeLines(c(layerName,"\n"),fileConn)
#parametry sieci
numberOfActors <- num_actors_ml(net)
numberOfActorsInLayer <- num_actors_ml(net,layerName)
layerActors <- actors_ml(net,layerName)
networkActors <- actors_ml(net)

# definicje zmiennych
#czas trwania "epidemii" -  liczba dni
time <- 150

# prawdopodobieñstwa SIR
beta <- 0.19 # zara¿enia
gamma <- 0.1 # wyzdrowienia
probabilities <- paste(paste("beta:",beta),paste("gamma:",gamma))
writeLines(paste(probabilities, layerName),fileConn)
close(fileConn)

#Stan SIR
numberOfSusceptible <- numberOfActorsInLayer
numberOfInfected  <- 0 #
numberOfRecovered <- 0 # ozdrowieñcy
SIR_Sum<- numberOfSusceptible + numberOfInfected + numberOfRecovered 


 # Stan pocz¹tkowy dla macierzy licznoœci 
SIR_group_States <- matrix(cbind(0,numberOfSusceptible, numberOfInfected, numberOfRecovered, SIR_Sum))


# zmienne pomocnicze
new_infected <- NULL # nowe zachorowania
new_recovered <- NULL # nowe ozdrowienia

#dodanie atrybutów (state -stan dla SIR,)
add_attributes_ml(net, "state", type = "string", target="actor", layer ="")

#ustawienie wartoœci atrybutu state dla SIR domyœlnie na S
set_values_ml(net, "state",actors_ml(net,layerName), values ="S" )

#sprawdzenie ustawionych wartoœci 
#get_values_ml(net,"state",actors_ml(net))
#get_values_ml(net,"awareness",actors_ml(net))
#get_values_ml(net,"epsilon",actors_ml(net))
#get_values_ml(net,"beta",actors_ml(net))


# stan pocz¹tkowy dla I ---------------------------------------------------
# (A)losowo X osób 
# x<-5 
# infected <- trunc(runif(x,1,215))
# while (n>0)
# { (print (infected[n]))
#   set_values_ml(net, "state",infected[n], values ="I" )
#  print(get_values_ml(net, "state",infected[n]))
#  n=n-1
# }

# (B)losowo x % sieci - preferowane np 1% - liczymi ile to aktorów w sieci a potem losujemy tylu aktorów jako seedy
x<-0.01
n<- round( x * num_actors_ml(net,layerName)) # dla warstwy x % aktorów z wybranej warstwy
infected <- trunc(runif(n,1,numberOfActorsInLayer))

# Aktualizowanie stanu SIR
numberOfInfected <- n
numberOfSusceptible <- numberOfSusceptible - numberOfInfected

# zainfekowanie wylosowanych aktorów
while (n>0)
{ print( paste("infekowanie w toku - aktor :", infected[n]))
  set_values_ml(net, "state",layerActors[infected[n]], values ="I" )
  #print(paste(n, get_values_ml(net, "state",infected[n]))) do sprawdzenia 
  n=n-1
}

timeline_SIR<- as.matrix(layerActors)
timeline_SIR = cbind(timeline_SIR, get_values_ml(net,"state", layerActors ))


for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzieñ
{  
  
  # wypisuje - Stan SIR na konsole 
  SIR_group_States <- cbind(SIR_group_States,rbind(i,numberOfSusceptible,numberOfInfected,numberOfRecovered, SIR_Sum))
  #writeLines(paste(paste(paste("Dzieñ epidemii:", i),paste("Stan SIR:", paste( paste( paste("Susceptible:", numberOfSusceptible),paste("Infected:", numberOfInfected), sep = " ; "),paste("Recovered:", numberOfRecovered),sep =" ; ")) ),"\n"),fileConn)
  #writeLines("\n", fileConn)
  print(paste("Dzieñ epidemii:", i)) 
  print(paste("Stan SIR:", paste( paste( paste("Susceptible:", numberOfSusceptible),paste("Infected:", numberOfInfected), sep = " ; "),paste("Recovered:", numberOfRecovered),sep =" ; ")))
  
  # print(paste("Susceptible", numberOfSusceptible)) 
  # print(paste("Infected", numberOfInfected)) 
  # print(paste("Recovered", numberOfRecovered)) 
  
  if(numberOfRecovered == numberOfActorsInLayer) break 
  
  new_infected <- NULL
  new_recovered <- NULL
  
  # Pêtla SIR
  for (j in 1:length(layerActors)) # odwiedzam po kolei aktorów
  { 
    if(get_values_ml(net,"state",layerActors[j]) =="I") # jeœli aktor jest zara¿ony
    { 
      # szukamy s¹siadów 
      neighbors <- neighbors_ml(net,layerActors[j],layerName,mode="all")
      for(s in 1:length(neighbors))
      {  if(get_values_ml(net, "state", neighbors[s])=="S")
      { 
        if( runif(1) < beta) 
        { #print( value)
          if(!(neighbors[s] %in% new_infected)) # is.element(neighbors[s])
            new_infected <- cbind(new_infected,neighbors[s]) # mamy tymczasow¹ listê nowo zainfekowanych  							
          #	print(paste("nowe zachorowanie, nadal zdrowi",numberOfSusceptible))
        }
      }
        #if(!is.null(new_infected))  set_values_ml(net, "state",new_infected, values ="I" ) 				   
      }
      
      if( runif(1) < gamma)
      { #print(test)
        if(!is.element(layerActors[j],new_recovered)){ new_recovered=cbind(new_recovered,layerActors[j])}
        #print(paste("nowy ozdrowieniec, jeszce choruje:", numberOfInfected))
      }
    }
    
  }
 
  # aktualizacja nowych zaka¿eñ i ozdrowienia jeœli siê pojawi³y
  if(!is.null(new_infected))  set_values_ml(net, "state",new_infected, values ="I" )
  if(!is.null(new_recovered))  set_values_ml(net, "state",new_recovered, values ="R" )
  #print(new_infected)
  #print (paste("new R", length(new_recovered)))
  #print(paste("new I", length(new_infected)))
  
  
  #Sprawdzenie stanu atrybutów - zawartoœæ wektora 
  SIR_attributes <- get_values_ml(net,"state", layerActors)
  numberOfSusceptible <- length( which('S' == SIR_attributes))
  numberOfInfected <- length( which('I' == SIR_attributes))
  numberOfRecovered <- length( which('R' == SIR_attributes))
  Sum = numberOfSusceptible + numberOfInfected + numberOfRecovered
  
    # zapis stanów poœrednich 
  
  #lastStateSIR <- get_values_ml(net,"state",actors_ml(net,layerName))
  timeline_SIR <- cbind(timeline_SIR, SIR_attributes)

}

SIR_group_States <- t(SIR_group_States)
# Operacje IO - zapis, katalog roboczy -------------------------------------


#zmienne pomocniecze do zapisu

experimentDescription <- paste(beta,"_Poland_AUCS", sep ="-")
seconddirectory <- paste("_experiment_AUCS",e, sep="")
#dir.create(seconddirectory)

#folder dla eksperymentów 
#setwd(paste(paste("C:/Users/Paulina/Desktop/Repository/", directory,sep=""),seconddirectory, sep= "/"))

# zapis do pliku dat.
write.table(SIR_group_States,file=paste(seconddirectory,paste("Summary_SIR",(paste(experimentDescription,".dat", sep = "")), sep=""),sep=""), col.names =TRUE, sep =";", row.names = TRUE )
write.table(timeline_SIR,file=paste(seconddirectory,paste("timeline_states",(paste(experimentDescription,".dat", sep = "")), sep=""),sep=""), col.names =TRUE, sep =";", row.names = TRUE )

# Zapis poszczególnych stanów SIR do pliku CSV
write.csv(timeline_SIR,file=paste(seconddirectory,paste("timeline_states",(paste(experimentDescription,".csv", sep = "")), sep=""),sep=""), row.names = TRUE)

# Zapis poszczególnych stanów SIR do pliku RDS
#saveRDS(timeline_SIR,file=paste(seconddirectory,paste("timeline_states",(paste(experimentDescription,".rds", sep = "")), sep=""),sep=""))
save(list = ls(all.names = TRUE), file =paste( seconddirectory,".RData",sep=""), envir = .GlobalEnv)
# zapis zmodyfikowanej sieci do pliku - niepe³ny 
#write_ml(net,file="experiment_Data/net_test.mpx",format="multilayer" )

}