# biblioteka 
library(multinet)

# definicja seed #set.seed(1313)

# wczytanie sieci 
fullnet <- read_ml("C:/Users/Paulina/Downloads/FullNet/CKM-Physicians-Innovation_4NoNature.edges", name="CKM", sep=',', aligned=FALSE)
#net<- read_ml ("experiment_Data/net_test.mpx","test",sep=',', aligned=FALSE)

#parametry sieci
numberOfActors <- num_actors_ml(fullnet)
numberOfActorsInLayer <- num_actors_ml(fullnet,"advice")
advice <- actors_ml(fullnet,"advice")
actors <- actors_ml(fullnet)

# definicje zmiennych
#czas trwania "epidemii" -  liczba dni
time <- 20

# prawdopodobieñstwa SIR
beta <- 0.85 # zara¿enia
gamma <- 0.03 # wyzdrowienia

# prawdopodobieñstwa SIS
epsilon <-0.1 # odpowiednik beta, uzuskania informacji
mi <- 0.2    # zwatpienia

#Stan SIR
numberOfSusceptible <- numberOfActorsInLayer
numberOfInfected  <- 0 #
numberOfRecovered <- 0 # ozdrowieñcy
SIR_Sum<- numberOfSusceptible + numberOfInfected + numberOfRecovered 

#Stan SIS
numberOfUnawarened <-numberOfActors
numberOfAwarened <- 0
SIS_Sum <- numberOfAwarened + numberOfUnawarened
# Stan pocz¹tkowy dla macierzy licznoœci 
SIR_group_States <- matrix(rbind(0,numberOfSusceptible, numberOfInfected, numberOfRecovered, SIR_Sum))
SIS_group_States <- matrix (rbind(0, numberOfUnawarened, numberOfAwarened, SIS_Sum))
# zmienne pomocnicze
new_infected <- NULL # nowe zachorowania
new_recovered <- NULL # nowe ozdrowienia

new_awarened <- NULL # œwiadomi S
new_unawarened <- NULL # nieœwiadomi versus wypieraj¹cy

#dodanie atrybutów (state -stan dla SIR, SIRbeta - prawdopodobieñstwo)
add_attributes_ml(fullnet, "state", type = "string", target="actor", layer ="")
add_attributes_ml(fullnet, "beta", type = "numeric", target = "actor", layer = "")
add_attributes_ml(fullnet, "awareness", type ="string", target ="actor", layer = "")
add_attributes_ml(fullnet, "epsilon", type = "numeric", target = "actor", layer = "")


#ustawienie wartoœci atrybutu state dla SIR domyœlnie na S
set_values_ml(fullnet, "state",actors_ml(fullnet,"advice"), values ="S" )
set_values_ml(fullnet, "awareness",actors_ml(fullnet), values ="S" )
set_values_ml(fullnet, "beta",actors_ml(fullnet), values = beta)
set_values_ml(fullnet, "epsilon",actors_ml(fullnet), values = epsilon)

#sprawdzenie ustawionych wartoœci 
#get_values_ml(fullnet,"state",actors_ml(fullnet))
#get_values_ml(fullnet,"awareness",actors_ml(fullnet))
#get_values_ml(fullnet,"epsilon",actors_ml(fullnet))
#get_values_ml(fullnet,"beta",actors_ml(fullnet))


# stan pocz¹tkowy dla I ---------------------------------------------------
# (A)losowo X osób 
# x<-5 
# infected <- trunc(runif(x,1,215))
# while (n>0)
# { (print (infected[n]))
#   set_values_ml(fullnet, "state",infected[n], values ="I" )
#  print(get_values_ml(fullnet, "state",infected[n]))
#  n=n-1
# }

# (B)losowo x % sieci - preferowane np 1% - liczymi ile to aktorów w sieci a potem losujemy tylu aktorów jako seedy
x<-0.01
n<- round( x * num_actors_ml(fullnet,"advice")) # dla warstwy x % aktorów z wybranej warstwy
infected <- trunc(runif(n,1,numberOfActorsInLayer))

# Aktualizowanie stanu SIR
numberOfInfected <- n
numberOfSusceptible <- numberOfSusceptible -numberOfInfected

# zainfekowanie wylosowanych aktorów
while (n>0)
{ print( paste("infekowanie w toku - aktor :", infected[n]))
  set_values_ml(fullnet, "state",advice[infected[n]], values ="I" )
  #print(paste(n, get_values_ml(fullnet, "state",infected[n]))) do sprawdzenia 
 n=n-1
}

#uœwiadomienie wybranych osób 
x<-0.10
m<- round( x * num_actors_ml(fullnet)) # x % aktorów z ca³ej sieci
awarened <- trunc(runif(m,1,numberOfActorsInLayer))

# Aktualizowanie stanu SIS
numberOfAwarened <- m
numberOfUnawarened <- numberOfUnawarened -numberOfAwarened

while (m>0)
{ print( paste("uœwiadamianie w toku - aktor :", awarened[m]))
  set_values_ml(fullnet, "awareness", actors[awarened[m]], values ="I" )
  #print(paste(m, get_values_ml(fullnet, "awareness",awarened[m]))) do sprawdzenia 
  m=m-1

}


timeline_SIR<- as.matrix(advice)
timeline_SIR = cbind(timeline_SIR, get_values_ml(fullnet,"state", advice ))
timeline_SIS <- as.matrix(actors)
timeline_SIS <- cbind(timeline_SIS, get_values_ml(fullnet,"awareness",actors))

	  for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzieñ
	{  

	# wypisuje - Stan SIR na konsole 
	   SIR_group_States <- cbind(SIR_group_States,rbind(i,numberOfSusceptible,numberOfInfected,numberOfRecovered, SIR_Sum))
	   print(paste("Dzieñ epidemii:", i)) 
	   print(paste("Stan SIR:", paste( paste( paste("Susceptible:", numberOfSusceptible),paste("Infected:", numberOfInfected), sep = " ; "),paste("Recovered:", numberOfRecovered),sep =" ; ")))
	   
	   # print(paste("Susceptible", numberOfSusceptible)) 
	   # print(paste("Infected", numberOfInfected)) 
	   # print(paste("Recovered", numberOfRecovered)) 
	   
	   if(numberOfRecovered == numberOfActorsInLayer) break 
	   
	   new_infected <- NULL
	   new_recovered <- NULL
			
			# Pêtla SIR
				for (j in 1:length(advice)) # odwiedzam po kolei aktorów
				{ 
					if(get_values_ml(fullnet,"state",advice[j]) =="I") # jeœli aktor jest zara¿ony
					{ 
						# szukamy s¹siadów 
						neighbors <- neighbors_ml(fullnet,advice[j],"advice",mode="all")
						for(s in 1:length(neighbors))
						{  if(get_values_ml(fullnet, "state", neighbors[s])=="S")
							{ 
								if( runif(1) < beta) 
								  { #print( value)
								  if(!(neighbors[s] %in% new_infected)) # is.element(neighbors[s])
										new_infected <- cbind(new_infected,neighbors[s]) # mamy tymczasow¹ listê nowo zainfekowanych  							
									#	print(paste("nowe zachorowanie, nadal zdrowi",numberOfSusceptible))
									}
						  }
						  #if(!is.null(new_infected))  set_values_ml(fullnet, "state",new_infected, values ="I" ) 				   
						}
								
					if( runif(1) < gamma)
						{ #print(test)
						  if(!is.element(advice[j],new_recovered)){ new_recovered=cbind(new_recovered,advice[j])}
						  #print(paste("nowy ozdrowieniec, jeszce choruje:", numberOfInfected))
						}
				}
	
				}
	   
	      print( paste("Stan SIS:", paste( paste( paste("Susceptible:", numberOfUnawarened),paste("Infected:", numberOfAwarened), sep = " ; "))))
	      SIS_group_States <- cbind(SIS_group_States,rbind(i,numberOfUnawarened,numberOfAwarened, SIS_Sum))
	      
	      new_awarened <- NULL # œwiadomi S
	      new_unawarened <- NULL # nieœwiadomi versus wypieraj¹cy
	      #Pêtla dla SIS
			   for(k in 1: length(actors))
			   {
				 if(get_values_ml(fullnet,"state",actors[k]) =="I")
				 {
				   # poszukiwanie s¹siadów
				   actorNeighbors <- neighbors_ml(fullnet,actors[k],"advice",mode="all")
				   for(l in 1:length(actorNeighbors))
				   {  
					 if(get_values_ml(fullnet, "awareness", actorNeighbors[l])=="S")
				   { 
					 if( runif(1) < epsilon) 
					 { #print( value)
					   if(!(actorNeighbors[l] %in% new_awarened)) # is.element(neighbors[s])
						 new_awarened <- cbind(new_awarened,actorNeighbors[l]) # mamy tymczasow¹ listê nowo zainfekowanych  							
					  }
				   }		   
				   }
				   				   if( runif(1) < mi)
				   { #print(test)
					 if(!is.element(actors[k],new_unawarened)){ new_unawarened=cbind(new_unawarened,actors[k])}
					 
				   }
				   }
	     
	   }

	   
	
	
	   # aktualizacja nowych zaka¿eñ i ozdrowienia jeœli siê pojawi³y
	   	  if(!is.null(new_infected))  set_values_ml(fullnet, "state",new_infected, values ="I" )
	      if(!is.null(new_recovered))  set_values_ml(fullnet, "state",new_recovered, values ="R" )
	      #print(new_infected)
	      #print (paste("new R", length(new_recovered)))
	      #print(paste("new I", length(new_infected)))
	   
	   #aktualizacja SIS    
	      if(!is.null(new_awarened))  set_values_ml(fullnet, "awareness",new_awarened, values ="I" )
	      if(!is.null(new_unawarened))  set_values_ml(fullnet, "awareness",new_unawarened, values ="S" )
	      
	# 	  # Obliczenia stan SIR
	# 	    numberOfSusceptible = numberOfSusceptible - length(new_infected)
	#       numberOfInfected = numberOfInfected +(length(new_infected) - length(new_recovered))
	#       numberOfRecovered = numberOfRecovered + length(new_recovered)
	#       SUM <- numberOfInfected + numberOfRecovered + numberOfSusceptible
	#       print(paste("Suma", SUM))
		  
		  #Sprawdzenie stanu atrybutów - zawartoœæ wektora 
		    SIR_attributes <- get_values_ml(fullnet,"state", advice)
		    numberOfSusceptible <- length( which('S' == SIR_attributes))
		    numberOfInfected <- length( which('I' == SIR_attributes))
		    numberOfRecovered <- length( which('R' == SIR_attributes))
		    Sum = numberOfSusceptible + numberOfInfected + numberOfRecovered
		  	
		    SIS_atributes <- get_values_ml(fullnet, "awareness", actors)  
		    numberOfUnawarened <- length(which("S"==SIS_atributes))
		    numberOfAwarened <- length(which("I"== SIS_atributes))
		    SIS_Sum <- numberOfAwarened + numberOfUnawarened 
		    # zapis stanów poœrednich 
	 
	      #lastStateSIR <- get_values_ml(fullnet,"state",actors_ml(fullnet,"advice"))
     	  timeline_SIR <- cbind(timeline_SIR, SIR_attributes)
     	  timeline_SIS <- cbind(timeline_SIS, SIS_atributes)
	}

# Operacje IO - zapis, katalog roboczy -------------------------------------
setwd("C:/Users/Paulina/Documents/SIR_experiments/Repository")
getwd()

#zmienne pomocniecze do zapisu
experimentFolder<- "Fullnet"
experimentDescription <- "Fullnet_SIR"
directory <- paste("eksperyment",experimentFolder, sep="")

#folder dla eksperymentów 
dir.create(directory)
# zapis do pliku dat.
write.table(SIR_group_States,file=paste(directory,paste("/Summary_SIR",(paste(experimentDescription,".dat", sep = "")), sep=""),sep=""), col.names =TRUE, sep =";", row.names = TRUE )
write.table(timeline_SIR,file=paste(directory,paste("/timeline_states",(paste(experimentDescription,".dat", sep = "")), sep=""),sep=""), col.names =TRUE, sep =";", row.names = TRUE )

# Zapis poszczególnych stanów SIR do pliku CSV
write.csv(timeline_SIR,file=paste(directory,paste("/timeline_states",(paste(experimentDescription,".csv", sep = "")), sep=""),sep=""), row.names = TRUE)

# Zapis poszczególnych stanów SIR do pliku RDS
saveRDS(timeline_SIR,file=paste(directory,paste("/timeline_states",(paste(experimentDescription,".rds", sep = "")), sep=""),sep=""))

# zapis zmodyfikowanej sieci do pliku - niepe³ny 
#write_ml(fullnet,file="experiment_Data/net_test.mpx",format="multilayer" )
