# biblioteka 
library(multinet)
load("C:/Users/Paulina/Documents/SIR_experiments/Repository/eksperymentfullnet/timeline_statesfullnet_SIR.dat")
# definicja seed
#set.seed(1313)

# wczytanie sieci 
fullnet <- read_ml("C:/Users/Paulina/Downloads/FullNet/CKM-Physicians-Innovation_4NoNature.edges", name="CKM", sep=',', aligned=FALSE)
#net<- read_ml ("experiment_Data/net_test.mpx","test",sep=',', aligned=FALSE)

#parametry sieci
numberOfActors <- num_actors_ml(fullnet)
numberOfActorsInLayer <- num_actors_ml(fullnet,"advice")

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
SUM<- numberOfSusceptible + numberOfInfected + numberOfRecovered 

# Stan pocz¹tkowy dla macierzy licznoœci 
SIR_group_States <- matrix(rbind(0,numberOfSusceptible,numberOfInfected,numberOfRecovered, SUM))

# zmienne pomocnicze
new_infected <-NULL # nowe zachorowania
new_recovered <-NULL # nowe ozdrowienia

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

advice<-actors_ml(fullnet,"advice")


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


timeline_SIR<- as.matrix( actors_ml(fullnet,"advice"))
timeline_SIR= cbind(timeline_SIR, get_values_ml(fullnet,"state",actors_ml(fullnet,"advice")))

#dodatkowy warunek na wyjœcie jak wszyscy s¹ w R
 #dodatkowy warunek na wyjœcie jak wszyscy s¹ w R


	  for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzieñ
	{  
	
	
	# wypisuje - Stan SIR na konsole 
	   SIR_group_States <- cbind(SIR_group_States,rbind(i,numberOfSusceptible,numberOfInfected,numberOfRecovered, SUM))
	   print(paste("Dzieñ epidemii:", i)) 
	   print(paste("Susceptible", numberOfSusceptible)) 
	   print(paste("Infected", numberOfInfected)) 
	   print(paste("Recovered", numberOfRecovered)) 
	   
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
					  if(!is.element(advice[j],new_infected)){ new_recovered=cbind(new_recovered,advice[j])}
					  #print(paste("nowy ozdrowieniec, jeszce choruje:", numberOfInfected))
					}
			}
	
			}
	
	
	   # aktualizacja nowych zaka¿eñ i ozdrowienia jeœli siê pojawi³y
	   	  if(!is.null(new_infected))  set_values_ml(fullnet, "state",new_infected, values ="I" )
	      if(!is.null(new_recovered))  set_values_ml(fullnet, "state",new_recovered, values ="R" )
	      print(new_infected)
	      print (paste("new R", length(new_recovered)))
	      print(paste("new I", length(new_infected)))
	      
		  # Obliczenia stan SIR
		  numberOfSusceptible = numberOfSusceptible - length(new_infected)
	      numberOfInfected = numberOfInfected +(length(new_infected) - length(new_recovered))
	      numberOfRecovered = numberOfRecovered + length(new_recovered)
	      SUM <- numberOfInfected + numberOfRecovered + numberOfSusceptible
	      print(paste("Suma", SUM))
		  
		  #Sprawdzenie stanu atrybutów - zawartoœæ wektora 
		    SIR_attributes <- get_values_ml(fullnet,"state", advice)
		    S<-length( which('S' == SIR_attributes))
		    I<-length( which('I' == SIR_attributes))
		    R <-length( which('R' == SIR_attributes))
		    Sum = S+I+R
		  	  # zapis stanów poœrednich 
	 
	      lastStateSIR <- get_values_ml(fullnet,"state",actors_ml(fullnet,"advice"))
     	  timeline_SIR <- cbind(timeline_SIR,lastStateSIR)
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
