# biblioteka 
library(multinet)

# Parametry zapisu eksperymentu  -------------------------------------------------
# # Folder roboczy
setwd("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/")
getwd()
#zmienne pomocniecze do zapisu
networkName <- "Lazega"
experimentFolder<- paste("SIR&SIS",networkName, sep="_")
dir.create(experimentFolder) 
directory <- "ModelForPoland-SIRonly"
#folder dla eksperymentów 
setwd(paste("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/", experimentFolder, sep=""))
dir.create(directory)
setwd(paste(paste("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/",experimentFolder,sep=""), directory,sep="/"))
getwd()

experimentNumber <- 20

for(e in 1:experimentNumber)
{ 
	# wczytanie sieci 
	#net <- ml_aucs()
	net <- fullnet <- read_ml("C:/Users/Paulina/Downloads/FullNet/Lazega-Law-Firm_4NoNatureNoLoops.edges", name="Lazega", sep=',', aligned=FALSE)
	AllLayers <- layers_ml(net)
  
	# aktualna warstwa
	layerName <- "co-work"

	#parametry sieci
	numberOfActors <- num_actors_ml(net)
	numberOfActorsInLayer <- num_actors_ml(net,layerName)
	layerActors <- actors_ml(net,layerName)
	networkActors <- actors_ml(net)

	# definicje zmiennych
	#czas trwania "epidemii" -  liczba dni
	time <- 150

	# prawdopodobieñstwa SIR
	beta <- 0.31 # zara¿enia
	gamma <- 0.1 # wyzdrowienia
	probabilities <- paste(paste("beta:",beta),paste("gamma:",gamma))

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
	x<-0.02
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
	timeline_SIR<-cbind(timeline_SIR, get_values_ml(net,"state", layerActors ))


	for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzieñ
	{  
	  
	  # wypisuje - Stan SIR na konsole 
	  SIR_group_States <- cbind(SIR_group_States,rbind(i,numberOfSusceptible,numberOfInfected,numberOfRecovered, SIR_Sum))

	  print(paste("Dzieñ epidemii:", i)) 
	  print(paste("Stan SIR:", paste( paste( paste("Susceptible:", numberOfSusceptible),paste("Infected:", numberOfInfected), sep = " ; "),paste("Recovered:", numberOfRecovered),sep =" ; ")))
		 
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
			  
			}
		  }       			   
		  }
		  
		  if( runif(1) < gamma)
		  { 
			if(!is.element(layerActors[j],new_recovered)){ new_recovered=cbind(new_recovered,layerActors[j])}
		  }
		}
	  }
	 
	  # aktualizacja nowych zaka¿eñ i ozdrowienia jeœli siê pojawi³y
	  if(!is.null(new_infected))  set_values_ml(net, "state",new_infected, values ="I" )
	  if(!is.null(new_recovered))  set_values_ml(net, "state",new_recovered, values ="R" )
	 
	  
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

	# zapis wyników z e-tej iteracji  -----------------------------------------
	experimentDescription <- paste(paste(paste(e,"eksperyment",sep="_"),beta, sep ="-"))

	# zapis do pliku dat.
	write.table(SIR_group_States,file=paste("Summary_SIR",(paste(experimentDescription,".dat", sep = "")), sep=""), col.names =TRUE, sep =";", row.names = TRUE )
	write.table(timeline_SIR,file=paste("timeline_states_SIR",(paste(experimentDescription,".dat", sep = "")), sep=""), col.names =TRUE, sep =";", row.names = TRUE )

	# zapis RData
	save(list = ls(all.names = TRUE), file =paste( experimentDescription,".RData",sep=""), envir = .GlobalEnv)
}