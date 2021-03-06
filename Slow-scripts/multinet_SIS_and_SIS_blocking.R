# biblioteka 
library(multinet)

listBeta <- c(0.19,0.28,0.22)
listgamma <-c(0.1,0.08,0.02)

networkName <- "Lazega"
countryDirectory <-"Italy"
scritpType<-"SIS&SIR_blocking"

for(value in 1:length(listBeta))
{
  if(value == 1){
    
    # Parametry zapisu eksperymentu  -------------------------------------------------
    # # Folder roboczy
    setwd("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/")
    getwd()
    #zmienne pomocniecze do zapisu
    experimentsMainDirectory<- paste("SIR&SIS",networkName, sep="-")
    if(dir.exists(experimentsMainDirectory) == FALSE) dir.create(experimentsMainDirectory)
    #folder dla eksperyment�w 
    setwd(paste("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/", experimentsMainDirectory, sep=""))
    if(dir.exists(countryDirectory) == FALSE) dir.create(countryDirectory)
    setwd(paste(getwd(), countryDirectory,sep="/"))
  }
  setwd(paste(paste("C:/Users/Paulina/Documents/MasterThesis/Eksperiments/",experimentsMainDirectory,sep=""),countryDirectory,sep="/"))  
  mainDirectory <-paste(listBeta[value],listgamma[value], sep = "-")
  if(dir.exists(mainDirectory) == FALSE) dir.create(mainDirectory)
  setwd(paste(getwd(),mainDirectory, sep="/") )
  if(dir.exists(scritpType) == FALSE) dir.create(scritpType)
  setwd(paste(getwd(),scritpType, sep="/") )
  getwd()


experimentsNumber <- 20

for(e in 1: experimentsNumber)
{
	#net <- ml_aucs()
  net <-read_ml("C:/Users/Paulina/Downloads/FullNet/Lazega-Law-Firm_4NoNatureNoLoops.edges", name="LAZEGA", sep=',', aligned=FALSE)
	AllLayers <- layers_ml(net)

	# aktualna warstwa
	layerName <- "co-work"

	#parametry sieci
	numberOfActors <- num_actors_ml(net)
	numberOfActorsInLayer <- num_actors_ml(net,layerName)
	layerActors <- actors_ml(net,layerName)
	networkActors <- actors_ml(net)

	# definicje zmiennych dla modeli
	#czas trwania "epidemii" -  liczba dni
	time <- 150
	
	# prawdopodobie�stwa SIR
	beta <- listBeta[value] # zara�enia
	betaI <- beta /10 #je�li SIS w stanie I  
	gamma <- listgamma[value] # wyzdrowienia
	
	# prawdopodobie�stwa SIS
	epsilon <- beta * 2 # odpowiednik beta, uzuskania informacji
	epsilonI <- 0.692 # je�li SIR w stanie I Japan  0.692 lub DiamonPrincess 0.821 
	mi <- gamma * 2    # zwatpienia
	blockingTime <- 21

	#Stan SIR
	numberOfSusceptible <- numberOfActorsInLayer
	numberOfInfected  <- 0 #
	numberOfRecovered <- 0 # ozdrowie�cy
	SIR_Sum<- numberOfSusceptible + numberOfInfected + numberOfRecovered 

	#Stan SIS
	numberOfUnawarened <-numberOfActors
	numberOfAwarened <- 0
	SIS_Sum <- numberOfAwarened + numberOfUnawarened
	 
	 # Stan pocz�tkowy dla macierzy liczno�ci 
	SIR_group_States <- matrix(rbind(0,numberOfSusceptible, numberOfInfected, numberOfRecovered, SIR_Sum))
	SIS_group_States <- matrix (rbind(0, numberOfUnawarened, numberOfAwarened, SIS_Sum))

	# zmienne pomocnicze
	new_infected <- NULL # nowe zachorowania
	new_recovered <- NULL # nowe ozdrowienia

	new_awarened <- NULL # �wiadomi S
	new_unawarened <- NULL # nie�wiadomi versus wypieraj�cy

	#dodanie atrybut�w (state -stan dla SIR, SIRbeta - prawdopodobie�stwo)
	add_attributes_ml(net, "state", type = "string", target="actor", layer ="")
	add_attributes_ml(net, "beta", type = "numeric", target = "actor", layer = "")
	add_attributes_ml(net, "awareness", type ="string", target ="actor", layer = "")
	add_attributes_ml(net, "epsilon", type = "numeric", target = "actor", layer = "")


	#ustawienie warto�ci atrybutu state dla SIR domy�lnie na S
	set_values_ml(net, "state",actors_ml(net,layerName), values ="S" )
	set_values_ml(net, "awareness",actors_ml(net), values ="S" )
	set_values_ml(net, "beta",actors_ml(net), values = beta)
	set_values_ml(net, "epsilon",actors_ml(net), values = epsilon)

	#sprawdzenie ustawionych warto�ci 
	#get_values_ml(net,"state",actors_ml(net))
	#get_values_ml(net,"awareness",actors_ml(net))
	#get_values_ml(net,"epsilon",actors_ml(net))
	#get_values_ml(net,"beta",actors_ml(net))


	# stan pocz�tkowy dla I ---------------------------------------------------
	# (A)losowo X os�b 
	# x<-5 
	# infected <- trunc(runif(x,1,215))
	# while (n>0)
	# { (print (infected[n]))
	#   set_values_ml(net, "state",infected[n], values ="I" )
	#  print(get_values_ml(net, "state",infected[n]))
	#  n=n-1
	# }

	# (B)losowo x % sieci - preferowane np 1% - liczymi ile to aktor�w w sieci a potem losujemy tylu aktor�w jako seedy
	x<-0.01
	n<- round( x * num_actors_ml(net,layerName)) # dla warstwy x % aktor�w z wybranej warstwy
	infected <- trunc(runif(n,1,numberOfActorsInLayer))

	# Aktualizowanie stanu SIR
	numberOfInfected <- n
	numberOfSusceptible <- numberOfSusceptible - numberOfInfected

	# zainfekowanie wylosowanych aktor�w
	while (n>0)
	{ print( paste("infekowanie w toku - aktor :", infected[n]))
	  set_values_ml(net, "state",layerActors[infected[n]], values ="I" )
	  n=n-1
	}

	#u�wiadomienie wybranych os�b 
	x<-0.01
	m<- round( x * num_actors_ml(net)) # x % aktor�w z ca�ej sieci
	awarened <- trunc(runif(m,1,numberOfActorsInLayer))

	# Aktualizowanie stanu SIS
	numberOfAwarened <- m
	numberOfUnawarened <- numberOfUnawarened - numberOfAwarened

	while (m>0)
	{ print( paste("u�wiadamianie w toku - aktor :", awarened[m]))
	  set_values_ml(net, "awareness", networkActors[awarened[m]], values ="I" )
	  m=m-1
	  
	}

	timeline_SIR <- as.matrix(layerActors)
	timeline_SIR <- cbind(timeline_SIR, get_values_ml(net,"state", layerActors ))
	timeline_SIS <- as.matrix(networkActors)
	timeline_SIS <- cbind(timeline_SIS, get_values_ml(net,"awareness",networkActors))

	for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzie�
	{  
	  # wypisuje - Stan SIR na konsole 
	  SIR_group_States <- cbind(SIR_group_States,rbind(i,numberOfSusceptible,numberOfInfected,numberOfRecovered, SIR_Sum))
	  print(paste("Dzie� epidemii:", i)) 
	  print(paste("Stan SIR:", paste( paste( paste("Susceptible:", numberOfSusceptible),paste("Infected:", numberOfInfected), sep = " ; "),paste("Recovered:", numberOfRecovered),sep =" ; ")))
	  
	  if(numberOfRecovered == numberOfActorsInLayer) break 
	  
	  new_infected <- NULL
	  new_recovered <- NULL
	  
	  # P�tla SIR
	  for (j in 1:length(layerActors)) # odwiedzam po kolei aktor�w
	  { 
		if(get_values_ml(net,"state",layerActors[j]) =="I") # je�li aktor jest zara�ony
		{ 
		  # szukamy s�siad�w 
		  neighbors <- neighbors_ml(net,layerActors[j],layerName,mode="all")
		  for(s in 1:length(neighbors))
		  {  if(get_values_ml(net, "state", neighbors[s])=="S")
				{ 
				if(get_values_ml(net, "awareness", neighbors[s])== "I") 
					{ 
					set_values_ml(net, "beta",neighbors[s], values = betaI)
					}
					
					if(runif(1) < get_values_ml(net, "beta", neighbors[s])) 
					{ #print( value)
					if(!(neighbors[s] %in% new_infected)) # is.element(neighbors[s])
						new_infected <- cbind(new_infected,neighbors[s]) # mamy tymczasow� list� nowo zainfekowanych  							
					}
				}
									
		  }
		  
		  if( runif(1) < gamma)
		  { #print(test)
			if(!is.element(layerActors[j],new_recovered))
			{
			new_recovered=cbind(new_recovered,layerActors[j])
			}
		  }
		}
		
	  }
	  
	  print( paste("Stan SIS:", paste( paste( paste("Susceptible:", numberOfUnawarened),paste("Infected:", numberOfAwarened), sep = " ; "))))
	  SIS_group_States <- cbind(SIS_group_States,rbind(i,numberOfUnawarened,numberOfAwarened, SIS_Sum))
	  
	  new_awarened <- NULL # �wiadomi S
	  new_unawarened <- NULL # nie�wiadomi versus wypieraj�cy
	  if(i>blockingTime)
		 { #P�tla dla SIS
		  for(k in 1: length(networkActors))
		  {
			if(get_values_ml(net,"state",networkActors[k]) =="I")
			{ 
			  # poszukiwanie s�siad�w
			  actorNeighbors <- neighbors_ml(net,networkActors[k],layerName,mode="all")
			  for(l in 1:length(actorNeighbors))
			  {  
				if(get_values_ml(net, "awareness", actorNeighbors[l])=="S")
				{  if(get_values_ml(net, "state", actorNeighbors[l])=="I")
					{	 
						set_values_ml(net, "epsilon",actorNeighbors[l], values = epsilonI)
					}
					 
					if( runif(1) < get_values_ml(net,"epsilon",actorNeighbors[l]))
					{ #print( value)
						if(!(actorNeighbors[l] %in% new_awarened)) # is.element(neighbors[s])
						new_awarened <- cbind(new_awarened,actorNeighbors[l]) # mamy tymczasow� list� nowo zainfekowanych  							
					}
				}		   
			  }
			  if( runif(1) < mi)
			  { #print(test)
				if(!is.element(networkActors[k],new_unawarened)){ new_unawarened=cbind(new_unawarened,networkActors[k])}
				
			  }
			}
			
		  }
	  
	  }
	  
	  # aktualizacja nowych zaka�e� i ozdrowienia je�li si� pojawi�y
	  if(!is.null(new_infected))  set_values_ml(net, "state",new_infected, values ="I" )
	  if(!is.null(new_recovered))  set_values_ml(net, "state",new_recovered, values ="R" )
	  	  
	  #aktualizacja SIS    
	  if(!is.null(new_awarened))  set_values_ml(net, "awareness",new_awarened, values ="I" )
	  if(!is.null(new_unawarened))  set_values_ml(net, "awareness",new_unawarened, values ="S" )
	  
	   #Sprawdzenie stanu atrybut�w - zawarto�� wektora 
	  SIR_attributes <- get_values_ml(net,"state", layerActors)
	  numberOfSusceptible <- length( which('S' == SIR_attributes))
	  numberOfInfected <- length( which('I' == SIR_attributes))
	  numberOfRecovered <- length( which('R' == SIR_attributes))
	  Sum = numberOfSusceptible + numberOfInfected + numberOfRecovered
	  
	  SIS_atributes <- get_values_ml(net, "awareness", networkActors)  
	  numberOfUnawarened <- length(which("S"==SIS_atributes))
	  numberOfAwarened <- length(which("I"== SIS_atributes))
	  SIS_Sum <- numberOfAwarened + numberOfUnawarened 
	  
	  # zapis stan�w po�rednich 
	  timeline_SIR <- cbind(timeline_SIR, SIR_attributes)
	  timeline_SIS <- cbind(timeline_SIS, SIS_atributes)
	}
	SIR_group_States <- t(SIR_group_States)
	SIS_group_States <- t(SIS_group_States)
	#get_values_ml(net,"beta",actors_ml(net))
	#get_values_ml(net,"epsilon",actors_ml(net))

	# zapis wynik�w z e-tej iteracji  -----------------------------------------
	experimentDescription <- paste("eksperymentData",e, sep="-")
	
	# zapis do pliku dat.
	write.table(SIR_group_States,file=paste("Summary_SIR",(paste(e,".dat", sep = "")), sep="-"), col.names =TRUE, sep =";", row.names = TRUE )
	write.table(timeline_SIR,file=paste("timelineStates_SIR",(paste(e,".dat", sep = "")), sep="-"), col.names =TRUE, sep =";", row.names = TRUE )
	
	# zapis do pliku dat.
	write.table(SIS_group_States,file=paste("Summary_SIS",(paste(e,".dat", sep = "")), sep="-"), col.names =TRUE, sep =";", row.names = TRUE )
	write.table(timeline_SIS,file=paste("timelineStates_SIS",(paste(e,".dat", sep = "")), sep="-"), col.names =TRUE, sep =";", row.names = TRUE )
	
	# zapis RData
	save(list = ls(all.names = TRUE), file =paste( experimentDescription,".RData",sep=""), envir = .GlobalEnv)
	
}
}