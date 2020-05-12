# biblioteka 
library(multinet)

# definicja seed
set.seed(1313)

# wczytanie sieci 
fullnet <- read_ml("C:/Users/Paulina/Downloads/FullNet/CKM-Physicians-Innovation_4NoNature.edges", name="CKM", sep=',', aligned=FALSE)

# definicje zmiennych
#czas trwania "epidemii" -  liczba dni
time <- 100

# prawdopodobieñstwa
beta <- 0.85 # zara¿enia
gamma <- 0.03 # wyzdrowienia

#Stan SIR
numberOfSusceptible <- num_actors_ml(fullnet, "advice")
numberOfInfected  <- 0 #
numberOfRecovered <- 0 # ozdrowieñcy

# zmienne pomocnicze
new_infected <-NULL # nowe zachorowania
new_recovered <-NULL # nowe ozdrowienia

#dodanie atrybutów (state -stan dla SIR, beta - prawdopodobieñstwo)
add_attributes_ml(fullnet, "state", type = "string", target="actor", layer ="")
add_attributes_ml(fullnet, "beta", type = "numeric", target = "actor", layer = "")

#ustawienie wartoœci atrybutu state dla SIR domyœlnie na S
set_values_ml(fullnet, "state",actors_ml(fullnet,"advice"), values ="S" )
get_values_ml(fullnet,"state",actors_ml(fullnet, "advice"))


# set_values_ml(fullnet, "beta",actors_ml(fullnet), values = 0.3)
# get_values_ml(fullnet, "beta", actors_ml(fullnet))
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
x<-0.05
n<- trunc( x * num_actors_ml(fullnet)) # dla warstwy n<- trunc( x * num_actors_ml(fullnet, "advice"))
infected <- trunc(runif(n,1,215))

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


timeline<- get_values_ml(fullnet,"state",actors_ml(fullnet,"advice"))

#dodatkowy warunek na wyjœcie jak wszyscy s¹ w R
 #dodatkowy warunek na wyjœcie jak wszyscy s¹ w R

	  for(i in 1:time ) # odliczamy kolejne dni 1 iteracja - 1 dzieñ
	{  # wipisuje - Stan SIR na konsole 
	   print(paste("Dzieñ epidemii:", i)) 
	   print(paste("Susceptible", numberOfSusceptible)) 
	   print(paste("Infected", numberOfInfected)) 
	   print(paste("Recovered", numberOfRecovered)) 
	   
	   if(numberOfRecovered == num_actors_ml(fullnet, "advice")) break 
	   new_infected <- NULL
	   new_recovered <- NULL
	  
			for (j in 1:length(advice)) # odwiedzam po kolei aktorów
			{ 
				if(get_values_ml(fullnet,"state",advice[j]) =="I") # jeœli aktor jest zara¿ony
				{ 
					# szukamy s¹siadów 
					neighbors <- neighbors_ml(fullnet,advice[j],"advice",mode="all")
					for(s in 1:length(neighbors))
					{  if(get_values_ml(fullnet, "state", neighbors[s])=="S")
						{ value <- runif(1,0,1000)
						  
							if( value <= beta*1000) 
							  { #print( value)
								new_infected <- cbind(new_infected,neighbors[s]) # mamy tymczasow¹ listê nowo zainfekowanych  
								# numberOfInfected = numberOfInfected + 1 
							#	print(paste("nowe zachorowanie, nadal zdrowi",numberOfSusceptible))
								# if(numberOfSusceptible > 0) numberOfSusceptible = numberOfSusceptible - 1
								}
					  }
					  if(!is.null(new_infected))  set_values_ml(fullnet, "state",new_infected, values ="I" ) 				   
					}
					
					test<- runif(1,0,1000)
					
				if( test<= gamma*1000)
					# ustaw recovered
					{ #print(test)
					  new_recovered=cbind(new_recovered,advice[j])
					  # numberOfRecovered =numberOfRecovered + 1
					  # numberOfInfected = numberOfInfected - 1 
					 #print(paste("nowy ozdrowieniec, jeszce choruje:", numberOfInfected))
					}
	   }
	}
	   # aktualizacja nowych zaka¿eñ i ozdrowienia jeœli siê pojawi³y
	   	 # if(!is.null(new_infected))  set_values_ml(fullnet, "state",new_infected, values ="I" )
	      if(!is.null(new_recovered))  set_values_ml(fullnet, "state",new_recovered, values ="R" )
	      #print(new_infected)
	      print (paste("new R", length(new_recovered)))
	      print(paste("new I", length(new_infected)))
	      
		  # Obliczenia stan SIR
		  numberOfSusceptible = numberOfSusceptible - length(new_infected)
	      numberOfInfected = numberOfInfected +(length(new_infected) - length(new_recovered))
	      numberOfRecovered = numberOfRecovered + length(new_recovered)
	      SUM <- numberOfInfected + numberOfRecovered + numberOfSusceptible
	      print(paste("Suma", SUM))
	  # zapis stanów poœrednich 
	  memory <- get_values_ml(fullnet,"state",actors_ml(fullnet,"advice"))
	  
	  timeline <- cbind(timeline,memory)
	}



