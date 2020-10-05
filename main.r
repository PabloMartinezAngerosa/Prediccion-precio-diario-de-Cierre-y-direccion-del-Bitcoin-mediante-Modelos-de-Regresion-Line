#################################################################################

        ###########################################################
        #Deveolped by Pablo Martinez Angerosa - Vanessa Gillespie #
        ###########################################################



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%,        ...../%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%#               ........%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%.                  ..........%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%                      ...........@%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%#                       ............@@%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%                        .............@@@@%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%                         ..............@@@@@%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%                        ...............@@@@@@@@%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%                      .................@@@@@@@@@@%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%      ,@@@@@@@@@     ..@@@@@@@@@....../@@@@@@@@@@@@@%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%    (@@@@@@@@@@    ...@@@@@@@@@@.....@@@@@@@@@@@@@@@@%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%    @@@@@@@@@#  @@@..@@@@@@@@@@....@@@@@@@@@@@@@@@@@@@@%%
# %%%%%%%%%%%%%%%%%%%%%%%      %,      @@@@@@@....../%......@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%             @@@@@@@@@.............@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%(   /       @@@@@@@@@.......%...@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%.  @@@           ........@@@..%@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%  @@@            .....* .#(..%,(*%#%%%%*.%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%   @@@,          .....(%,# . %%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%            ,%.%............*@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%#                ...........@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%               ..........@@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%            .........@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@.      .....*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# #########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# TODO 1) Leer csv de los datasets

library(tidyverse)
library(lubridate)
library(nycflights13)

bitcoin_price = read.csv("bitcoin_price.csv")
bitcoin_dataset = read.csv("bitcoin_dataset.csv")

summary(bitcoin_price)  
bitcoin_dataset$btc_market_price

head(bitcoin_price)
head(bitcoin_dataset)
# TODO 2) Mergear en 1 sola datset

# Creamos las columnas con los Lag agregando un 0 por iesimo lag
Today = bitcoin_dataset$btc_market_price[176:length(bitcoin_dataset$btc_market_price)]
Lag1 = c(0,Today)
Lag2 = c(0,Lag1)
Lag3 = c(0,Lag2)
Lag4 = c(0,Lag3)
Lag5 = c(0,Lag4)

# Cortamos todas las entradas con 0 iniciales para comencar en el mismo i esimo momento
Today = Today[6:length(Today)]
Lag1 = Lag1[6:length(Lag1)]
Lag2 = Lag2[6:length(Lag2)]
Lag3 = Lag3[6:length(Lag3)]
Lag4 = Lag4[6:length(Lag4)]
Lag5 = Lag5[6:length(Lag5)]

# Cortamos los extremos superiores para que tengan el mismo length
Lag1 = Lag1[1:(length(Lag1)-1)]
Lag2 = Lag2[1:(length(Lag2)-2)]
Lag3 = Lag3[1:(length(Lag3)-3)]
Lag4 = Lag4[1:(length(Lag4)-4)]
Lag5 = Lag5[1:(length(Lag5)-5)]


DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5 )

# Creamos las direcciones de los precios diarios
Direction = c(rep("Up",length(Today)))

for(index in c(2:length(Today))){
  Direction[index] = if(Today[index]> Today[index-1]) "Up" else "Down"
}

# Hacemos el Dataframe Final    
DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5 , "Direction" = Direction)



# DONE 3) Agregar una columna con las direcciones de precio

# TODO 4) Integrar codigo de clasification stock market y ver resultados con algoritmo loco!



# TODO 5) Enjoy your money!

# dates <- c("May 27 1984", "July 7 2005")
# betterDates <- as.Date(dates, "%B %d %Y")
# dates <- c("05/27/84", "07/07/05", "08/17/20")
# betterDates <- as.Date(dates, "%m/%d/%y")
