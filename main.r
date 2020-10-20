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



library(tidyverse)
library(lubridate)
library(nycflights13)

bitcoin_price = read.csv("Coinbase_BTCUSD_1h.csv")

##################################
# breve descriptiva de los datos #
##################################


summary(bitcoin_price) 
head(bitcoin_price)

# revisamos que los datos no tengan faltanes
which(is.na(bitcoin_price))

##############################
# ajuste de la base de datos #
##############################


# Creamos las columnas con los Lag agregando un 0 por iesimo lag
Today = bitcoin_price$Close
Lag1 = Today[2:length(Today)]
Lag2 = Today[3:length(Today)]
Lag3 = Today[4:length(Today)]
Lag4 = Today[5:length(Today)]
Lag5 = Today[6:length(Today)]


# Cortamos los extremos superiores para que tengan el mismo length
Today = Today[1:(length(Today)-5)]
Lag1 = Lag1[1:(length(Lag1)-4)]
Lag2 = Lag2[1:(length(Lag2)-3)]
Lag3 = Lag3[1:(length(Lag3)-2)]
Lag4 = Lag4[1:(length(Lag4)-1)]

# Agregamos Volumen.BTC y Volumen.USD
Volumen.BTC = bitcoin_price$Volume.BTC[1:(length(bitcoin_price$Volume.BTC)-5)]
Volumen.USD = bitcoin_price$Volume.USD[1:(length(bitcoin_price$Volume.USD)-5)]

# Base de datos para prediccion de precio de cierre en modelo continuo
DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5, "Volumen.BTC"= Volumen.BTC, "Volumen.USD" = Volumen.USD)

###################################
# Base para prediccion categorica #
###################################

# pseudocodigo. #

# definimos delta_del_precio
# definimos precio_inicial = X_0

# hacemos un for dese el primer precio de cierre hasta adelante
  # cota_superior = precio_inicial + delta_del_precio.
  # cota_inferior = precio_inicial - delta_del_precio
  # si precio_actual >= cota_superior
           # subio se agrega
           # precio_inicial = precio_actual
  # elseif precio_actual <= cota_inferior
          # bajo se agrega
          # precio_inicial = precio_actual
  # precio_acumulado sumamos con el precio anterior


# delta en dolares
delta_precio = 10
precio_inicial = bitcoin_price$Close[length(bitcoin_price$Close)]
precio_escalon = c() 

promedio_volumen.BTC = c()
Volume.BTC = 0

promedio_volumen.USD = c()
Volume.USD = 0

horas_acumuladas = c()
cantidad_horas = 0

for(i in c(0:(length(bitcoin_price$Close) - 1 ))){
  
  cota_superior = precio_inicial + delta_precio
  cota_inferior = precio_inicial - delta_precio
  precio_actual = bitcoin_price$Close[length(bitcoin_price$Close)-i]
  
  cantidad_horas = cantidad_horas + 1 
  Volume.BTC = Volume.BTC  + bitcoin_price$Volume.BTC[length(bitcoin_price$Volume.BTC)-i] 
  Volume.USD = Volume.USD  + bitcoin_price$Volume.USD[length(bitcoin_price$Volume.USD)-i] 
  
  if (precio_actual >= cota_superior){
    precio_escalon[length(precio_escalon)+1] = "Up"
    precio_inicial = precio_actual
    
    promedio_volumen.BTC[length(promedio_volumen.BTC)+1] = Volume.BTC/cantidad_horas
    promedio_volumen.USD[length(promedio_volumen.USD)+1] = Volume.USD/cantidad_horas
    cantidad_horas[length(cantidad_horas)+1] = cantidad_horas
    
    cantidad_horas = 0
    Volume.BTC = 0
    Volume.USD = 0
    
  
  }else if (precio_actual <= cota_inferior){
    precio_escalon[length(precio_escalon)+1] = "Down"
    precio_inicial = precio_actual
    
    promedio_volumen.BTC[length(promedio_volumen.BTC)+1] = Volume.BTC/cantidad_horas
    promedio_volumen.USD[length(promedio_volumen.USD)+1] = Volume.USD/cantidad_horas
    cantidad_horas[length(cantidad_horas)+1] = cantidad_horas
    
    cantidad_horas = 0
    Volume.BTC = 0
    Volume.USD = 0
  }
}


DataAcumPrice = data.frame("index"= c(1:length(precio_escalon)), "Actual"=precio_escalon, "Prom.Volume.BTC"= promedio_volumen.BTC, "Prom.Volume.USD" = promedio_volumen.USD)

# cambiamos el orden
DataAcumPrice = DataAcumPrice %>% arrange(desc(index))


# Creamos las columnas con los Lag agregando un 0 por iesimo lag
Today = DataAcumPrice$Actual
Lag1 = Today[2:length(Today)]
Lag2 = Today[3:length(Today)]
Lag3 = Today[4:length(Today)]
Lag4 = Today[5:length(Today)]
Lag5 = Today[6:length(Today)]


# Cortamos los extremos superiores para que tengan el mismo length
Today = Today[1:(length(Today)-5)]
Lag1 = Lag1[1:(length(Lag1)-4)]
Lag2 = Lag2[1:(length(Lag2)-3)]
Lag3 = Lag3[1:(length(Lag3)-2)]
Lag4 = Lag4[1:(length(Lag4)-1)]

# Agregamos Volumen.BTC y Volumen.USD
Prom.Volumen.BTC = DataAcumPrice$Prom.Volume.BTC[1:(length(DataAcumPrice$Prom.Volume.BTC)-5)]
Prom.Volumen.USD = DataAcumPrice$Prom.Volume.USD[1:(length(DataAcumPrice$Prom.Volume.USD)-5)]

# Base de datos para prediccion de precio de cierre en modelo continuo
DataAcumPriceWithLag = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5, "Prom.Volumen.BTC"= Prom.Volumen.BTC, "Prom.Volumen.USD" = Prom.Volumen.USD)

# DataAcumPriceWithLag$Lag1 = as.factor(DataAcumPriceWithLag$Lag1)
# DataAcumPriceWithLag$Lag2 = as.factor(DataAcumPriceWithLag$Lag2)
# DataAcumPriceWithLag$Lag3 = as.factor(DataAcumPriceWithLag$Lag3)
# DataAcumPriceWithLag$Lag4 = as.factor(DataAcumPriceWithLag$Lag4)
# DataAcumPriceWithLag$Lag5 = as.factor(DataAcumPriceWithLag$Lag5)

# # Creamos las direcciones de los precios diarios
# Direction = c(rep("Up",length(Today)))
# 
# for(index in c(2:length(Today))){
#   Direction[index] = if(Today[index]> Today[index-1]) "Up" else "Down"
# }
# 
# # Hacemos el Dataframe Final    
# DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5 , "Direction" = Direction)



library(glmulti)


# separamos los dataset para test y entranamiento 
dataprice.train = DataAcumPriceWithLag[1:floor(length(Today)*0.7),]
dataprice.test = DataAcumPriceWithLag[(floor(length(Today)*0.7)+1):length(Today),]



glm_fit3 = glmulti(
  y = Today ~ .*.,
  data =  dataprice.train,
  family = binomial,
  method = "g",
  plotty = FALSE,
  report = TRUE,
  marginality = TRUE,
  deltaB = 0,
  deltaM = 0.01,
  conseq = 6,
  sexrate = 0.15,
  imm = 0.2
) 
fit = glm_fit3@objects[[1]]
glm.probs = predict(fit,dataprice.test, type="response")

glm.pred = rep("Down",length(dataprice.test$Today))
glm.pred[glm.probs >.5] = "Up"
table(glm.pred ,dataprice.test$Today)



# TODO 5) Enjoy your money!

# dates <- c("May 27 1984", "July 7 2005")
# betterDates <- as.Date(dates, "%B %d %Y")
# dates <- c("05/27/84", "07/07/05", "08/17/20")
# betterDates <- as.Date(dates, "%m/%d/%y")
