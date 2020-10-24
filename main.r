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
library(ISLR)

################
# source files #
################
source("databaseMerge.R")




##################
# cargamos datos #
##################

###########
# Bitcoin #
###########



cor(bitcoin_price[,4:8])

#############################
# principales critpomonedas #
#############################
eth_price = read.csv("Coinbase_ETHUSD_d.csv")
ltc_price = read.csv("Coinbase_LTCUSD_d.csv")
bch_price = read.csv("Coinbase_BCHUSD_d.csv")
etc_price = read.csv("Coinbase_ETCUSD_d.csv")
link_price = read.csv("Coinbase_LINKUSD_d.csv")
rep_price = read.csv("Coinbase_REPUSD_d.csv")
xlm_price = read.csv("Coinbase_XLMUSD_d.csv")
xrp_price = read.csv("Coinbase_XRPUSD_d.csv")


#########################
# google trends bitcion #
#########################
google_trends_bitcoin = read.csv("GoogleTrends_Bitcoin_d.csv")
google_trends_bitcoin = data.frame("index"= c(1:233), "bitcoin"= google_trends_bitcoin$bitcoin[1:233] , "day" = google_trends_bitcoin$Day[1:233])
google_trends_bitcoin = google_trends_bitcoin %>% arrange(desc(index))

coins_data = data.frame("btc_trend" = google_trends_bitcoin$bitcoin[2:233], "btc_close" = btc_price$Close[1:232], "btc_open" = btc_price$Open[1:232], "btc_vol" = btc_price$Volume.BTC[2:233],"eth_open" = eth_price$Open[1:232], "eth_vol" = eth_price$Volume.ETH[2:233],"ltc_open" = ltc_price$Open[1:232], "ltc_vol" = ltc_price$Volume.LTC[2:233],"bch_open" = bch_price$open[1:232], "bch_vol" = bch_price$Volume.BCH[2:233],"etc_open" = etc_price$open[1:232], "etc_vol" = etc_price$Volume.ETC[2:233],"link_open" = link_price$open[1:232], "link_vol" = link_price$Volume.LIN[2:233],"rep_open" = rep_price$open[1:232], "rep_vol" = rep_price$Volume.REP[2:233] )
cor(coins_data)

library(HH)

########### Camino Inferencial, buscando el modelo que cumpla todas las condiciones de diagnostico 

# buscamos las multicolinealidad 
vif(lm_Bitcoin_Close) # nos da vif  >10 _DANGER_ **  btc_open, eth_open, ltc_open, ltc_vol, bch_open, etc_open, link_open **
coins_data = coins_data[,-c(3,4,6,7,8,10,12)]
coins_data = coins_data[-182,]

coins_data_y = coins_data$btc_close
coins_data = log(coins_data)
coins_data$btc_close = coins_data_y

pairs(coins_data)
cor(coins_data)


# queremos que el ultimo 70% sea de entrenamiento
dataprice.train = coins_data[(floor(length(coins_data$btc_trend)*0.03)+1):length(coins_data$btc_trend),]
# queremos que el primer 30 % sea d test
dataprice.test = coins_data[1:floor(length(coins_data$btc_trend)*0.03),]



cor(DataPrice)
# Queremos ajustar el siguiente modelo
# close_n = open_n + volumen(Lag1)_(n-1) + ... + volumen(Lag5)_(n-5) 
# entonces se borra el volumen_today
DataPrice = DataPrice[,-1]

lmClose = lm(btc_close~ ., data = dataprice.train)
summary(lmClose)

# utiliza test y predict para obtener los  RMSE
pred.dataprice <- predict(lmClose, dataprice.test, interval = "prediction")

# residual  
RMSE_dataprice = sqrt(sum((pred.dataprice[,1] - dataprice.test$btc_close )^2)/length(dataprice.test$btc_close))

5+5
a = "5*2" 
eval(parse(text=a))

lm_Bitcoin_Close = lm(btc_close~., data = coins_data) # 
summary(lm_Bitcoin_Close) # Resiual Standard error =  484.9 


library(MASS)

# Stepwise regression model
step.model <- stepAIC(lmClose, direction = "both", 
                      trace = FALSE)
summary(step.model) # Resiual Standar error =  482.9
# utiliza test y predict para obtener los  RMSE
pred.dataprice <- predict(step.model, dataprice.test, interval = "prediction")

# residual  
RMSE_dataprice = sqrt(sum((pred.dataprice[,1] - dataprice.test$btc_close )^2)/length(dataprice.test$btc_close))



glm_multi_bitcoin = glmulti(
  y = btc_close ~ btc_open+eth_open+ltc_open+etc_open+eth_vol,
  data =  coins_data,
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

fit.bitcoin = glm_multi_bitcoin@objects[[1]]
summary(fit.bitcoin)
# utiliza test y predict para obtener los  RMSE
pred.dataprice <- predict(fit.bitcoin, dataprice.test, interval = "prediction")

# residual  
RMSE_dataprice = sqrt(sum((pred.dataprice - dataprice.test$btc_close )^2)/length(dataprice.test$btc_close))



vif(step.model)

# menos variables que indica el VIF 
step.model = lm(btc_close ~ bch_open  + etc_open +  link_open +   link_vol  + rep_open , data = coins_data)
summary(lm_Bitcoin_Close) # 0.06341

step.model <- stepAIC(step.model, direction = "both", 
                      trace = FALSE)
summary(step.model) # 543.*











# rechaza normalidad , H_0 es normal
shapiro.test(step.model$residuals)
# analizamos diagnostico residuos
par(mfrow=c(2,2))
plot(step.model) # la 182 parece outlier
# homogenidad en varianza ok
ncvTest(step.model)
bptest(step.model)

# outlier 
outlierTest(step.model) # -> 182

# influyentes 
dffits(step.model)
cooks.distance(step.model)
sum(cooks.distance(step.model)>=(0.5))


# menos variables que indica el VIF 
lm_Bitcoin_Close = lm(btc_close ~ btc_trend + eth_open + bch_open  + link_vol + rep_open , data = coins_data)
summary(lm_Bitcoin_Close) # 0.06341
vif(lm_Bitcoin_Close)



# analizamos diagnostico residuos
par(mfrow=c(2,2))
plot(lm_Bitcoin_Close)

# normalidad
library(lmtest)
library(leaps)
library(nortest)





#

# hacemos el recorrido con todas las variables
coins_data = data.frame("btc_trend" = google_trends_bitcoin$bitcoin[2:233], "btc_close" = btc_price$Close[1:232], "btc_open" = btc_price$Open[1:232], "btc_vol" = btc_price$Volume.BTC[2:233],"eth_open" = eth_price$Open[1:232], "eth_vol" = eth_price$Volume.ETH[2:233],"ltc_open" = ltc_price$Open[1:232], "ltc_vol" = ltc_price$Volume.LTC[2:233],"bch_open" = bch_price$open[1:232], "bch_vol" = bch_price$Volume.BCH[2:233],"etc_open" = etc_price$open[1:232], "etc_vol" = etc_price$Volume.ETC[2:233],"link_open" = link_price$open[1:232], "link_vol" = link_price$Volume.LIN[2:233],"rep_open" = rep_price$open[1:232], "rep_vol" = rep_price$Volume.REP[2:233] )
cor(coins_data)

lm_Bitcoin_Close = lm(btc_close~., data = coins_data)
summary(lm_Bitcoin_Close) # Resiual Standard error = 363.3

#***********************************************************************************************
# Stepwise regression model
step.model <- stepAIC(lm_Bitcoin_Close, direction = "both", trace = FALSE)
summary(step.model) # Resiual Standar error = 357.6


# https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result

# Residual sum of squares:
RSS <- c(crossprod(step.model$residuals))

# Mean squared error:
MSE <- RSS / length(step.model$residuals)

# Root MSE:
RMSE <- sqrt(MSE)

# MAPE con LOG
# https://www.rdocumentation.org/packages/MLmetrics/versions/1.1.1/topics/MAPE

# data(cars)
# reg <- lm(log(dist) ~ log(speed), data = cars)
# MAPE(y_pred = exp(reg$fitted.values), y_true = cars$dist)

#******************************************************************************************

Bitcoin_close.glm <- glm(btc_close  ~ ., data = coins_data)
summary(Bitcoin_close.glm)

# croos validation
library(boot)

library(glmulti)


cv.glm(coins_data, Bitcoin_close.glm, K = 6)$delta


glm_multi_bitcoin = glmulti(
  y = btc_close ~ .*.,
  data =  coins_data,
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
fit.bitcoin = glm_multi_bitcoin@objects[[1]]
summary(fit.bitcoin)

# Residual sum of squares:
RSS <- c(crossprod(fit.bitcoin$residuals))

# Mean squared error:
MSE <- RSS / length(fit.bitcoin$residuals)

# Root MSE:
RMSE <- sqrt(MSE)  # 313.1415

vif(step.model)
#*********************************** first winner ************

# diagnostico del first winner 
par(mfrow=c(2,2))
plot(step.model)

shapiro.test(step.model$residuals)
library(car)
library(carData)
# varianza H_0 varianza constante , se rechaza varianza constante. 
ncvTest(step.model)


# hacemos el recorrido sin Btc_close
coins_data = data.frame("btc_trend" = google_trends_bitcoin$bitcoin[2:233], "btc_close" = btc_price$Close[1:232],  "btc_vol" = btc_price$Volume.BTC[2:233],"eth_open" = eth_price$Open[1:232], "eth_vol" = eth_price$Volume.ETH[2:233],"ltc_open" = ltc_price$Open[1:232], "ltc_vol" = ltc_price$Volume.LTC[2:233],"bch_open" = bch_price$open[1:232], "bch_vol" = bch_price$Volume.BCH[2:233],"etc_open" = etc_price$open[1:232], "etc_vol" = etc_price$Volume.ETC[2:233],"link_open" = link_price$open[1:232], "link_vol" = link_price$Volume.LIN[2:233],"rep_open" = rep_price$open[1:232], "rep_vol" = rep_price$Volume.REP[2:233] )
cor(coins_data)

lm_Bitcoin_Close = lm(btc_close~., data = coins_data)
summary(lm_Bitcoin_Close) # Resiual Standard error = 463.7 

# Stepwise regression model
step.model <- stepAIC(lm_Bitcoin_Close, direction = "both", 
                      trace = FALSE)
summary(step.model) # Resiual Standar error = 459.9
vif(step.model)


# revisamos que los datos no tengan faltanes
which(is.na(bitcoin_price))

##############################C:\Users\pablo\Descargas
# ajuste de la base de datos #
##############################

# Creamos las columnas con los Lag agregando un 0 por iesimo lag
btc_price = read.csv("Coinbase_BTCUSD_d.csv")
Today = btc_price$Close

timerseries = ts(bitcoin_price$Close)
# timerseries = rev(timerseries) obtiene valores rarosz
timeserie_without_tendency = diff(timerseries)
newtimeserie = diffinv(timeserie_without_tendency, xi = 1)
timeserie_without_tendency = rev(timeserie_without_tendency)



Open_dataframe = as.data.frame(timeserie_without_tendency)
Today = Open_dataframe$timeserie_without_tendency
# Today = Smarket$Today
# # index = c(1:length(Smarket$Today))
# # Smarket2 = cbind(Smarket,index)
# # Smarket2 = Smarket2 %>% arrange(desc(index))
# # Today = Smarket2$Today

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

Today = btc_price$Volume.BTC
Lag1.V = Today[2:length(Today)]
Lag2.V = Today[3:length(Today)]
Lag3.V = Today[4:length(Today)]
Lag4.V = Today[5:length(Today)]
Lag5.V = Today[6:length(Today)]


# Cortamos los extremos superiores para que tengan el mismo length
Today = Today[1:(length(Today)-5)]
Lag1.V = Lag1.V[1:(length(Lag1.V)-4)]
Lag2.V = Lag2.V[1:(length(Lag2.V)-3)]
Lag3.V = Lag3.V[1:(length(Lag3.V)-2)]
Lag4.V = Lag4.V[1:(length(Lag4.V)-1)]


# Agregamos Volumen.BTC y Volumen.USD
Open = btc_price$Open[1:(length(btc_price$Open)-5)]
# Variable explicada 
Close = btc_price$Close[1:(length(btc_price$Close)-5)]


# Base de datos para prediccion de precio de cierre en modelo continuo
# DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5, "Volumen.BTC"= Volumen.BTC, "Volumen.USD" = Volumen.USD)
DataPrice = data.frame("Lag1"=Lag1,"Lag2"=Lag2,"Lag3"=Lag3,"Lag4"=Lag4,"Lag1.V"=Lag1.V,"Lag2.V"=Lag2.V,"Lag3.V"=Lag3.V,"Lag4.V"=Lag4.V,  "Close"= Close)
DataPrice = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5)


# queremos que el ultimo 70% sea de entrenamiento
dataprice.train = DataPrice[(floor(length(Today)*0.3)+1):length(Today),]
# queremos que el primer 30 % sea d test
dataprice.test = DataPrice[1:floor(length(Today)*0.3),]



cor(DataPrice)
# Queremos ajustar el siguiente modelo
# close_n = open_n + volumen(Lag1)_(n-1) + ... + volumen(Lag5)_(n-5) 
# entonces se borra el volumen_today
DataPrice = DataPrice[,-1]

lmClose = lm(Close~ ., data = dataprice.train)
summary(lmClose)

# utiliza test y predict para obtener los  RMSE
pred.dataprice <- predict(lmClose, dataprice.test, interval = "prediction")

# residual  
RMSE_dataprice = sqrt(sum((pred.dataprice[,1] - dataprice.test$Close )^2)/length(dataprice.test$Close))


# Residual sum of squares:
RSS <- c(crossprod(lmClose$residuals))

# Mean squared error:
MSE <- RSS / length(lmClose$residuals)

# Root MSE:
RMSE <- sqrt(MSE)  # 9.239326e-12
library(devtools)
library(sjstats)

rmse(lmClose) # 9.239326e-12

library(MASS)

# Stepwise regression model
step.model <- stepAIC(lmClose, direction = "both", 
                      trace = FALSE)
summary(step.model)


##################################
# breve descriptiva de los datos #
##################################

# Vemos la correlacion de las variables excplivativas
# no hay correlaciones fuertes
cor(DataPrice [,-7])
cor(DataPrice)
summary(bitcoin_price) 
head(bitcoin_price)



#########################################
# Base RENKO para prediccion categorica #
#########################################

bitcoin_price = read.csv("Coinbase_BTCUSD_1h.csv")
cor(bitcoin_price)
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
    horas_acumuladas[length(horas_acumuladas)+1] = cantidad_horas
    
    cantidad_horas = 0
    Volume.BTC = 0
    Volume.USD = 0
    
  
  }else if (precio_actual <= cota_inferior){
    precio_escalon[length(precio_escalon)+1] = "Down"
    precio_inicial = precio_actual
    
    promedio_volumen.BTC[length(promedio_volumen.BTC)+1] = Volume.BTC/cantidad_horas
    promedio_volumen.USD[length(promedio_volumen.USD)+1] = Volume.USD/cantidad_horas
    horas_acumuladas[length(horas_acumuladas)+1] = cantidad_horas
    
    cantidad_horas = 0
    Volume.BTC = 0
    Volume.USD = 0
  }
}


DataAcumPrice = data.frame("index"= c(1:length(precio_escalon)), "Actual"=precio_escalon, "Prom.Volume.BTC"= promedio_volumen.BTC, "Horas" = horas_acumuladas)

# cambiamos el orden
DataAcumPrice = DataAcumPrice %>% arrange(desc(index))
# correlacion entre variables cuantitativas
cor(DataAcumPrice[,c(3,4)])


# Creamos las columnas con los Lag agregando un 0 por iesimo lag
Today = DataAcumPrice$Horas
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

# Agregamos Volumen.BTC y horas acumuladas
Prom.Volumen.BTC = DataAcumPrice$Prom.Volume.BTC[1:(length(DataAcumPrice$Prom.Volume.BTC)-5)]
Horas = DataAcumPrice$Horas[1:(length(DataAcumPrice$Horas)-5)]

# Base de datos para prediccion de precio de cierre en modelo continuo
DataAcumPriceWithLag = data.frame("Today"=Today, "Lag1"=Lag1, "Lag2"=Lag2, "Lag3"=Lag3, "Lag4"=Lag4,"Lag5"=Lag5, "Prom.Volumen.BTC"= Prom.Volumen.BTC, "Horas" = Horas)
cor(DataAcumPriceWithLag)
# Categorical correlation
# https://rpubs.com/hoanganhngo610/558925 #TODO leer y estudiar
required_packages <- c('MASS', 'rcompanion', 'lsr', 'vcd', 'DescTools')
for (p in required_packages) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p, dep = TRUE)
  }
}
# Lag 1 vs 2,3,4,5,today
UncertCoef(table(DataAcumPriceWithLag$Lag1, DataAcumPriceWithLag$Lag2), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag1, DataAcumPriceWithLag$Lag3), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag1, DataAcumPriceWithLag$Lag4), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag1, DataAcumPriceWithLag$Lag5), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag1, DataAcumPriceWithLag$Today), direction = "column")

# Lag 2 vs 2,3,4,5,today
UncertCoef(table(DataAcumPriceWithLag$Lag2, DataAcumPriceWithLag$Lag1), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag2, DataAcumPriceWithLag$Lag3), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag2, DataAcumPriceWithLag$Lag4), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag2, DataAcumPriceWithLag$Lag5), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag2, DataAcumPriceWithLag$Today), direction = "column")

# Lag 3 vs 2,3,4,5,today
UncertCoef(table(DataAcumPriceWithLag$Lag3, DataAcumPriceWithLag$Lag2), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag3, DataAcumPriceWithLag$Lag1), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag3, DataAcumPriceWithLag$Lag4), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag3, DataAcumPriceWithLag$Lag5), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag3, DataAcumPriceWithLag$Today), direction = "column")

# Lag 4 vs 2,3,4,5,today
UncertCoef(table(DataAcumPriceWithLag$Lag4, DataAcumPriceWithLag$Lag2), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag4, DataAcumPriceWithLag$Lag3), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag4, DataAcumPriceWithLag$Lag1), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag4, DataAcumPriceWithLag$Lag5), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag4, DataAcumPriceWithLag$Today), direction = "column")

# Lag 5 vs 2,3,4,5,today
UncertCoef(table(DataAcumPriceWithLag$Lag5, DataAcumPriceWithLag$Lag2), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag5, DataAcumPriceWithLag$Lag3), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag5, DataAcumPriceWithLag$Lag4), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag5, DataAcumPriceWithLag$Lag1), direction = "column")
UncertCoef(table(DataAcumPriceWithLag$Lag5, DataAcumPriceWithLag$Today), direction = "column")

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
# queremos que el ultimo 70% sea de entrenamiento
dataprice.train = DataAcumPriceWithLag[(floor(length(Today)*0.3)+1):length(Today),]
# queremos que el primer 30 % sea d test
dataprice.test = DataAcumPriceWithLag[1:floor(length(Today)*0.3),]



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


