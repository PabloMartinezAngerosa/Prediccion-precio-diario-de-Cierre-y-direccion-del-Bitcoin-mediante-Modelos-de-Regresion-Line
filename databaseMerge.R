##########################################################
#Deveolped by Pablo Martinez Angerosa - Vanessa Alcalde  #
##########################################################



############
# librarys #
############

library(tidyverse)


#############
# databases #
#############

# Bitcoin precio diario desde 2020/9/9 
btc_price = read.csv("Coinbase_BTCUSD_d.csv")

# principales criptomonedas diarias desde 2020/9/9 
eth_price = read.csv("Coinbase_ETHUSD_d.csv")
ltc_price = read.csv("Coinbase_LTCUSD_d.csv")
bch_price = read.csv("Coinbase_BCHUSD_d.csv")
etc_price = read.csv("Coinbase_ETCUSD_d.csv")
link_price = read.csv("Coinbase_LINKUSD_d.csv")
rep_price = read.csv("Coinbase_REPUSD_d.csv")
xlm_price = read.csv("Coinbase_XLMUSD_d.csv")
xrp_price = read.csv("Coinbase_XRPUSD_d.csv")

# Google trends de busqueda "Bitcoin" desde 2020/9/9
google_trends_bitcoin = read.csv("GoogleTrends_Bitcoin_d.csv")
google_trends_bitcoin = data.frame("index"= c(1:233), "bitcoin"= google_trends_bitcoin$bitcoin[1:233] , "day" = google_trends_bitcoin$Day[1:233])
google_trends_bitcoin = google_trends_bitcoin %>% arrange(desc(index))


#######################
# Mergeamos los datos #
#######################

# se toma los "precios del open" en el momento n
# la "cantidad de volumen" en el momento n-1
# la variable explicada "precio del close de bitcoin" esta en el momento n.
# se agregan los lags de 1 a 5 del precio del bitcion y volumen de bitcoin. 

DATABASE_COINS = data.frame(
  "btc_trend" = google_trends_bitcoin$bitcoin[2:233], 
  "btc_close" = btc_price$Close[1:232], 
  "btc_open" = btc_price$Open[1:232],
  "eth_open"  = eth_price$Open[1:232], 
  "eth_vol"   = eth_price$Volume.ETH[2:233],
  "ltc_open"  = ltc_price$Open[1:232], 
  "ltc_vol"   = ltc_price$Volume.LTC[2:233],
  "bch_open"  = bch_price$open[1:232], 
  "bch_vol"   = bch_price$Volume.BCH[2:233],
  "etc_open"  = etc_price$open[1:232], 
  "etc_vol"   = etc_price$Volume.ETC[2:233],
  "link_open" = link_price$open[1:232], 
  "link_vol"  = link_price$Volume.LIN[2:233],
  "rep_open"  = rep_price$open[1:232], 
  "rep_vol"   = rep_price$Volume.REP[2:233],
  "btc_close_lag1" = btc_price$Close[2:233],
  "btc_close_lag2" = btc_price$Close[3:234],
  "btc_close_lag3" = btc_price$Close[4:235],
  "btc_close_lag4" = btc_price$Close[5:236],
  "btc_close_lag5" = btc_price$Close[6:237],
  "btc_close_lag6" = btc_price$Close[7:238],
  "btc_close_lag7" = btc_price$Close[8:239],
  "btc_close_lag8" = btc_price$Close[9:240],
  "btc_close_lag9" = btc_price$Close[10:241],
  "btc_close_lag10" = btc_price$Close[11:242],
  "btc_vol_lag1" = btc_price$Volume.BTC[2:233],
  "btc_vol_lag2" = btc_price$Volume.BTC[3:234],
  "btc_vol_lag3" = btc_price$Volume.BTC[4:235],
  "btc_vol_lag4" = btc_price$Volume.BTC[5:236],
  "btc_vol_lag5" = btc_price$Volume.BTC[6:237],
  "btc_vol_lag6" = btc_price$Volume.BTC[7:238],
  "btc_vol_lag7" = btc_price$Volume.BTC[8:239],
  "btc_vol_lag8" = btc_price$Volume.BTC[9:240],
  "btc_vol_lag9" = btc_price$Volume.BTC[10:241],
  "btc_vol_lag10" = btc_price$Volume.BTC[11:242]
)


################################
# Base de datos para logistico #
################################

# Buscamos en el momento n, si la diferencia del open y close, es positiva (Up) o negativa (Down)

price_direction = c()

for(i in 1:nrow(DATABASE_COINS)){
  price_direction[i] = if((DATABASE_COINS[i,]$btc_close - DATABASE_COINS[i,]$btc_open) >= 0){ "Up"} else{ "Down"}
}

DATABASE_DIRECTION_Buffer = cbind(DATABASE_COINS,price_direction)

# Seleccionamos solo un grupo pre seleccionadas de variables
DATABASE_DIRECTION = data.frame(
  "btc_trend"      = DATABASE_DIRECTION_Buffer$btc_trend,
  "btc_close_lag1" = DATABASE_DIRECTION_Buffer$btc_close_lag1,
  "btc_close_lag2" = DATABASE_DIRECTION_Buffer$btc_close_lag2,
  "btc_close_lag3" = DATABASE_DIRECTION_Buffer$btc_close_lag3,
  "btc_close_lag4" = DATABASE_DIRECTION_Buffer$btc_close_lag4,
  "btc_close_lag5" = DATABASE_DIRECTION_Buffer$btc_close_lag5,
  "btc_vol_lag1"   = DATABASE_DIRECTION_Buffer$btc_vol_lag1,
  "btc_vol_lag2"   = DATABASE_DIRECTION_Buffer$btc_vol_lag2,
  "btc_vol_lag3"   = DATABASE_DIRECTION_Buffer$btc_vol_lag3,
  "btc_vol_lag4"   = DATABASE_DIRECTION_Buffer$btc_vol_lag4,
  "btc_vol_lag5"   = DATABASE_DIRECTION_Buffer$btc_vol_lag5,
  "eth_open"       = DATABASE_DIRECTION_Buffer$eth_open,
  "eth_vol"        = DATABASE_DIRECTION_Buffer$eth_vol,
  "ltc_open"       = DATABASE_DIRECTION_Buffer$ltc_open, 
  "ltc_vol"        = DATABASE_DIRECTION_Buffer$ltc_vol,
  "price_direction"= DATABASE_DIRECTION_Buffer$price_direction
)








